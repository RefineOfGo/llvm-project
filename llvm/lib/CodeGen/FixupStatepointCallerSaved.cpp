//===-- FixupStatepointCallerSaved.cpp - Fixup caller saved registers  ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Statepoint instruction in deopt parameters contains values which are
/// meaningful to the runtime and should be able to be read at the moment the
/// call returns. So we can say that we need to encode the fact that these
/// values are "late read" by runtime. If we could express this notion for
/// register allocator it would produce the right form for us.
/// The need to fixup (i.e this pass) is specifically handling the fact that
/// we cannot describe such a late read for the register allocator.
/// Register allocator may put the value on a register clobbered by the call.
/// This pass forces the spill of such registers and replaces corresponding
/// statepoint operands to added spill slots.
///
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/FixupStatepointCallerSaved.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/LiveDebugVariables.h"
#include "llvm/CodeGen/LiveIntervals.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/CodeGen/SlotIndexes.h"
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/VirtRegMap.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Debug.h"
#include <cstdlib>
#include <iterator>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "fixup-statepoint-caller-saved"
STATISTIC(NumSpilledRegisters, "Number of spilled register");
STATISTIC(NumSpillSlotsAllocated, "Number of spill slots allocated");
STATISTIC(NumSpillSlotsExtended, "Number of spill slots extended");

static cl::opt<bool> FixupSCSExtendSlotSize(
    "fixup-scs-extend-slot-size", cl::Hidden, cl::init(false),
    cl::desc("Allow spill in spill slot of greater size than register size"),
    cl::Hidden);

static cl::opt<bool> PassGCPtrInCSR(
    "fixup-allow-gcptr-in-csr", cl::Hidden, cl::init(false),
    cl::desc("Allow passing GC Pointer arguments in callee saved registers"));

static cl::opt<bool> EnableCopyProp(
    "fixup-scs-enable-copy-propagation", cl::Hidden, cl::init(true),
    cl::desc("Enable simple copy propagation during register reloading"));

// This is purely debugging option.
// It may be handy for investigating statepoint spilling issues.
static cl::opt<unsigned> MaxStatepointsWithRegs(
    "fixup-max-csr-statepoints", cl::Hidden,
    cl::desc("Max number of statepoints allowed to pass GC Ptrs in registers"));

namespace {

struct FixupStatepointCallerSavedImpl {
  bool run(MachineFunction &MF);
};

class FixupStatepointCallerSavedLegacy : public MachineFunctionPass {
public:
  static char ID;

  FixupStatepointCallerSavedLegacy() : MachineFunctionPass(ID) {
    initializeFixupStatepointCallerSavedLegacyPass(
        *PassRegistry::getPassRegistry());
  }
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  StringRef getPassName() const override {
    return "Fixup Statepoint Caller Saved";
  }

  bool runOnMachineFunction(MachineFunction &MF) override;
};

} // End anonymous namespace.

char FixupStatepointCallerSavedLegacy::ID = 0;
char &llvm::FixupStatepointCallerSavedID = FixupStatepointCallerSavedLegacy::ID;

INITIALIZE_PASS_BEGIN(FixupStatepointCallerSavedLegacy, DEBUG_TYPE,
                      "Fixup Statepoint Caller Saved", false, false)
INITIALIZE_PASS_END(FixupStatepointCallerSavedLegacy, DEBUG_TYPE,
                    "Fixup Statepoint Caller Saved", false, false)

// Utility function to get size of the register.
static unsigned getRegisterSize(const TargetRegisterInfo &TRI, Register Reg) {
  const TargetRegisterClass *RC = TRI.getMinimalPhysRegClass(Reg);
  return TRI.getSpillSize(*RC);
}

// Try to eliminate redundant copy to register which we're going to
// spill, i.e. try to change:
//    X = COPY Y
//    SPILL X
//  to
//    SPILL Y
//  If there are no uses of X between copy and STATEPOINT, that COPY
//  may be eliminated.
//  Reg - register we're about to spill
//  RI - On entry points to statepoint.
//       On successful copy propagation set to new spill point.
//  IsKill - set to true if COPY is Kill (there are no uses of Y)
//  Returns either found source copy register or original one.
static Register performCopyPropagation(Register Reg,
                                       MachineBasicBlock::iterator &RI,
                                       bool &IsKill, const TargetInstrInfo &TII,
                                       const TargetRegisterInfo &TRI) {
  // First check if statepoint itself uses Reg in non-meta operands.
  int Idx = RI->findRegisterUseOperandIdx(Reg, &TRI, false);
  if (Idx >= 0 && (unsigned)Idx < StatepointOpers(&*RI).getNumDeoptArgsIdx()) {
    IsKill = false;
    return Reg;
  }

  if (!EnableCopyProp)
    return Reg;

  MachineBasicBlock *MBB = RI->getParent();
  MachineBasicBlock::reverse_iterator E = MBB->rend();
  MachineInstr *Def = nullptr, *Use = nullptr;
  for (auto It = ++(RI.getReverse()); It != E; ++It) {
    if (It->readsRegister(Reg, &TRI) && !Use)
      Use = &*It;
    if (It->modifiesRegister(Reg, &TRI)) {
      Def = &*It;
      break;
    }
  }

  if (!Def)
    return Reg;

  auto DestSrc = TII.isCopyInstr(*Def);
  if (!DestSrc || DestSrc->Destination->getReg() != Reg)
    return Reg;

  Register SrcReg = DestSrc->Source->getReg();

  if (getRegisterSize(TRI, Reg) != getRegisterSize(TRI, SrcReg))
    return Reg;

  LLVM_DEBUG(dbgs() << "spillRegisters: perform copy propagation "
                    << printReg(Reg, &TRI) << " -> " << printReg(SrcReg, &TRI)
                    << "\n");

  // Insert spill immediately after Def
  RI = ++MachineBasicBlock::iterator(Def);
  IsKill = DestSrc->Source->isKill();

  if (!Use) {
    // There are no uses of original register between COPY and STATEPOINT.
    // There can't be any after STATEPOINT, so we can eliminate Def.
    LLVM_DEBUG(dbgs() << "spillRegisters: removing dead copy " << *Def);
    Def->eraseFromParent();
  } else if (IsKill) {
    // COPY will remain in place, spill will be inserted *after* it, so it is
    // not a kill of source anymore.
    const_cast<MachineOperand *>(DestSrc->Source)->setIsKill(false);
  }

  return SrcReg;
}

namespace {
// Pair {Register, FrameIndex}
using RegSlotPair = std::pair<Register, int>;

// Keeps track of what reloads were inserted in MBB.
class RegReloadCache {
  using ReloadSet = SmallSet<RegSlotPair, 8>;
  DenseMap<const MachineBasicBlock *, ReloadSet> Reloads;

public:
  RegReloadCache() = default;

  // Record reload of Reg from FI in block MBB if not present yet.
  // Return true if the reload is successfully recorded.
  bool tryRecordReload(Register Reg, int FI, const MachineBasicBlock *MBB) {
    RegSlotPair RSP(Reg, FI);
    return Reloads[MBB].insert(RSP).second;
  }
};

// Cache used frame indexes during statepoint re-write to re-use them in
// processing next statepoint instruction.
// Two strategies. One is to preserve the size of spill slot while another one
// extends the size of spill slots to reduce the number of them, causing
// the less total frame size. But unspill will have "implicit" any extend.
class FrameIndexesCache {
private:
  struct FrameIndexesPerSize {
    // List of used frame indexes during processing previous statepoints.
    SmallVector<int, 8> Slots;
    // Current index of un-used yet frame index.
    unsigned Index = 0;
  };
  MachineFrameInfo &MFI;
  const TargetRegisterInfo &TRI;
  // Map size to list of frame indexes of this size. If the mode is
  // FixupSCSExtendSlotSize then the key 0 is used to keep all frame indexes.
  // If the size of required spill slot is greater than in a cache then the
  // size will be increased.
  DenseMap<unsigned, FrameIndexesPerSize> Cache;

  // Keeps track of slots reserved for the shared landing pad processing.
  // Initialized from GlobalIndices for the current EHPad.
  SmallSet<int, 8> ReservedSlots;

  // Landing pad can be destination of several statepoints. Every register
  // defined by such statepoints must be spilled to the same stack slot.
  // This map keeps that information.
  DenseMap<const MachineBasicBlock *, SmallVector<RegSlotPair, 8>>
      GlobalIndices;

  FrameIndexesPerSize &getCacheBucket(unsigned Size) {
    // In FixupSCSExtendSlotSize mode the bucket with 0 index is used
    // for all sizes.
    return Cache[FixupSCSExtendSlotSize ? 0 : Size];
  }

public:
  FrameIndexesCache(MachineFrameInfo &MFI, const TargetRegisterInfo &TRI)
      : MFI(MFI), TRI(TRI) {}
  // Reset the current state of used frame indexes. After invocation of
  // this function all frame indexes are available for allocation with
  // the exception of slots reserved for landing pad processing (if any).
  void reset(const MachineBasicBlock *EHPad) {
    for (auto &It : Cache)
      It.second.Index = 0;

    ReservedSlots.clear();
    if (EHPad)
      if (auto It = GlobalIndices.find(EHPad); It != GlobalIndices.end())
        ReservedSlots.insert_range(llvm::make_second_range(It->second));
  }

  // Get frame index to spill the register.
  int getFrameIndex(Register Reg, MachineBasicBlock *EHPad) {
    // Check if slot for Reg is already reserved at EHPad.
    auto It = GlobalIndices.find(EHPad);
    if (It != GlobalIndices.end()) {
      auto &Vec = It->second;
      auto Idx = llvm::find_if(
          Vec, [Reg](RegSlotPair &RSP) { return Reg == RSP.first; });
      if (Idx != Vec.end()) {
        int FI = Idx->second;
        LLVM_DEBUG(dbgs() << "Found global FI " << FI << " for register "
                          << printReg(Reg, &TRI) << " at "
                          << printMBBReference(*EHPad) << "\n");
        assert(ReservedSlots.count(FI) && "using unreserved slot");
        return FI;
      }
    }

    unsigned Size = getRegisterSize(TRI, Reg);
    FrameIndexesPerSize &Line = getCacheBucket(Size);
    while (Line.Index < Line.Slots.size()) {
      int FI = Line.Slots[Line.Index++];
      if (ReservedSlots.count(FI))
        continue;
      // If all sizes are kept together we probably need to extend the
      // spill slot size.
      if (MFI.getObjectSize(FI) < Size) {
        MFI.setObjectSize(FI, Size);
        MFI.setObjectAlignment(FI, Align(Size));
        NumSpillSlotsExtended++;
      }
      return FI;
    }
    int FI = MFI.CreateSpillStackObject(Size, Align(Size));
    NumSpillSlotsAllocated++;
    Line.Slots.push_back(FI);
    ++Line.Index;

    // Remember assignment {Reg, FI} for EHPad
    if (EHPad) {
      GlobalIndices[EHPad].push_back(std::make_pair(Reg, FI));
      LLVM_DEBUG(dbgs() << "Reserved FI " << FI << " for spilling reg "
                        << printReg(Reg, &TRI) << " at landing pad "
                        << printMBBReference(*EHPad) << "\n");
    }

    return FI;
  }

  // Sort all registers to spill in descendent order. In the
  // FixupSCSExtendSlotSize mode it will minimize the total frame size.
  // In non FixupSCSExtendSlotSize mode we can skip this step.
  void sortRegisters(SmallVectorImpl<Register> &Regs) {
    if (!FixupSCSExtendSlotSize)
      return;
    llvm::sort(Regs, [&](Register &A, Register &B) {
      return getRegisterSize(TRI, A) > getRegisterSize(TRI, B);
    });
  }
};

// Describes the state of the current processing statepoint instruction.
class StatepointState {
private:
  // statepoint instruction.
  MachineInstr &MI;
  MachineFunction &MF;
  // If non-null then statepoint is invoke, and this points to the landing pad.
  MachineBasicBlock *EHPad;
  const TargetRegisterInfo &TRI;
  const TargetInstrInfo &TII;
  MachineFrameInfo &MFI;
  // Mask with callee saved registers.
  const uint32_t *Mask;
  // Cache of frame indexes used on previous instruction processing.
  FrameIndexesCache &CacheFI;
  bool AllowGCPtrInCSR;
  // Operands with physical registers requiring spilling.
  SmallVector<unsigned, 8> OpsToSpill;
  // Set of register to spill.
  SmallVector<Register, 8> RegsToSpill;
  // Set of registers to reload after statepoint.
  SmallVector<Register, 8> RegsToReload;
  // Map Register to Frame Slot index.
  DenseMap<Register, int> RegToSlotIdx;

public:
  StatepointState(MachineInstr &MI, const uint32_t *Mask,
                  FrameIndexesCache &CacheFI, bool AllowGCPtrInCSR)
      : MI(MI), MF(*MI.getMF()), TRI(*MF.getSubtarget().getRegisterInfo()),
        TII(*MF.getSubtarget().getInstrInfo()), MFI(MF.getFrameInfo()),
        Mask(Mask), CacheFI(CacheFI), AllowGCPtrInCSR(AllowGCPtrInCSR) {

    // Find statepoint's landing pad, if any.
    EHPad = nullptr;
    MachineBasicBlock *MBB = MI.getParent();
    // Invoke statepoint must be last one in block.
    bool Last = std::none_of(++MI.getIterator(), MBB->end().getInstrIterator(),
                             [](MachineInstr &I) {
                               return I.getOpcode() == TargetOpcode::STATEPOINT;
                             });

    if (!Last)
      return;

    auto IsEHPad = [](MachineBasicBlock *B) { return B->isEHPad(); };

    assert(llvm::count_if(MBB->successors(), IsEHPad) < 2 && "multiple EHPads");

    auto It = llvm::find_if(MBB->successors(), IsEHPad);
    if (It != MBB->succ_end())
      EHPad = *It;
  }

  MachineBasicBlock *getEHPad() const { return EHPad; }

  // Return true if register is callee saved.
  bool isCalleeSaved(Register Reg) {
    return (Mask[Reg.id() / 32] >> (Reg.id() % 32)) & 1;
  }

  // Iterates over statepoint meta args to find caller saver registers.
  // Also cache the size of found registers.
  // Returns true if caller save registers found.
  bool findRegistersToSpill() {
    SmallSet<Register, 8> GCRegs;
    // All GC pointer operands assigned to registers produce new value.
    // Since they're tied to their defs, it is enough to collect def registers.
    for (const auto &Def : MI.defs())
      GCRegs.insert(Def.getReg());

    SmallSet<Register, 8> VisitedRegs;
    for (unsigned Idx = StatepointOpers(&MI).getVarIdx(),
                  EndIdx = MI.getNumOperands();
         Idx < EndIdx; ++Idx) {
      MachineOperand &MO = MI.getOperand(Idx);
      if (!MO.isReg() || MO.isImplicit() || MO.isUndef())
        continue;
      Register Reg = MO.getReg();
      assert(Reg.isPhysical() && "Only physical regs are expected");

      if (isCalleeSaved(Reg) && (AllowGCPtrInCSR || !GCRegs.contains(Reg)))
        continue;

      LLVM_DEBUG(dbgs() << "Will spill " << printReg(Reg, &TRI) << " at index "
                        << Idx << "\n");

      if (VisitedRegs.insert(Reg).second)
        RegsToSpill.push_back(Reg);
      OpsToSpill.push_back(Idx);
    }
    CacheFI.sortRegisters(RegsToSpill);
    return !RegsToSpill.empty();
  }

  // Spill all caller saved registers right before statepoint instruction.
  // Remember frame index where register is spilled.
  void spillRegisters() {
    for (Register Reg : RegsToSpill) {
      int FI = CacheFI.getFrameIndex(Reg, EHPad);

      NumSpilledRegisters++;
      RegToSlotIdx[Reg] = FI;

      LLVM_DEBUG(dbgs() << "Spilling " << printReg(Reg, &TRI) << " to FI " << FI
                        << "\n");

      // Perform trivial copy propagation
      bool IsKill = true;
      MachineBasicBlock::iterator InsertBefore(MI);
      Reg = performCopyPropagation(Reg, InsertBefore, IsKill, TII, TRI);
      const TargetRegisterClass *RC = TRI.getMinimalPhysRegClass(Reg);

      LLVM_DEBUG(dbgs() << "Insert spill before " << *InsertBefore);
      TII.storeRegToStackSlot(*MI.getParent(), InsertBefore, Reg, IsKill, FI,
                              RC, Register());
    }
  }

  void insertReloadBefore(Register Reg, MachineBasicBlock::iterator It,
                          MachineBasicBlock *MBB) {
    const TargetRegisterClass *RC = TRI.getMinimalPhysRegClass(Reg);
    int FI = RegToSlotIdx[Reg];
    if (It != MBB->end()) {
      TII.loadRegFromStackSlot(*MBB, It, Reg, FI, RC, Register());
      return;
    }

    // To insert reload at the end of MBB, insert it before last instruction
    // and then swap them.
    assert(!MBB->empty() && "Empty block");
    --It;
    TII.loadRegFromStackSlot(*MBB, It, Reg, FI, RC, Register());
    MachineInstr *Reload = It->getPrevNode();
    int Dummy = 0;
    (void)Dummy;
    assert(TII.isLoadFromStackSlot(*Reload, Dummy) == Reg);
    assert(Dummy == FI);
    MBB->remove(Reload);
    MBB->insertAfter(It, Reload);
  }

  // Insert reloads of (relocated) registers spilled in statepoint.
  void insertReloads(MachineInstr *NewStatepoint, RegReloadCache &RC) {
    MachineBasicBlock *MBB = NewStatepoint->getParent();
    auto InsertPoint = std::next(NewStatepoint->getIterator());

    for (auto Reg : RegsToReload) {
      insertReloadBefore(Reg, InsertPoint, MBB);
      LLVM_DEBUG(dbgs() << "Reloading " << printReg(Reg, &TRI) << " from FI "
                        << RegToSlotIdx[Reg] << " after statepoint\n");

      if (EHPad && RC.tryRecordReload(Reg, RegToSlotIdx[Reg], EHPad)) {
        auto EHPadInsertPoint =
            EHPad->SkipPHIsLabelsAndDebug(EHPad->begin(), Reg);
        insertReloadBefore(Reg, EHPadInsertPoint, EHPad);
        LLVM_DEBUG(dbgs() << "...also reload at EHPad "
                          << printMBBReference(*EHPad) << "\n");
      }
    }
  }

  // Re-write statepoint machine instruction to replace caller saved operands
  // with indirect memory location (frame index).
  MachineInstr *rewriteStatepoint() {
    MachineInstr *NewMI =
        MF.CreateMachineInstr(TII.get(MI.getOpcode()), MI.getDebugLoc(), true);
    MachineInstrBuilder MIB(MF, NewMI);

    unsigned NumOps = MI.getNumOperands();

    // New indices for the remaining defs.
    SmallVector<unsigned, 8> NewIndices;
    unsigned NumDefs = MI.getNumDefs();
    for (unsigned I = 0; I < NumDefs; ++I) {
      MachineOperand &DefMO = MI.getOperand(I);
      assert(DefMO.isReg() && DefMO.isDef() && "Expected Reg Def operand");
      Register Reg = DefMO.getReg();
      assert(DefMO.isTied() && "Def is expected to be tied");
      // We skipped undef uses and did not spill them, so we should not
      // proceed with defs here.
      if (MI.getOperand(MI.findTiedOperandIdx(I)).isUndef()) {
        if (AllowGCPtrInCSR) {
          NewIndices.push_back(NewMI->getNumOperands());
          MIB.addReg(Reg, RegState::Define);
        }
        continue;
      }
      if (!AllowGCPtrInCSR) {
        assert(is_contained(RegsToSpill, Reg));
        RegsToReload.push_back(Reg);
      } else {
        if (isCalleeSaved(Reg)) {
          NewIndices.push_back(NewMI->getNumOperands());
          MIB.addReg(Reg, RegState::Define);
        } else {
          NewIndices.push_back(NumOps);
          RegsToReload.push_back(Reg);
        }
      }
    }

    // Add End marker.
    OpsToSpill.push_back(MI.getNumOperands());
    unsigned CurOpIdx = 0;

    for (unsigned I = NumDefs; I < MI.getNumOperands(); ++I) {
      MachineOperand &MO = MI.getOperand(I);
      if (I == OpsToSpill[CurOpIdx]) {
        int FI = RegToSlotIdx[MO.getReg()];
        MIB.addImm(StackMaps::IndirectMemRefOp);
        MIB.addImm(getRegisterSize(TRI, MO.getReg()));
        assert(MO.isReg() && "Should be register");
        assert(MO.getReg().isPhysical() && "Should be physical register");
        MIB.addFrameIndex(FI);
        MIB.addImm(0);
        ++CurOpIdx;
      } else {
        MIB.add(MO);
        unsigned OldDef;
        if (AllowGCPtrInCSR && MI.isRegTiedToDefOperand(I, &OldDef)) {
          assert(OldDef < NumDefs);
          assert(NewIndices[OldDef] < NumOps);
          MIB->tieOperands(NewIndices[OldDef], MIB->getNumOperands() - 1);
        }
      }
    }
    assert(CurOpIdx == (OpsToSpill.size() - 1) && "Not all operands processed");
    // Add mem operands.
    NewMI->setMemRefs(MF, MI.memoperands());
    for (auto It : RegToSlotIdx) {
      Register R = It.first;
      int FrameIndex = It.second;
      auto PtrInfo = MachinePointerInfo::getFixedStack(MF, FrameIndex);
      MachineMemOperand::Flags Flags = MachineMemOperand::MOLoad;
      if (is_contained(RegsToReload, R))
        Flags |= MachineMemOperand::MOStore;
      auto *MMO =
          MF.getMachineMemOperand(PtrInfo, Flags, getRegisterSize(TRI, R),
                                  MFI.getObjectAlign(FrameIndex));
      NewMI->addMemOperand(MF, MMO);
    }

    // Insert new statepoint and erase old one.
    MI.getParent()->insert(MI, NewMI);

    LLVM_DEBUG(dbgs() << "rewritten statepoint to : " << *NewMI << "\n");
    MI.eraseFromParent();
    return NewMI;
  }
};

class StatepointProcessor {
private:
  MachineFunction &MF;
  const TargetRegisterInfo &TRI;
  FrameIndexesCache CacheFI;
  RegReloadCache ReloadCache;

public:
  StatepointProcessor(MachineFunction &MF)
      : MF(MF), TRI(*MF.getSubtarget().getRegisterInfo()),
        CacheFI(MF.getFrameInfo(), TRI) {}

  bool process(MachineInstr &MI, bool AllowGCPtrInCSR) {
    StatepointOpers SO(&MI);
    uint64_t Flags = SO.getFlags();
    // Do nothing for LiveIn, it supports all registers.
    if (Flags & (uint64_t)StatepointFlags::DeoptLiveIn)
      return false;
    LLVM_DEBUG(dbgs() << "\nMBB " << MI.getParent()->getNumber() << " "
                      << MI.getParent()->getName() << " : process statepoint "
                      << MI);
    CallingConv::ID CC = SO.getCallingConv();
    const uint32_t *Mask = TRI.getCallPreservedMask(MF, CC);
    StatepointState SS(MI, Mask, CacheFI, AllowGCPtrInCSR);
    CacheFI.reset(SS.getEHPad());

    if (!SS.findRegistersToSpill())
      return false;

    SS.spillRegisters();
    auto *NewStatepoint = SS.rewriteStatepoint();
    SS.insertReloads(NewStatepoint, ReloadCache);
    return true;
  }
};
// ROG strip-deopt spike (post-ISel, pre-RA, env-gated). Validates the "operand
// through ISel, remove before RA" design: with -use-registers-for-deopt-values
// the statepoint carries the live pointers as vreg operands after ISel (so they
// are captured here), but those operands also pressure RA. This pass reads each
// statepoint's deopt vreg operands and records them in a side table keyed by the
// statepoint ID (globally unique via RogStackMap's NextId++), then rebuilds the
// statepoint with an EMPTY deopt section so RA sees a plain call (no pressure,
// like the statepoint-free baseline). A later post-RA pass (RogQueryDeopt) reads
// the side table and resolves each captured vreg's final location.

struct RogDeoptInfo {
  SmallVector<Register, 8> Captured;
  // Non-register deopt entries (alloca Direct mem-refs + their trailing size
  // Constants emitted by RogStackMap for address-taken pointer-bearing stack
  // objects) that strip removes wholesale. The query pass re-emits these
  // verbatim; without it, GC roots held in such stack objects are silently
  // dropped from the stack map. Each pair is {isFrameIndex, value}: a frame
  // index number when isFrameIndex, otherwise the immediate.
  SmallVector<std::pair<bool, int64_t>, 16> PreservedOps;
  unsigned PreservedEntries = 0;
};

// Side table: statepoint ID -> captured deopt vregs (and preserved imm-tag
// entries). LLVM may codegen functions in parallel (one thread per module
// partition), so this is thread_local: strip and query for a given function run
// on the same thread, and different partitions get independent tables. Cleared
// per function.
static DenseMap<uint64_t, RogDeoptInfo> &rogDeoptTable() {
  static thread_local DenseMap<uint64_t, RogDeoptInfo> T;
  return T;
}

class RogStripDeopt : public MachineFunctionPass {
public:
  static char ID;
  RogStripDeopt() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return "ROG strip deopt (spike)"; }
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    // Run after coalescing (so captured vregs are the final pre-RA names) but
    // before greedy. Keep LiveIntervals/SlotIndexes valid via shrinkToUses.
    AU.addRequired<LiveIntervalsWrapperPass>();
    AU.addPreserved<LiveIntervalsWrapperPass>();
    AU.addPreserved<LiveDebugVariablesWrapperLegacy>();
    AU.addPreserved<SlotIndexesWrapperPass>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
  bool runOnMachineFunction(MachineFunction &MF) override {
    if (std::getenv("ROG_DISABLE_PRECISE_DEOPT"))
      return false;
    const Function &F = MF.getFunction();
    if (!F.hasGC() || F.getGC() != "rog")
      return false;

    LiveIntervals &LIS = getAnalysis<LiveIntervalsWrapperPass>().getLIS();

    // Bound the side table to this function: captured vregs are per-function, so
    // clear before recording to avoid reading another function's (or module's)
    // vreg numbers in the query pass. Safe because each MachineFunction runs the
    // full machine pipeline (strip -> RA -> query) before the next one starts.
    rogDeoptTable().clear();

    SmallVector<MachineInstr *, 16> SPs;
    for (MachineBasicBlock &BB : MF)
      for (MachineInstr &I : BB)
        if (I.getOpcode() == TargetOpcode::STATEPOINT)
          SPs.push_back(&I);
    if (SPs.empty())
      return false;

    unsigned NCaptured = 0;
    bool Changed = false;
    SmallSet<Register, 32> Affected;

    for (MachineInstr *MI : SPs) {
      unsigned E = MI->getNumOperands();
      unsigned VI = StatepointOpers(MI).getVarIdx();
      if (VI + 5 >= E || !MI->getOperand(VI + 5).isImm())
        continue;
      unsigned NumDeopt = MI->getOperand(VI + 5).getImm();
      uint64_t SpID = StatepointOpers(MI).getID();
      SmallVector<Register, 8> Captured;
      SmallVector<std::pair<bool, int64_t>, 16> Preserved;
      unsigned PreservedEntries = 0;
      unsigned K = VI + 6;
      for (unsigned D = 0; D < NumDeopt && K < E; ++D) {
        const MachineOperand &MO = MI->getOperand(K);
        unsigned N = 1;
        if (MO.isImm()) {
          switch (MO.getImm()) {
          case StackMaps::DirectMemRefOp: N = 3; break;
          case StackMaps::IndirectMemRefOp: N = 4; break;
          case StackMaps::ConstantOp: N = 2; break;
          default: N = 1; break;
          }
          // Preserve this whole imm-tag entry verbatim (Direct alloca mem-refs
          // and their size Constants). The deopt section is dropped below and
          // the query pass only re-emits resolved vreg locations, so without
          // this the address-taken stack objects RogStackMap recorded would be
          // silently dropped from the stack map (lost GC roots).
          for (unsigned J = 0; J < N && K + J < E; ++J) {
            const MachineOperand &P = MI->getOperand(K + J);
            if (P.isFI())
              Preserved.emplace_back(true, (int64_t)P.getIndex());
            else
              Preserved.emplace_back(false, P.isImm() ? P.getImm() : 0);
          }
          ++PreservedEntries;
        } else if (MO.isReg() && MO.getReg().isVirtual()) {
          Captured.push_back(MO.getReg());
          ++NCaptured;
        }
        K += N;
      }
      unsigned DeoptEnd = K;
      if (DeoptEnd <= VI + 6)
        continue;
      if (!Captured.empty() || PreservedEntries > 0) {
        for (Register R : Captured)
          Affected.insert(R);
        RogDeoptInfo &Info = rogDeoptTable()[SpID];
        Info.Captured = std::move(Captured);
        Info.PreservedOps = std::move(Preserved);
        Info.PreservedEntries = PreservedEntries;
      }

      // In place: drop the whole deopt section [VI+6, DeoptEnd) (removing reg
      // uses updates MRI use lists) and set numdeopt = 0. Instruction indices
      // are untouched, so SlotIndexes stay valid; LiveIntervals are repaired
      // below.
      for (unsigned Idx = DeoptEnd; Idx > VI + 6; --Idx)
        MI->removeOperand(Idx - 1);
      MI->getOperand(VI + 5).setImm(0);
      Changed = true;
    }

    // Repair LiveIntervals: dropping the deopt uses may shorten a value's live
    // range (it is no longer kept live across the call by the statepoint use),
    // which is exactly what relieves RA pressure. Do not remove intervals here
    // even when only debug users remain; LiveDebugVariables may still inspect
    // them and can lower now-unavailable debug values to undef on its own.
    //
    // Experiment (ROG_STRIP_NO_SHRINK): keep the pre-strip live ranges so RA
    // must keep every root addressable across its calls (no dead-at-safepoint
    // misses). RESULT (2026-07-02): does not work as-is -- greedy's
    // SplitAnalysis asserts ("range ends mid block with no uses",
    // SplitKit.cpp:229) on ranges with no anchoring use. The sibling
    // experiment (ROG_DISABLE_PRECISE_DEOPT, keeping the operands on the
    // statepoint through RA) dies too: large root sets exhaust the allocator
    // ("ran out of registers", e.g. json fallback.typeFields). A real
    // keep-alive path needs statepoint spill semantics (pre-RA fixed slots),
    // not just longer ranges. Kept for reference.
    if (!std::getenv("ROG_STRIP_NO_SHRINK"))
      for (Register V : Affected) {
        if (LIS.hasInterval(V))
          LIS.shrinkToUses(&LIS.getInterval(V));
      }

    if (std::getenv("ROG_STRIP_DEOPT_DEBUG"))
      errs() << "[strip-deopt] " << MF.getName() << " statepoints=" << SPs.size()
             << " captured_vregs=" << NCaptured << "\n";
    return Changed;
  }
};
char RogStripDeopt::ID = 0;

// Post-RA / pre-rewrite query spike: for each STATEPOINT, look up its captured
// deopt vregs (recorded pre-RA by RogStripDeopt) and resolve each vreg's final
// location at the statepoint's slot index. Splitting is followed via
// VirtRegMap::getOriginal (the captured pre-RA vreg is the split root); the live
// descendant covering the safepoint slot gives the location (physreg or stack
// slot). Prints per-function resolution stats. Must run after register
// allocation but BEFORE VirtRegRewriter (needs live VirtRegMap + LiveIntervals
// and vregs still present).
class RogQueryDeopt : public MachineFunctionPass {
public:
  static char ID;
  RogQueryDeopt() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return "ROG query deopt (spike)"; }
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<VirtRegMapWrapperLegacy>();
    AU.addRequired<LiveIntervalsWrapperPass>();
    AU.setPreservesAll();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
  bool runOnMachineFunction(MachineFunction &MF) override {
    if (std::getenv("ROG_DISABLE_PRECISE_DEOPT"))
      return false;
    const Function &F = MF.getFunction();
    if (!F.hasGC() || F.getGC() != "rog")
      return false;

    VirtRegMap &VRM = getAnalysis<VirtRegMapWrapperLegacy>().getVRM();
    LiveIntervals &LIS = getAnalysis<LiveIntervalsWrapperPass>().getLIS();
    MachineRegisterInfo &MRI = MF.getRegInfo();
    MachineFrameInfo &MFI = MF.getFrameInfo();
    const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
    const TargetRegisterInfo &TRI = *MF.getSubtarget().getRegisterInfo();
    // Ensure VirtRegMap's per-vreg maps cover every current vreg (passes after
    // RA may have added vregs without growing it); const indexing asserts on
    // out-of-range vregs.
    VRM.grow();
    bool ObjectFallbackOnMiss = std::getenv("ROG_OBJECT_FALLBACK_ON_MISS");
    DenseMap<MCRegister, int> LateSpillSlots;
    auto getLateSpillSlot = [&](MCRegister Phys) {
      auto Inserted = LateSpillSlots.try_emplace(Phys, VirtRegMap::NO_STACK_SLOT);
      int &FI = Inserted.first->second;
      if (Inserted.second) {
        const TargetRegisterClass *RC = TRI.getMinimalPhysRegClass(Phys);
        unsigned Size = TRI.getSpillSize(*RC);
        FI = MFI.CreateSpillStackObject(Size, Align(Size));
      }
      return FI;
    };

    // original-vreg -> all descendants (covers RA splitting). The captured
    // pre-RA vreg is the split root returned by getOriginal. Include vregs with
    // no live interval: a split piece that was later spilled has its interval
    // removed but still carries the stack slot we need for the slot lookup.
    DenseMap<Register, SmallVector<Register, 4>> Children;
    for (unsigned i = 0, e = MRI.getNumVirtRegs(); i != e; ++i) {
      Register V = Register::index2VirtReg(i);
      Children[VRM.getOriginal(V)].push_back(V);
    }

    auto collectCandidates = [&](Register Orig,
                                 SmallVectorImpl<Register> &Cands) {
      SmallSet<Register, 8> Seen;
      auto add = [&](Register R) {
        if (R.isVirtual() && Seen.insert(R).second)
          Cands.push_back(R);
      };
      Register Root = VRM.getOriginal(Orig);
      add(Orig);
      add(Root);
      if (auto It = Children.find(Root); It != Children.end())
        for (Register C : It->second)
          add(C);
      if (Root != Orig)
        if (auto It = Children.find(Orig); It != Children.end())
          for (Register C : It->second)
            add(C);
    };

    auto regPreservedByStatepoint = [](const MachineInstr &MI,
                                       MCRegister Phys) {
      bool SawMask = false;
      for (const MachineOperand &MO : MI.operands()) {
        if (!MO.isRegMask())
          continue;
        SawMask = true;
        if (MO.clobbersPhysReg(Phys))
          return false;
      }
      return SawMask;
    };

    // Resolve where the value derived from captured root Orig lives after the
    // safepoint call. Candidates are Orig plus its split descendants
    // (getOriginal==Orig).
    //   1 = live in a phys reg at S (OutRegPreserved says whether the call's
    //       regmask preserves it; only preserved regs are runtime-recoverable)
    //   2 = spilled to a stack slot (value sits in the slot across the call;
    //       no vreg interval covers S because reloads are tight around uses)
    //   0 = unresolved (value not live at S, or root coalesced away pre-RA)
    auto resolve = [&](Register Orig, SlotIndex S, const MachineInstr &MI,
                       int &OutFI, MCRegister &OutPhys,
                       bool &OutRegPreserved) -> int {
      OutFI = VirtRegMap::NO_STACK_SLOT;
      OutPhys = MCRegister();
      OutRegPreserved = false;
      SmallVector<Register, 8> Cands;
      collectCandidates(Orig, Cands);
      // Prefer a phys reg live across S.
      for (Register C : Cands)
        if (LIS.hasInterval(C) && !MRI.reg_nodbg_empty(C) &&
            LIS.getInterval(C).liveAt(S) && VRM.hasPhys(C)) {
          OutPhys = VRM.getPhys(C);
          OutRegPreserved = regPreservedByStatepoint(MI, OutPhys);
          return 1;
        }
      // Otherwise a spilled candidate: the value resides in its stack slot
      // across the call.
      for (Register C : Cands) {
        int SS = VRM.getStackSlot(C);
        if (SS != VirtRegMap::NO_STACK_SLOT) {
          OutFI = SS;
          return 2;
        }
      }
      return 0;
    };

    // Is any candidate's live interval live at S (ignoring its location)?
    auto liveAtS = [&](Register Orig, SlotIndex S) -> bool {
      auto chk = [&](Register C) {
        return LIS.hasInterval(C) && !MRI.reg_nodbg_empty(C) &&
               LIS.getInterval(C).liveAt(S);
      };
      SmallVector<Register, 8> Cands;
      collectCandidates(Orig, Cands);
      for (Register C : Cands)
        if (chk(C))
          return true;
      return false;
    };

    // Does the value RESURRECT after S -- i.e. some candidate in the vreg
    // lineage has a live segment beginning after the safepoint (a remat /
    // re-materialized definition)? A dead-at-S root that resurrects is NOT
    // program-dead: the register allocator merely chose not to keep it live
    // across the call (our strip removed the use that would have forced it),
    // and will recompute it afterwards. Between the call and the recompute the
    // object may have no recorded reference at all -- the reedsolomon
    // use-after-free. A dead-at-S root with NO later segment anywhere in its
    // lineage (and no spill slot, or resolve() would have returned it) is
    // genuinely dead: no code will ever read the value again, so dropping it
    // is safe.
    auto resurrectsAfterS = [&](Register Orig, SlotIndex S) -> bool {
      SmallVector<Register, 8> Cands;
      collectCandidates(Orig, Cands);
      for (Register C : Cands) {
        if (!LIS.hasInterval(C))
          continue;
        for (const LiveRange::Segment &Seg : LIS.getInterval(C))
          if (Seg.start > S)
            return true;
      }
      return false;
    };

    unsigned NReg = 0, NRegPreserved = 0, NRegClobbered = 0, NRegSpilled = 0,
             NSlot = 0, NObjectFallbacks = 0,
             NObjectFallbackSlots = 0, NMMOFallbacks = 0,
             NMMOFallbackSlots = 0, NMiss = 0, NMissStale = 0,
             NMissResurrect = 0, NMissTrueDead = 0, NMissLiveNoLoc = 0,
             NTotal = 0, NIncompleteSPs = 0;
    bool Changed = false;
    for (MachineBasicBlock &BB : MF) {
      for (MachineInstr &MI : BB) {
        if (MI.getOpcode() != TargetOpcode::STATEPOINT)
          continue;
        uint64_t SpID = StatepointOpers(&MI).getID();
        auto It = rogDeoptTable().find(SpID);
        if (It == rogDeoptTable().end())
          continue;
        RogDeoptInfo &Info = It->second;
        SlotIndex S = LIS.getInstructionIndex(MI).getDeadSlot();
        SmallVector<int, 8> SlotFIs; // stack slots to record at this safepoint
        SmallSet<unsigned, 8> LateSpilledRegIds;
        bool NeedsObjectFallback = false;
        // Set when a root of THIS safepoint is dangerously unresolvable (stale
        // lineage / live-without-location / dead-but-resurrects); marks only
        // this statepoint incomplete (bit 63 of its ID) so the runtime scans
        // this frame conservatively only when the GC stops here.
        bool SPIncomplete = false;
        const char *QDiagPat = std::getenv("ROG_M2D_FN");
        bool QDiag = QDiagPat && MF.getName().contains(QDiagPat);
        for (Register Orig : Info.Captured) {
          ++NTotal;
          int FI;
          MCRegister Phys;
          bool RegPreserved;
          int Res = resolve(Orig, S, MI, FI, Phys, RegPreserved);
          if (QDiag)
            errs() << "[q-sp] " << MF.getName()
                   << " id=" << StatepointOpers(&MI).getID() << " "
                   << printReg(Orig) << " -> "
                   << (Res == 1 ? (RegPreserved ? "reg-preserved" : "reg-clobbered")
                       : Res == 2 ? "slot"
                                  : "MISS")
                   << (Res == 2 ? (" FI=" + std::to_string(FI)) : std::string())
                   << "\n";
          switch (Res) {
          case 1:
            ++NReg;
            if (RegPreserved)
              ++NRegPreserved;
            else {
              ++NRegClobbered;
            }
            // Diagnostic (ROG_SPILL_PRESERVED): also spill call-preserved
            // register roots into recorded slots instead of relying on the
            // runtime's conservative callee-saved recovery chain -- isolates
            // whether that chain is the source of missed roots.
            if (!RegPreserved || std::getenv("ROG_SPILL_PRESERVED")) {
              int SpillFI = getLateSpillSlot(Phys);
              if (LateSpilledRegIds.insert(Phys.id()).second) {
                const TargetRegisterClass *RC = TRI.getMinimalPhysRegClass(Phys);
                TII.storeRegToStackSlot(BB, MI.getIterator(), Phys,
                                        /*isKill=*/false, SpillFI, RC,
                                        Register());
                SlotFIs.push_back(SpillFI);
                ++NRegSpilled;
              }
            }
            break;
          case 2: ++NSlot; SlotFIs.push_back(FI); break;
          default: {
            ++NMiss;
            // Classify the miss: "stale" = the whole vreg lineage of the
            // captured root is gone (every candidate is dead with no slot) ->
            // the value was renamed away (coalescing) between strip (pre-RA) and
            // RA, so we lost the handle (dangerous: a real root we cannot
            // locate). Otherwise the value is present but not live across S
            // (plausibly program-dead at the safepoint -> safe to drop).
            bool anyPresence = false;
            auto seen = [&](Register C) {
              if (!MRI.reg_nodbg_empty(C) ||
                  VRM.getStackSlot(C) != VirtRegMap::NO_STACK_SLOT)
                anyPresence = true;
            };
            SmallVector<Register, 8> Cands;
            collectCandidates(Orig, Cands);
            for (Register C : Cands)
              seen(C);
            if (!anyPresence) {
              // Whole lineage renamed away (coalescing): the handle is lost
              // and nothing can be proven about the value -- dangerous.
              ++NMissStale;
              NeedsObjectFallback = true;
              SPIncomplete = true;
            } else if (liveAtS(Orig, S)) {
              ++NMissLiveNoLoc; // live at S but no resolvable location (bug?)
              NeedsObjectFallback = true;
              SPIncomplete = true;
            } else if (resurrectsAfterS(Orig, S)) {
              // Dead at S but re-defined later (remat): the value is NOT
              // program-dead, RA just chose not to keep it across the call.
              // Between the call and the recompute the object may be entirely
              // unreferenced -- the reedsolomon use-after-free class.
              ++NMissResurrect;
              NeedsObjectFallback = true;
              SPIncomplete = true;
            } else {
              // No location, no spill slot, and no later definition anywhere
              // in the LINEAGE (Orig + split descendants). NOT yet provably
              // dead: the value may live on through a COPY into a different
              // lineage that collectCandidates cannot see. Experiment knob
              // ROG_TRUEDEAD_EXEMPT drops these without marking (unsound until
              // copy-chasing is implemented); default marks them.
              ++NMissTrueDead;
              if (!std::getenv("ROG_TRUEDEAD_EXEMPT"))
                SPIncomplete = true;
            }
            break;
          }
          }
        }

        // Re-inject the resolved stack-slot roots as deopt operands so the
        // standard StackMaps emission records them. Encode each as the
        // statepoint indirect-memref quad [IndirectMemRefOp, size, FI, 0]; PEI
        // resolves the FI to FrameReg+offset (matching emitPatchPoint's form),
        // and StackSlotColoring remaps the FI if slots are merged. Inserted at
        // the start of the (currently empty) deopt section, right after the
        // num-deopt operand; num-deopt is set to the count. Runtime-recoverable
        // preserved-reg roots are left to the conservative callee-saved scan;
        // clobbered reg roots are late-spilled above. Misses fall back to the
        // statepoint's fixed-stack memory operands; the wider all-frame-object
        // fallback remains available under ROG_OBJECT_FALLBACK_ON_MISS.
        SmallVector<int, 8> DirectObjectFIs;
        SmallSet<int, 8> DirectObjectFISet;
        auto addDirectObjectFI = [&](int FI) {
          if (MFI.isDeadObjectIndex(FI) || MFI.isVariableSizedObjectIndex(FI) ||
              MFI.getStackID(FI) != TargetStackID::Default ||
              MFI.getObjectSize(FI) <= 0 || !DirectObjectFISet.insert(FI).second)
            return;
          DirectObjectFIs.push_back(FI);
        };
        if (ObjectFallbackOnMiss && NeedsObjectFallback) {
          ++NObjectFallbacks;
          for (int FI = MFI.getObjectIndexBegin(), FE = MFI.getObjectIndexEnd();
               FI != FE; ++FI)
            addDirectObjectFI(FI);
          NObjectFallbackSlots += DirectObjectFIs.size();
        } else if (NeedsObjectFallback) {
          for (MachineMemOperand *MMO : MI.memoperands()) {
            const auto *FSV =
                dyn_cast_or_null<FixedStackPseudoSourceValue>(
                    MMO->getPseudoValue());
            if (!FSV)
              continue;
            addDirectObjectFI(FSV->getFrameIndex());
          }
          if (!DirectObjectFIs.empty()) {
            ++NMMOFallbacks;
            NMMOFallbackSlots += DirectObjectFIs.size();
          }
        }

        if (!SlotFIs.empty() || !DirectObjectFIs.empty() ||
            Info.PreservedEntries > 0) {
          unsigned VI = StatepointOpers(&MI).getVarIdx();
          SmallVector<MachineOperand, 32> NewOps;
          // Re-emit the imm-tag entries strip preserved (alloca Direct mem-refs
          // + size Constants) so address-taken stack objects stay scanned.
          for (const auto &P : Info.PreservedOps) {
            if (P.first)
              NewOps.push_back(MachineOperand::CreateFI((int)P.second));
            else
              NewOps.push_back(MachineOperand::CreateImm(P.second));
          }
          for (int FI : SlotFIs) {
            NewOps.push_back(
                MachineOperand::CreateImm(StackMaps::IndirectMemRefOp));
            NewOps.push_back(MachineOperand::CreateImm(MFI.getObjectSize(FI)));
            NewOps.push_back(MachineOperand::CreateFI(FI));
            NewOps.push_back(MachineOperand::CreateImm(0));
          }
          for (int FI : DirectObjectFIs) {
            NewOps.push_back(
                MachineOperand::CreateImm(StackMaps::DirectMemRefOp));
            NewOps.push_back(MachineOperand::CreateFI(FI));
            NewOps.push_back(MachineOperand::CreateImm(0));
            NewOps.push_back(MachineOperand::CreateImm(StackMaps::ConstantOp));
            NewOps.push_back(MachineOperand::CreateImm(MFI.getObjectSize(FI)));
          }
          MI.insert(MI.operands_begin() + (VI + 6), NewOps);
          MI.getOperand(VI + 5).setImm(Info.PreservedEntries + SlotFIs.size() +
                                       DirectObjectFIs.size() * 2);
          Changed = true;
        }
        // Second incomplete gate (the first is RogMarkersToDeopt's, for
        // markers it could not convert): a dangerously unresolvable root at
        // THIS safepoint -- a stale handle renamed away across RA, a value
        // live without a location, or one that is dead here but resurrects
        // (rematerialized) afterwards -- means this safepoint's precise
        // record under-approximates the frame's live roots. The object
        // fallback meant to cover these is opt-in (ROG_OBJECT_FALLBACK_ON_MISS,
        // off by default), which left such roots with no recovery path at all
        // (reproduced as GC use-after-free: vendor reedsolomon TestEncoding).
        // Mark bit 63 of this statepoint's ID; the runtime scans the frame
        // conservatively only when the GC stops at this safepoint. Provably
        // true-dead roots (no location, no slot, no later definition) are
        // dropped without marking. This gate is per-safepoint, unlike the
        // marker gate: a miss is a property of (safepoint, root) -- the same
        // root at other safepoints either resolves or marks those safepoints
        // itself.
        if (SPIncomplete) {
          StatepointOpers SO(&MI);
          MI.getOperand(SO.getIDPos()).setImm(SO.getID() | (1ULL << 63));
          ++NIncompleteSPs;
          Changed = true;
        }
      }
    }
    if (std::getenv("ROG_STRIP_DEOPT_DEBUG") && NTotal)
      errs() << "[query-deopt] " << MF.getName() << " total=" << NTotal
             << " reg=" << NReg << " (preserved=" << NRegPreserved
             << " clobbered=" << NRegClobbered
             << " late_spilled=" << NRegSpilled << ") slot=" << NSlot
             << " mmo_fallbacks=" << NMMOFallbacks
             << " mmo_slots=" << NMMOFallbackSlots
             << " object_fallbacks=" << NObjectFallbacks
             << " object_slots=" << NObjectFallbackSlots
             << " miss=" << NMiss
             << " (stale=" << NMissStale << " resurrect=" << NMissResurrect
             << " truedead=" << NMissTrueDead
             << " liveNoLoc=" << NMissLiveNoLoc
             << ") incomplete_sps=" << NIncompleteSPs << "\n";
    return Changed;
  }
};
char RogQueryDeopt::ID = 0;

// ===========================================================================
// RogGcReadDeopt (ROG_GC_DBG_READ): the debug-value "late use" reader.
//
// Runs in addPreEmitPass2 -- after LiveDebugValues has resolved the $gcroot
// markers (emitted by RogStackMap under ROG_GC_DBG) to physical locations, and
// after PEI resolved frame indices. For each STATEPOINT it reads the live
// $gcroot markers' resolved locations and injects the stack-slot ones as deopt
// operands, exactly like RogQueryDeopt -- but the location source is the
// zero-overhead debug-value channel instead of statepoint operands (which
// constrain RA). Register (callee-saved) roots are left to the conservative CSR
// scan, matching RogQueryDeopt. With ROG_STACKMAP_MAXOPS=0 (empty deopt at
// ISEL, no operands, no overhead) + ROG_GC_DBG + this reader, precise GC is
// driven entirely by debug-value markers.
// ===========================================================================
class RogGcReadDeopt : public MachineFunctionPass {
public:
  static char ID;
  RogGcReadDeopt() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return "ROG gc-dbg deopt reader"; }
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override {
    // Default-on: consume the $gcroot markers emitted by RogStackMap. The
    // opt-out matches the emitter (ROG_GC_DBG_DISABLE forces the legacy operand
    // path), in which case there are no markers and this is a cheap no-op.
    if (std::getenv("ROG_GC_DBG_DISABLE"))
      return false;
    bool Dbg = std::getenv("ROG_STRIP_DEOPT_DEBUG") != nullptr;

    // Current resolved location of each live $gcroot marker. `slot` => an
    // indirect [base - off] spill slot (injected); otherwise a register
    // location (left to conservative CSR scan).
    struct Loc {
      bool slot = false;
      Register base;
      int64_t off = 0;
    };
    DenseMap<const DILocalVariable *, Loc> Live;

    auto isGcRoot = [](const DILocalVariable *V) {
      return V && V->getName().starts_with("$gcroot");
    };

    const TargetRegisterInfo &TRI = *MF.getSubtarget().getRegisterInfo();
    const BitVector Reserved = TRI.getReservedRegs(MF);

    // Classify a $gcroot marker (DBG_VALUE or DBG_VALUE_LIST, single location R)
    // by interpreting its DIExpression to recover where the GC pointer actually
    // is at the safepoint:
    //   bare / [DW_OP_LLVM_arg 0]                    -> value in register R
    //   [.. DW_OP_deref]                             -> value at *(R + off)
    //   [.. DW_OP_stack_value] (no deref)            -> value is R + off
    // Returns false if the location was dropped ($noreg) or uses an op we don't
    // model. Sets L.slot only for a memory load off a RESERVED register (frame /
    // stack / base pointer = a genuine stack slot): those are the roots that
    // neither the conservative callee-saved scan (register roots) nor the heap
    // traversal (a load off a value register is a heap-object field) covers, so
    // only they must be injected. A computed/interior pointer's base register is
    // itself conservatively scanned. An interior offset trailing a deref is
    // irrelevant for ROG's non-moving GC (the base load already reaches it).
    // Returns 0 = recorded, 1 = multi-operand, 2 = $noreg (LLVM dropped it),
    // 3 = unmodeled DIExpression op (diagnostic split of the "lost" bucket).
    auto classify = [&](const MachineInstr &MI, Loc &L) -> int {
      if (MI.getNumDebugOperands() != 1)
        return 1;
      const MachineOperand &MO = MI.getDebugOperand(0);
      if (!MO.isReg() || !MO.getReg())
        return 2; // $noreg => location lost by LiveDebugValues (not reader-fixable)
      Register R = MO.getReg();
      const DIExpression *E = MI.getDebugExpression();
      ArrayRef<uint64_t> Els = E ? E->getElements() : ArrayRef<uint64_t>();
      size_t i = 0;
      if (i + 1 < Els.size() && Els[i] == dwarf::DW_OP_LLVM_arg &&
          Els[i + 1] == 0)
        i += 2;
      int64_t off = 0;
      bool deref = false;
      while (i < Els.size()) {
        uint64_t op = Els[i];
        if (op == dwarf::DW_OP_constu && i + 2 < Els.size() &&
            Els[i + 2] == dwarf::DW_OP_minus) {
          off -= (int64_t)Els[i + 1];
          i += 3;
        } else if (op == dwarf::DW_OP_constu && i + 2 < Els.size() &&
                   Els[i + 2] == dwarf::DW_OP_plus) {
          off += (int64_t)Els[i + 1];
          i += 3;
        } else if (op == dwarf::DW_OP_plus_uconst && i + 1 < Els.size()) {
          off += (int64_t)Els[i + 1];
          i += 2;
        } else if (op == dwarf::DW_OP_deref) {
          deref = true;
          break; // interior offset after the load is irrelevant (non-moving)
        } else if (op == dwarf::DW_OP_stack_value) {
          break;
        } else {
          if (Dbg) {
            errs() << "[gc-read] UNMODELED $gcroot expr on "
                   << MF.getName() << ": [";
            for (uint64_t E2 : Els)
              errs() << " " << E2;
            errs() << " ]\n";
          }
          return 3; // unmodeled op => reader-fixable gap
        }
      }
      if (MI.getOpcode() == TargetOpcode::DBG_VALUE && MI.isIndirectDebugValue())
        deref = true;
      L.slot = deref && Reserved.test(R.id());
      L.base = R;
      L.off = off;
      return 0;
    };

    unsigned NInject = 0, NReg = 0, NSP = 0;
    // Split of the roots we could not inject: a multi-operand DBG_VALUE_LIST, a
    // $noreg location (LiveDebugValues dropped it -- covered by the conservative
    // callee-saved scan / alloca / heap), or an unmodeled DIExpression form.
    unsigned NLostMultiOp = 0, NLostNoReg = 0, NLostUnmodeled = 0;
    // The $gcroot debug values exist only to carry GC-root locations to this
    // reader. Once consumed they must NOT reach the DWARF emitter (AsmPrinter,
    // which runs after this pass), or they pollute .debug_info/.debug_loclists
    // with fake `$gcroot.N` locals. Collect and erase them here.
    SmallVector<MachineInstr *, 32> GcDbgToErase;
    bool Changed = false;
    for (MachineBasicBlock &MBB : MF) {
      for (MachineInstr &MI : MBB) {
        if (MI.getOpcode() == TargetOpcode::DBG_VALUE ||
            MI.getOpcode() == TargetOpcode::DBG_VALUE_LIST) {
          const DILocalVariable *V = MI.getDebugVariable();
          if (!isGcRoot(V))
            continue;
          GcDbgToErase.push_back(&MI);
          Loc L;
          switch (classify(MI, L)) {
          case 0:
            Live[V] = L;
            break;
          case 1:
            Live.erase(V);
            ++NLostMultiOp;
            break;
          case 2:
            Live.erase(V);
            ++NLostNoReg;
            break;
          default:
            Live.erase(V);
            ++NLostUnmodeled;
            break;
          }
          continue;
        }

        if (MI.getOpcode() != TargetOpcode::STATEPOINT)
          continue;
        ++NSP;

        // Inject each live slot marker as an indirect-memref deopt operand in
        // the form StackMaps expects post-PEI: [IndirectMemRefOp, size, baseReg,
        // offset] -- the marker already gives baseReg+offset (e.g. $rbp - 48),
        // so no frame-index round-trip is needed. Dedup by (reg, offset) since a
        // pointer live across several calls leaves a marker per call that may
        // linger to the same slot.
        SmallVector<std::pair<Register, int64_t>, 8> Slots;
        SmallSet<int64_t, 8> Seen;
        for (auto &KV : Live) {
          const Loc &L = KV.second;
          if (!L.slot) {
            ++NReg;
            continue;
          }
          int64_t Key = ((int64_t)L.base.id() << 32) ^ (L.off & 0xffffffff);
          if (Seen.insert(Key).second)
            Slots.push_back({L.base, L.off});
        }

        if (Slots.empty())
          continue;

        unsigned VI = StatepointOpers(&MI).getVarIdx();
        SmallVector<MachineOperand, 16> NewOps;
        for (auto &S : Slots) {
          NewOps.push_back(
              MachineOperand::CreateImm(StackMaps::IndirectMemRefOp));
          NewOps.push_back(MachineOperand::CreateImm(8)); // pointer size
          NewOps.push_back(MachineOperand::CreateReg(S.first, /*isDef=*/false));
          NewOps.push_back(MachineOperand::CreateImm(S.second));
        }
        unsigned PrevDeopt = MI.getOperand(VI + 5).getImm();
        MI.insert(MI.operands_begin() + (VI + 6), NewOps);
        MI.getOperand(VI + 5).setImm(PrevDeopt + Slots.size());
        NInject += Slots.size();
        Changed = true;
      }
    }
    // Drop the consumed $gcroot markers so they never reach the DWARF emitter.
    for (MachineInstr *DbgMI : GcDbgToErase) {
      DbgMI->eraseFromParent();
      Changed = true;
    }
    if (Dbg && NSP)
      errs() << "[gc-read] " << MF.getName() << " sps=" << NSP
             << " injected=" << NInject << " reg_skipped=" << NReg
             << " lost(multiop=" << NLostMultiOp << " noreg=" << NLostNoReg
             << " unmodeled=" << NLostUnmodeled << ")\n";
    return Changed;
  }
};
char RogGcReadDeopt::ID = 0;

// RogMarkersToDeopt (markers-only LOSSLESS path): pre-RA, before LiveDebugVariables
// strips debug values, rewrite each direct single-vreg $gcroot marker into a plain
// vreg deopt operand on its safepoint's STATEPOINT, then drop the marker. This feeds
// the existing RogStripDeopt/RogQueryDeopt path (capture pre-RA vreg -> strip ->
// RA -> resolve final location), which is LOSSLESS: register allocation must assign
// every use a concrete location, so a live-across root can never degrade to $noreg
// the way a LiveDebugValues-reconstructed debug location can (that degradation is the
// markers-only correctness hole). Crucially the operand is added AFTER instruction
// selection, so unlike the ISel-time operand path it imposes no DAG-scheduling
// pressure -- the frame stays at the marker baseline while the location becomes
// lossless. Only the direct single-vreg form is converted here; complex/indirect
// markers are left in place for the post-RA RogGcReadDeopt (unchanged fallback).
//
// Scoping is positional: RogStackMap emits each marker immediately before its call,
// so the markers seen since the previous statepoint are exactly the live-across roots
// of the next safepoint. This mirrors the emitter's per-call liveAfter set and avoids
// injecting a root into a later call it is not live across (a persistent Live map
// cannot do this pre-RA, since LiveDebugValues -- the source of the $noreg scoping the
// post-RA reader relies on -- has not run yet).
class RogMarkersToDeopt : public MachineFunctionPass {
public:
  static char ID;
  RogMarkersToDeopt() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override {
    return "ROG markers -> statepoint deopt (pre-RA)";
  }
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addRequired<MachineDominatorTreeWrapperPass>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override {
    if (std::getenv("ROG_GC_DBG_DISABLE"))
      return false; // legacy ISel operand path: no markers
    if (std::getenv("ROG_GC_LIVE_DISABLE"))
      return false; // fall back to the post-RA debug-value reader
    const Function &F = MF.getFunction();
    if (!F.hasGC() || F.getGC() != "rog")
      return false;

    MachineDominatorTree &DT =
        getAnalysis<MachineDominatorTreeWrapperPass>().getDomTree();
    MachineRegisterInfo &MRI = MF.getRegInfo();

    auto isGcRoot = [](const DILocalVariable *V) {
      return V && V->getName().starts_with("$gcroot");
    };
    // A GC root is either a virtual register holding the pointer (RA resolves it
    // via strip/query) or a fixed stack slot (indirect memref, resolved by PEI).
    struct Root {
      bool isFI = false;
      Register Reg;
      int FI = 0;
    };

    // Debug instruction-number -> defining MI, to resolve DBG_INSTR_REF markers
    // back to the vreg they name (isel emits most pointer roots as instr refs).
    DenseMap<unsigned, MachineInstr *> InstrNumToMI;
    for (MachineBasicBlock &MBB : MF)
      for (MachineInstr &MI : MBB)
        if (unsigned N = MI.peekDebugInstrNum())
          InstrNumToMI[N] = &MI;

    // Resolve a DBG_INSTR_REF (InstNo, OpNo) to its defining vreg, following the
    // debug value-substitution table (records where a value's def moved during
    // optimization). Subregister qualifiers are irrelevant for a non-moving GC:
    // the full register that holds the pointer is scanned.
    unsigned NMapMiss = 0, NNotDef = 0, NDomFail = 0;
    auto resolveInstrRef = [&](unsigned InstNo, unsigned OpNo,
                               const MachineInstr &Marker, Register &Out) -> bool {
      auto Sought =
          MachineFunction::DebugSubstitution({InstNo, OpNo}, {0, 0}, 0);
      auto It = llvm::lower_bound(MF.DebugValueSubstitutions, Sought);
      while (It != MF.DebugValueSubstitutions.end() && It->Src == Sought.Src) {
        std::tie(InstNo, OpNo) = It->Dest;
        Sought.Src = It->Dest;
        It = llvm::lower_bound(MF.DebugValueSubstitutions, Sought);
      }
      auto MIIt = InstrNumToMI.find(InstNo);
      if (MIIt == InstrNumToMI.end()) {
        ++NMapMiss;
        return false;
      }
      MachineInstr *Def = MIIt->second;
      if (OpNo >= Def->getNumOperands()) {
        ++NNotDef;
        return false;
      }
      const MachineOperand &DefMO = Def->getOperand(OpNo);
      if (!DefMO.isReg() || !DefMO.isDef() || !DefMO.getReg() ||
          !DefMO.getReg().isVirtual()) {
        ++NNotDef;
        return false;
      }
      // The resolved def must dominate the marker (hence the safepoint just
      // after it), or a use added at the statepoint would have no reaching def
      // and crash RA -- a substitution can legitimately point at a def on a
      // different control-flow path (e.g. a PHI input). Conservatively missing
      // such a rare root is safe (the CSR scan still covers register roots);
      // crashing is not.
      if (!DT.dominates(Def, &Marker)) {
        ++NDomFail;
        return false;
      }
      Out = DefMO.getReg();
      return true;
    };

    // A marker is only ERASED once its root has actually been injected into a
    // statepoint's deopt section. Any marker we cannot convert (unresolvable
    // instr-ref, non-dominating def, physreg, multi-operand form) or that never
    // reaches a statepoint (dangling at block end) is LEFT IN PLACE for the
    // post-RA RogGcReadDeopt reader, which recovers a best-effort location and
    // is backed by the conservative callee-saved scan. Silently dropping such a
    // marker would delete the root entirely -- worse than the lossy baseline.
    struct PendingRoot {
      Root Rt;
      MachineInstr *Marker;
    };
    SmallVector<PendingRoot, 8> Pending;
    SmallVector<MachineInstr *, 32> ToErase;
    unsigned NConv = 0, NSP = 0, NUnres = 0, NLeft = 0;
    // Split of the markers left to the reader, to know what we still miss:
    // multi-operand form, unresolvable instr-ref, non-dominating def, physreg /
    // indirect / other operand form, dangling (no statepoint consumed them).
    unsigned NLMulti = 0, NLInstrRef = 0, NLDom = 0, NLForm = 0, NLDangling = 0;
    bool Diag = std::getenv("ROG_MARKERS_DIAG") != nullptr;
    SmallVector<MachineInstr *, 16> SPs;
    bool Changed = false;

    for (MachineBasicBlock &MBB : MF) {
      Pending.clear(); // markers do not carry across block boundaries
      for (MachineInstr &MI : MBB) {
        unsigned Op = MI.getOpcode();
        if (Op == TargetOpcode::DBG_VALUE || Op == TargetOpcode::DBG_VALUE_LIST ||
            Op == TargetOpcode::DBG_INSTR_REF) {
          if (!isGcRoot(MI.getDebugVariable()))
            continue;
          bool Resolved = false;
          Root Rt;
          if (MI.getNumDebugOperands() != 1) {
            ++NLMulti;
          } else {
            const MachineOperand &MO = MI.getDebugOperand(0);
            if (MO.isDbgInstrRef()) {
              Register R;
              if (resolveInstrRef(MO.getInstrRefInstrIndex(),
                                  MO.getInstrRefOpIndex(), MI, R)) {
                Rt.Reg = R;
                Resolved = true;
              } else {
                ++NUnres;
                ++NLInstrRef;
                if (Diag)
                  errs() << "[m2d-left] " << MF.getName() << " instr-ref"
                         << " useInstrRef=" << MF.useDebugInstrRef() << "\n";
              }
            } else if (MO.isReg() && MO.getReg() && MO.getReg().isVirtual() &&
                       !(Op == TargetOpcode::DBG_VALUE &&
                         MI.isIndirectDebugValue())) {
              // The marker's vreg must have a def that dominates the marker
              // (hence the safepoint), or a use added at the statepoint has no
              // reaching def and crashes RA. A dbg.value can name a vreg not
              // live here.
              MachineInstr *Def = MRI.getVRegDef(MO.getReg());
              if (Def && DT.dominates(Def, &MI)) {
                Rt.Reg = MO.getReg();
                Resolved = true;
              } else {
                ++NDomFail;
                ++NLDom;
              }
            } else if (MO.isFI()) {
              Rt.isFI = true;
              Rt.FI = MO.getIndex();
              Resolved = true;
            } else {
              ++NLForm;
              if (Diag)
                errs() << "[m2d-left] " << MF.getName() << " form op=" << Op
                       << " kind="
                       << (MO.isReg() ? (MO.getReg()
                                             ? (MO.getReg().isVirtual() ? "vreg"
                                                                        : "preg")
                                             : "noreg")
                                      : "other")
                       << " indirect=" << MI.isIndirectDebugValue() << "\n";
            }
          }
          if (Resolved)
            Pending.push_back({Rt, &MI});
          else
            ++NLeft; // left for the post-RA reader
          continue;
        }
        if (Op != TargetOpcode::STATEPOINT)
          continue;
        ++NSP;
        SPs.push_back(&MI);
        if (Pending.empty()) {
          if (const char *FnPat = std::getenv("ROG_M2D_FN"))
            if (MF.getName().contains(FnPat))
              errs() << "[m2d-sp] " << MF.getName()
                     << " id=" << StatepointOpers(&MI).getID()
                     << " pending=0 (empty)\n";
          continue;
        }
        unsigned VI = StatepointOpers(&MI).getVarIdx();
        if (VI + 5 >= MI.getNumOperands() || !MI.getOperand(VI + 5).isImm()) {
          // Malformed statepoint: leave the pending markers for the reader.
          NLeft += Pending.size();
          Pending.clear();
          continue;
        }
        SmallSet<Register, 8> SeenReg;
        SmallSet<int, 8> SeenFI;
        SmallVector<MachineOperand, 16> NewOps;
        unsigned Added = 0; // number of deopt LOCATIONS added (not operands)
        for (const PendingRoot &P : Pending) {
          const Root &Rt = P.Rt;
          if (!Rt.isFI) {
            if (SeenReg.insert(Rt.Reg).second) {
              NewOps.push_back(
                  MachineOperand::CreateReg(Rt.Reg, /*isDef=*/false));
              ++Added;
            }
          } else if (SeenFI.insert(Rt.FI).second) {
            // Pointer in a fixed stack slot: emit the same indirect-memref quad
            // the query pass uses for spilled roots. Strip preserves it, query
            // re-emits it, PEI resolves the frame index to a frame-relative slot.
            NewOps.push_back(
                MachineOperand::CreateImm(StackMaps::IndirectMemRefOp));
            NewOps.push_back(MachineOperand::CreateImm(8)); // pointer size
            NewOps.push_back(MachineOperand::CreateFI(Rt.FI));
            NewOps.push_back(MachineOperand::CreateImm(0));
            ++Added;
          }
        }
        if (Added) {
          unsigned PrevDeopt = MI.getOperand(VI + 5).getImm();
          MI.insert(MI.operands_begin() + (VI + 6), NewOps);
          MI.getOperand(VI + 5).setImm(PrevDeopt + Added);
          NConv += Added;
          Changed = true;
        }
        if (const char *FnPat = std::getenv("ROG_M2D_FN"))
          if (MF.getName().contains(FnPat)) {
            errs() << "[m2d-sp] " << MF.getName()
                   << " id=" << StatepointOpers(&MI).getID()
                   << " pending=" << Pending.size() << " added=" << Added
                   << " regs={";
            for (const PendingRoot &P : Pending)
              if (!P.Rt.isFI)
                errs() << " " << printReg(P.Rt.Reg);
            errs() << " }\n";
          }
        // Only markers whose roots were injected (or deduped into an injected
        // location) are consumed; erase them so the reader does not double-track.
        for (const PendingRoot &P : Pending)
          ToErase.push_back(P.Marker);
        Pending.clear();
      }
      // Dangling markers at block end (no statepoint consumed them): leave them
      // for the post-RA reader.
      NLeft += Pending.size();
      NLDangling += Pending.size();
    }
    for (MachineInstr *MI : ToErase) {
      MI->eraseFromParent();
      Changed = true;
    }
    // Any marker we could NOT convert means this function's precise records
    // under-approximate its live roots (the post-RA reader is best-effort
    // only). Mark every safepoint of the function "incomplete" by setting bit
    // 63 of its statepoint ID: the ID travels into the stackmap record's
    // patchpoint-id field, where the runtime sees the bit and scans such
    // frames fully conservatively instead of trusting the precise record.
    // Functions with no leftover markers (the common case) keep bit 63 clear
    // and are scanned precisely.
    if (NLeft)
      for (MachineInstr *SP : SPs) {
        StatepointOpers SO(SP);
        uint64_t ID = SO.getID();
        SP->getOperand(SO.getIDPos()).setImm(ID | (1ULL << 63));
        Changed = true;
      }
    if (std::getenv("ROG_STRIP_DEOPT_DEBUG") && NSP)
      errs() << "[markers2deopt] " << MF.getName() << " sps=" << NSP
             << " converted=" << NConv << " left=" << NLeft
             << " [multi=" << NLMulti << " iref=" << NLInstrRef
             << " dom=" << NLDom << " form=" << NLForm
             << " dangling=" << NLDangling << "]"
             << " irefdetail(mapmiss=" << NMapMiss << " notdef=" << NNotDef
             << ")\n";
    return Changed;
  }
};
char RogMarkersToDeopt::ID = 0;
} // namespace

namespace llvm {
FunctionPass *createRogStripDeopt() { return new RogStripDeopt(); }
FunctionPass *createRogQueryDeopt() { return new RogQueryDeopt(); }
FunctionPass *createRogGcReadDeopt() { return new RogGcReadDeopt(); }
FunctionPass *createRogMarkersToDeopt() { return new RogMarkersToDeopt(); }
} // namespace llvm

bool FixupStatepointCallerSavedImpl::run(MachineFunction &MF) {
  const Function &F = MF.getFunction();
  if (!F.hasGC())
    return false;

  SmallVector<MachineInstr *, 16> Statepoints;
  for (MachineBasicBlock &BB : MF)
    for (MachineInstr &I : BB)
      if (I.getOpcode() == TargetOpcode::STATEPOINT)
        Statepoints.push_back(&I);

  if (Statepoints.empty())
    return false;

  bool Changed = false;
  StatepointProcessor SPP(MF);
  unsigned NumStatepoints = 0;
  bool AllowGCPtrInCSR = PassGCPtrInCSR;
  for (MachineInstr *I : Statepoints) {
    ++NumStatepoints;
    if (MaxStatepointsWithRegs.getNumOccurrences() &&
        NumStatepoints >= MaxStatepointsWithRegs)
      AllowGCPtrInCSR = false;
    Changed |= SPP.process(*I, AllowGCPtrInCSR);
  }
  return Changed;
}

bool FixupStatepointCallerSavedLegacy::runOnMachineFunction(
    MachineFunction &MF) {
  if (skipFunction(MF.getFunction()))
    return false;

  return FixupStatepointCallerSavedImpl().run(MF);
}

PreservedAnalyses
FixupStatepointCallerSavedPass::run(MachineFunction &MF,
                                    MachineFunctionAnalysisManager &MFAM) {

  if (!FixupStatepointCallerSavedImpl().run(MF))
    return PreservedAnalyses::all();

  auto PA = getMachineFunctionPassPreservedAnalyses();
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
