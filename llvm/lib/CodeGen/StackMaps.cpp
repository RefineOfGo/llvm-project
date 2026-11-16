//===- StackMaps.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/StackMaps.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Instructions.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <map>
#include <utility>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "stackmaps"

static cl::opt<int> StackMapVersion(
    "stackmap-version", cl::init(3), cl::Hidden,
    cl::desc("Specify the stackmap encoding version (default = 3)"));

const char *StackMaps::WSMP = "Stack Maps: ";
static constexpr char ROGStackObjMetadata[] = "rog.stackobj";

static uint64_t getConstMetaVal(const MachineInstr &MI, unsigned Idx) {
  assert(MI.getOperand(Idx).isImm() &&
         MI.getOperand(Idx).getImm() == StackMaps::ConstantOp);
  const auto &MO = MI.getOperand(Idx + 1);
  assert(MO.isImm());
  return MO.getImm();
}

StackMapOpers::StackMapOpers(const MachineInstr *MI)
  : MI(MI) {
  assert(getVarIdx() <= MI->getNumOperands() &&
         "invalid stackmap definition");
}

PatchPointOpers::PatchPointOpers(const MachineInstr *MI)
    : MI(MI), HasDef(MI->getOperand(0).isReg() && MI->getOperand(0).isDef() &&
                     !MI->getOperand(0).isImplicit()) {
#ifndef NDEBUG
  unsigned CheckStartIdx = 0, e = MI->getNumOperands();
  while (CheckStartIdx < e && MI->getOperand(CheckStartIdx).isReg() &&
         MI->getOperand(CheckStartIdx).isDef() &&
         !MI->getOperand(CheckStartIdx).isImplicit())
    ++CheckStartIdx;

  assert(getMetaIdx() == CheckStartIdx &&
         "Unexpected additional definition in Patchpoint intrinsic.");
#endif
}

unsigned PatchPointOpers::getNextScratchIdx(unsigned StartIdx) const {
  if (!StartIdx)
    StartIdx = getVarIdx();

  // Find the next scratch register (implicit def and early clobber)
  unsigned ScratchIdx = StartIdx, e = MI->getNumOperands();
  while (ScratchIdx < e &&
         !(MI->getOperand(ScratchIdx).isReg() &&
           MI->getOperand(ScratchIdx).isDef() &&
           MI->getOperand(ScratchIdx).isImplicit() &&
           MI->getOperand(ScratchIdx).isEarlyClobber()))
    ++ScratchIdx;

  assert(ScratchIdx != e && "No scratch register available");
  return ScratchIdx;
}

unsigned StatepointOpers::getNumGcMapEntriesIdx() {
  // Take index of num of allocas and skip all allocas records.
  unsigned CurIdx = getNumAllocaIdx();
  unsigned NumAllocas = getConstMetaVal(*MI, CurIdx - 1);
  CurIdx++;
  while (NumAllocas--)
    CurIdx = StackMaps::getNextMetaArgIdx(MI, CurIdx);
  return CurIdx + 1; // skip <StackMaps::ConstantOp>
}

unsigned StatepointOpers::getNumAllocaIdx() {
  // Take index of num of gc ptrs and skip all gc ptr records.
  unsigned CurIdx = getNumGCPtrIdx();
  unsigned NumGCPtrs = getConstMetaVal(*MI, CurIdx - 1);
  CurIdx++;
  while (NumGCPtrs--)
    CurIdx = StackMaps::getNextMetaArgIdx(MI, CurIdx);
  return CurIdx + 1; // skip <StackMaps::ConstantOp>
}

unsigned StatepointOpers::getNumGCPtrIdx() {
  // Take index of num of deopt args and skip all deopt records.
  unsigned CurIdx = getNumDeoptArgsIdx();
  unsigned NumDeoptArgs = getConstMetaVal(*MI, CurIdx - 1);
  CurIdx++;
  while (NumDeoptArgs--) {
    CurIdx = StackMaps::getNextMetaArgIdx(MI, CurIdx);
  }
  return CurIdx + 1; // skip <StackMaps::ConstantOp>
}

int StatepointOpers::getFirstGCPtrIdx() {
  unsigned NumGCPtrsIdx = getNumGCPtrIdx();
  unsigned NumGCPtrs = getConstMetaVal(*MI, NumGCPtrsIdx - 1);
  if (NumGCPtrs == 0)
    return -1;
  ++NumGCPtrsIdx; // skip <num gc ptrs>
  assert(NumGCPtrsIdx < MI->getNumOperands());
  return (int)NumGCPtrsIdx;
}

unsigned StatepointOpers::getGCPointerMap(
    SmallVectorImpl<std::pair<unsigned, unsigned>> &GCMap) {
  unsigned CurIdx = getNumGcMapEntriesIdx();
  unsigned GCMapSize = getConstMetaVal(*MI, CurIdx - 1);
  CurIdx++;
  for (unsigned N = 0; N < GCMapSize; ++N) {
    unsigned B = MI->getOperand(CurIdx++).getImm();
    unsigned D = MI->getOperand(CurIdx++).getImm();
    GCMap.push_back(std::make_pair(B, D));
  }

  return GCMapSize;
}

bool StatepointOpers::isFoldableReg(Register Reg) const {
  unsigned FoldableAreaStart = getVarIdx();
  for (const MachineOperand &MO : MI->uses()) {
    if (MO.getOperandNo() >= FoldableAreaStart)
      break;
    if (MO.isReg() && MO.getReg() == Reg)
      return false;
  }
  return true;
}

bool StatepointOpers::isFoldableReg(const MachineInstr *MI, Register Reg) {
  if (MI->getOpcode() != TargetOpcode::STATEPOINT)
    return false;
  return StatepointOpers(MI).isFoldableReg(Reg);
}

StackMaps::StackMaps(AsmPrinter &AP) : AP(AP) {
  if (StackMapVersion != 3)
    llvm_unreachable("Unsupported stackmap version!");
}

unsigned StackMaps::getNextMetaArgIdx(const MachineInstr *MI, unsigned CurIdx) {
  assert(CurIdx < MI->getNumOperands() && "Bad meta arg index");
  const auto &MO = MI->getOperand(CurIdx);
  if (MO.isImm()) {
    switch (MO.getImm()) {
    default:
      llvm_unreachable("Unrecognized operand type.");
    case StackMaps::DirectMemRefOp:
      CurIdx += 2;
      break;
    case StackMaps::IndirectMemRefOp:
      CurIdx += 3;
      break;
    case StackMaps::ConstantOp:
      ++CurIdx;
      break;
    }
  }
  ++CurIdx;
  assert(CurIdx < MI->getNumOperands() && "points past operand list");
  return CurIdx;
}

/// Go up the super-register chain until we hit a valid dwarf register number.
static unsigned getDwarfRegNum(MCRegister Reg, const TargetRegisterInfo *TRI) {
  int RegNum;
  for (MCPhysReg SR : TRI->superregs_inclusive(Reg)) {
    RegNum = TRI->getDwarfRegNum(SR, false);
    if (RegNum >= 0)
      break;
  }

  assert(RegNum >= 0 && isUInt<16>(RegNum) && "Invalid Dwarf register number.");
  return (unsigned)RegNum;
}

MachineInstr::const_mop_iterator
StackMaps::parseOperand(MachineInstr::const_mop_iterator MOI,
                        MachineInstr::const_mop_iterator MOE, LocationVec &Locs,
                        LiveOutVec &LiveOuts) {
  const TargetRegisterInfo *TRI = AP.MF->getSubtarget().getRegisterInfo();
  if (MOI->isImm()) {
    switch (MOI->getImm()) {
    default:
      llvm_unreachable("Unrecognized operand type.");
    case StackMaps::DirectMemRefOp: {
      auto &DL = AP.MF->getDataLayout();

      unsigned Size = DL.getPointerSizeInBits();
      assert((Size % 8) == 0 && "Need pointer size in bytes.");
      Size /= 8;
      Register Reg = (++MOI)->getReg();
      int64_t Imm = (++MOI)->getImm();
      Locs.emplace_back(StackMaps::Location::Direct, Size,
                        getDwarfRegNum(Reg, TRI), Imm);
      break;
    }
    case StackMaps::IndirectMemRefOp: {
      int64_t Size = (++MOI)->getImm();
      assert(Size > 0 && "Need a valid size for indirect memory locations.");
      Register Reg = (++MOI)->getReg();
      int64_t Imm = (++MOI)->getImm();
      Locs.emplace_back(StackMaps::Location::Indirect, Size,
                        getDwarfRegNum(Reg, TRI), Imm);
      break;
    }
    case StackMaps::ConstantOp: {
      ++MOI;
      assert(MOI->isImm() && "Expected constant operand.");
      int64_t Imm = MOI->getImm();
      if (isInt<32>(Imm)) {
        Locs.emplace_back(Location::Constant, sizeof(int64_t), 0, Imm);
      } else {
        auto Result = ConstPool.insert(std::make_pair(Imm, Imm));
        Locs.emplace_back(Location::ConstantIndex, sizeof(int64_t), 0,
                          Result.first - ConstPool.begin());
      }
      break;
    }
    }
    return ++MOI;
  }

  // The physical register number will ultimately be encoded as a DWARF regno.
  // The stack map also records the size of a spill slot that can hold the
  // register content. (The runtime can track the actual size of the data type
  // if it needs to.)
  if (MOI->isReg()) {
    // Skip implicit registers (this includes our scratch registers)
    if (MOI->isImplicit())
      return ++MOI;

    assert(MOI->getReg().isPhysical() &&
           "Virtreg operands should have been rewritten before now.");
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(MOI->getReg());
    assert(!MOI->getSubReg() && "Physical subreg still around.");

    unsigned Offset = 0;
    unsigned DwarfRegNum = getDwarfRegNum(MOI->getReg(), TRI);
    MCRegister LLVMRegNum = *TRI->getLLVMRegNum(DwarfRegNum, false);
    unsigned SubRegIdx = TRI->getSubRegIndex(LLVMRegNum, MOI->getReg());
    if (SubRegIdx)
      Offset = TRI->getSubRegIdxOffset(SubRegIdx);

    Locs.emplace_back(Location::Register, TRI->getSpillSize(*RC),
                      DwarfRegNum, Offset);
    return ++MOI;
  }

  if (MOI->isRegLiveOut())
    LiveOuts = parseRegisterLiveOutMask(MOI->getRegLiveOut());

  return ++MOI;
}

void StackMaps::print(raw_ostream &OS) {
  const TargetRegisterInfo *TRI =
      AP.MF ? AP.MF->getSubtarget().getRegisterInfo() : nullptr;
  OS << WSMP << "callsites:\n";
  for (const auto &CSI : CSInfos) {
    const LocationVec &CSLocs = CSI.Locations;
    const LiveOutVec &LiveOuts = CSI.LiveOuts;

    OS << WSMP << "callsite " << CSI.ID << "\n";
    OS << WSMP << "  has " << CSLocs.size() << " locations\n";

    unsigned Idx = 0;
    for (const auto &Loc : CSLocs) {
      OS << WSMP << "\t\tLoc " << Idx << ": ";
      switch (Loc.Type) {
      case Location::Unprocessed:
        OS << "<Unprocessed operand>";
        break;
      case Location::Register:
        OS << "Register ";
        if (TRI)
          OS << printReg(Loc.Reg, TRI);
        else
          OS << Loc.Reg;
        break;
      case Location::Direct:
        OS << "Direct ";
        if (TRI)
          OS << printReg(Loc.Reg, TRI);
        else
          OS << Loc.Reg;
        if (Loc.Offset)
          OS << " + " << Loc.Offset;
        break;
      case Location::Indirect:
        OS << "Indirect ";
        if (TRI)
          OS << printReg(Loc.Reg, TRI);
        else
          OS << Loc.Reg;
        OS << "+" << Loc.Offset;
        break;
      case Location::Constant:
        OS << "Constant " << Loc.Offset;
        break;
      case Location::ConstantIndex:
        OS << "Constant Index " << Loc.Offset;
        break;
      }
      OS << "\t[encoding: .byte " << Loc.Type << ", .byte 0"
         << ", .short " << Loc.Size << ", .short " << Loc.Reg << ", .short 0"
         << ", .int " << Loc.Offset << "]\n";
      Idx++;
    }

    OS << WSMP << "\thas " << LiveOuts.size() << " live-out registers\n";

    Idx = 0;
    for (const auto &LO : LiveOuts) {
      OS << WSMP << "\t\tLO " << Idx << ": ";
      if (TRI)
        OS << printReg(LO.Reg, TRI);
      else
        OS << LO.Reg;
      OS << "\t[encoding: .short " << LO.DwarfRegNum << ", .byte 0, .byte "
         << LO.Size << "]\n";
      Idx++;
    }
  }
}

/// Create a live-out register record for the given register Reg.
StackMaps::LiveOutReg
StackMaps::createLiveOutReg(unsigned Reg, const TargetRegisterInfo *TRI) const {
  unsigned DwarfRegNum = getDwarfRegNum(Reg, TRI);
  unsigned Size = TRI->getSpillSize(*TRI->getMinimalPhysRegClass(Reg));
  return LiveOutReg(Reg, DwarfRegNum, Size);
}

/// Parse the register live-out mask and return a vector of live-out registers
/// that need to be recorded in the stackmap.
StackMaps::LiveOutVec
StackMaps::parseRegisterLiveOutMask(const uint32_t *Mask) const {
  assert(Mask && "No register mask specified");
  const TargetRegisterInfo *TRI = AP.MF->getSubtarget().getRegisterInfo();
  LiveOutVec LiveOuts;

  // Create a LiveOutReg for each bit that is set in the register mask.
  for (unsigned Reg = 0, NumRegs = TRI->getNumRegs(); Reg != NumRegs; ++Reg)
    if ((Mask[Reg / 32] >> (Reg % 32)) & 1)
      LiveOuts.push_back(createLiveOutReg(Reg, TRI));

  // We don't need to keep track of a register if its super-register is already
  // in the list. Merge entries that refer to the same dwarf register and use
  // the maximum size that needs to be spilled.

  llvm::sort(LiveOuts, [](const LiveOutReg &LHS, const LiveOutReg &RHS) {
    // Only sort by the dwarf register number.
    return LHS.DwarfRegNum < RHS.DwarfRegNum;
  });

  for (auto I = LiveOuts.begin(), E = LiveOuts.end(); I != E; ++I) {
    for (auto *II = std::next(I); II != E; ++II) {
      if (I->DwarfRegNum != II->DwarfRegNum) {
        // Skip all the now invalid entries.
        I = --II;
        break;
      }
      I->Size = std::max(I->Size, II->Size);
      if (I->Reg && TRI->isSuperRegister(I->Reg, II->Reg))
        I->Reg = II->Reg;
      II->Reg = 0; // mark for deletion.
    }
  }

  llvm::erase_if(LiveOuts, [](const LiveOutReg &LO) { return LO.Reg == 0; });

  return LiveOuts;
}

// See statepoint MI format description in StatepointOpers' class comment
// in include/llvm/CodeGen/StackMaps.h
void StackMaps::parseStatepointOpers(const MachineInstr &MI,
                                     MachineInstr::const_mop_iterator MOI,
                                     MachineInstr::const_mop_iterator MOE,
                                     LocationVec &Locations,
                                     LiveOutVec &LiveOuts) {
  LLVM_DEBUG(dbgs() << "record statepoint : " << MI << "\n");
  StatepointOpers SO(&MI);
  MOI = parseOperand(MOI, MOE, Locations, LiveOuts); // CC
  MOI = parseOperand(MOI, MOE, Locations, LiveOuts); // Flags
  MOI = parseOperand(MOI, MOE, Locations, LiveOuts); // Num Deopts

  // Record Deopt Args.
  unsigned NumDeoptArgs = Locations.back().Offset;
  assert(Locations.back().Type == Location::Constant);
  assert(NumDeoptArgs == SO.getNumDeoptArgs());

  while (NumDeoptArgs--)
    MOI = parseOperand(MOI, MOE, Locations, LiveOuts);

  // Record gc base/derived pairs
  assert(MOI->isImm() && MOI->getImm() == StackMaps::ConstantOp);
  ++MOI;
  assert(MOI->isImm());
  unsigned NumGCPointers = MOI->getImm();
  ++MOI;
  if (NumGCPointers) {
    // Map logical index of GC ptr to MI operand index.
    SmallVector<unsigned, 8> GCPtrIndices;
    unsigned GCPtrIdx = (unsigned)SO.getFirstGCPtrIdx();
    assert((int)GCPtrIdx != -1);
    assert(MOI - MI.operands_begin() == GCPtrIdx + 0LL);
    while (NumGCPointers--) {
      GCPtrIndices.push_back(GCPtrIdx);
      GCPtrIdx = StackMaps::getNextMetaArgIdx(&MI, GCPtrIdx);
    }

    SmallVector<std::pair<unsigned, unsigned>, 8> GCPairs;
    unsigned NumGCPairs = SO.getGCPointerMap(GCPairs);
    (void)NumGCPairs;
    LLVM_DEBUG(dbgs() << "NumGCPairs = " << NumGCPairs << "\n");

    auto MOB = MI.operands_begin();
    for (auto &P : GCPairs) {
      assert(P.first < GCPtrIndices.size() && "base pointer index not found");
      assert(P.second < GCPtrIndices.size() &&
             "derived pointer index not found");
      unsigned BaseIdx = GCPtrIndices[P.first];
      unsigned DerivedIdx = GCPtrIndices[P.second];
      LLVM_DEBUG(dbgs() << "Base : " << BaseIdx << " Derived : " << DerivedIdx
                        << "\n");
      (void)parseOperand(MOB + BaseIdx, MOE, Locations, LiveOuts);
      (void)parseOperand(MOB + DerivedIdx, MOE, Locations, LiveOuts);
    }

    MOI = MOB + GCPtrIdx;
  }

  // Record gc allocas
  assert(MOI < MOE);
  assert(MOI->isImm() && MOI->getImm() == StackMaps::ConstantOp);
  ++MOI;
  unsigned NumAllocas = MOI->getImm();
  ++MOI;
  while (NumAllocas--) {
    MOI = parseOperand(MOI, MOE, Locations, LiveOuts);
    assert(MOI < MOE);
  }
}

void StackMaps::recordStackMapOpers(const MCSymbol &MILabel,
                                    const MachineInstr &MI, uint64_t ID,
                                    MachineInstr::const_mop_iterator MOI,
                                    MachineInstr::const_mop_iterator MOE,
                                    bool recordResult) {
  MCContext &OutContext = AP.OutStreamer->getContext();

  LocationVec Locations;
  LiveOutVec LiveOuts;

  if (recordResult) {
    assert(PatchPointOpers(&MI).hasDef() && "Stackmap has no return value.");
    parseOperand(MI.operands_begin(), std::next(MI.operands_begin()), Locations,
                 LiveOuts);
  }

  // Parse operands.
  if (MI.getOpcode() == TargetOpcode::STATEPOINT)
    parseStatepointOpers(MI, MOI, MOE, Locations, LiveOuts);
  else
    while (MOI != MOE)
      MOI = parseOperand(MOI, MOE, Locations, LiveOuts);

  // Create an expression to calculate the offset of the callsite from function
  // entry.
  const MCExpr *CSOffsetExpr = MCBinaryExpr::createSub(
      MCSymbolRefExpr::create(&MILabel, OutContext),
      MCSymbolRefExpr::create(AP.CurrentFnSymForSize, OutContext), OutContext);

  CSInfos.emplace_back(CSOffsetExpr, ID, std::move(Locations),
                       std::move(LiveOuts));

  // Record the stack size of the current function and update callsite count.
  const MachineFrameInfo &MFI = AP.MF->getFrameInfo();
  const TargetRegisterInfo *RegInfo = AP.MF->getSubtarget().getRegisterInfo();
  bool HasDynamicFrameSize =
      MFI.hasVarSizedObjects() || RegInfo->hasStackRealignment(*(AP.MF));
  uint64_t FrameSize = HasDynamicFrameSize ? UINT64_MAX : MFI.getStackSize();

  auto [CurrentIt, Inserted] = FnInfos.try_emplace(AP.CurrentFnSym, FrameSize);
  if (!Inserted) {
    CurrentIt->second.RecordCount++;
    return;
  }

  // ROG precise GC: record the callee-saved-register save area as a [Lo, Hi)
  // byte range relative to the frame pointer. The runtime scans it
  // conservatively to recover GC pointers that outer frames hold in
  // callee-saved registers (physically spilled here by this frame). Taken from
  // MachineFrameInfo, so it is independent of where shrink-wrapping places the
  // actual save instructions.
  const TargetFrameLowering *TFI = AP.MF->getSubtarget().getFrameLowering();
  const TargetLowering *TLI = AP.MF->getSubtarget().getTargetLowering();
  Register SPReg =
      TLI ? TLI->getStackPointerRegisterToSaveRestore() : Register();
  Register FPReg = RegInfo->getFrameRegister(*AP.MF);
  int32_t Lo = 0, Hi = 0;
  bool Any = false;
  for (const CalleeSavedInfo &CSI : MFI.getCalleeSavedInfo()) {
    if (CSI.isSpilledToReg())
      continue;
    int FI = CSI.getFrameIdx();
    Register FrameReg;
    StackOffset Off = TFI->getFrameIndexReference(*AP.MF, FI, FrameReg);
    // Only frame-pointer-relative slots can be located by the runtime (which
    // keys off the saved RBP). With ROG forcing frame pointers this holds for
    // the callee-saved slots; skip anything else defensively.
    if (FrameReg != FPReg)
      continue;
    int32_t O = static_cast<int32_t>(Off.getFixed());
    int32_t Sz = static_cast<int32_t>(MFI.getObjectSize(FI));
    if (!Any) {
      Lo = O;
      Hi = O + Sz;
      Any = true;
    } else {
      Lo = std::min(Lo, O);
      Hi = std::max(Hi, O + Sz);
    }
  }
  CurrentIt->second.CSRLo = Lo;
  CurrentIt->second.CSRHi = Hi;

  SmallVector<FunctionInfo::StackObjectInfo, 16> StackObjects;
  for (int FI = MFI.getObjectIndexBegin(), FE = MFI.getObjectIndexEnd();
       FI != FE; ++FI) {
    if (MFI.isDeadObjectIndex(FI))
      continue;
    const AllocaInst *AI = MFI.getObjectAllocation(FI);
    if (!AI || !AI->getMetadata(ROGStackObjMetadata))
      continue;
    if (MFI.isVariableSizedObjectIndex(FI))
      report_fatal_error("ROG stack object cannot be variable-sized");
    int64_t Size = MFI.getObjectSize(FI);
    if (Size <= 0 || Size > UINT32_MAX)
      report_fatal_error("ROG stack object size is not representable");

    Register FrameReg;
    StackOffset Off = TFI->getFrameIndexReference(*AP.MF, FI, FrameReg);
    using ROGStackObjectKind =
        FunctionInfo::StackObjectInfo::ROGStackObjectKind;
    ROGStackObjectKind Kind;
    if (SPReg && RegInfo->isSuperOrSubRegisterEq(FrameReg, SPReg))
      Kind = FunctionInfo::StackObjectInfo::ROGStackObjectRsp;
    else if (FPReg && RegInfo->isSuperOrSubRegisterEq(FrameReg, FPReg))
      Kind = FunctionInfo::StackObjectInfo::ROGStackObjectRbp;
    else
      report_fatal_error(
          "ROG stack object is not stack-pointer or frame-pointer relative");
    if (Off.getScalable() != 0 || !isInt<32>(Off.getFixed()))
      report_fatal_error("ROG stack object offset is not representable");
    StackObjects.push_back(
        {Kind, static_cast<int32_t>(Off.getFixed()), static_cast<uint32_t>(Size)});
  }
  llvm::sort(StackObjects, [](const FunctionInfo::StackObjectInfo &A,
                              const FunctionInfo::StackObjectInfo &B) {
    if (A.Kind != B.Kind)
      return A.Kind < B.Kind;
    if (A.Offset != B.Offset)
      return A.Offset < B.Offset;
    return A.Size < B.Size;
  });
  StackObjects.erase(std::unique(StackObjects.begin(), StackObjects.end(),
                                 [](const FunctionInfo::StackObjectInfo &A,
                                    const FunctionInfo::StackObjectInfo &B) {
                                   return A.Kind == B.Kind &&
                                          A.Offset == B.Offset &&
                                          A.Size == B.Size;
                                 }),
                     StackObjects.end());
  CurrentIt->second.StackObjects = std::move(StackObjects);
}

void StackMaps::recordStackMap(const MCSymbol &L, const MachineInstr &MI) {
  assert(MI.getOpcode() == TargetOpcode::STACKMAP && "expected stackmap");

  StackMapOpers opers(&MI);
  const int64_t ID = MI.getOperand(PatchPointOpers::IDPos).getImm();
  recordStackMapOpers(L, MI, ID, std::next(MI.operands_begin(),
                                           opers.getVarIdx()),
                      MI.operands_end());
}

void StackMaps::recordPatchPoint(const MCSymbol &L, const MachineInstr &MI) {
  assert(MI.getOpcode() == TargetOpcode::PATCHPOINT && "expected patchpoint");

  PatchPointOpers opers(&MI);
  const int64_t ID = opers.getID();
  auto MOI = std::next(MI.operands_begin(), opers.getStackMapStartIdx());
  recordStackMapOpers(L, MI, ID, MOI, MI.operands_end(),
                      opers.isAnyReg() && opers.hasDef());

#ifndef NDEBUG
  // verify anyregcc
  auto &Locations = CSInfos.back().Locations;
  if (opers.isAnyReg()) {
    unsigned NArgs = opers.getNumCallArgs();
    for (unsigned i = 0, e = (opers.hasDef() ? NArgs + 1 : NArgs); i != e; ++i)
      assert(Locations[i].Type == Location::Register &&
             "anyreg arg must be in reg.");
  }
#endif
}

void StackMaps::recordStatepoint(const MCSymbol &L, const MachineInstr &MI) {
  assert(MI.getOpcode() == TargetOpcode::STATEPOINT && "expected statepoint");

  StatepointOpers opers(&MI);
  const unsigned StartIdx = opers.getVarIdx();
  recordStackMapOpers(L, MI, opers.getID(), MI.operands_begin() + StartIdx,
                      MI.operands_end(), false);
}

/// Emit the stackmap header.
///
/// Header {
///   uint8  : Stack Map Version (currently 3)
///   uint8  : Reserved (expected to be 0)
///   uint16 : Reserved (expected to be 0)
/// }
/// uint32 : NumFunctions
/// uint32 : NumConstants
/// uint32 : NumRecords
void StackMaps::emitStackmapHeader(MCStreamer &OS) {
  // Header.
  OS.AddComment("Version");
  OS.emitIntValue(StackMapVersion, 1); // Version.
  OS.emitIntValue(0, 1);               // Reserved.
  OS.emitInt16(0);                     // Reserved.

  // Num functions.
  LLVM_DEBUG(dbgs() << WSMP << "#functions = " << FnInfos.size() << '\n');
  OS.AddComment("  Num Functions");
  OS.emitInt32(FnInfos.size());
  // Num constants.
  LLVM_DEBUG(dbgs() << WSMP << "#constants = " << ConstPool.size() << '\n');
  OS.AddComment("  Num Constants");
  OS.emitInt32(ConstPool.size());
  // Num callsites.
  LLVM_DEBUG(dbgs() << WSMP << "#callsites = " << CSInfos.size() << '\n');
  OS.AddComment("  Num CallSites");
  OS.emitInt32(CSInfos.size());
}

/// Emit the function frame record for each function.
///
/// StkSizeRecord[NumFunctions] {
///   uint64 : Function Address
///   uint64 : Stack Size
///   uint64 : Record Count
/// }
void StackMaps::emitFunctionFrameRecords(MCStreamer &OS) {
  // Function Frame records.
  LLVM_DEBUG(dbgs() << WSMP << "functions:\n");
  for (auto const &FR : FnInfos) {
    LLVM_DEBUG(dbgs() << WSMP << "function addr: " << FR.first
                      << " frame size: " << FR.second.StackSize
                      << " callsite count: " << FR.second.RecordCount << '\n');
    OS.AddComment("Function Address");
    OS.emitSymbolValue(FR.first, 8);
    OS.AddComment("  Stack Size");
    OS.emitIntValue(FR.second.StackSize, 8);
    OS.AddComment("  Record Count");
    OS.emitIntValue(FR.second.RecordCount, 8);
  }
}

/// Emit the constant pool.
///
/// int64  : Constants[NumConstants]
void StackMaps::emitConstantPoolEntries(MCStreamer &OS) {
  // Constant pool entries.
  int N = 0;
  LLVM_DEBUG(dbgs() << WSMP << "constants:\n");
  for (const auto &ConstEntry : ConstPool) {
    LLVM_DEBUG(dbgs() << WSMP << ConstEntry.second << '\n');
    OS.AddComment("Constant #" + Twine(N++));
    OS.emitIntValue(ConstEntry.second, 8);
  }
}

/// Emit the callsite info for each callsite.
///
/// StkMapRecord[NumRecords] {
///   uint64 : PatchPoint ID
///   uint32 : Instruction Offset
///   uint16 : Reserved (record flags)
///   uint16 : NumLocations
///   Location[NumLocations] {
///     uint8  : Register | Direct | Indirect | Constant | ConstantIndex
///     uint8  : Size in Bytes
///     uint16 : Dwarf RegNum
///     int32  : Offset
///   }
///   uint16 : Padding
///   uint16 : NumLiveOuts
///   LiveOuts[NumLiveOuts] {
///     uint16 : Dwarf RegNum
///     uint8  : Reserved
///     uint8  : Size in Bytes
///   }
///   uint32 : Padding (only if required to align to 8 byte)
/// }
///
/// Location Encoding, Type, Value:
///   0x1, Register, Reg                 (value in register)
///   0x2, Direct, Reg + Offset          (frame index)
///   0x3, Indirect, [Reg + Offset]      (spilled value)
///   0x4, Constant, Offset              (small constant)
///   0x5, ConstIndex, Constants[Offset] (large constant)
void StackMaps::emitCallsiteEntry(MCStreamer &OS, const CallsiteInfo &CSI) {
  const LocationVec &CSLocs = CSI.Locations;
  const LiveOutVec &LiveOuts = CSI.LiveOuts;

  // Verify stack map entry. It's better to communicate a problem to the
  // runtime than crash in case of in-process compilation. Currently, we do
  // simple overflow checks, but we may eventually communicate other
  // compilation errors this way.
  if (CSLocs.size() > UINT16_MAX || LiveOuts.size() > UINT16_MAX) {
    OS.AddComment("Invalid CallSite");
    OS.emitIntValue(UINT64_MAX, 8); // Invalid ID.
    OS.AddComment("  Offset");
    OS.emitValue(CSI.CSOffsetExpr, 4);
    OS.emitInt16(0); // Reserved.
    OS.AddComment("  Num Locations");
    OS.emitInt16(0); // 0 locations.
    OS.emitInt16(0); // padding.
    OS.AddComment("  Num Live-out Registers");
    OS.emitInt16(0); // 0 live-out registers.
    OS.emitInt32(0); // padding.
    return;
  }

  OS.AddComment("PatchPoint #" + Twine(CSI.ID));
  OS.emitIntValue(CSI.ID, 8);
  OS.AddComment("  Offset");
  OS.emitValue(CSI.CSOffsetExpr, 4);

  // Reserved for flags.
  OS.emitInt16(0);
  OS.AddComment("  Num Locations");
  OS.emitInt16(CSLocs.size());

  for (const auto &Loc : CSLocs) {
    switch (Loc.Type) {
      case Location::Unprocessed   : OS.AddComment("    Location: Unprocessed"  ); break;
      case Location::Register      : OS.AddComment("    Location: Register"     ); break;
      case Location::Direct        : OS.AddComment("    Location: Direct"       ); break;
      case Location::Indirect      : OS.AddComment("    Location: Indirect"     ); break;
      case Location::Constant      : OS.AddComment("    Location: Constant"     ); break;
      case Location::ConstantIndex : OS.AddComment("    Location: ConstantIndex"); break;
    }
    OS.emitIntValue(Loc.Type, 1);
    OS.emitIntValue(0, 1);  // Reserved
    OS.AddComment("    Size");
    OS.emitInt16(Loc.Size);
    OS.AddComment("    Register");
    OS.emitInt16(Loc.Reg);
    OS.emitInt16(0); // Reserved
    OS.AddComment("    Offset");
    OS.emitInt32(Loc.Offset);
  }

  // Emit alignment to 8 byte.
  OS.emitValueToAlignment(Align(8));

  // Num live-out registers and padding to align to 4 byte.
  OS.emitInt16(0);
  OS.AddComment("  Num LiveOuts");
  OS.emitInt16(LiveOuts.size());

  for (const auto &LO : LiveOuts) {
    OS.AddComment("    DWARF Reg Num");
    OS.emitInt16(LO.DwarfRegNum);
    OS.emitIntValue(0, 1);
    OS.AddComment("    Size");
    OS.emitIntValue(LO.Size, 1);
  }
  // Emit alignment to 8 byte.
  OS.emitValueToAlignment(Align(8));
}

void StackMaps::emitCallsiteEntries(MCStreamer &OS) {
  LLVM_DEBUG(print(dbgs()));
  // Callsite entries.
  for (const auto &CSI : CSInfos)
    emitCallsiteEntry(OS, CSI);
}

/// Serialize the stackmap data.
void StackMaps::serializeToStackMapSection() {
  (void)WSMP;
  // Bail out if there's no stack map data.
  assert((!CSInfos.empty() || ConstPool.empty()) &&
         "Expected empty constant pool too!");
  assert((!CSInfos.empty() || FnInfos.empty()) &&
         "Expected empty function record too!");
  if (CSInfos.empty())
    return;

  MCContext &OutContext = AP.OutStreamer->getContext();
  MCStreamer &OS = *AP.OutStreamer;

  // ROG precise GC: on ELF with an empty constant pool (always the case for
  // ROG's deopt-pointer maps), emit one SHF_LINK_ORDER section per function so
  // the map follows --gc-sections function liveness instead of being kept
  // wholesale (which would resurrect dead functions through their recorded
  // addresses). Fall back to the single-section layout otherwise: ConstantIndex
  // locations are pool-relative and cannot be split per function.
  if (OutContext.isELF() && ConstPool.empty()) {
    serializeToStackMapSectionPerFunction();
    CSInfos.clear();
    ConstPool.clear();
    return;
  }

  // Create the section.
  MCSection *StackMapSection =
      OutContext.getObjectFileInfo()->getStackMapSection();
  OS.switchSection(StackMapSection);

  // Emit a dummy symbol to force section inclusion.
  OS.emitLabel(OutContext.getOrCreateSymbol(Twine("__LLVM_StackMaps")));

  // Serialize data.
  LLVM_DEBUG(dbgs() << "********** Stack Map Output **********\n");
  emitStackmapHeader(OS);
  emitFunctionFrameRecords(OS);
  emitConstantPoolEntries(OS);
  emitCallsiteEntries(OS);
  OS.addBlankLine();

  // Clean up.
  CSInfos.clear();
  ConstPool.clear();
}

/// ROG precise GC: emit one self-describing stack-map blob per function, each in
/// its own `.llvm_stackmaps` section tagged SHF_LINK_ORDER and linked to the
/// function's text (and placed in the function's comdat group, if any). The
/// linker keeps each blob exactly when it keeps the function and merges the
/// survivors into the final `.llvm_stackmaps`; the runtime reads it as the same
/// concatenation of self-describing blobs. The caller guarantees an empty
/// constant pool, so no cross-function constant references need preserving.
void StackMaps::serializeToStackMapSectionPerFunction() {
  MCContext &OutContext = AP.OutStreamer->getContext();
  MCStreamer &OS = *AP.OutStreamer;

  LLVM_DEBUG(dbgs() << "***** Stack Map Output (per-function) *****\n");

  // CSInfos is ordered by function in FnInfos order, RecordCount entries each;
  // walk both in lockstep.
  unsigned CSIdx = 0;
  for (const auto &[FnSym, FnInfo] : FnInfos) {
    // Mirror the function's comdat group (if any) and link the section to the
    // function's text via SHF_LINK_ORDER, so --gc-sections and comdat dedup
    // drop the blob together with the function.
    // On ELF (guaranteed by the caller) every symbol/section is an ELF variant.
    const MCSymbolELF *FnSymELF = static_cast<const MCSymbolELF *>(FnSym);
    const MCSymbolELF *Group = nullptr;
    if (FnSym->isInSection())
      Group = static_cast<const MCSectionELF &>(FnSym->getSection()).getGroup();

    unsigned Flags = ELF::SHF_ALLOC | ELF::SHF_LINK_ORDER;
    if (Group)
      Flags |= ELF::SHF_GROUP;

    MCSectionELF *Sec = OutContext.getELFSection(
        ".llvm_stackmaps", ELF::SHT_PROGBITS, Flags, /*EntrySize=*/0, Group,
        /*IsComdat=*/Group != nullptr, MCSection::NonUniqueID, FnSymELF);
    OS.switchSection(Sec);

    emitCompactFunctionBlob(OS, FnSym, FnInfo, CSIdx);
    CSIdx += FnInfo.RecordCount;

    OS.addBlankLine();
  }
  assert(CSIdx == CSInfos.size() &&
         "per-function callsite count must cover all records");
}

/// ROG compact stackmap 0x55: each function owns its roots, stack objects,
/// deduplicated root/object bitmaps, and safepoint records. The root projection
/// preserves the old location order and values. Objects from per-PC Direct
/// descriptors and the function table share a normalized scan-range encoding;
/// their original offsets remain available for retention attribution.
///
/// Layout (little-endian, 8-byte aligned):
///   Header[64]: version/reserved/header_bytes/blob_bytes,
///     PC-relative FunctionAddress, StackSize, CSRLo/CSRHi,
///     NumRoots/NumObjects/NumRootSets/NumObjectSets/NumRecords,
///     FunctionObjectSet (UINT32_MAX if absent), reserved u64
///   Object[NumObjects]: u8 base, u8 0, u16 0, u32 words,
///                       i64 byte_offset, i32 owner_offset, u32 0
///   Root[NumRoots]: u8 kind, u8 0, u16 dwarf_reg, i32 offset, u32 size
///   <pad to 8>
///   RootBits[NumRootSets][ceil(NumRoots/64)]
///   ObjectBits[NumObjectSets][ceil(NumObjects/64)]
///   Record[NumRecords]: u32 instr_offset, u32 root_set, u32 object_set,
///                       u32 flags (bit 0: prologue entry)
void StackMaps::emitCompactFunctionBlob(MCStreamer &OS, const MCSymbol *FnSym,
                                        const FunctionInfo &FnInfo,
                                        unsigned StartIdx) {
  constexpr uint32_t WitnessFlag = uint32_t(1) << 30;
  constexpr uint64_t PreciseID = uint64_t(1) << 63;
  constexpr uint64_t PrologueID = uint64_t(1) << 62;
  auto checkedCount = [](uint64_t Count) -> uint32_t {
    if (Count > UINT32_MAX)
      report_fatal_error("ROG compact stackmap count exceeds 32 bits");
    return uint32_t(Count);
  };
  struct Slot {
    uint8_t Kind;
    uint16_t Reg;
    int32_t Offset;
    uint32_t SizeFlags;
  };
  struct Object {
    uint8_t Base;
    uint32_t Words;
    int64_t Offset;
    int32_t OwnerOffset;
  };
  struct Rec {
    const MCExpr *Offset;
    uint32_t Slots;
    bool Precise;
    bool Prologue;
    uint32_t RootSet = 0;
    uint32_t ObjectSet = 0;
  };
  SmallVector<Slot, 32> Dict;
  DenseMap<std::pair<uint64_t, uint64_t>, uint32_t> DictIdx;
  std::vector<std::vector<uint32_t>> SlotSets;
  std::map<std::vector<uint32_t>, uint32_t> SlotSetIdx;
  std::vector<Rec> Recs;
  Recs.reserve(FnInfo.RecordCount);

  for (uint64_t I = 0; I < FnInfo.RecordCount; ++I) {
    const CallsiteInfo &CSI = CSInfos[StartIdx + I];
    // Fold size annotations before interning locations in their original order.
    SmallVector<Slot, 64> RecSlots;
    bool AwaitingSize = false;
    for (const Location &Loc : CSI.Locations) {
      switch (Loc.Type) {
      case Location::Register:
      case Location::Indirect:
        RecSlots.push_back(
            {uint8_t(Loc.Type), Loc.Reg, Loc.Offset, /*SizeFlags=*/0});
        AwaitingSize = false;
        break;
      case Location::Direct:
        RecSlots.push_back(
            {uint8_t(Loc.Type), Loc.Reg, Loc.Offset, /*SizeFlags=*/0});
        AwaitingSize = true;
        break;
      case Location::Constant:
        if (AwaitingSize) {
          RecSlots.back().SizeFlags =
              uint32_t(std::max<int32_t>(Loc.Offset, 0));
          AwaitingSize = false;
        }
        break;
      default:
        AwaitingSize = false;
        break;
      }
    }
    std::vector<uint32_t> Set;
    for (const Slot &S : RecSlots) {
      auto Key =
          std::make_pair((uint64_t(S.Kind) << 48) | (uint64_t(S.Reg) << 32) |
                             uint64_t(uint32_t(S.Offset)),
                         uint64_t(S.SizeFlags));
      auto [It, New] = DictIdx.try_emplace(Key, checkedCount(Dict.size()));
      if (New)
        Dict.push_back(S);
      Set.push_back(It->second);
    }
    llvm::sort(Set);
    Set.erase(llvm::unique(Set), Set.end());
    auto [SetIt, NewSet] =
        SlotSetIdx.try_emplace(Set, checkedCount(SlotSets.size()));
    if (NewSet)
      SlotSets.push_back(std::move(Set));
    Recs.push_back({CSI.CSOffsetExpr, SetIt->second, bool(CSI.ID & PreciseID),
                    bool(CSI.ID & PrologueID)});
  }

  // Preserve original slot identities, including two different size-flag
  // annotations that normalize to the same object. This also preserves owner
  // selection order when stack coloring maps several objects to one base.
  std::vector<Object> Objects;
  std::vector<uint32_t> DirectObjects(Dict.size(), UINT32_MAX);
  for (const Rec &R : Recs) {
    if (!R.Precise || R.Prologue)
      continue;
    for (uint32_t Index : SlotSets[R.Slots]) {
      const Slot &S = Dict[Index];
      if (S.Kind != Location::Direct)
        continue;
      DirectObjects[Index] = 0; // Mark slots requiring an object projection.
    }
  }
  for (uint32_t Index = 0; Index < Dict.size(); ++Index) {
    if (DirectObjects[Index] == UINT32_MAX)
      continue;
    const Slot &S = Dict[Index];
    if (S.Reg != 6 && S.Reg != 7)
      report_fatal_error("ROG stack object must be RSP- or RBP-relative");
    // The x86-64 frame bases are word aligned. Match stackmap_location's
    // inward alignment, using signed 64-bit offsets to avoid truncating an
    // i32 offset whose aligned successor is outside the i32 range.
    if (FnInfo.StackSize != UINT64_MAX && FnInfo.StackSize % 8 != 0)
      report_fatal_error("ROG stack object frame is not word aligned");
    int64_t Offset = S.Offset;
    int64_t Start = Offset + ((8 - (Offset & 7)) & 7);
    int64_t Bytes = std::max<uint32_t>(S.SizeFlags & ~WitnessFlag, 8);
    int64_t End = (Offset + Bytes) & ~int64_t(7);
    DirectObjects[Index] = checkedCount(Objects.size());
    Objects.push_back({uint8_t(S.Reg == 7 ? 1 : 2),
                       checkedCount(End > Start ? (End - Start) / 8 : 0), Start,
                       S.Offset});
  }

  std::vector<uint32_t> FunctionObjects;
  for (const FunctionInfo::StackObjectInfo &S : FnInfo.StackObjects) {
    FunctionObjects.push_back(checkedCount(Objects.size()));
    Objects.push_back({uint8_t(S.Kind),
                       checkedCount((uint64_t(S.Size) + 7) / 8), S.Offset,
                       S.Offset});
  }

  // The zero bitmap is always set 0, including when its dictionary is empty.
  std::vector<std::vector<uint32_t>> RootSets(1), ObjectSets(1);
  std::map<std::vector<uint32_t>, uint32_t> RootSetIdx{{{}, 0}},
      ObjectSetIdx{{{}, 0}};
  auto intern = [&](const std::vector<uint32_t> &Set,
                    std::vector<std::vector<uint32_t>> &Sets,
                    std::map<std::vector<uint32_t>, uint32_t> &Indices) {
    auto [It, New] = Indices.try_emplace(Set, checkedCount(Sets.size()));
    if (New)
      Sets.push_back(Set);
    return It->second;
  };
  uint32_t FunctionObjectSet = UINT32_MAX;
  if (!FunctionObjects.empty())
    FunctionObjectSet = intern(FunctionObjects, ObjectSets, ObjectSetIdx);
  for (Rec &R : Recs) {
    std::vector<uint32_t> Roots, EligibleObjects;
    for (uint32_t Index : SlotSets[R.Slots]) {
      const Slot &S = Dict[Index];
      bool Witness = S.Kind == Location::Direct && (S.SizeFlags & WitnessFlag);
      if (!R.Precise || !Witness)
        Roots.push_back(Index);
      if (R.Precise && !R.Prologue && S.Kind == Location::Direct)
        EligibleObjects.push_back(DirectObjects[Index]);
    }
    if (!R.Precise && !R.Prologue)
      EligibleObjects = FunctionObjects;
    R.RootSet = intern(Roots, RootSets, RootSetIdx);
    // The stack-growth call precedes frame setup. Its incoming-argument
    // roots remain valid, but no body-frame objects exist yet, regardless of
    // whether the ID also carries the authoritative-object bit.
    R.ObjectSet = R.Prologue ? 0 : intern(EligibleObjects, ObjectSets, ObjectSetIdx);
  }

  uint32_t NumRoots = checkedCount(Dict.size());
  uint32_t NumObjects = checkedCount(Objects.size());
  uint32_t NumRootSets = checkedCount(RootSets.size());
  uint32_t NumObjectSets = checkedCount(ObjectSets.size());
  uint32_t NumRecords = checkedCount(Recs.size());
  uint64_t RootWords = (uint64_t(NumRoots) + 63) / 64;
  uint64_t ObjectWords = (uint64_t(NumObjects) + 63) / 64;
  uint64_t BlobBytes = alignTo(uint64_t(64) + uint64_t(24) * NumObjects +
                                   uint64_t(12) * NumRoots,
                               Align(8)) +
                       RootWords * 8 * NumRootSets +
                       ObjectWords * 8 * NumObjectSets +
                       uint64_t(16) * NumRecords;

  OS.emitValueToAlignment(Align(8));
  OS.AddComment("ROG compact stackmap version");
  OS.emitIntValue(0x55, 1);
  OS.emitIntValue(0, 1);
  OS.emitInt16(64);
  OS.emitInt32(checkedCount(BlobBytes));
  // Field-relative function addresses require no DSO dynamic relocation.
  MCContext &Ctx = OS.getContext();
  MCSymbol *Here = Ctx.createTempSymbol();
  OS.emitLabel(Here);
  OS.emitValue(MCBinaryExpr::createSub(MCSymbolRefExpr::create(FnSym, Ctx),
                                       MCSymbolRefExpr::create(Here, Ctx), Ctx),
               8);
  OS.emitIntValue(FnInfo.StackSize, 8);
  OS.emitInt32(uint32_t(FnInfo.CSRLo));
  OS.emitInt32(uint32_t(FnInfo.CSRHi));
  OS.emitInt32(NumRoots);
  OS.emitInt32(NumObjects);
  OS.emitInt32(NumRootSets);
  OS.emitInt32(NumObjectSets);
  OS.emitInt32(NumRecords);
  OS.emitInt32(FunctionObjectSet);
  OS.emitIntValue(0, 8);

  for (const Object &O : Objects) {
    OS.emitIntValue(O.Base, 1);
    OS.emitIntValue(0, 1);
    OS.emitInt16(0);
    OS.emitInt32(O.Words);
    OS.emitIntValue(uint64_t(O.Offset), 8);
    OS.emitInt32(uint32_t(O.OwnerOffset));
    OS.emitInt32(0);
  }
  for (const Slot &S : Dict) {
    OS.emitIntValue(S.Kind, 1);
    OS.emitIntValue(0, 1);
    OS.emitInt16(S.Reg);
    OS.emitInt32(uint32_t(S.Offset));
    OS.emitInt32(S.SizeFlags);
  }
  OS.emitValueToAlignment(Align(8));
  auto emitSets = [&](const std::vector<std::vector<uint32_t>> &Sets,
                      uint64_t Words) {
    SmallVector<uint64_t, 64> Bits;
    for (const auto &Set : Sets) {
      Bits.assign(Words, 0);
      for (uint32_t Index : Set)
        Bits[Index / 64] |= uint64_t(1) << (Index % 64);
      for (uint64_t Word : Bits)
        OS.emitIntValue(Word, 8);
    }
  };
  emitSets(RootSets, RootWords);
  emitSets(ObjectSets, ObjectWords);
  for (const Rec &R : Recs) {
    OS.emitValue(R.Offset, 4);
    OS.emitInt32(R.RootSet);
    OS.emitInt32(R.ObjectSet);
    OS.emitInt32(R.Prologue ? 1 : 0);
  }
}
