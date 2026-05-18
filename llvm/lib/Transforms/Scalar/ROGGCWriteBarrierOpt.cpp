//===- ROGGCWriteBarrierOpt.cpp - ROG GC write barrier opts --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar/ROGGCWriteBarrierOpt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/MemoryLocation.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/MemorySSAUpdater.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/ROGGC.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include <cassert>

using namespace llvm;

// This pass reduces ROG GC barrier work in three phases:
//
// Phase 1 — Bulk barrier decomposition: For bulk barriers with
// !rog.bulk_write_barrier.ptrmap metadata and a constant size, decompose
// rog_bulk_write_barrier into individual rog_write_barrier_2 calls for each
// pointer slot when the total pointer count is at most 4. Self-copies are
// also eliminated here.
//
// Phase 2 — InstCombine: Run InstCombine on functions modified by Phase 1 to
// simplify the generated GEP + load + freeze patterns.
//
// Phase 3 — WB2 simplification: For scalar barriers it peels the CodeGen
// materialization of the old value, uses MemorySSA plus alias analysis to
// prove the destination slot still points into a fresh alloc-like object, and
// folds rog_write_barrier_2(old, new) to either nothing when old and new are
// known equal or both sides are ignorable, or rog_write_barrier_1(live).
// For remaining bulk barriers (without ptrmap or with too many pointers), it
// reuses the same fresh-and-untouched proof for the destination range and
// downgrades rog_bulk_write_barrier(dest, src, size) to
// rog_src_bulk_write_barrier(src, size) when only the incoming source values
// still need scanning.
//
// The MemorySSA walk skips barrier helper calls as bookkeeping-only clobbers
// and otherwise stays conservative at real writes or merged control flow.

namespace {

static FunctionCallee getOrInsertROGWriteBarrier1(Module *M, Type *ArgTy) {
  GlobalValue *Callee = M->getNamedValue("rog_write_barrier_1");
  FunctionType *Ty = FunctionType::get(Type::getVoidTy(M->getContext()),
                                       {ArgTy}, false);

  if (Callee != nullptr) {
    assert(cast<Function>(Callee)->getCallingConv() == CallingConv::ROG &&
           "rog_write_barrier_1 should have ROG calling convention");
    return FunctionCallee(Ty, Callee);
  }

  Function *Fn = Function::Create(Ty, GlobalValue::ExternalWeakLinkage,
                                  "rog_write_barrier_1", *M);
  Fn->setCallingConv(CallingConv::ROG);
  return FunctionCallee(Ty, Fn);
}

static FunctionCallee getOrInsertROGSrcBulkWriteBarrier(Module *M, Type *PtrTy,
                                                        Type *SizeTy) {
  GlobalValue *Callee = M->getNamedValue("rog_src_bulk_write_barrier");
  FunctionType *Ty = FunctionType::get(Type::getVoidTy(M->getContext()),
                                       {PtrTy, SizeTy}, false);

  if (Callee != nullptr) {
    assert(cast<Function>(Callee)->getCallingConv() == CallingConv::ROG &&
           "rog_src_bulk_write_barrier should have ROG calling convention");
    return FunctionCallee(Ty, Callee);
  }

  Function *Fn = Function::Create(Ty, GlobalValue::ExternalWeakLinkage,
                                  "rog_src_bulk_write_barrier", *M);
  Fn->setCallingConv(CallingConv::ROG);
  return FunctionCallee(Ty, Fn);
}

static FunctionCallee getOrInsertROGWriteBarrier2(Module *M) {
  GlobalValue *Callee = M->getNamedValue("rog_write_barrier_2");
  Type *I64 = Type::getInt64Ty(M->getContext());
  FunctionType *Ty =
      FunctionType::get(Type::getVoidTy(M->getContext()), {I64, I64}, false);

  if (Callee != nullptr) {
    assert(cast<Function>(Callee)->getCallingConv() == CallingConv::ROG &&
           "rog_write_barrier_2 should have ROG calling convention");
    return FunctionCallee(Ty, Callee);
  }

  Function *Fn = Function::Create(Ty, GlobalValue::ExternalWeakLinkage,
                                  "rog_write_barrier_2", *M);
  Fn->setCallingConv(CallingConv::ROG);
  return FunctionCallee(Ty, Fn);
}

} // namespace

static bool isStaticDataAddress(const Value *V) {
  if (isa<GlobalVariable>(V) || isa<GlobalAlias>(V))
    return true;

  auto *Expr = dyn_cast<ConstantExpr>(V);
  if (Expr == nullptr)
    return false;

  switch (Expr->getOpcode()) {
  case Instruction::PtrToInt:
  case Instruction::IntToPtr:
  case Instruction::BitCast:
  case Instruction::AddrSpaceCast:
  case Instruction::GetElementPtr:
    return isStaticDataAddress(Expr->getOperand(0));
  default:
    return false;
  }
}

static bool isConstZero(const Value *V) {
  if (auto *IntVal = dyn_cast<ConstantInt>(V))
    return IntVal->isZero();
  return false;
}

static bool areKnownEqualI64(Value *LHS, Value *RHS) {
  assert(LHS->getType()->isIntegerTy(64) && RHS->getType()->isIntegerTy(64) &&
         "rog_write_barrier_2 arguments should be i64");
  return LHS == RHS;
}

static bool hasAllocKind(AllocFnKind Kind, AllocFnKind Wanted) {
  return (Kind & Wanted) == Wanted;
}

static bool isNoAliasAllocCall(const CallBase *CB) {
  if (!CB || !CB->hasRetAttr(Attribute::NoAlias))
    return false;
  Attribute AllocKindAttr = CB->getFnAttr(Attribute::AllocKind);
  if (!AllocKindAttr.isValid())
    return false;
  AllocFnKind Kind = static_cast<AllocFnKind>(AllocKindAttr.getValueAsInt());
  return hasAllocKind(Kind, AllocFnKind::Alloc);
}

namespace {

class MemTracker {
  const CallBase *AllocCall;
  MemorySSA &MSSA;
  BatchAAResults &AA;

public:
  MemTracker(const CallBase *AllocCall, MemorySSA &MSSA, BatchAAResults &AA)
      : AllocCall(AllocCall), MSSA(MSSA), AA(AA) {}

  bool run(MemoryUseOrDef *MA, MemoryLocation Loc) {
    if (!MA)
      return false;
    MemoryAccess *Clobber = MSSA.getWalker()->getClobberingMemoryAccess(
        MA->getDefiningAccess(), Loc, AA);
    while (true) {
      if (auto *MD = dyn_cast<MemoryDef>(Clobber)) {
        // If the clobbering walk reaches the allocation itself, the queried
        // location has no intervening writes between allocation and use.
        if (MSSA.dominates(Clobber, MSSA.getMemoryAccess(AllocCall))) {
          assert(MD->getMemoryInst() == AllocCall);
          return true;
        }
        auto *CI = dyn_cast<CallInst>(MD->getMemoryInst());
        if (CI) {
          auto *Callee =
              dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
          // Skip barrier helpers as clobbers: they update GC bookkeeping but do
          // not overwrite the tracked program location.
          if (Callee && (Callee->getName() == "rog_write_barrier_2" ||
                         Callee->getName() == "rog_write_barrier_1" ||
                         Callee->getName() == "rog_bulk_write_barrier" ||
                         Callee->getName() == "rog_src_bulk_write_barrier")) {
            Clobber = MSSA.getWalker()->getClobberingMemoryAccess(
                MD->getDefiningAccess(), Loc, AA);
            continue;
          }
        }
      }
      if (auto *MP = dyn_cast<MemoryPhi>(Clobber)) {
        // TODO: support PHI if needed.
        // Conservatively give up when the clobber path merges through a phi.
        (void)MP;
        return false;
      }
      // Any other clobber means the queried location is no longer provably
      // untouched.
      return false;
    }
  }
};

} // namespace

/// Check whether Ptr refers to a location in a fresh noalias alloc-like object
/// whose queried bytes have not been clobbered before BeforeInst.
static bool isPointerToNewlyAllocatedMemory(const Value *Ptr,
                                            const Instruction *BeforeInst,
                                            MemorySSA &MSSA, BatchAAResults &AA,
                                            LocationSize LocSize) {
  if (Ptr == nullptr)
    return false;
  const Value *Base = getUnderlyingObject(Ptr);
  auto *AllocCall = dyn_cast<CallBase>(Base);
  if (!isNoAliasAllocCall(AllocCall))
    return false;
  MemoryLocation Loc(Ptr, LocSize);
  MemTracker Tracker{AllocCall, MSSA, AA};
  return Tracker.run(MSSA.getMemoryAccess(BeforeInst), Loc);
}

static bool isWriteBarrier2Call(const CallInst *CI) {
  auto *Callee = dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
  return Callee != nullptr && Callee->getName() == "rog_write_barrier_2";
}

static bool isBulkWriteBarrierCall(const CallInst *CI) {
  auto *Callee =
      dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
  return Callee != nullptr && Callee->getName() == "rog_bulk_write_barrier";
}

namespace {
struct UnmaterializeWriteBarrier2 {
  const Value *OldValue;
  const Value *OldSlot;
  const Value *NewValue;

  UnmaterializeWriteBarrier2(CallInst *CI) {
    OldValue = CI->getArgOperand(0);
    if (auto *Freeze = dyn_cast<FreezeInst>(OldValue))
      OldValue = Freeze->getOperand(0);
    auto *Load = dyn_cast<LoadInst>(OldValue);
    OldSlot = Load == nullptr ? nullptr : Load->getPointerOperand();

    NewValue = CI->getArgOperand(1);
    if (auto *PtrToInt = dyn_cast<PtrToIntInst>(NewValue))
      NewValue = PtrToInt->getOperand(0);
  }
};
} // namespace

static bool simplifyWriteBarrier2Call(CallInst *CI, MemorySSA &MSSA,
                                      BatchAAResults &AA,
                                      MemorySSAUpdater &MSSAU) {
  assert(isWriteBarrier2Call(CI) && "expected rog_write_barrier_2 call");

  // The old value is read from a single destination pointer slot.
  const DataLayout &DL = CI->getModule()->getDataLayout();
  if (areKnownEqualI64(CI->getArgOperand(0), CI->getArgOperand(1))) {
    MSSAU.removeMemoryAccess(CI);
    CI->eraseFromParent();
    return true;
  }

  LocationSize PtrSlotSize = LocationSize::precise(DL.getPointerSize());

  // CodeGen usually materializes the old value as freeze(load ptr); peel that
  // back to the load and destination slot.
  UnmaterializeWriteBarrier2 WB{CI};
  const bool IgnoreOld =
      isConstZero(WB.OldValue) ||
      isPointerToNewlyAllocatedMemory(WB.OldSlot, CI, MSSA, AA, PtrSlotSize);
  const bool IgnoreNew =
      isConstZero(WB.NewValue) || isStaticDataAddress(WB.NewValue);

  if (!IgnoreOld && !IgnoreNew)
    return false;

  if (IgnoreOld && IgnoreNew) {
    MSSAU.removeMemoryAccess(CI);
    CI->eraseFromParent();
    return true;
  }

  Value *LiveArg = CI->getArgOperand(IgnoreOld ? 1 : 0);
  auto *Replacement = CallInst::Create(
      getOrInsertROGWriteBarrier1(CI->getModule(), LiveArg->getType()),
      {LiveArg}, "", CI->getIterator());
  Replacement->setCallingConv(CallingConv::ROG);
  Replacement->setTailCallKind(CI->getTailCallKind());
  Replacement->setDebugLoc(CI->getDebugLoc());
  Replacement->copyMetadata(*CI);

  auto *OldAccess = cast<MemoryUseOrDef>(MSSA.getMemoryAccess(CI));
  auto *NewAccess = cast<MemoryDef>(
      MSSAU.createMemoryAccessBefore(Replacement, nullptr, OldAccess));
  MSSAU.insertDef(NewAccess, /*RenameUses=*/true);
  MSSAU.removeMemoryAccess(OldAccess);

  CI->eraseFromParent();
  return true;
}

/// When a rog_bulk_write_barrier call has !rog.bulk_write_barrier.ptrmap
/// metadata and a constant size, decompose it into individual
/// rog_write_barrier_2 calls for each pointer slot identified by the ptrmap
/// bitmap. This is only done when the total number of pointer slots across all
/// objects is at most 4. Does not simplify the generated rog_write_barrier_2
/// calls; that is done in a later phase after InstCombine has had a chance to
/// simplify the IR.
static bool decomposeBulkWriteBarrierWithPtrMap(CallInst *CI) {
  assert(isBulkWriteBarrierCall(CI) && "expected rog_bulk_write_barrier call");

  // Read ptrmap metadata.
  unsigned PtrMapKind =
      CI->getContext().getMDKindID("rog.bulk_write_barrier.ptrmap");
  MDNode *MD = CI->getMetadata(PtrMapKind);
  if (!MD)
    return false;
  assert(MD->getNumOperands() == 1 && "inavlid ptrmap metadata");
  auto *StrMD = cast<MDString>(MD->getOperand(0));

  StringRef Bitmap = StrMD->getString();
  if (Bitmap.empty())
    return false;

  // Size must be a constant.
  Value *SizeVal = CI->getArgOperand(2);
  auto *SizeConst = dyn_cast<ConstantInt>(SizeVal);
  if (!SizeConst)
    return false;

  uint64_t Size = SizeConst->getZExtValue();
  uint64_t ObjectSize = 8 * Bitmap.size();

  if (ObjectSize == 0 || Size % ObjectSize != 0)
    return false;

  uint64_t NumObjects = Size / ObjectSize;

  // Collect pointer positions from the bitmap.
  SmallVector<unsigned, 8> PointerPositions;
  for (unsigned Pos = 0; Pos < Bitmap.size(); ++Pos)
    if (Bitmap[Pos] == '1')
      PointerPositions.push_back(Pos);

  uint64_t TotalPtrs = PointerPositions.size() * NumObjects;
  if (TotalPtrs == 0 || TotalPtrs > 4)
    return false;

  // Decompose: for each object, for each pointer slot, emit
  // rog_write_barrier_2.
  Value *Dest = CI->getArgOperand(0);
  Value *Src = CI->getArgOperand(1);
  Type *I64 = Type::getInt64Ty(CI->getContext());
  Type *I8 = Type::getInt8Ty(CI->getContext());
  const Align PtrAlign(8);

  FunctionCallee WB2 = getOrInsertROGWriteBarrier2(CI->getModule());

  IRBuilder<> Builder(CI);
  Builder.SetCurrentDebugLocation(CI->getDebugLoc());

  for (uint64_t ObjIdx = 0; ObjIdx < NumObjects; ++ObjIdx) {
    for (unsigned Pos : PointerPositions) {
      uint64_t Offset = ObjIdx * ObjectSize + Pos * 8;

      Value *DstSlot = Builder.CreateGEP(I8, Dest, Builder.getInt64(Offset));
      Value *SrcSlot = Builder.CreateGEP(I8, Src, Builder.getInt64(Offset));

      // old = freeze(load i64, dst_slot)
      LoadInst *OldLoad = Builder.CreateAlignedLoad(I64, DstSlot, PtrAlign);
      Value *OldFrozen = Builder.CreateFreeze(OldLoad);

      // new = load i64, src_slot
      LoadInst *NewLoad = Builder.CreateAlignedLoad(I64, SrcSlot, PtrAlign);

      // call rog_write_barrier_2(old, new)
      CallInst *WB2Call = Builder.CreateCall(WB2, {OldFrozen, NewLoad});
      WB2Call->setCallingConv(CallingConv::ROG);
      WB2Call->setTailCallKind(CI->getTailCallKind());
    }
  }

  CI->eraseFromParent();
  return true;
}

/// Decompose bulk write barriers using ptrmap metadata and eliminate
/// self-copies. This runs before WB2 optimization and before InstCombine so
/// that the generated IR can be simplified by InstCombine before the WB2
/// optimization pass processes it.
static bool decomposeBulkWriteBarriers(Function &F) {
  SmallVector<CallInst *, 8> BulkWorklist;

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (CI && isBulkWriteBarrierCall(CI))
        BulkWorklist.push_back(CI);
    }
  }

  bool MadeChanges = false;
  for (CallInst *CI : BulkWorklist) {
    // A self-copy leaves every destination word unchanged, so the barrier is
    // redundant.
    if (CI->getArgOperand(0) == CI->getArgOperand(1)) {
      CI->eraseFromParent();
      MadeChanges = true;
      continue;
    }
    MadeChanges |= decomposeBulkWriteBarrierWithPtrMap(CI);
  }
  return MadeChanges;
}

/// When the destination range is still proved fresh and untouched, the pass
/// can ignore the old destination values and downgrade to
/// rog_src_bulk_write_barrier(src, size), which scans only the incoming source
/// values.
static bool simplifyBulkWriteBarrierCall(CallInst *CI, MemorySSA &MSSA,
                                         BatchAAResults &AA,
                                         MemorySSAUpdater &MSSAU) {
  assert(isBulkWriteBarrierCall(CI) && "expected rog_bulk_write_barrier call");

  Value *Dest = CI->getArgOperand(0);
  Value *Src = CI->getArgOperand(1);
  Value *Size = CI->getArgOperand(2);

  // A self-copy leaves every destination word unchanged, so the barrier is
  // redundant.
  if (Dest == Src) {
    MSSAU.removeMemoryAccess(CI);
    CI->eraseFromParent();
    return true;
  }

  // Model the destination conservatively as an open-ended range rooted at
  // Dest: any earlier overlapping write blocks the downgrade.
  bool IgnoreDest = isPointerToNewlyAllocatedMemory(
      Dest, CI, MSSA, AA, LocationSize::afterPointer());

  if (!IgnoreDest)
    return false;

  // Downgrade: rog_bulk_write_barrier(dest, src, size)
  //       -> rog_src_bulk_write_barrier(src, size)
  // The source side still carries the incoming values being written.
  auto *Replacement =
      CallInst::Create(getOrInsertROGSrcBulkWriteBarrier(
                           CI->getModule(), Src->getType(), Size->getType()),
                       {Src, Size}, "", CI->getIterator());
  Replacement->setCallingConv(CallingConv::ROG);
  Replacement->setTailCallKind(CI->getTailCallKind());
  Replacement->setDebugLoc(CI->getDebugLoc());
  Replacement->copyMetadata(*CI);

  auto *OldAccess = cast<MemoryUseOrDef>(MSSA.getMemoryAccess(CI));
  auto *NewAccess = cast<MemoryDef>(
      MSSAU.createMemoryAccessBefore(Replacement, nullptr, OldAccess));
  MSSAU.insertDef(NewAccess, /*RenameUses=*/true);
  MSSAU.removeMemoryAccess(OldAccess);

  CI->eraseFromParent();
  return true;
}

static bool simplifyWriteBarrierCalls(Function &F, MemorySSA &MSSA,
                                      BatchAAResults &AA) {
  SmallVector<CallInst *, 8> WB2Worklist;
  SmallVector<CallInst *, 8> BulkWorklist;

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (CI == nullptr)
        continue;
      if (isWriteBarrier2Call(CI))
        WB2Worklist.push_back(CI);
      else if (isBulkWriteBarrierCall(CI))
        BulkWorklist.push_back(CI);
    }
  }

  MemorySSAUpdater MSSAU(&MSSA);

  bool MadeChanges = false;
  for (CallInst *CI : WB2Worklist)
    MadeChanges |= simplifyWriteBarrier2Call(CI, MSSA, AA, MSSAU);
  for (CallInst *CI : BulkWorklist)
    MadeChanges |= simplifyBulkWriteBarrierCall(CI, MSSA, AA, MSSAU);
  return MadeChanges;
}

PreservedAnalyses ROGGCWriteBarrierOptPass::run(Module &M,
                                                ModuleAnalysisManager &AM) {
  bool MadeChanges = false;
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();

  // Phase 1: Decompose bulk write barriers using ptrmap metadata and eliminate
  // self-copies. This must happen before WB2 optimization so that the
  // decomposed WB2 calls can benefit from InstCombine and then be optimized
  // together with pre-existing WB2 calls.
  SmallVector<Function *, 8> DecomposedFuncs;
  for (Function &F : M) {
    if (F.isDeclaration() || !F.hasGC() || F.getGC() != ROG_GC_NAME)
      continue;
    if (F.hasOptNone())
      continue;
    if (decomposeBulkWriteBarriers(F)) {
      DecomposedFuncs.push_back(&F);
      MadeChanges = true;
    }
  }

  // Phase 2: Run InstCombine on functions where decomposition happened, to
  // simplify the generated GEP + load + freeze patterns before WB2
  // optimization tries to reason about them.
  for (Function *F : DecomposedFuncs) {
    FAM.invalidate(*F, PreservedAnalyses::none());
    InstCombinePass().run(*F, FAM);
  }

  // Phase 3: Simplify WB2 calls and remaining bulk barriers using MemorySSA.
  for (Function &F : M) {
    if (F.isDeclaration() || !F.hasGC() || F.getGC() != ROG_GC_NAME)
      continue;
    if (F.hasOptNone())
      continue;

    auto &MSSAResult = FAM.getResult<MemorySSAAnalysis>(F);
    MemorySSA &MSSA = MSSAResult.getMSSA();
    AAResults &AA = FAM.getResult<AAManager>(F);
    BatchAAResults BAA(AA);

    MadeChanges |= simplifyWriteBarrierCalls(F, MSSA, BAA);
    if (F.getAlign().valueOrOne() < 16) {
      F.setAlignment(Align(16));
      MadeChanges = true;
    }
  }

  return MadeChanges ? PreservedAnalyses::none() : PreservedAnalyses::all();
}
