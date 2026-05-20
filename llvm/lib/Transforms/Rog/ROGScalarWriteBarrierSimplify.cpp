//===- ROGScalarWriteBarrierSimplify.cpp - ROG scalar WB opts ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ROGWriteBarrierUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/MemorySSAUpdater.h"
#include "llvm/IR/DataLayout.h"

using namespace llvm;

namespace llvm {
namespace rog {
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

  const DataLayout &DL = CI->getModule()->getDataLayout();
  if (areKnownEqualI64(CI->getArgOperand(0), CI->getArgOperand(1))) {
    MSSAU.removeMemoryAccess(CI);
    CI->eraseFromParent();
    return true;
  }

  LocationSize PtrSlotSize = LocationSize::precise(DL.getPointerSize());

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

bool simplifyWriteBarrier2Calls(Function &F, MemorySSA &MSSA,
                                BatchAAResults &AA) {
  SmallVector<CallInst *, 8> WB2Worklist;

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (CI && isWriteBarrier2Call(CI))
        WB2Worklist.push_back(CI);
    }
  }

  MemorySSAUpdater MSSAU(&MSSA);

  bool MadeChanges = false;
  for (CallInst *CI : WB2Worklist)
    MadeChanges |= simplifyWriteBarrier2Call(CI, MSSA, AA, MSSAU);
  return MadeChanges;
}

} // namespace rog
} // namespace llvm