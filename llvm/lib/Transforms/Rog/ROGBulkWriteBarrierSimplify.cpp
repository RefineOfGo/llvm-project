//===- ROGBulkWriteBarrierSimplify.cpp - ROG bulk WB opts ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ROGWriteBarrierUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/MemorySSAUpdater.h"

using namespace llvm;

namespace llvm {
namespace rog {

static bool simplifyBulkWriteBarrierCall(CallInst *CI, MemorySSA &MSSA,
                                         BatchAAResults &AA,
                                         MemorySSAUpdater &MSSAU) {
  assert(isBulkWriteBarrierCall(CI) && "expected rog_bulk_write_barrier call");

  Value *Dest = CI->getArgOperand(0);
  Value *Src = CI->getArgOperand(1);
  Value *Size = CI->getArgOperand(2);

  if (Dest == Src) {
    MSSAU.removeMemoryAccess(CI);
    CI->eraseFromParent();
    return true;
  }

  bool IgnoreDest = isPointerToNewlyAllocatedMemory(
      Dest, CI, MSSA, AA, LocationSize::afterPointer());
  if (!IgnoreDest)
    return false;

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

bool simplifyBulkWriteBarrierCalls(Function &F, MemorySSA &MSSA,
                                   BatchAAResults &AA) {
  SmallVector<CallInst *, 8> BulkWorklist;

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (CI && isBulkWriteBarrierCall(CI))
        BulkWorklist.push_back(CI);
    }
  }

  MemorySSAUpdater MSSAU(&MSSA);

  bool MadeChanges = false;
  for (CallInst *CI : BulkWorklist)
    MadeChanges |= simplifyBulkWriteBarrierCall(CI, MSSA, AA, MSSAU);
  return MadeChanges;
}

} // namespace rog
} // namespace llvm