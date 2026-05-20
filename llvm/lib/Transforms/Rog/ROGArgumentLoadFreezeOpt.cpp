//===- ROGArgumentLoadFreezeOpt.cpp - ROG load freeze opts ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ROGWriteBarrierUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;

namespace llvm {
namespace rog {
namespace {

bool isLoadFromPointerArgument(const LoadInst *Load) {
  auto *Arg = dyn_cast<Argument>(
      getUnderlyingObject(Load->getPointerOperand()));
  return Arg && Arg->getType()->isPointerTy();
}

} // namespace

bool removeFreezesFromArgumentLoads(Function &F) {
  if (!F.hasFnAttribute("go func"))
    return false;

  SmallVector<FreezeInst *, 8> Freezes;

  for (BasicBlock &BB : F) {
    for (Instruction &I : BB) {
      auto *Freeze = dyn_cast<FreezeInst>(&I);
      if (!Freeze)
        continue;

      auto *Load = dyn_cast<LoadInst>(Freeze->getOperand(0));
      if (!Load)
        continue;

      if (isLoadFromPointerArgument(Load))
        Freezes.push_back(Freeze);
    }
  }

  for (FreezeInst *Freeze : Freezes) {
    Freeze->replaceAllUsesWith(Freeze->getOperand(0));
    Freeze->eraseFromParent();
  }

  return !Freezes.empty();
}

} // namespace rog
} // namespace llvm