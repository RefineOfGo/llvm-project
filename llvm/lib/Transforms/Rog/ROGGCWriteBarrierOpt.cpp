//===- ROGGCWriteBarrierOpt.cpp - ROG GC write barrier opts --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Rog/ROGGCWriteBarrierOpt.h"
#include "ROGWriteBarrierUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ROGGC.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"

using namespace llvm;

static bool shouldOptimizeROGFunction(Function &F) {
  if (F.isDeclaration() || !F.hasGC() || F.getGC() != ROG_GC_NAME)
    return false;
  return !F.hasOptNone();
}

PreservedAnalyses ROGGCWriteBarrierOptPass::run(Module &M,
                                                ModuleAnalysisManager &AM) {
  bool MadeChanges = false;
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();

  SmallVector<Function *, 8> DecomposedFuncs;
  for (Function &F : M) {
    if (!shouldOptimizeROGFunction(F))
      continue;
    if (rog::decomposeBulkWriteBarriers(F)) {
      DecomposedFuncs.push_back(&F);
      MadeChanges = true;
    }
  }

  for (Function *F : DecomposedFuncs) {
    FAM.invalidate(*F, PreservedAnalyses::none());
    InstCombinePass().run(*F, FAM);
  }

  // ROG Go functions guarantee memory reached from pointer-typed parameters holds
  // defined values, making a freeze of a load from that memory redundant.
  for (Function &F : M) {
    if (!shouldOptimizeROGFunction(F))
      continue;
    MadeChanges |= rog::removeFreezesFromArgumentLoads(F);
  }

  for (Function &F : M) {
    if (!shouldOptimizeROGFunction(F))
      continue;

    auto &MSSAResult = FAM.getResult<MemorySSAAnalysis>(F);
    MemorySSA &MSSA = MSSAResult.getMSSA();
    AAResults &AA = FAM.getResult<AAManager>(F);
    BatchAAResults BAA(AA);

    MadeChanges |= rog::simplifyWriteBarrier2Calls(F, MSSA, BAA);
    MadeChanges |= rog::simplifyBulkWriteBarrierCalls(F, MSSA, BAA);
  }

  return MadeChanges ? PreservedAnalyses::none() : PreservedAnalyses::all();
}