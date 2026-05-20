//===- ROGGCWBSwitchOpt.h - ROG GCWB switch opts ---------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_ROG_ROGGCWBSWITCHOPT_H
#define LLVM_TRANSFORMS_ROG_ROGGCWBSWITCHOPT_H

#include "llvm/IR/PassManager.h"

namespace llvm {

struct ROGGCWBSwitchOptPass : public PassInfoMixin<ROGGCWBSwitchOptPass> {
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

} // namespace llvm

#endif
