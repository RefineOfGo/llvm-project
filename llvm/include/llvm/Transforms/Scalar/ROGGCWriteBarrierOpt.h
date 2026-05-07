//===- ROGGCWriteBarrierOpt.h - ROG GC write barrier opts -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_SCALAR_ROGGCWRITEBARRIEROPT_H
#define LLVM_TRANSFORMS_SCALAR_ROGGCWRITEBARRIEROPT_H

#include "llvm/IR/PassManager.h"

namespace llvm {

struct ROGGCWriteBarrierOptPass
    : public PassInfoMixin<ROGGCWriteBarrierOptPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};

} // namespace llvm

#endif