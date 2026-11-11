//===- RogPreparePseudoProbes.h - Prepare ROG probes -------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_IPO_ROGPREPAREPSEUDOPROBES_H
#define LLVM_TRANSFORMS_IPO_ROGPREPAREPSEUDOPROBES_H

#include "llvm/IR/PassManager.h"
#include "llvm/Support/Compiler.h"

namespace llvm {
/// Prepare optimized runtime IR for a new pseudo-probe profiling baseline.
/// Runs after ThinLTO pre-link optimization and before SampleProfileProbePass.
/// Each remaining physical function becomes a profiling unit: old inline
/// locations are folded to the outermost callsite and obsolete inline debug
/// records are removed. Missing function scopes and call locations are
/// supplied, and debug linkage names are aligned with IR symbols.
/// Already-probed modules are rejected to avoid mixing probe numbering from
/// different baselines.
class RogPreparePseudoProbesPass
    : public PassInfoMixin<RogPreparePseudoProbesPass> {
public:
  LLVM_ABI PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};
} // namespace llvm

#endif
