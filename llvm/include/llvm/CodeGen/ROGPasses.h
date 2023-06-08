#ifndef LLVM_CODEGEN_ROG_PASSES_H
#define LLVM_CODEGEN_ROG_PASSES_H

#include "llvm/IR/PassManager.h"

namespace llvm {
struct ROGGCLoweringPass : public PassInfoMixin<ROGGCLoweringPass> {
    PreservedAnalyses run(Function &fn, FunctionAnalysisManager &am);
};

struct ROGCheckPointInsertionPass : public PassInfoMixin<ROGCheckPointInsertionPass> {
    PreservedAnalyses run(Function &fn, FunctionAnalysisManager &am);
};
}

#endif