#ifndef LLVM_LIB_CODEGEN_ROGSTACKCHECKOPTIONS_H
#define LLVM_LIB_CODEGEN_ROGSTACKCHECKOPTIONS_H

namespace llvm {
static const char *       kROGStackLimit       = "rog_stack_limit";
static const char *       kROGMoreStackFn      = "rog_morestack_abi";
static const char *       kROGStackCheckAttr   = "rog-stack-check";
static const unsigned int kROGStackRedZoneSize = 1024;
}

#endif
