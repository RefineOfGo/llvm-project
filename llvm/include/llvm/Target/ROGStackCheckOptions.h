#ifndef LLVM_LIB_CODEGEN_ROGSTACKCHECKDEFS_H
#define LLVM_LIB_CODEGEN_ROGSTACKCHECKDEFS_H

#include <stdint.h>

#define kROGCurrentGRegisterX86         X86::R15
#define kROGCurrentGRegisterAArch64     AArch64::X28

namespace llvm {
static const char *       kROGMoreStackFn      = "rog_morestack_abi";
static const uint64_t     kROGStackRedZone     = 256;
static const unsigned int kROGStackLimitOffset = 16;

static_assert(!(kROGStackLimitOffset & 7), "Unaligned stack guard");
}

#endif
