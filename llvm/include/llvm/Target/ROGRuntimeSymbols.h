#ifndef LLVM_LIB_CODEGEN_ROGRUNTIMESYMBOLS_H
#define LLVM_LIB_CODEGEN_ROGRUNTIMESYMBOLS_H

namespace llvm {
static const char *        kROGStackLimit           = "rog_stack_limit";
static const char *        kROGStackCheckFn         = "rog_morestack_abi";
static const char *        kROGStackCheckFnLarge    = "rog_morestack_abi_large";
static const char *        kROGStackCheckAttr       = "rog-stack-check";
static const unsigned long kROGStackRedZoneSize     = 1024;
// Stackmap ID of the record adjustForROGPrologue emits at the return point
// of the kROGStackCheckFn call: bit 62 marks the record as "prologue-entry"
// (the function is caught between entry and prologue; the record's only
// locations describe the incoming stack-argument area relative to the entry
// RSP). The compact-blob emitter folds this ID bit into version 0x55
// record flags bit 0.
static const unsigned long long kROGPrologueEntryStackMapID = 1ULL << 62;
}

#endif
