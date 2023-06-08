#ifndef LLVM_LIB_CODEGEN_ROGRUNTIMESYMBOLS_H
#define LLVM_LIB_CODEGEN_ROGRUNTIMESYMBOLS_H

namespace llvm {
static const char *        kROGStackLimit           = "rog_stack_limit";
static const char *        kROGStackCheckFn         = "rog_morestack_abi";
static const char *        kROGStackCheckAttr       = "rog-stack-check";
static const unsigned long kROGStackRedZoneSize     = 1024;

static const char *        kROGCheckpointSw         = "rog_checkpoint_switch";
static const char *        kROGCheckpointFn         = "rog_checkpoint_abi";
static const char *        kROGCheckpointAttr       = "rog-checkpoint";

static const char *        kROGWriteBarrierSw       = "rog_gcwb_switch";
static const char *        kROGWriteBarrierFn       = "rog_write_barrier";
static const char *        kROGBulkWriteBarrierFn   = "rog_bulk_write_barrier";
}

#endif
