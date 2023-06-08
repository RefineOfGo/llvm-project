#ifndef LLVM_IR_ROGGC_H
#define LLVM_IR_ROGGC_H

#define ROG_GC_NAME     "rog"
#define ROG_GCWB_SW     "rog_gcwb_switch"
#define ROG_GCWB_ONE    "rog_write_barrier"
#define ROG_GCWB_BULK   "rog_bulk_write_barrier"

namespace llvm {
void linkROGGC();
}

#endif
