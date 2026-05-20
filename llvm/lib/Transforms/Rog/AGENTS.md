# ROG Transform Requirements

This directory contains LLVM transforms for the ROG garbage collector and its
write-barrier ABI.

see [ROG_README](../../../../ROG_README.md)

## Scope And Pipeline

- The main pass is the module pass `rog-gc-write-barrier-opt`, implemented by
	`ROGGCWriteBarrierOptPass`.
- Optimize only functions that are definitions, have `gc "rog"`, and are not
	`optnone`. Do not apply these transforms to ordinary LLVM functions.
- The pass is also inserted into the standard optimization pipeline after
	constant propagation and inlining.

## Build Files

- New public pass declarations belong under `llvm/include/llvm/Transforms/Rog`.
- New implementation files in this directory must be added to both
	`llvm/lib/Transforms/Rog/CMakeLists.txt` and
	`llvm/utils/gn/secondary/llvm/lib/Transforms/Rog/BUILD.gn`.
