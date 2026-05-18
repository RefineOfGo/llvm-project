# ROG

## What ROG changed

- Added a new LLVM scalar pass: `rog-gc-write-barrier-opt`.
    - Introduced write-barrier simplification rules for `rog_write_barrier_2`.
    - Introduced bulk write-barrier simplification rules for `rog_bulk_write_barrier`.

## ABI

ROG exposes the following GC write-barrier entry points from
`library/runtime/src/abi.rs`. They are `#[unsafe(no_mangle)]`, use the ROG
calling convention (`extern "rog"` in Rust, `rogcc` in LLVM IR), and are kept
alive for linker/runtime ABI use by `#[used(linker)]` function-pointer statics.
Each entry point is a no-split wrapper around `crate::gc::wb_buffer`.

| Symbol                       | ABI signature                                                                                                                   | Purpose                                                                                                                                                                                                                                                                                                                                                                                                                             |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `rog_write_barrier_1`        | `rogcc void @rog_write_barrier_1(i64 old)` / `unsafe extern "rog" fn(usize)`                                                    | Records one pointer-sized value in the per-P write-barrier buffer when GC write barriers are enabled and the value is an unmarked heap object. LLVM uses this as the reduced form of `rog_write_barrier_2` when only one side of the scalar barrier still needs scanning; the runtime parameter is named `old`, but the optimized call may pass either the old or new value.                                                        |
| `rog_write_barrier_2`        | `rogcc void @rog_write_barrier_2(i64 old, i64 new)` / `unsafe extern "rog" fn(usize, usize)`                                    | Scalar pointer-write barrier emitted before a heap pointer slot is overwritten. Codegen loads the current destination value as `old`, converts the incoming pointer to `new`, and calls this entry point. The runtime skips equal values, tests both values with `WbBuffer::should_buffer`, and buffers the old value, the new value, or both.                                                                                      |
| `rog_bulk_write_barrier`     | `rogcc void @rog_bulk_write_barrier(ptr dest, ptr src, i64 size)` / `unsafe extern "rog" fn(*const usize, *const usize, usize)` | Bulk pointer-write barrier emitted before pointer-containing copies. `dest` points at the destination words before the copy, `src` points at the source words, and `size` is in bytes. The runtime scans `size / 8` pointer-sized slots, compares each old destination word with the corresponding new source word, and buffers whichever side still needs marking; it skips empty ranges and destination ranges in Go stack space. |
| `rog_src_bulk_write_barrier` | `rogcc void @rog_src_bulk_write_barrier(ptr src, i64 size)` / `unsafe extern "rog" fn(*const usize, usize)`                     | Source-only bulk barrier used when the destination old values are known to be ignorable, for example newly allocated and unchanged zeroed memory. The runtime scans only `src` for `size / 8` pointer-sized words and buffers source values that still need marking.                                                                                                                                                                |

The `rog-gc-write-barrier-opt` pass relies on these ABI meanings. It treats null
and static data addresses as ignorable write-barrier arguments, and uses
MemorySSA/alias analysis to prove when a destination points to newly allocated,
unchanged memory. For scalar barriers, `rog_write_barrier_2(old, new)` can be
deleted if both sides are ignorable, or replaced with `rog_write_barrier_1` if
only one side remains live. For bulk barriers, `rog_bulk_write_barrier(dest,
src, size)` can be downgraded to `rog_src_bulk_write_barrier(src, size)` when
the destination old values are ignorable; the source side must still be scanned
because it contains the pointer values being written.

## test

ROG regression tests in [rog](./rog):

```bash
ninja check-ROG
```
