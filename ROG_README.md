# ROG

## What ROG changed

- Added a new LLVM scalar pass: `rog-gc-write-barrier-opt`.
  - Introduced write-barrier simplification rules for `rog_write_barrier_2(old, new)`:
    - Remove the call when both arguments are ignorable.
    - Rewrite to `rog_write_barrier_1(live_arg)` when exactly one side is ignorable.

## test

ROG regression tests in [rog](./Rog):

```bash
ninja check-ROG
```
