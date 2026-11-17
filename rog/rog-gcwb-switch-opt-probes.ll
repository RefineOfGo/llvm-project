; RUN: opt -passes=rog-gcwb-switch-opt -S %s | FileCheck %s --check-prefix=OPT
; RUN: opt -passes=rog-gcwb-switch-opt -rog-gcwb-switch-opt-budget=3 -S %s | FileCheck %s --check-prefix=AGE

@ROG_GCWB_SWITCH = external global i8

declare void @llvm.pseudoprobe(i64, i64, i32, i64)

define i8 @probes_do_not_age_load() gc "rog" {
; OPT-LABEL: define i8 @probes_do_not_age_load(
; OPT: %l0 = load atomic i8
; OPT-NOT: load atomic
; OPT: ret i8 %l0
; AGE-LABEL: define i8 @probes_do_not_age_load(
; AGE: %l0 = load atomic i8
; AGE-NOT: load atomic
; AGE: ret i8 %l0
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  call void @llvm.pseudoprobe(i64 1, i64 1, i32 0, i64 -1)
  call void @llvm.pseudoprobe(i64 1, i64 2, i32 0, i64 -1)
  br label %next
next:
  call void @llvm.pseudoprobe(i64 1, i64 3, i32 0, i64 -1)
  call void @llvm.pseudoprobe(i64 1, i64 4, i32 0, i64 -1)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}
