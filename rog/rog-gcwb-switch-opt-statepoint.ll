; RUN: opt -passes=rog-gcwb-switch-opt -S %s | FileCheck %s

@ROG_GCWB_SWITCH = external global { { { { i8 } } }, [127 x i8] }

declare void @pre_write_2(i64, i64)
declare void @blocking(i64, i64)
declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)

define i8 @barrier_statepoint() gc "rog" {
; CHECK-LABEL: define i8 @barrier_statepoint(
; CHECK: %l0 = load atomic i8
; CHECK: call token {{.*}}@llvm.experimental.gc.statepoint.p0({{.*}}@pre_write_2
; CHECK-NOT: load atomic
; CHECK: ret i8 %l0
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (i64, i64)) @pre_write_2, i32 2, i32 0, i64 0, i64 0, i32 0, i32 0)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}

define i8 @blocking_statepoint() gc "rog" {
; CHECK-LABEL: define i8 @blocking_statepoint(
; CHECK: %l0 = load atomic i8
; CHECK: call token {{.*}}@llvm.experimental.gc.statepoint.p0({{.*}}@blocking
; CHECK: %l1 = load atomic i8
; CHECK: ret i8 %l1
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (i64, i64)) @blocking, i32 2, i32 0, i64 0, i64 0, i32 0, i32 0)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}

define i8 @indirect_statepoint(ptr %target) gc "rog" {
; CHECK-LABEL: define i8 @indirect_statepoint(
; CHECK: %l0 = load atomic i8
; CHECK: call token {{.*}}@llvm.experimental.gc.statepoint.p0({{.*}}%target
; CHECK: %l1 = load atomic i8
; CHECK: ret i8 %l1
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (i64, i64)) %target, i32 2, i32 0, i64 0, i64 0, i32 0, i32 0)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}
