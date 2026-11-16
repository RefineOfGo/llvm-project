; RUN: split-file %s %t
; RUN: opt -passes=rog-gcwb-switch-opt -S %t/scalar.ll | FileCheck %s --check-prefix=SCALAR
; RUN: opt -passes=rog-gcwb-switch-opt -rog-gcwb-switch-opt-budget=3 -S %t/scalar.ll | FileCheck %s --check-prefix=PROBE
; RUN: opt -passes=rog-gcwb-switch-opt -S %t/padded.ll | FileCheck %s --check-prefix=PADDED
; RUN: opt -passes=rog-gcwb-switch-opt -S %t/nested.ll | FileCheck %s --check-prefix=NESTED
; RUN: opt -passes=rog-gcwb-switch-opt -S %t/named-alias.ll | FileCheck %s --check-prefix=ALIAS

; Runtime bitcode uses cache-padded storage for the byte flag. Profiling probes
; and statepoint-wrapped barrier calls also survive until this pass runs.

;--- scalar.ll
@ROG_GCWB_SWITCH = external global i8

declare void @llvm.pseudoprobe(i64, i64, i32, i64)
declare void @pre_write_2(i64, i64)
declare void @blocking(i64, i64)
declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)

define i8 @probes_do_not_age_load() gc "rog" {
; SCALAR-LABEL: define i8 @probes_do_not_age_load(
; SCALAR: %l0 = load atomic i8
; SCALAR-NOT: load atomic
; SCALAR: ret i8 %l0
; PROBE-LABEL: define i8 @probes_do_not_age_load(
; PROBE: %l0 = load atomic i8
; PROBE-NOT: load atomic
; PROBE: ret i8 %l0
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

define i8 @barrier_statepoint() gc "rog" {
; SCALAR-LABEL: define i8 @barrier_statepoint(
; SCALAR: %l0 = load atomic i8
; SCALAR: call token {{.*}}@llvm.experimental.gc.statepoint.p0({{.*}}@pre_write_2
; SCALAR-NOT: load atomic
; SCALAR: ret i8 %l0
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (i64, i64)) @pre_write_2, i32 2, i32 0, i64 0, i64 0, i32 0, i32 0)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}

define i8 @blocking_statepoint() gc "rog" {
; SCALAR-LABEL: define i8 @blocking_statepoint(
; SCALAR: %l0 = load atomic i8
; SCALAR: call token {{.*}}@llvm.experimental.gc.statepoint.p0({{.*}}@blocking
; SCALAR: %l1 = load atomic i8
; SCALAR: ret i8 %l1
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (i64, i64)) @blocking, i32 2, i32 0, i64 0, i64 0, i32 0, i32 0)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}

define i8 @indirect_statepoint(ptr %target) gc "rog" {
; SCALAR-LABEL: define i8 @indirect_statepoint(
; SCALAR: %l0 = load atomic i8
; SCALAR: call token {{.*}}@llvm.experimental.gc.statepoint.p0({{.*}}%target
; SCALAR: %l1 = load atomic i8
; SCALAR: ret i8 %l1
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (i64, i64)) %target, i32 2, i32 0, i64 0, i64 0, i32 0, i32 0)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}

;--- padded.ll
@ROG_GCWB_SWITCH = global <{ [1 x i8], [127 x i8] }> zeroinitializer, align 128
@flag_alias = alias i8, ptr @ROG_GCWB_SWITCH

declare void @llvm.pseudoprobe(i64, i64, i32, i64)
declare void @pre_write_2(i64, i64)
declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)

define i8 @padded_barrier_chain() gc "rog" {
; PADDED-LABEL: define i8 @padded_barrier_chain(
; PADDED: %l0 = load atomic i8
; PADDED: call token {{.*}}@llvm.experimental.gc.statepoint.p0({{.*}}@pre_write_2
; PADDED-NOT: load atomic
; PADDED: ret i8 %l0
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %enabled = icmp ne i8 %l0, 0
  br i1 %enabled, label %barrier, label %merge
barrier:
  call void @llvm.pseudoprobe(i64 2, i64 1, i32 0, i64 -1)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (i64, i64)) @pre_write_2, i32 2, i32 0, i64 0, i64 0, i32 0, i32 0)
  br label %merge
merge:
  call void @llvm.pseudoprobe(i64 2, i64 2, i32 0, i64 -1)
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}

define i8 @alias_at_zero_offset() gc "rog" {
; PADDED-LABEL: define i8 @alias_at_zero_offset(
; PADDED: %l0 = load atomic i8
; PADDED-NOT: load atomic
; PADDED: ret i8 %l0
  %l0 = load atomic i8, ptr @flag_alias seq_cst, align 1
  %l1 = load atomic i8, ptr getelementptr (i8, ptr @ROG_GCWB_SWITCH, i64 0) seq_cst, align 1
  ret i8 %l1
}

define i8 @padding_byte_is_not_flag() gc "rog" {
; PADDED-LABEL: define i8 @padding_byte_is_not_flag(
; PADDED: %l0 = load atomic i8
; PADDED: %l1 = load atomic i8
; PADDED: ret i8 %l1
  %l0 = load atomic i8, ptr getelementptr (i8, ptr @ROG_GCWB_SWITCH, i64 1) seq_cst, align 1
  %l1 = load atomic i8, ptr getelementptr (i8, ptr @ROG_GCWB_SWITCH, i64 1) seq_cst, align 1
  ret i8 %l1
}

define i16 @wider_loads_are_not_flags() gc "rog" {
; PADDED-LABEL: define i16 @wider_loads_are_not_flags(
; PADDED: %l0 = load atomic i16
; PADDED: %l1 = load atomic i16
; PADDED: ret i16 %l1
  %l0 = load atomic i16, ptr @ROG_GCWB_SWITCH seq_cst, align 2
  %l1 = load atomic i16, ptr @ROG_GCWB_SWITCH seq_cst, align 2
  ret i16 %l1
}

define i16 @mixed_load_widths() gc "rog" {
; PADDED-LABEL: define i16 @mixed_load_widths(
; PADDED: %l0 = load atomic i8
; PADDED-NEXT: %wide = load atomic i16
; PADDED-NOT: load atomic
; PADDED: %x = xor i8 %l0, %l0
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %wide = load atomic i16, ptr @ROG_GCWB_SWITCH seq_cst, align 2
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %x = xor i8 %l0, %l1
  %z = zext i8 %x to i16
  %result = xor i16 %wide, %z
  ret i16 %result
}

;--- nested.ll
@ROG_GCWB_SWITCH = external global { { { { i8 } } }, [127 x i8] }

define i8 @nested_declaration() gc "rog" {
; NESTED-LABEL: define i8 @nested_declaration(
; NESTED: %l0 = load atomic i8
; NESTED-NOT: load atomic
; NESTED: ret i8 %l0
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}

;--- named-alias.ll
@storage = global <{ [1 x i8], [127 x i8] }> zeroinitializer, align 128
@ROG_GCWB_SWITCH = alias i8, ptr @storage

define i8 @named_global_alias() gc "rog" {
; ALIAS-LABEL: define i8 @named_global_alias(
; ALIAS: %l0 = load atomic i8
; ALIAS-NOT: load atomic
; ALIAS: ret i8 %l0
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  ret i8 %l1
}
