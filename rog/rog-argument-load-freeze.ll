; RUN: opt -passes=rog-gc-write-barrier-opt -S %s | FileCheck %s

@global_slot = global i64 0, align 8

declare void @use_i64(i64)

define void @arg_load_freeze_removed(ptr %slot) "go-func" gc "rog" {
; CHECK-LABEL: define void @arg_load_freeze_removed(
; CHECK: [[LOADED:%.*]] = load i64, ptr %slot, align 8
; CHECK-NOT: freeze
; CHECK: [[NEXT:%.*]] = add i64 [[LOADED]], 1
; CHECK: call void @use_i64(i64 [[LOADED]])
; CHECK: call void @use_i64(i64 [[NEXT]])
; CHECK-NEXT: ret void
entry:
  %loaded = load i64, ptr %slot, align 8
  %frozen = freeze i64 %loaded
  %next = add i64 %frozen, 1
  call void @use_i64(i64 %frozen)
  call void @use_i64(i64 %next)
  ret void
}

define void @arg_gep_load_freeze_removed(ptr %base) "go-func" gc "rog" {
; CHECK-LABEL: define void @arg_gep_load_freeze_removed(
; CHECK: [[SLOT:%.*]] = getelementptr i64, ptr %base, i64 1
; CHECK: [[LOADED:%.*]] = load i64, ptr [[SLOT]], align 8
; CHECK-NOT: freeze
; CHECK: call void @use_i64(i64 [[LOADED]])
; CHECK-NEXT: ret void
entry:
  %slot = getelementptr i64, ptr %base, i64 1
  %loaded = load i64, ptr %slot, align 8
  %frozen = freeze i64 %loaded
  call void @use_i64(i64 %frozen)
  ret void
}

define void @global_load_freeze_kept() gc "rog" {
; CHECK-LABEL: define void @global_load_freeze_kept(
; CHECK: [[LOADED:%.*]] = load i64, ptr @global_slot, align 8
; CHECK: [[FROZEN:%.*]] = freeze i64 [[LOADED]]
; CHECK: call void @use_i64(i64 [[FROZEN]])
entry:
  %loaded = load i64, ptr @global_slot, align 8
  %frozen = freeze i64 %loaded
  call void @use_i64(i64 %frozen)
  ret void
}

define void @rog_without_go_func_attr_keeps_freeze(ptr %slot) gc "rog" {
; CHECK-LABEL: define void @rog_without_go_func_attr_keeps_freeze(
; CHECK: [[LOADED:%.*]] = load i64, ptr %slot, align 8
; CHECK: [[FROZEN:%.*]] = freeze i64 [[LOADED]]
; CHECK: call void @use_i64(i64 [[FROZEN]])
entry:
  %loaded = load i64, ptr %slot, align 8
  %frozen = freeze i64 %loaded
  call void @use_i64(i64 %frozen)
  ret void
}

define void @non_rog_arg_load_freeze_kept(ptr %slot) {
; CHECK-LABEL: define void @non_rog_arg_load_freeze_kept(
; CHECK: [[LOADED:%.*]] = load i64, ptr %slot, align 8
; CHECK: [[FROZEN:%.*]] = freeze i64 [[LOADED]]
; CHECK: call void @use_i64(i64 [[FROZEN]])
entry:
  %loaded = load i64, ptr %slot, align 8
  %frozen = freeze i64 %loaded
  call void @use_i64(i64 %frozen)
  ret void
}