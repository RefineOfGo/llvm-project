; RUN: opt -passes=rog-gc-write-barrier-opt -S %s | FileCheck %s

@glob = global i64 0, align 8
@arr = global [2 x i64] zeroinitializer, align 8

declare rogcc void @rog_write_barrier_1(i64)
declare rogcc void @rog_write_barrier_2(i64, i64)
declare rogcc void @rog_bulk_write_barrier(ptr, ptr, i64)
declare noalias ptr @rog_alloc(i64) allockind("alloc")
declare noalias ptr @rog_noalias_nonalloc()
declare void @unknown_mutate(ptr)

define void @drop_both() gc "rog" {
; CHECK-LABEL: define void @drop_both()
; CHECK-NOT: rog_write_barrier_
entry:
  call rogcc void @rog_write_barrier_2(i64 0, i64 ptrtoint (ptr @glob to i64))
  ret void
}

define void @drop_old(i64 %value) gc "rog" {
; CHECK-LABEL: define void @drop_old(
; CHECK: call rogcc void @rog_write_barrier_1(i64 %value)
; CHECK-NOT: rog_write_barrier_2
entry:
  call rogcc void @rog_write_barrier_2(i64 0, i64 %value)
  ret void
}

define void @drop_new(i64 %value) gc "rog" {
; CHECK-LABEL: define void @drop_new(
; CHECK: call rogcc void @rog_write_barrier_1(i64 %value)
; CHECK-NOT: rog_write_barrier_2
entry:
  call rogcc void @rog_write_barrier_2(
    i64 %value,
    i64 ptrtoint (ptr getelementptr inbounds ([2 x i64], ptr @arr, i64 0, i64 1) to i64)
  )
  ret void
}

define void @keep_both(i64 %old, i64 %new) gc "rog" {
; CHECK-LABEL: define void @keep_both(
; CHECK: call rogcc void @rog_write_barrier_2(i64 %old, i64 %new)
entry:
  call rogcc void @rog_write_barrier_2(i64 %old, i64 %new)
  ret void
}

define void @drop_same_value(i64 %value) gc "rog" {
; CHECK-LABEL: define void @drop_same_value(
; CHECK-NOT: rog_write_barrier_
; CHECK: ret void
entry:
  call rogcc void @rog_write_barrier_2(i64 %value, i64 %value)
  ret void
}

define void @alloc_freeze_load_drop_both() gc "rog" {
; CHECK-LABEL: define void @alloc_freeze_load_drop_both(
; CHECK-NOT: call rogcc void @rog_write_barrier_
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 8)
  %slot = getelementptr i64, ptr %p, i64 0
  %loaded = load i64, ptr %slot
  %old = freeze i64 %loaded
  %new = ptrtoint ptr @glob to i64
  call rogcc void @rog_write_barrier_2(i64 %old, i64 %new)
  ret void
}

define void @alloc_written_slot_keeps_wb2(i64 %first, i64 %second) gc "rog" {
; CHECK-LABEL: define void @alloc_written_slot_keeps_wb2(
; CHECK: store i64 %first, ptr %slot
; CHECK: [[OLD:%.*]] = load i64, ptr %slot
; CHECK: call rogcc void @rog_write_barrier_2(i64 [[OLD]], i64 %second)
entry:
  %p = call noalias ptr @rog_alloc(i64 8)
  %slot = getelementptr i64, ptr %p, i64 0
  store i64 %first, ptr %slot
  %old = load i64, ptr %slot
  call rogcc void @rog_write_barrier_2(i64 %old, i64 %second)
  ret void
}

define void @alloc_escape_via_call_keeps_wb2(i64 %new) gc "rog" {
; CHECK-LABEL: define void @alloc_escape_via_call_keeps_wb2(
; CHECK: call void @unknown_mutate(ptr %p)
; CHECK: [[OLD:%.*]] = load i64, ptr %p
; CHECK: call rogcc void @rog_write_barrier_2(i64 [[OLD]], i64 %new)
; CHECK-NOT: rog_write_barrier_1
entry:
  %p = call noalias ptr @rog_alloc(i64 8)
  call void @unknown_mutate(ptr %p)
  %old = load i64, ptr %p
  call rogcc void @rog_write_barrier_2(i64 %old, i64 %new)
  ret void
}

define void @alloc_escape_via_local_alias_keeps_wb2(i64 %first,
                                                    i64 %second) gc "rog" {
; CHECK-LABEL: define void @alloc_escape_via_local_alias_keeps_wb2(
; CHECK: %slot.addr = alloca ptr, align 8
; CHECK: store ptr %p, ptr %slot.addr, align 8
; CHECK: [[ALIAS:%.*]] = load ptr, ptr %slot.addr, align 8
; CHECK: store i64 %first, ptr [[ALIAS]], align 4
; CHECK: [[OLD:%.*]] = load i64, ptr %p, align 4
; CHECK: call rogcc void @rog_write_barrier_2(i64 [[OLD]], i64 %second)
; CHECK-NOT: rog_write_barrier_1
entry:
  %p = call noalias ptr @rog_alloc(i64 8)
  %slot.addr = alloca ptr, align 8
  store ptr %p, ptr %slot.addr, align 8
  %alias = load ptr, ptr %slot.addr, align 8
  store i64 %first, ptr %alias, align 4
  %old = load i64, ptr %p, align 4
  call rogcc void @rog_write_barrier_2(i64 %old, i64 %second)
  ret void
}

; --- Per-location check: store to offset 0 should not block optimization at offset 8 ---

define void @alloc_per_location_wb2(i64 %val0, i64 %val8) gc "rog" {
; CHECK-LABEL: define void @alloc_per_location_wb2(
; CHECK: call rogcc void @rog_write_barrier_1(i64 %val0)
; CHECK: store i64 %val0, ptr %gep0
; CHECK: call rogcc void @rog_write_barrier_1(i64 %val8)
; CHECK: store i64 %val8, ptr %gep8
; CHECK-NOT: rog_write_barrier_2
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  %gep0 = getelementptr i64, ptr %p, i64 0
  %gep8 = getelementptr i64, ptr %p, i64 1
  ; First: barrier + store at offset 0
  %old0 = load i64, ptr %gep0
  call rogcc void @rog_write_barrier_2(i64 %old0, i64 %val0)
  store i64 %val0, ptr %gep0
  ; Second: barrier + store at offset 8 — should still be optimized
  ; (old value is zero-initialized, store at offset 0 doesn't affect offset 8)
  %old8 = load i64, ptr %gep8
  call rogcc void @rog_write_barrier_2(i64 %old8, i64 %val8)
  store i64 %val8, ptr %gep8
  ret void
}

define void @alloc_no_prior_write_bulk(ptr %src, i64 %size) gc "rog" {
; CHECK-LABEL: define void @alloc_no_prior_write_bulk(
; CHECK: call rogcc void @rog_src_bulk_write_barrier(ptr %src, i64 %size)
; CHECK-NOT: rog_bulk_write_barrier
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  ; Dest is newly allocated with no prior writes — all old values are zero.
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 %size)
  ret void
}

define void @bulk_self_copy_drops_barrier(ptr %p, i64 %size) gc "rog" {
; CHECK-LABEL: define void @bulk_self_copy_drops_barrier(
; CHECK-NOT: rog_bulk_write_barrier
; CHECK-NOT: rog_src_bulk_write_barrier
; CHECK: ret void
entry:
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %p, i64 %size)
  ret void
}

define void @bulk_ignores_prior_barrier_clobber(ptr %src, i64 %value,
                                                i64 %size) gc "rog" {
; CHECK-LABEL: define void @bulk_ignores_prior_barrier_clobber(
; CHECK: call rogcc void @rog_write_barrier_1(i64 %value)
; CHECK-NOT: call rogcc void @rog_bulk_write_barrier
; CHECK: call rogcc void @rog_src_bulk_write_barrier(ptr %src, i64 %size)
entry:
  %p = call noalias ptr @rog_alloc(i64 32)
  call rogcc void @rog_write_barrier_1(i64 %value)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 %size)
  ret void
}

define void @bulk_escape_via_call_keeps_full(ptr %src) gc "rog" {
; CHECK-LABEL: define void @bulk_escape_via_call_keeps_full(
; CHECK: call void @unknown_mutate(ptr %p)
; CHECK-NOT: rog_src_bulk_write_barrier
; CHECK: call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16)
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  call void @unknown_mutate(ptr %p)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16)
  ret void
}

define void @bulk_escape_via_local_alias_keeps_full(ptr %src,
                                                    i64 %value) gc "rog" {
; CHECK-LABEL: define void @bulk_escape_via_local_alias_keeps_full(
; CHECK: %slot.addr = alloca ptr, align 8
; CHECK: store ptr %p, ptr %slot.addr, align 8
; CHECK: [[ALIAS:%.*]] = load ptr, ptr %slot.addr, align 8
; CHECK: store i64 %value, ptr [[ALIAS]], align 4
; CHECK-NOT: rog_src_bulk_write_barrier
; CHECK: call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16)
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  %slot.addr = alloca ptr, align 8
  store ptr %p, ptr %slot.addr, align 8
  %alias = load ptr, ptr %slot.addr, align 8
  store i64 %value, ptr %alias, align 4
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16)
  ret void
}

define void @noalias_nonalloc_bulk(ptr %src, i64 %size) gc "rog" {
; CHECK-LABEL: define void @noalias_nonalloc_bulk(
; CHECK: %p = call noalias ptr @rog_noalias_nonalloc()
; CHECK-NOT: rog_src_bulk_write_barrier
; CHECK: call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 %size)
entry:
  %p = call noalias ptr @rog_noalias_nonalloc()
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 %size)
  ret void
}

; --- Counterexample: a prior write anywhere in the bulk range must block downgrade ---

define void @bulk_partial_write(ptr %src) gc "rog" {
; CHECK-LABEL: define void @bulk_partial_write(
; CHECK: store i64 42, ptr %gep8
; CHECK-NOT: rog_src_bulk_write_barrier
; CHECK: call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16)
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  %gep8 = getelementptr i64, ptr %p, i64 1
  ; A write at offset 8 means the destination range is no longer untouched.
  store i64 42, ptr %gep8
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16)
  ret void
}

define void @bulk_phi_dest_keeps_full(ptr %src, i1 %cond) gc "rog" {
; CHECK-LABEL: define void @bulk_phi_dest_keeps_full(
; CHECK: %dest = phi ptr [ %p0, %then ], [ %p1, %else ]
; CHECK-NOT: rog_src_bulk_write_barrier
; CHECK: call rogcc void @rog_bulk_write_barrier(ptr %dest, ptr %src, i64 16)
entry:
  br i1 %cond, label %then, label %else

then:
  %p0 = call noalias ptr @rog_alloc(i64 16)
  br label %merge

else:
  %p1 = call noalias ptr @rog_alloc(i64 16)
  br label %merge

merge:
  %dest = phi ptr [ %p0, %then ], [ %p1, %else ]
  call rogcc void @rog_bulk_write_barrier(ptr %dest, ptr %src, i64 16)
  ret void
}
