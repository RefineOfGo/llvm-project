; RUN: opt -passes=rog-gc-write-barrier-opt -S %s | FileCheck %s

declare rogcc void @rog_write_barrier_1(i64)
declare rogcc void @rog_write_barrier_2(i64, i64)
declare rogcc void @rog_bulk_write_barrier(ptr, ptr, i64)
declare noalias ptr @rog_alloc(i64) allockind("alloc")

; Decompose: fresh dest, one pointer slot -> WB1
define void @decompose_fresh_dest_to_wb1(ptr %src) gc "rog" {
; CHECK-LABEL: define void @decompose_fresh_dest_to_wb1(
; CHECK-NOT: rog_bulk_write_barrier
; CHECK-NOT: rog_write_barrier_2
; CHECK: call rogcc void @rog_write_barrier_1(
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16), !rog.bulk_write_barrier.ptrmap !0
  ret void
}

; Decompose: non-fresh dest, one pointer slot -> WB2 kept
define void @decompose_non_fresh_keeps_wb2(ptr %src, i64 %val) gc "rog" {
; CHECK-LABEL: define void @decompose_non_fresh_keeps_wb2(
; CHECK-NOT: rog_bulk_write_barrier
; CHECK: call rogcc void @rog_write_barrier_2(
; CHECK-NOT: rog_write_barrier_1
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  store i64 %val, ptr %p
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16), !rog.bulk_write_barrier.ptrmap !0
  ret void
}

; Decompose: two pointer slots, fresh dest -> 2 x WB1
define void @decompose_two_ptrs_fresh_dest(ptr %src) gc "rog" {
; CHECK-LABEL: define void @decompose_two_ptrs_fresh_dest(
; CHECK-NOT: rog_bulk_write_barrier
; CHECK-NOT: rog_write_barrier_2
; CHECK: call rogcc void @rog_write_barrier_1(
; CHECK: call rogcc void @rog_write_barrier_1(
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 32)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 32), !rog.bulk_write_barrier.ptrmap !1
  ret void
}

; Decompose: two objects, one pointer each, fresh dest -> 2 x WB1
define void @decompose_two_objects(ptr %src) gc "rog" {
; CHECK-LABEL: define void @decompose_two_objects(
; CHECK-NOT: rog_bulk_write_barrier
; CHECK-NOT: rog_write_barrier_2
; CHECK: call rogcc void @rog_write_barrier_1(
; CHECK: call rogcc void @rog_write_barrier_1(
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 32)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 32), !rog.bulk_write_barrier.ptrmap !0
  ret void
}

; Decompose: partial write — slot 0 clobbered (WB2 kept), slot 3 fresh (WB1)
define void @decompose_partial_write(ptr %src, i64 %val) gc "rog" {
; CHECK-LABEL: define void @decompose_partial_write(
; CHECK-NOT: rog_bulk_write_barrier
; CHECK: call rogcc void @rog_write_barrier_2(
; CHECK: call rogcc void @rog_write_barrier_1(
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 32)
  store i64 %val, ptr %p
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 32), !rog.bulk_write_barrier.ptrmap !1
  ret void
}

; Decompose: too many pointers (>4) -> fall through to fresh-dest downgrade
define void @decompose_too_many_ptrs(ptr %src) gc "rog" {
; CHECK-LABEL: define void @decompose_too_many_ptrs(
; CHECK-NOT: rog_write_barrier_2
; CHECK-NOT: rog_write_barrier_1
; CHECK: call rogcc void @rog_src_bulk_write_barrier(ptr %src, i64 40)
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 40)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 40), !rog.bulk_write_barrier.ptrmap !2
  ret void
}

; Decompose: non-constant size -> fall through to fresh-dest downgrade
define void @decompose_non_const_size(ptr %src, i64 %size) gc "rog" {
; CHECK-LABEL: define void @decompose_non_const_size(
; CHECK-NOT: rog_write_barrier_2
; CHECK-NOT: rog_write_barrier_1
; CHECK: call rogcc void @rog_src_bulk_write_barrier(ptr %src, i64 %size)
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 64)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 %size), !rog.bulk_write_barrier.ptrmap !0
  ret void
}

; Decompose: size not divisible by ObjectSize -> fall through to fresh-dest downgrade
define void @decompose_size_not_divisible(ptr %src) gc "rog" {
; CHECK-LABEL: define void @decompose_size_not_divisible(
; CHECK-NOT: rog_write_barrier_2
; CHECK-NOT: rog_write_barrier_1
; CHECK: call rogcc void @rog_src_bulk_write_barrier(ptr %src, i64 24)
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 24)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 24), !rog.bulk_write_barrier.ptrmap !0
  ret void
}

; Decompose: zero pointer slots -> fall through to fresh-dest downgrade
define void @decompose_zero_ptrs(ptr %src) gc "rog" {
; CHECK-LABEL: define void @decompose_zero_ptrs(
; CHECK-NOT: rog_write_barrier_2
; CHECK-NOT: rog_write_barrier_1
; CHECK: call rogcc void @rog_src_bulk_write_barrier(ptr %src, i64 16)
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %src, i64 16), !rog.bulk_write_barrier.ptrmap !3
  ret void
}

; Decompose: self-copy with ptrmap -> eliminated before decomposition
define void @decompose_self_copy_with_ptrmap() gc "rog" {
; CHECK-LABEL: define void @decompose_self_copy_with_ptrmap(
; CHECK-NOT: rog_bulk_write_barrier
; CHECK-NOT: rog_src_bulk_write_barrier
; CHECK-NOT: rog_write_barrier_
; CHECK: ret void
entry:
  %p = call noalias ptr @rog_alloc(i64 16)
  call rogcc void @rog_bulk_write_barrier(ptr %p, ptr %p, i64 16), !rog.bulk_write_barrier.ptrmap !0
  ret void
}

; ptrmap: "10" — one pointer at word 0, ObjectSize = 16
!0 = !{!"10"}
; ptrmap: "1001" — pointers at words 0 and 3, ObjectSize = 32
!1 = !{!"1001"}
; ptrmap: "11111" — five pointers, ObjectSize = 40
!2 = !{!"11111"}
; ptrmap: "00" — zero pointers, ObjectSize = 16
!3 = !{!"00"}
