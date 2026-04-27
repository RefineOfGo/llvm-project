; RUN: opt -passes=rog-gc-write-barrier-opt -S %s | FileCheck %s

@glob = global i64 0, align 8
@arr = global [2 x i64] zeroinitializer, align 8

declare rogcc void @rog_write_barrier_2(i64, i64)

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
