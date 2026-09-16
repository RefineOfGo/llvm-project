; RUN: opt -passes=reassociate -S < %s | FileCheck %s

; Rank-only canonicalization must not reverse short-circuit conditions that
; code generation can split back into branches.
define i32 @and_branch(ptr %slot, i64 %key, i64 %bits) {
; CHECK-LABEL: @and_branch(
; CHECK: %cond = and i1 %equal, %occupied
; CHECK-NEXT: br i1 %cond,
  %value = load i64, ptr %slot, align 8
  %equal = icmp eq i64 %value, %key
  %low = and i64 %bits, 1
  %occupied = icmp ne i64 %low, 0
  %cond = and i1 %equal, %occupied
  br i1 %cond, label %yes, label %no
yes:
  ret i32 1
no:
  ret i32 0
}

define i32 @or_branch(ptr %slot, i64 %key, i64 %bits) {
; CHECK-LABEL: @or_branch(
; CHECK: %skip = or i1 %unequal, %vacant
; CHECK-NEXT: br i1 %skip,
  %value = load i64, ptr %slot, align 8
  %unequal = icmp ne i64 %value, %key
  %low = and i64 %bits, 1
  %vacant = icmp eq i64 %low, 0
  %skip = or i1 %unequal, %vacant
  br i1 %skip, label %no, label %yes
yes:
  ret i32 1
no:
  ret i32 0
}

; Keep constant canonicalization, including when the result feeds a branch.
define i32 @constant_branch(i1 %arg) {
; CHECK-LABEL: @constant_branch(
; CHECK: %cond = or i1 %arg, true
  %cond = or i1 true, %arg
  br i1 %cond, label %yes, label %no
yes:
  ret i32 1
no:
  ret i32 0
}

; A shared value is not just a short-circuit branch condition.
define i32 @shared_condition(ptr %slot, i64 %key, i64 %bits) {
; CHECK-LABEL: @shared_condition(
; CHECK: %cond = or i1 %vacant, %unequal
  %value = load i64, ptr %slot, align 8
  %unequal = icmp ne i64 %value, %key
  %low = and i64 %bits, 1
  %vacant = icmp eq i64 %low, 0
  %cond = or i1 %unequal, %vacant
  %result = zext i1 %cond to i32
  br i1 %cond, label %yes, label %no
yes:
  ret i32 %result
no:
  ret i32 0
}

; Boolean values defined elsewhere do not recover a short-circuit comparison.
define i32 @boolean_inputs(i1 %first, i1 %second) {
; CHECK-LABEL: @boolean_inputs(
; CHECK: %cond = and i1 %first, %second
  %cond = and i1 %second, %first
  br i1 %cond, label %yes, label %no
yes:
  ret i32 1
no:
  ret i32 0
}
