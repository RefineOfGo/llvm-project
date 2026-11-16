; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -function-sections | FileCheck %s --implicit-check-not=.llvm_stackobjs
;
; The Direct and function-table descriptions use their existing different
; range conversions before entering the same object dictionary. Preserve the
; original offset for owner attribution after rounding the Direct inward.
;
; CHECK: .section .llvm_stackmaps,"ao",@progbits,inward
; CHECK: .byte 85
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 64
; CHECK-NEXT: .long 184
; CHECK: .quad inward-
; CHECK-NEXT: .quad {{[0-9]+}}
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 3
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 1
; CHECK-NEXT: .quad 0
; Direct projection: original [-12, 0) -> scan [-8, 0), owner offset -12.
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 0
; CHECK-NEXT: .long 1
; CHECK-NEXT: .quad -8
; CHECK-NEXT: .long 4294967284
; CHECK-NEXT: .long 0
; Original function range: base -12, ceil(12 / 8) = 2 words.
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 0
; CHECK-NEXT: .long 2
; CHECK-NEXT: .quad -12
; CHECK-NEXT: .long 4294967284
; CHECK-NEXT: .long 0

define hidden void @inward() {
entry:
  %object = alloca [3 x i32], align 4, !rog.stackobj !0
  call void @use(ptr %object)
  call void (i64, i32, ...) @llvm.experimental.stackmap(
      i64 -9223372036854775808, i32 0, ptr %object, i64 12)
  ret void
}

declare void @use(ptr)
declare void @llvm.experimental.stackmap(i64, i32, ...)
!0 = !{}
