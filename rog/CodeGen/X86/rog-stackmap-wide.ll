; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -function-sections | FileCheck %s --implicit-check-not=.llvm_stackobjs
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -function-sections -code-model=large | FileCheck %s --implicit-check-not=.llvm_stackobjs
;
; Cross both root and object bitmap-word boundaries. There are 65 Direct
; objects followed by 65 function-table objects. The function set therefore
; occupies bits 65..129; the safepoint's Direct set occupies bits 0..64.
;
; CHECK: .section .llvm_stackmaps,"ao",@progbits,wide
; CHECK: .byte 85
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 64
; CHECK-NEXT: .long 4088
; CHECK: .quad wide-
; CHECK-NEXT: .quad {{[0-9]+}}
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 65
; CHECK-NEXT: .long 130
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 3
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 1
; CHECK-NEXT: .quad 0
; CHECK: .p2align 3
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad -1
; CHECK-NEXT: .quad 1
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad -2
; CHECK-NEXT: .quad 3
; CHECK-NEXT: .quad -1
; CHECK-NEXT: .quad 1
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .long {{.*}}-wide
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 0

define hidden void @wide() {
entry:
  %a0 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a1 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a2 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a3 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a4 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a5 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a6 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a7 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a8 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a9 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a10 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a11 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a12 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a13 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a14 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a15 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a16 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a17 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a18 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a19 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a20 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a21 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a22 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a23 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a24 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a25 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a26 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a27 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a28 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a29 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a30 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a31 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a32 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a33 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a34 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a35 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a36 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a37 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a38 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a39 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a40 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a41 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a42 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a43 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a44 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a45 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a46 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a47 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a48 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a49 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a50 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a51 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a52 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a53 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a54 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a55 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a56 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a57 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a58 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a59 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a60 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a61 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a62 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a63 = alloca [2 x ptr], align 8, !rog.stackobj !0
  %a64 = alloca [2 x ptr], align 8, !rog.stackobj !0
  call void (i64, i32, ...) @llvm.experimental.stackmap(
      i64 -9223372036854775808, i32 0,
      ptr %a0, i64 16,
      ptr %a1, i64 16,
      ptr %a2, i64 16,
      ptr %a3, i64 16,
      ptr %a4, i64 16,
      ptr %a5, i64 16,
      ptr %a6, i64 16,
      ptr %a7, i64 16,
      ptr %a8, i64 16,
      ptr %a9, i64 16,
      ptr %a10, i64 16,
      ptr %a11, i64 16,
      ptr %a12, i64 16,
      ptr %a13, i64 16,
      ptr %a14, i64 16,
      ptr %a15, i64 16,
      ptr %a16, i64 16,
      ptr %a17, i64 16,
      ptr %a18, i64 16,
      ptr %a19, i64 16,
      ptr %a20, i64 16,
      ptr %a21, i64 16,
      ptr %a22, i64 16,
      ptr %a23, i64 16,
      ptr %a24, i64 16,
      ptr %a25, i64 16,
      ptr %a26, i64 16,
      ptr %a27, i64 16,
      ptr %a28, i64 16,
      ptr %a29, i64 16,
      ptr %a30, i64 16,
      ptr %a31, i64 16,
      ptr %a32, i64 16,
      ptr %a33, i64 16,
      ptr %a34, i64 16,
      ptr %a35, i64 16,
      ptr %a36, i64 16,
      ptr %a37, i64 16,
      ptr %a38, i64 16,
      ptr %a39, i64 16,
      ptr %a40, i64 16,
      ptr %a41, i64 16,
      ptr %a42, i64 16,
      ptr %a43, i64 16,
      ptr %a44, i64 16,
      ptr %a45, i64 16,
      ptr %a46, i64 16,
      ptr %a47, i64 16,
      ptr %a48, i64 16,
      ptr %a49, i64 16,
      ptr %a50, i64 16,
      ptr %a51, i64 16,
      ptr %a52, i64 16,
      ptr %a53, i64 16,
      ptr %a54, i64 16,
      ptr %a55, i64 16,
      ptr %a56, i64 16,
      ptr %a57, i64 16,
      ptr %a58, i64 16,
      ptr %a59, i64 16,
      ptr %a60, i64 16,
      ptr %a61, i64 16,
      ptr %a62, i64 16,
      ptr %a63, i64 16,
      ptr %a64, i64 16
  )
  ret void
}

declare void @llvm.experimental.stackmap(i64, i32, ...)
!0 = !{}
