; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -function-sections \
; RUN:    -enable-implicit-null-checks | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -function-sections \
; RUN:    -enable-implicit-null-checks -filetype=obj -o %t
; RUN: llvm-readobj --elf-output-style=GNU --sections --section-groups %t \
; RUN:    | FileCheck %s -check-prefix OBJECT

;; A fault-map section must join the function's COMDAT group in addition to
;; linking its lifetime to the function section. Otherwise COMDAT selection can
;; keep one copy of the function but fault-map records from every copy.

$imp_null_check_load = comdat any

define linkonce_odr i32 @imp_null_check_load(ptr %x) comdat {
; CHECK:      .section .text.imp_null_check_load,"axG",@progbits,imp_null_check_load,comdat
; CHECK:      .section .llvm_faultmaps,"aoG",@progbits,imp_null_check_load,imp_null_check_load,comdat
; CHECK-NEXT: .byte 1
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 0
; CHECK-NEXT: .long 1
; CHECK-NEXT: .quad imp_null_check_load
entry:
  %c = icmp eq ptr %x, null
  br i1 %c, label %is_null, label %not_null, !make.implicit !0

is_null:
  ret i32 42

not_null:
  %t = load i32, ptr %x
  ret i32 %t
}

!0 = !{}

; OBJECT: {{\[ *}}[[TEXT:[0-9]+]]{{\]}} .text.imp_null_check_load {{.*}} AXG
; OBJECT: {{\[ *}}[[FAULTMAP:[0-9]+]]{{\]}} .llvm_faultmaps {{.*}} ALG [[TEXT]]
; OBJECT: COMDAT group section {{.*}} [imp_null_check_load] contains 3 sections:
; OBJECT: {{\[ *}}[[TEXT]]{{\]}}   .text.imp_null_check_load
; OBJECT: {{\[ *}}[[FAULTMAP]]{{\]}}   .llvm_faultmaps
