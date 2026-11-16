; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=pic -function-sections | FileCheck %s --implicit-check-not=.llvm_stackobjs
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=pic -function-sections -filetype=obj -o %t
; RUN: llvm-readobj --relocations %t | FileCheck %s --check-prefix=RELOC
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=static -function-sections | FileCheck %s --implicit-check-not=.llvm_stackobjs
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=static -function-sections -filetype=obj -o %t.static
; RUN: llvm-readobj --relocations %t.static | FileCheck %s --check-prefix=RELOC
;
; Stackmap metadata follows its function. Both PIC and static output use the
; field-relative function address, without an independent PC/object side table.
;
; CHECK: .section .llvm_stackmaps,"ao",@progbits,live
; CHECK: .byte 85
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 64
; CHECK-NEXT: .long 152
; CHECK-NEXT: [[FIELD:.Ltmp[0-9]+]]:
; CHECK-NEXT: .quad live-[[FIELD]]
; CHECK-NEXT: .quad {{[0-9]+}}
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 1
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 0
; CHECK-NEXT: .long 2
; CHECK-NEXT: .quad -16
; CHECK-NEXT: .long 4294967280
; CHECK-NEXT: .long 0
;
; RELOC: Section {{.*}} .rela.llvm_stackmaps {
; RELOC-NEXT: {{.*}} R_X86_64_PC64
; RELOC-NEXT: }
; RELOC-NOT: .rela.llvm_stackobjs

define hidden void @live() {
entry:
  %object = alloca [2 x ptr], align 8, !rog.stackobj !0
  call void @use(ptr %object)
  call void (i64, i32, ...) @llvm.experimental.stackmap(i64 0, i32 0, ptr %object)
  ret void
}

declare void @use(ptr)
declare void @llvm.experimental.stackmap(i64, i32, ...)
!0 = !{}
