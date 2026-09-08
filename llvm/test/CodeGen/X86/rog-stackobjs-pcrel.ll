; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=pic -function-sections | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=pic -function-sections -filetype=obj -o %t
; RUN: llvm-readobj --relocations %t | FileCheck %s --check-prefix=RELOC
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=static -function-sections | FileCheck %s --check-prefix=STATIC
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=static -function-sections -filetype=obj -o %t.static
; RUN: llvm-readobj --relocations %t.static | FileCheck %s --check-prefix=STATIC-RELOC
;
; Stack-object metadata follows its function and uses field-relative PCs so
; plugin DSOs need no absolute function-address relocations in this section.
; Static output retains the absolute format used by existing runtime readers.
;
; CHECK: .section .llvm_stackobjs,"ao",@progbits,live
; CHECK: .long 2147483649
; CHECK: [[FIELD:.Ltmp[0-9]+]]:
; CHECK-NEXT: .quad {{.*}}-[[FIELD]]
; CHECK: .long 1
;
; RELOC: Section {{.*}} .rela.llvm_stackobjs {
; RELOC-NEXT: {{.*}} R_X86_64_PC64
; RELOC-NEXT: }

; STATIC: .section .llvm_stackobjs,"ao",@progbits,live
; STATIC: .long 1
; STATIC-NEXT: .quad live+
; STATIC-RELOC: Section {{.*}} .rela.llvm_stackobjs {
; STATIC-RELOC-NEXT: {{.*}} R_X86_64_64
; STATIC-RELOC-NEXT: }

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
