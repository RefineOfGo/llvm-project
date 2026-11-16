; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=pic -function-sections | FileCheck %s --implicit-check-not=.llvm_stackobjs
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=pic -function-sections -filetype=obj -o %t
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -relocation-model=static -function-sections | FileCheck %s --implicit-check-not=.llvm_stackobjs
; RUN: llvm-readobj --relocations %t | FileCheck %s --check-prefix=RELOC
;
; The same alloca is first described as witness-only and then as a plain
; Direct. Its two slot identities survive object normalization even though
; they have identical ranges. A non-precise record retains the raw witness
; size in its roots and selects the original function-wide object set.
;
; CHECK: .section .llvm_stackmaps,"ao",@progbits,mixed_stack_objects
; CHECK: .byte 85
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 64
; CHECK-NEXT: .long 240
; CHECK-NEXT: [[ADDR:.Ltmp[0-9]+]]:
; CHECK-NEXT: .quad mixed_stack_objects-[[ADDR]]
; CHECK-NEXT: .quad {{[0-9]+}}
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 3
; CHECK-NEXT: .long 3
; CHECK-NEXT: .long 3
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 1
; CHECK-NEXT: .quad 0
; Three object entries: two Direct projections and the function object.
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 0
; CHECK-NEXT: .long 2
; CHECK-NEXT: .quad -16
; CHECK-NEXT: .long 4294967280
; CHECK-NEXT: .long 0
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 0
; CHECK-NEXT: .long 2
; CHECK-NEXT: .quad -16
; CHECK-NEXT: .long 4294967280
; CHECK-NEXT: .long 0
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 0
; CHECK-NEXT: .long 2
; CHECK-NEXT: .quad -16
; CHECK-NEXT: .long 4294967280
; CHECK-NEXT: .long 0
; Root dictionary: witness first, plain second, matching the original slots.
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 6
; CHECK-NEXT: .long 4294967280
; CHECK-NEXT: .long 1073741840
; CHECK-NEXT: .byte 2
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 6
; CHECK-NEXT: .long 4294967280
; CHECK-NEXT: .long 16
; CHECK-NEXT: .p2align 3
; Roots: empty, plain-only, raw witness-only.
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad 2
; CHECK-NEXT: .quad 1
; Objects: empty, function range, both Direct identities.
; CHECK-NEXT: .quad 0
; CHECK-NEXT: .quad 4
; CHECK-NEXT: .quad 3
; CHECK-NEXT: .long {{.*}}-mixed_stack_objects
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long {{.*}}-mixed_stack_objects
; CHECK-NEXT: .long 2
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 0
;
; RELOC: Section {{.*}} .rela.llvm_stackmaps {
; RELOC-NEXT: {{.*}} R_X86_64_PC64
; RELOC-NEXT: }
; RELOC-NOT: .rela.llvm_stackobjs

define hidden void @mixed_stack_objects() {
entry:
  %object = alloca [2 x ptr], align 8, !rog.stackobj !0
  call void @use(ptr %object)
  call void (i64, i32, ...) @llvm.experimental.stackmap(
      i64 -9223372036854775808, i32 0,
      ptr %object, i64 1073741840, ptr %object, i64 16)
  call void asm sideeffect "nop", ""()
  call void (i64, i32, ...) @llvm.experimental.stackmap(
      i64 0, i32 0, ptr %object, i64 1073741840)
  ret void
}

declare void @use(ptr)
declare void @llvm.experimental.stackmap(i64, i32, ...)
!0 = !{}

; An empty precise record still references two explicit empty sets, without
; allocating bitmap words when the dictionaries themselves are empty.
; CHECK: .section .llvm_stackmaps,"ao",@progbits,empty
; CHECK: .byte 85
; CHECK-NEXT: .byte 0
; CHECK-NEXT: .short 64
; CHECK-NEXT: .long 80
; CHECK: .quad empty-
; CHECK-NEXT: .quad {{[0-9]+}}
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 1
; CHECK-NEXT: .long 4294967295
; CHECK-NEXT: .quad 0
; CHECK: .long {{.*}}-empty
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0
; CHECK-NEXT: .long 0

define hidden void @empty() {
entry:
  call void (i64, i32, ...) @llvm.experimental.stackmap(
      i64 -9223372036854775808, i32 0)
  ret void
}
