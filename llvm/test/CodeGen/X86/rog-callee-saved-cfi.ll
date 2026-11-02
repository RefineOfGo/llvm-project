; RUN: llc -mtriple=x86_64-unknown-linux-gnu -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -mtriple=x86_64-unknown-linux-gnu -filetype=obj < %s -o %t
; RUN: llvm-dwarfdump --eh-frame %t | FileCheck %s --check-prefix=FRAME

; FRAME:       FDE cie=
; FRAME:       DW_CFA_same_value: RBX
; FRAME-NEXT:  DW_CFA_same_value: R12
; FRAME-NEXT:  DW_CFA_same_value: R13
; FRAME-NEXT:  DW_CFA_same_value: R14
; FRAME-NEXT:  DW_CFA_same_value: R15
; FRAME-NEXT:  DW_CFA_same_value: RBP

; Describe unchanged ABI registers even in leaf functions. Debuggers that only
; recover explicit CFI rules must not lose the caller's register locations.
; The rules are per function, so other calling conventions keep their defaults.

define rogcc void @rog_leaf() uwtable {
; CHECK-LABEL: rog_leaf:
; CHECK:       .cfi_startproc
; CHECK-NEXT:  .cfi_same_value %rbx
; CHECK-NEXT:  .cfi_same_value %r12
; CHECK-NEXT:  .cfi_same_value %r13
; CHECK-NEXT:  .cfi_same_value %r14
; CHECK-NEXT:  .cfi_same_value %r15
; CHECK-NEXT:  .cfi_same_value %rbp
; CHECK:       retq
; CHECK:       .cfi_endproc
  ret void
}

define rog_coldcc void @rog_cold_leaf() uwtable {
; CHECK-LABEL: rog_cold_leaf:
; CHECK:       .cfi_startproc
; CHECK:       .cfi_same_value %r15
; CHECK:       .cfi_same_value %rax
; CHECK:       .cfi_same_value %xmm0
; CHECK:       retq
; CHECK:       .cfi_endproc
  ret void
}

; Respect the actual function's ABI override rather than a fixed SysV list.
define rogcc void @rog_no_csr() uwtable "no_callee_saved_registers" {
; CHECK-LABEL: rog_no_csr:
; CHECK:       .cfi_startproc
; CHECK-NOT:   .cfi_same_value
; CHECK:       .cfi_endproc
  ret void
}

define void @c_leaf() uwtable {
; CHECK-LABEL: c_leaf:
; CHECK:       .cfi_startproc
; CHECK-NOT:   .cfi_same_value
; CHECK:       .cfi_endproc
  ret void
}

; The no-caller-saved override dynamically excludes argument/return registers.
; Here RAX carries both, and its incoming value is changed by the function.
define rogcc i64 @rog_modified_arg(i64 %value) uwtable "no_caller_saved_registers" {
; CHECK-LABEL: rog_modified_arg:
; CHECK:       .cfi_startproc
; CHECK-NOT:   .cfi_same_value %rax
; CHECK:       .cfi_same_value %rbx
; CHECK-NOT:   .cfi_same_value %rax
; CHECK:       .cfi_same_value %xmm15
; CHECK-NOT:   .cfi_same_value %rax
; CHECK:       addq $7, %rax
; CHECK-NOT:   .cfi_same_value %rax
; CHECK:       .cfi_endproc
  %result = add i64 %value, 7
  ret i64 %result
}

; The initial rules must precede the ROG stack check. A spilled register gets
; its normal offset rule, then explicitly becomes SameValue again: restoring
; the CIE rule would discard the FDE's entry rule on the out-of-line slow path.
define rogcc void @rog_spill() uwtable "rog-stack-check" "frame-pointer"="all" {
; CHECK-LABEL: rog_spill:
; CHECK:       .cfi_startproc
; CHECK:       .cfi_same_value %r15
; CHECK:       cmpq %fs:
; CHECK:       .cfi_offset %r15,
; CHECK:       retq
; CHECK:       .cfi_same_value %r15
; CHECK:       callq rog_morestack_abi
; CHECK:       .cfi_endproc
; FRAME:       DW_CFA_offset: R15 -24
; FRAME:       DW_CFA_same_value: R15
; FRAME:       CFA=RBP+16: {{.*}}R15=[CFA-24]
; FRAME:       CFA=RSP+8: {{.*}}RBP=same{{.*}}R15=same
  call void asm sideeffect "", "~{r15}"()
  ret void
}
