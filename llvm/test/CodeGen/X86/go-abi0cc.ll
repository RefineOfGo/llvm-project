; RUN: llc -mtriple=x86_64-unknown-linux-gnu -verify-machineinstrs -fast-isel=false < %s | FileCheck %s
; RUN: llc -mtriple=x86_64-pc-linux -stackrealign -verify-machineinstrs < %s -o /dev/null

; go_abi0cc lowers all arguments and results to the Go ABI0 stack area, zeroes
; XMM15 before the call, preserves RBP (frame pointer) without a caller-side
; save/restore, and demotes aggregate sret returns to the ABI0 return slot.

declare go_abi0cc i64 @abi0_scalar(i64, i32, double)

define i64 @caller(i64 %a, i32 %b, double %c) {
; CHECK-LABEL: caller:
; CHECK: subq ${{[0-9]+}}, %rsp
; CHECK-DAG: movq %rdi, {{[0-9]*}}(%rsp)
; CHECK-DAG: movl %esi, {{[0-9]+}}(%rsp)
; CHECK-DAG: movsd %xmm0, {{[0-9]+}}(%rsp)
; CHECK: {{xorps|pxor|vxorps|vpxor}} {{.*}}%xmm15
; CHECK: callq abi0_scalar
; CHECK: movq {{[0-9]+}}(%rsp), %rax
; CHECK: addq ${{[0-9]+}}, %rsp
  %ret = call go_abi0cc i64 @abi0_scalar(i64 %a, i32 %b, double %c)
  ret i64 %ret
}

%cpuid_ret = type { i32, i32, i32, i32 }

declare go_abi0cc %cpuid_ret @abi0_cpuid(i32, i32)

define void @caller_sret(ptr %dst) {
; CHECK-LABEL: caller_sret:
; CHECK: subq ${{[0-9]+}}, %rsp
; CHECK: movq $0, (%rsp)
; CHECK: {{xorps|pxor|vxorps|vpxor}} {{.*}}%xmm15
; CHECK: callq abi0_cpuid
; CHECK: {{8|0x8}}(%rsp)
; CHECK: addq ${{[0-9]+}}, %rsp
  %ret = call go_abi0cc %cpuid_ret @abi0_cpuid(i32 0, i32 0)
  store %cpuid_ret %ret, ptr %dst, align 4
  ret void
}

declare go_abi0cc void @abi0_void(i64, i32)

define void @caller_void(i64 %a, i32 %b) {
; A void go_abi0cc call zeroes XMM15 and calls the ABI0 body but loads no
; result back. (Arg passing form — push vs subq+store — is left unconstrained.)
; CHECK-LABEL: caller_void:
; CHECK: {{xorps|pxor|vxorps|vpxor}} {{.*}}%xmm15
; CHECK: callq abi0_void
  call go_abi0cc void @abi0_void(i64 %a, i32 %b)
  ret void
}

declare go_abi0cc i64 @abi0_mixed(i64, i32, double, ptr)

define i64 @caller_mixed(i64 %a, i32 %b, double %c, ptr %d) {
; Mixed integer/float/pointer args: the float must be spilled from an XMM reg
; to the ABI0 stack area, XMM15 is zeroed, and the result is loaded back.
; CHECK-LABEL: caller_mixed:
; CHECK: movsd %xmm0, {{[0-9]+}}(%rsp)
; CHECK: {{xorps|pxor|vxorps|vpxor}} {{.*}}%xmm15
; CHECK: callq abi0_mixed
; CHECK: movq {{[0-9]+}}(%rsp), %rax
  %ret = call go_abi0cc i64 @abi0_mixed(i64 %a, i32 %b, double %c, ptr %d)
  ret i64 %ret
}
