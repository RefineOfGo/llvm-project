; RUN: llc -mtriple=x86_64-unknown-linux-gnu -verify-machineinstrs -fast-isel=false < %s | FileCheck %s

; go_abiinternalcc passes arguments and results in the Go ABIInternal register
; sequence: integers RAX,RBX,RCX,RDI,RSI,R8,R9,R10,R11 (RDX is reserved as the
; closure context, NOT an argument register); floats X0-X14; overflow on the
; stack. XMM15 is zeroed before the call; RBP is preserved without a caller-side
; save/restore.

declare go_abiinternalcc i64 @abi_internal_add(i64, i64, i64)

define i64 @caller(i64 %a, i64 %b, i64 %c) {
; The SysV caller (args in RDI/RSI/RDX) remaps to ABIInternal RAX/RBX/RCX.
; Pin the source registers so a regression that drops the remap (e.g. leaves an
; argument in its SysV register) is actually caught.
; CHECK-LABEL: caller:
; CHECK-DAG: movq %rdi, %rax
; CHECK-DAG: movq %rsi, %rbx
; CHECK-DAG: movq %rdx, %rcx
; CHECK-DAG: {{xorps|pxor|vxorps|vpxor}} {{.*}}%xmm15
; CHECK: callq abi_internal_add
  %r = call go_abiinternalcc i64 @abi_internal_add(i64 %a, i64 %b, i64 %c)
  ret i64 %r
}

declare go_abiinternalcc void @f9(i64, i64, i64, i64, i64, i64, i64, i64, i64)

define void @call_nine_ints() {
; Exactly pin the integer register sequence with constant args. The 9 values
; must land in RAX,RBX,RCX,RDI,RSI,R8,R9,R10,R11 (in that order) and NEVER RDX.
; CHECK-LABEL: call_nine_ints:
; CHECK-DAG: movl $1, %eax
; CHECK-DAG: movl $2, %ebx
; CHECK-DAG: movl $3, %ecx
; CHECK-DAG: movl $4, %edi
; CHECK-DAG: movl $5, %esi
; CHECK-DAG: movl $6, %r8d
; CHECK-DAG: movl $7, %r9d
; CHECK-DAG: movl $8, %r10d
; CHECK-DAG: movl $9, %r11d
; CHECK: callq f9
  call go_abiinternalcc void @f9(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8, i64 9)
  ret void
}

declare go_abiinternalcc void @f10(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64)

define void @call_ten_ints() {
; The 9th arg fills the last integer register (R11); the 10th overflows to the
; stack.
; CHECK-LABEL: call_ten_ints:
; CHECK-DAG: movl $9, %r11d
; CHECK-DAG: movq $10, (%rsp)
; CHECK: callq f10
  call go_abiinternalcc void @f10(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8, i64 9, i64 10)
  ret void
}

declare go_abiinternalcc void @fmix(i64, double, i64, double, i64)

define void @call_mixed_int_float() {
; Integers and floats are assigned from independent sequences: the 3 ints go to
; RAX,RBX,RCX regardless of the interleaved float args.
; CHECK-LABEL: call_mixed_int_float:
; CHECK-DAG: movl $1, %eax
; CHECK-DAG: movl $2, %ebx
; CHECK-DAG: movl $3, %ecx
; CHECK: callq fmix
  call go_abiinternalcc void @fmix(i64 1, double 1.0, i64 2, double 2.0, i64 3)
  ret void
}

%ret3 = type { i64, i64, i64 }
declare go_abiinternalcc %ret3 @f_ret3()

define i64 @call_multi_return() {
; A 3-int aggregate return comes back in RAX,RBX,RCX; the caller consumes all
; three (so RBX and RCX must be read after the call).
; CHECK-LABEL: call_multi_return:
; CHECK: callq f_ret3
; CHECK-DAG: %rbx
; CHECK-DAG: %rcx
  %r = call go_abiinternalcc %ret3 @f_ret3()
  %a = extractvalue %ret3 %r, 0
  %b = extractvalue %ret3 %r, 1
  %c = extractvalue %ret3 %r, 2
  %ab = add i64 %a, %b
  %abc = add i64 %ab, %c
  ret i64 %abc
}

declare go_abiinternalcc i64 @abi_internal_indexbyte(ptr, i64, i8)

define i64 @caller_indexbyte(ptr %p, i64 %n, i8 %c) {
; A []byte-style signature (ptr, len, byte) -> int, like a register-only
; ABIInternal helper. i8 is promoted and placed in the 3rd integer register.
; CHECK-LABEL: caller_indexbyte:
; CHECK: {{xorps|pxor|vxorps|vpxor}} {{.*}}%xmm15
; CHECK: callq abi_internal_indexbyte
  %r = call go_abiinternalcc i64 @abi_internal_indexbyte(ptr %p, i64 %n, i8 %c)
  ret i64 %r
}

declare go_abiinternalcc double @abi_internal_fadd(double, double)

define double @caller_float(double %a, double %b) {
; Float args use X0.., result in X0.
; CHECK-LABEL: caller_float:
; CHECK: {{xorps|pxor|vxorps|vpxor}} {{.*}}%xmm15
; CHECK: callq abi_internal_fadd
  %r = call go_abiinternalcc double @abi_internal_fadd(double %a, double %b)
  ret double %r
}

declare go_abiinternalcc void @abi_internal_sink(i64)

define void @caller_base_pointer(i64 %n) {
; The Go ABI regmask (CSR_64_NoneRegs) preserves only RBP, NOT the base pointer
; (RBX on x86-64), and go_abiinternalcc uses RBX as an argument register. A
; caller that needs a base pointer (a dynamic alloca forces a frame pointer, an
; over-aligned object forces stack realignment) must therefore save/restore RBX
; around the call. Regression guard: if the base-pointer clobber stops being
; tracked, the push/pop %rbx bracketing the call disappears and frame access
; through RBX is corrupted.
; CHECK-LABEL: caller_base_pointer:
; CHECK: movq %rsp, %rbx
; CHECK: pushq %rbx
; CHECK: callq abi_internal_sink
; CHECK: popq %rbx
  %dyn = alloca i8, i64 %n
  %over = alloca <8 x i64>, align 64
  store volatile <8 x i64> zeroinitializer, ptr %over, align 64
  call go_abiinternalcc void @abi_internal_sink(i64 %n)
  store volatile i8 7, ptr %dyn
  ret void
}
