; RUN: llc -mtriple=x86_64-unknown-linux-gnu -enable-shrink-wrap=true < %s | FileCheck %s

; ROG stack checks must remain at function entry because they are also
; preemption checkpoints. The ordinary frame setup can still be shrink-wrapped
; to the cold path that actually needs the stack object.

%array = type [64 x i64]

define rogcc i64 @swcase(i64 %x) nounwind "rog-stack-check" "frame-pointer"="all" gc "rog" {
; CHECK-LABEL: swcase:
; CHECK:       # %bb.0:
; CHECK-NEXT:    cmpq %fs:128, %rsp
; CHECK-NEXT:    jbe .LBB0_[[MORESTACK:[0-9]+]]
; CHECK-NEXT:  # %bb.1: # %entry
; CHECK-NEXT:    testq %rax, %rax
; CHECK-NEXT:    je .LBB0_[[FAST:[0-9]+]]
; CHECK-NEXT:  .LBB0_[[COLD:[0-9]+]]: # %cold
; CHECK-NEXT:    pushq %rbp
; CHECK-NEXT:    movq %rsp, %rbp
; CHECK-NEXT:    subq $512, %rsp
; CHECK:         callq use@PLT
; CHECK:         addq $512, %rsp
; CHECK-NEXT:    popq %rbp
; CHECK-NEXT:    retq
; CHECK-NEXT:  .LBB0_[[MORESTACK]]:
; CHECK-NEXT:    leaq -520(%rsp), %r11
; CHECK-NEXT:    callq rog_morestack_abi
; CHECK-NEXT:    testq %rax, %rax
; CHECK-NEXT:    jne .LBB0_[[COLD]]
; CHECK-NEXT:  .LBB0_[[FAST]]: # %fast
; CHECK-NEXT:    movl $7, %eax
; CHECK-NEXT:    retq
entry:
  %a = alloca %array, align 8
  %is_zero = icmp eq i64 %x, 0
  br i1 %is_zero, label %fast, label %cold

cold:
  %slot = getelementptr inbounds %array, ptr %a, i64 0, i64 0
  store i64 %x, ptr %slot, align 8
  call rogcc void @use(ptr nonnull %a)
  %v = load i64, ptr %slot, align 8
  ret i64 %v

fast:
  ret i64 7
}

declare rogcc void @use(ptr)
