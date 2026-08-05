; RUN: llc -O3 -verify-machineinstrs < %s | FileCheck %s

; A "rog-stack-check" function calls rog_morestack_abi BEFORE it establishes
; RBP, so the GC's frame-pointer walk drops this function's caller at that
; call. adjustForROGPrologue emits a prologue-entry stackmap record (ID bit
; 62 -> compact-record flag bit 30, value 0x40000000) at the call's return
; point; its only location is a Direct describing the incoming
; stack-argument area [entry_rsp + 8, entry_rsp + 8 + args), and the set is
; empty when the function takes no stack arguments.

target triple = "x86_64-unknown-linux-gnu"

declare rogcc void @bar()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)

; Eight pointer arguments fill ROG's GPR argument registers; %s0 and %s1
; land in incoming fixed stack slots (16 bytes of stack arguments).
define rogcc ptr @grow_with_stack_args(
    ptr %a0, ptr %a1, ptr %a2, ptr %a3,
    ptr %a4, ptr %a5, ptr %a6, ptr %a7,
    ptr %s0, ptr %s1) #0 gc "statepoint-example" {
; CHECK-LABEL: grow_with_stack_args:
; CHECK:         callq rog_morestack_abi
; CHECK-NEXT:  .Ltmp[[PE:[0-9]+]]:
entry:
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 1, i32 0, ptr elementtype(void ()) @bar,
          i32 0, i32 0, i32 0, i32 0)
      [ "deopt"(ptr %s0) ]
  ret ptr %s1
}

; Register-only arguments still get a prologue-entry record -- with an empty
; set. Its presence is what tells the runtime "no stack arguments" without
; trusting toolchain provenance.
define rogcc ptr @grow_reg_only(ptr %a0, ptr %a1) #0 gc "statepoint-example" {
; CHECK-LABEL: grow_reg_only:
; CHECK:         callq rog_morestack_abi
; CHECK-NEXT:  .Ltmp[[PE2:[0-9]+]]:
entry:
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 2, i32 0, ptr elementtype(void ()) @bar,
          i32 0, i32 0, i32 0, i32 0)
      [ "deopt"(ptr %a0) ]
  ret ptr %a1
}

; The per-function compact blobs follow the function bodies. Version 0x53;
; the Direct arg-area slot is (kind 2, reserved 0, DWARF reg 7 = RSP,
; offset 8, size 16); the record at the morestack return point carries
; set_idx | 1 << 30 (1073741825 = 0x40000001).
; CHECK:         .section .llvm_stackmaps,"ao",@progbits,grow_with_stack_args
; CHECK:         .byte 83
; CHECK:         .byte 2
; CHECK-NEXT:    .byte 0
; CHECK-NEXT:    .short 7
; CHECK-NEXT:    .long 8
; CHECK-NEXT:    .long 16
; CHECK:         .long .Ltmp[[PE]]-grow_with_stack_args
; CHECK-NEXT:    .long 1073741825

; CHECK:         .section .llvm_stackmaps,"ao",@progbits,grow_reg_only
; CHECK:         .byte 83
; CHECK:         .long .Ltmp[[PE2]]-grow_reg_only
; CHECK-NEXT:    .long 1073741825
; CHECK-NOT:     .llvm_stackmaps,"ao",@progbits,grow_no_gc

; A stack-check function WITHOUT a GC strategy (the Rust runtime/std built
; by ROG's rustc) must get the check but NO stackmap blob: the GC never
; consumes records for such frames, and the blob's absolute function-address
; relocation breaks PIC dylib links (rust-lld rejects R_X86_64_64 against
; preemptible symbols when building libstd).
define rogcc ptr @grow_no_gc(ptr %a0, ptr %a1) #0 {
; The stackmap section check lives at the end of the file (after the last
; positive .llvm_stackmaps match): no blob may exist for this function.
entry:
  %mem = alloca [64 x i8]
  call void asm sideeffect "", "r"(ptr %mem)
  ret ptr null
}

attributes #0 = { "rog-stack-check" "frame-pointer"="all" }
