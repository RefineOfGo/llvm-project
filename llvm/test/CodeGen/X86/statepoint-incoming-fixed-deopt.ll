; RUN: llc -O3 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -O3 -stop-after=finalize-isel < %s | FileCheck %s --check-prefix=MIR

target triple = "x86_64-unknown-linux-gnu"

declare rogcc void @bar()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)

; The eight scalar arguments consume ROG's GPR argument registers, leaving
; every slice field in an incoming fixed stack slot.  The data pointer is a
; deopt root at the statepoint.  Record its original incoming slot directly;
; do not load it into a callee-saved register across the call merely to report
; its location.
define rogcc ptr @incoming_fixed_pointer(
    ptr %a0, ptr %a1, ptr %a2, ptr %a3,
    ptr %a4, ptr %a5, ptr %a6, ptr %a7,
    { ptr, i64, i64 } %slice) gc "statepoint-example" {
; CHECK-LABEL: incoming_fixed_pointer:
; CHECK-NOT:     pushq %rbx
; CHECK:         callq bar@PLT
; CHECK-NEXT:  .Ltmp{{[0-9]+}}:
; CHECK-NEXT:    xorl %eax, %eax

; MIR-LABEL: name: incoming_fixed_pointer
; MIR-NOT:     MOV64rm %fixed-stack
; MIR:         STATEPOINT {{.*}} 2, 1, 1, 8, %fixed-stack.{{[0-9]+}}, 0
entry:
  %data = extractvalue { ptr, i64, i64 } %slice, 0
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 1, i32 0, ptr elementtype(void ()) @bar,
          i32 0, i32 0, i32 0, i32 0)
      [ "deopt"(ptr %data) ]
  ret ptr null
}

; Cross-block deopt uses.  A leaf extracted in a NON-entry block reaches
; statepoint lowering as a cross-block value: getValue() would export the
; scalar through a vreg, hoisting the slot load above the first safepoint and
; pinning a callee-saved register across both calls purely to report the
; root's location.  The (argument, leaf) -> fixed-slot map must resolve it
; from any block, and the dead export must leave no load before the calls.
define rogcc ptr @incoming_fixed_pointer_cross_block(
    ptr %a0, ptr %a1, ptr %a2, ptr %a3,
    ptr %a4, ptr %a5, ptr %a6, ptr %a7,
    { ptr, i64, i64 } %slice) gc "statepoint-example" {
; CHECK-LABEL: incoming_fixed_pointer_cross_block:
; CHECK-NOT:     pushq %rbx
; CHECK-NOT:     pushq %r{{1[2-5]}}
; CHECK-NOT:     movq {{[0-9]+}}(%rsp)
; CHECK:         callq bar@PLT
; CHECK-NOT:     movq {{[0-9]+}}(%rsp)
; CHECK:         callq bar@PLT

; MIR-LABEL: name: incoming_fixed_pointer_cross_block
; MIR:         STATEPOINT {{.*}} 2, 1, 1, 8, %fixed-stack.[[SLOT:[0-9]+]], 0
; MIR:         STATEPOINT {{.*}} 2, 1, 1, 8, %fixed-stack.[[SLOT]], 0
entry:
  %data0 = extractvalue { ptr, i64, i64 } %slice, 0
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 2, i32 0, ptr elementtype(void ()) @bar,
          i32 0, i32 0, i32 0, i32 0)
      [ "deopt"(ptr %data0) ]
  br label %next

next:
  %data1 = extractvalue { ptr, i64, i64 } %slice, 0
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 3, i32 0, ptr elementtype(void ()) @bar,
          i32 0, i32 0, i32 0, i32 0)
      [ "deopt"(ptr %data1) ]
  ret ptr null
}

; The Go-repro shape: the argument's real use comes AFTER the safepoints
; (`return p[0]` after runtime.GC()).  The cross-block real use exports the
; scalar through a vreg pinned across the calls -- that is pre-existing
; baseline codegen for a stack argument used after a call (an empty-deopt
; control produces the identical sequence: LowerArguments emits the export
; load in entry, and MachineSinking skips single-successor blocks).  The
; deopt records must add no cost on top: exactly one slot load, no extra
; reload or spill, and both safepoints describe the fixed slot itself.
define rogcc ptr @incoming_fixed_pointer_real_use_after(
    ptr %a0, ptr %a1, ptr %a2, ptr %a3,
    ptr %a4, ptr %a5, ptr %a6, ptr %a7,
    { ptr, i64, i64 } %slice) gc "statepoint-example" {
; CHECK-LABEL: incoming_fixed_pointer_real_use_after:
; CHECK:         movq {{[0-9]+}}(%rsp), %[[R:r[a-z0-9]+]]
; CHECK-NEXT:    callq bar@PLT
; CHECK-NOT:     movq
; CHECK:         callq bar@PLT
; CHECK-NOT:     {{movq.*\(%rsp\)}}
; CHECK:         movq %[[R]], %rax

; MIR-LABEL: name: incoming_fixed_pointer_real_use_after
; MIR:         STATEPOINT {{.*}} 2, 1, 1, 8, %fixed-stack.[[SLOT2:[0-9]+]], 0
; MIR:         STATEPOINT {{.*}} 2, 1, 1, 8, %fixed-stack.[[SLOT2]], 0
entry:
  %data0 = extractvalue { ptr, i64, i64 } %slice, 0
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 4, i32 0, ptr elementtype(void ()) @bar,
          i32 0, i32 0, i32 0, i32 0)
      [ "deopt"(ptr %data0) ]
  br label %next

next:
  %data1 = extractvalue { ptr, i64, i64 } %slice, 0
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 5, i32 0, ptr elementtype(void ()) @bar,
          i32 0, i32 0, i32 0, i32 0)
      [ "deopt"(ptr %data1) ]
  br label %out

out:
  %data2 = extractvalue { ptr, i64, i64 } %slice, 0
  ret ptr %data2
}
