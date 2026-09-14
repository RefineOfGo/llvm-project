; RUN: llc -O0 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -O0 -stop-after=finalize-isel < %s | FileCheck %s --check-prefix=MIR

target triple = "x86_64-unknown-linux-gnu"

declare rogcc void @callee(i64, i64, i64, i64, i64, i64, i64, i64)
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)

; The eight integer arguments occupy every rogcc argument register at the
; statepoint. The fast register allocator gives each virtual-register operand
; of the STATEPOINT its own physical register next to those pre-assigned ones,
; so ten roots live across the call cannot all be placed and -O0 used to fail
; with "ran out of registers during register allocation". At -O0 the deopt
; roots must be pre-spilled to statepoint stack slots instead.
define rogcc void @many_roots(ptr %base, ptr %out) #0 gc "statepoint-example" {
; CHECK-LABEL: many_roots:
; CHECK:         callq callee@PLT
; CHECK-NEXT:  .Ltmp{{[0-9]+}}:

; MIR-LABEL: name: many_roots
; MIR:         STATEPOINT {{.*}} 2, 10, 1, 8, %stack.{{[0-9]+}}, 0, 1, 8, %stack.{{[0-9]+}}, 0
; MIR-NOT:     STATEPOINT {{.*}} %{{[0-9]+}}:gr64, 0, 2, 0, 2, 0
entry:
  %s1 = getelementptr ptr, ptr %base, i64 1
  %s2 = getelementptr ptr, ptr %base, i64 2
  %s3 = getelementptr ptr, ptr %base, i64 3
  %s4 = getelementptr ptr, ptr %base, i64 4
  %s5 = getelementptr ptr, ptr %base, i64 5
  %s6 = getelementptr ptr, ptr %base, i64 6
  %s7 = getelementptr ptr, ptr %base, i64 7
  %s8 = getelementptr ptr, ptr %base, i64 8
  %s9 = getelementptr ptr, ptr %base, i64 9
  %p0 = load ptr, ptr %base
  %p1 = load ptr, ptr %s1
  %p2 = load ptr, ptr %s2
  %p3 = load ptr, ptr %s3
  %p4 = load ptr, ptr %s4
  %p5 = load ptr, ptr %s5
  %p6 = load ptr, ptr %s6
  %p7 = load ptr, ptr %s7
  %p8 = load ptr, ptr %s8
  %p9 = load ptr, ptr %s9
  call rogcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 1, i32 0, ptr elementtype(void (i64, i64, i64, i64, i64, i64, i64, i64)) @callee,
          i32 8, i32 0, i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8, i32 0, i32 0)
      [ "deopt"(ptr %p0, ptr %p1, ptr %p2, ptr %p3, ptr %p4,
                ptr %p5, ptr %p6, ptr %p7, ptr %p8, ptr %p9) ]
  %o1 = getelementptr ptr, ptr %out, i64 1
  %o2 = getelementptr ptr, ptr %out, i64 2
  %o3 = getelementptr ptr, ptr %out, i64 3
  %o4 = getelementptr ptr, ptr %out, i64 4
  %o5 = getelementptr ptr, ptr %out, i64 5
  %o6 = getelementptr ptr, ptr %out, i64 6
  %o7 = getelementptr ptr, ptr %out, i64 7
  %o8 = getelementptr ptr, ptr %out, i64 8
  %o9 = getelementptr ptr, ptr %out, i64 9
  store ptr %p0, ptr %out
  store ptr %p1, ptr %o1
  store ptr %p2, ptr %o2
  store ptr %p3, ptr %o3
  store ptr %p4, ptr %o4
  store ptr %p5, ptr %o5
  store ptr %p6, ptr %o6
  store ptr %p7, ptr %o7
  store ptr %p8, ptr %o8
  store ptr %p9, ptr %o9
  ret void
}

attributes #0 = { "frame-pointer"="all" }
