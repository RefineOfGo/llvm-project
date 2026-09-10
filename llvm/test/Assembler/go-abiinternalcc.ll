; RUN: llvm-as < %s | llvm-dis | FileCheck %s
; RUN: verify-uselistorder %s

; The go_abiinternalcc calling convention must parse, print, and round-trip on
; declarations and call sites.

; CHECK: declare go_abiinternalcc i64 @abi_internal_scalar(i64, i64)
declare go_abiinternalcc i64 @abi_internal_scalar(i64, i64)

; CHECK: declare go_abiinternalcc void @abi_internal_void(ptr, i64)
declare go_abiinternalcc void @abi_internal_void(ptr, i64)

; CHECK: declare go_abiinternalcc double @abi_internal_float(double, i64, ptr)
declare go_abiinternalcc double @abi_internal_float(double, i64, ptr)

%ret3 = type { i64, i64, i64 }
; CHECK: declare go_abiinternalcc %ret3 @abi_internal_multiret(i64, i64)
declare go_abiinternalcc %ret3 @abi_internal_multiret(i64, i64)

; CHECK: declare go_abiinternalcc void @abi_internal_overflow(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64)
declare go_abiinternalcc void @abi_internal_overflow(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64)

define i64 @call_scalar(i64 %a, i64 %b) {
; CHECK-LABEL: define i64 @call_scalar(i64 %a, i64 %b)
; CHECK: call go_abiinternalcc i64 @abi_internal_scalar(i64 %a, i64 %b)
  %r = call go_abiinternalcc i64 @abi_internal_scalar(i64 %a, i64 %b)
  ret i64 %r
}

define void @call_void(ptr %p, i64 %n) {
; CHECK-LABEL: define void @call_void(ptr %p, i64 %n)
; CHECK: call go_abiinternalcc void @abi_internal_void(ptr %p, i64 %n)
  call go_abiinternalcc void @abi_internal_void(ptr %p, i64 %n)
  ret void
}

define double @call_float(double %a, i64 %b, ptr %c) {
; CHECK-LABEL: define double @call_float(double %a, i64 %b, ptr %c)
; CHECK: call go_abiinternalcc double @abi_internal_float(double %a, i64 %b, ptr %c)
  %r = call go_abiinternalcc double @abi_internal_float(double %a, i64 %b, ptr %c)
  ret double %r
}

define i64 @call_multiret(i64 %a, i64 %b) {
; CHECK-LABEL: define i64 @call_multiret(i64 %a, i64 %b)
; CHECK: call go_abiinternalcc %ret3 @abi_internal_multiret(i64 %a, i64 %b)
  %r = call go_abiinternalcc %ret3 @abi_internal_multiret(i64 %a, i64 %b)
  %x = extractvalue %ret3 %r, 0
  ret i64 %x
}

define void @call_overflow() {
; CHECK-LABEL: define void @call_overflow()
; CHECK: call go_abiinternalcc void @abi_internal_overflow(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8, i64 9, i64 10)
  call go_abiinternalcc void @abi_internal_overflow(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8, i64 9, i64 10)
  ret void
}
