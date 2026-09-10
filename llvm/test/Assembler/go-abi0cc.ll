; RUN: llvm-as < %s | llvm-dis | FileCheck %s
; RUN: verify-uselistorder %s

; The go_abi0cc calling convention must parse, print, and round-trip on both
; declarations and call sites, for scalar, void, sret-aggregate and mixed
; argument types.

; CHECK: declare go_abi0cc i64 @abi0_scalar(i64)
declare go_abi0cc i64 @abi0_scalar(i64)

; CHECK: declare go_abi0cc void @abi0_void(i64, i32)
declare go_abi0cc void @abi0_void(i64, i32)

%cpuid_ret = type { i32, i32, i32, i32 }
; CHECK: declare go_abi0cc %cpuid_ret @abi0_aggregate(i32, i32)
declare go_abi0cc %cpuid_ret @abi0_aggregate(i32, i32)

; CHECK: declare go_abi0cc i64 @abi0_mixed(i64, i32, double, ptr)
declare go_abi0cc i64 @abi0_mixed(i64, i32, double, ptr)

define i64 @call_scalar(i64 %x) {
; CHECK-LABEL: define i64 @call_scalar(i64 %x)
; CHECK: call go_abi0cc i64 @abi0_scalar(i64 %x)
  %r = call go_abi0cc i64 @abi0_scalar(i64 %x)
  ret i64 %r
}

define void @call_void(i64 %a, i32 %b) {
; CHECK-LABEL: define void @call_void(i64 %a, i32 %b)
; CHECK: call go_abi0cc void @abi0_void(i64 %a, i32 %b)
  call go_abi0cc void @abi0_void(i64 %a, i32 %b)
  ret void
}

define void @call_aggregate(ptr %dst) {
; CHECK-LABEL: define void @call_aggregate(ptr %dst)
; CHECK: call go_abi0cc %cpuid_ret @abi0_aggregate(i32 0, i32 0)
  %r = call go_abi0cc %cpuid_ret @abi0_aggregate(i32 0, i32 0)
  store %cpuid_ret %r, ptr %dst, align 4
  ret void
}

define i64 @call_mixed(i64 %a, i32 %b, double %c, ptr %d) {
; CHECK-LABEL: define i64 @call_mixed(i64 %a, i32 %b, double %c, ptr %d)
; CHECK: call go_abi0cc i64 @abi0_mixed(i64 %a, i32 %b, double %c, ptr %d)
  %r = call go_abi0cc i64 @abi0_mixed(i64 %a, i32 %b, double %c, ptr %d)
  ret i64 %r
}
