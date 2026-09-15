; ODR copies may have different CFGs after pre-instrumentation inlining. Split
; their profile storage by hash without renaming the function or weakening its
; linkage: another module may be the only remaining caller.
;
; RUN: opt < %s -passes=instrprof -vp-static-alloc=true -S | FileCheck %s -DHASH=123
; RUN: sed 's/i64 123/i64 456/g' %s | opt -passes=instrprof -vp-static-alloc=true -S | FileCheck %s -DHASH=456
; RUN: opt < %s -passes=instrprof -hash-based-counter-split=false -S | FileCheck %s --check-prefix=DISABLED
; RUN: sed '/@__llvm_profile_raw_version =/d' %s | opt -passes=instrprof -S | FileCheck %s --check-prefix=DISABLED

target triple = "x86_64-unknown-linux-gnu"

$odr = comdat any
$weak = comdat any

@__llvm_profile_raw_version = constant i64 72057594037927936
@__profn_odr = weak_odr hidden constant [3 x i8] c"odr"
@__profn_plain = weak_odr hidden constant [5 x i8] c"plain"
@__profn_weak = weak hidden constant [4 x i8] c"weak"
@odr_address = global ptr @odr

; CHECK-DAG: $__profc_odr.[[HASH]] = comdat any
; CHECK-DAG: $__profbm_odr.[[HASH]] = comdat any
; CHECK-DAG: @odr_address = global ptr @odr
; CHECK-DAG: @__profc_odr.[[HASH]] = weak_odr hidden global [2 x i64] zeroinitializer, section "__llvm_prf_cnts", comdat, align 8
; CHECK-DAG: @__profbm_odr.[[HASH]] = weak_odr hidden global [1 x i8] zeroinitializer, section "__llvm_prf_bits", comdat, align 1
; CHECK-DAG: @__profvp_odr.[[HASH]] = weak_odr hidden global [1 x i64] zeroinitializer, section "__llvm_prf_vals", comdat($__profc_odr.[[HASH]]), align 8
; The first field stays the hash of the original profile name "odr".
; CHECK-DAG: @__profd_odr.[[HASH]] = weak_odr hidden global {{.*}}i64 -7019667885480636396, i64 [[HASH]],{{.*}}ptr @__profc_odr.[[HASH]]{{.*}}ptr @__profbm_odr.[[HASH]]{{.*}}ptr @__profvp_odr.[[HASH]]{{.*}}, section "__llvm_prf_data", comdat($__profc_odr.[[HASH]]), align 8
; CHECK-DAG: @__profc_plain = weak_odr hidden global
; CHECK-DAG: @__profc_weak = weak hidden global
; CHECK-DAG: @__profd_plain = {{.*}}, comdat($__profc_plain), align 8
; CHECK-DAG: @__profd_weak = {{.*}}, comdat($__profc_weak), align 8
; CHECK-LABEL: define weak_odr void @odr(ptr %target) comdat {
; CHECK: call void @__llvm_profile_instrument_target(i64 {{.*}}, ptr @__profd_odr.[[HASH]], i32 0)
; CHECK-LABEL: define weak_odr void @plain()
; CHECK-LABEL: define weak void @weak() comdat {
;
; DISABLED-DAG: @__profc_odr = weak_odr hidden global
; DISABLED-DAG: @__profbm_odr = weak_odr hidden global
; DISABLED-DAG: @__profvp_odr = weak_odr hidden global
; DISABLED-DAG: @__profd_odr = weak_odr hidden global
; DISABLED-LABEL: define weak_odr void @odr(ptr %target) comdat {

define weak_odr void @odr(ptr %target) comdat {
  %bitmap = alloca i32, align 4
  store i32 0, ptr %bitmap, align 4
  call void @llvm.instrprof.increment(ptr @__profn_odr, i64 123, i32 2, i32 0)
  call void @llvm.instrprof.mcdc.parameters(ptr @__profn_odr, i64 123, i32 1)
  call void @llvm.instrprof.mcdc.tvbitmap.update(ptr @__profn_odr, i64 123, i32 0, ptr %bitmap)
  %value = ptrtoint ptr %target to i64
  call void @llvm.instrprof.value.profile(ptr @__profn_odr, i64 123, i64 %value, i32 0, i32 0)
  call void %target()
  ret void
}

define weak_odr void @plain() {
  call void @llvm.instrprof.increment(ptr @__profn_plain, i64 123, i32 1, i32 0)
  ret void
}

define weak void @weak() comdat {
  call void @llvm.instrprof.increment(ptr @__profn_weak, i64 123, i32 1, i32 0)
  ret void
}

declare void @llvm.instrprof.increment(ptr, i64, i32, i32)
declare void @llvm.instrprof.mcdc.parameters(ptr, i64, i32)
declare void @llvm.instrprof.mcdc.tvbitmap.update(ptr, i64, i32, ptr)
declare void @llvm.instrprof.value.profile(ptr, i64, i64, i32, i32)
