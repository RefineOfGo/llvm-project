; RUN: llc -mtriple=x86_64-unknown-linux-gnu -O3 %s -o - | FileCheck %s
; RUN: llc -mtriple=x86_64-unknown-linux-gnu -O3 -filetype=obj %s -o %t
; RUN: llvm-dwarfdump --debug-info %t | FileCheck %s --check-prefix=FRESH
; RUN: opt -passes=rewrite-statepoints-for-gc %s | llc -mtriple=x86_64-unknown-linux-gnu -O3 -o - | FileCheck %s --check-prefix=SP
;
; The invalidation label survives optimization without being copied by
; LiveDebugValues. Availability is reconstructed after machine stores settle.
target triple = "x86_64-unknown-linux-gnu"

; CHECK-LABEL: partial:
; CHECK: #DEBUG_VALUE: partial:range.state <- 0
; CHECK: movl $21,
; CHECK-NOT: range.state <- 1
; CHECK: callq checkpoint
; CHECK: movl $31,
; CHECK: #DEBUG_VALUE: partial:range.state <- 1
; CHECK: callq consume
define rogcc void @partial() !dbg !4 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !7, !DIExpression(DW_OP_stack_value), !11)
    #dbg_value(i8 0, !9, !DIExpression(), !11)
    #dbg_label(!12, !11)
  store i32 21, ptr %p, align 8, !dbg !13
  call void @checkpoint(), !dbg !14
  %high = getelementptr i8, ptr %p, i64 4
  store i32 31, ptr %high, align 4, !dbg !15
  call void @consume(ptr %p), !dbg !16
  ret void
}

; One initialized predecessor cannot make the other path's bytes readable.
; CHECK-LABEL: one_path:
; CHECK: #DEBUG_VALUE: one_path:range.state <- 0
; CHECK: movq $21,
; CHECK: #DEBUG_VALUE: one_path:range.state <- 1
; CHECK: # %merge
; CHECK: #DEBUG_VALUE: one_path:range.state <- 0
; CHECK: callq consume
define rogcc void @one_path(i1 %condition) !dbg !17 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !18, !DIExpression(DW_OP_stack_value), !20)
    #dbg_value(i8 0, !19, !DIExpression(), !20)
    #dbg_label(!21, !20)
  br i1 %condition, label %write, label %merge
write:
  store i64 21, ptr %p, align 8, !dbg !22
  br label %merge
merge:
  call void @consume(ptr %p), !dbg !23
  ret void
}

; Both paths initialize the same bytes, so the common successor is readable.
; CHECK-LABEL: both_paths:
; CHECK: #DEBUG_VALUE: both_paths:range.state <- 0
; CHECK: movq $21,
; CHECK: #DEBUG_VALUE: both_paths:range.state <- 1
; CHECK: movq $31,
; CHECK: #DEBUG_VALUE: both_paths:range.state <- 1
; CHECK: callq consume
define rogcc void @both_paths(i1 %condition) !dbg !24 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !25, !DIExpression(DW_OP_stack_value), !27)
    #dbg_value(i8 0, !26, !DIExpression(), !27)
    #dbg_label(!28, !27)
  br i1 %condition, label %left, label %right
left:
  store i64 21, ptr %p, align 8, !dbg !29
  call void @checkpoint(), !dbg !29
  br label %merge
right:
  store i64 31, ptr %p, align 8, !dbg !30
  call void @checkpoint(), !dbg !30
  br label %merge
merge:
  call void @consume(ptr %p), !dbg !31
  ret void
}

; Inactive descriptors must not restrict another object reusing the stack slot,
; and must not act as proof that a caller's unavailable bytes were initialized.
; CHECK-LABEL: lifetime:
; CHECK: #DEBUG_VALUE: lifetime:range.state <- 2
; CHECK: callq checkpoint
; CHECK: #DEBUG_VALUE: lifetime:range.state <- 0
; CHECK: callq checkpoint
; CHECK: movq $21,
; CHECK: #DEBUG_VALUE: lifetime:range.state <- 1
; CHECK: callq consume
; CHECK: #DEBUG_VALUE: lifetime:range.state <- 2
; CHECK: callq checkpoint
define rogcc void @lifetime() !dbg !36 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !37, !DIExpression(DW_OP_stack_value), !39)
    #dbg_value(i8 0, !38, !DIExpression(), !39)
  call void @checkpoint(), !dbg !42
    #dbg_label(!40, !39)
  call void @checkpoint(), !dbg !42
  store i64 21, ptr %p, align 8, !dbg !42
  call void @consume(ptr %p), !dbg !42
    #dbg_label(!41, !39)
  call void @checkpoint(), !dbg !42
  ret void
}

; A written path merged with an inactive path cannot assert a write on both.
; CHECK-LABEL: inactive_join:
; CHECK: #DEBUG_VALUE: inactive_join:range.state <- 2
; CHECK: #DEBUG_VALUE: inactive_join:range.state <- 0
; CHECK: movq $21,
; CHECK: #DEBUG_VALUE: inactive_join:range.state <- 1
; CHECK: callq checkpoint
; CHECK: # %merge
; CHECK: #DEBUG_VALUE: inactive_join:range.state <- 0
; CHECK: callq consume
define rogcc void @inactive_join(i1 %condition) !dbg !75 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !76, !DIExpression(DW_OP_stack_value), !78)
    #dbg_value(i8 0, !77, !DIExpression(), !78)
  br i1 %condition, label %write, label %merge
write:
    #dbg_label(!79, !78)
  store i64 21, ptr %p, align 8
  call void @checkpoint(), !dbg !78
  br label %merge
merge:
  call void @consume(ptr %p), !dbg !78
  ret void
}

; Full unmasked vector stores initialize each covered scalar descriptor.
; CHECK-LABEL: vector_store:
; CHECK: #DEBUG_VALUE: vector_store:range.state <- 0
; CHECK: movaps
; CHECK: #DEBUG_VALUE: vector_store:range.state <- 1
; CHECK: callq consume
define rogcc void @vector_store(<2 x i64> noundef %value) !dbg !70 {
entry:
  %p = alloca <2 x i64>, align 16
    #dbg_declare(ptr %p, !71, !DIExpression(DW_OP_stack_value), !73)
    #dbg_value(i8 0, !72, !DIExpression(), !73)
    #dbg_label(!74, !73)
  store <2 x i64> %value, ptr %p, align 16
  call void @consume(ptr %p), !dbg !73
  ret void
}

; Possible writes are not proof that every described byte was initialized.
; CHECK-LABEL: atomic_store:
; CHECK: #DEBUG_VALUE: atomic_store:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
; CHECK-NOT: range.state <- 1
; CHECK: .Lfunc_end
define rogcc void @atomic_store() !dbg !50 {
entry:
  %p = alloca <2 x i64>, align 16
    #dbg_declare(ptr %p, !51, !DIExpression(DW_OP_stack_value), !53)
    #dbg_value(i8 0, !52, !DIExpression(), !53)
    #dbg_label(!54, !53)
  store atomic i64 21, ptr %p release, align 8
  call void @consume(ptr %p), !dbg !53
  ret void
}

; Possible writes are not proof that every described byte was initialized.
; CHECK-LABEL: conditional_store:
; CHECK: #DEBUG_VALUE: conditional_store:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
; CHECK-NOT: range.state <- 1
; CHECK: .Lfunc_end
define rogcc void @conditional_store() !dbg !55 {
entry:
  %p = alloca <2 x i64>, align 16
    #dbg_declare(ptr %p, !56, !DIExpression(DW_OP_stack_value), !58)
    #dbg_value(i8 0, !57, !DIExpression(), !58)
    #dbg_label(!59, !58)
  %old = cmpxchg ptr %p, i64 0, i64 21 monotonic monotonic, align 8
  call void @consume(ptr %p), !dbg !58
  ret void
}

; Possible writes are not proof that every described byte was initialized.
; CHECK-LABEL: masked_store:
; CHECK: #DEBUG_VALUE: masked_store:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
; CHECK-NOT: range.state <- 1
; CHECK: .Lfunc_end
define rogcc void @masked_store(<2 x i1> %mask) "target-features"="+avx512f,+avx512vl" !dbg !60 {
entry:
  %p = alloca <2 x i64>, align 16
    #dbg_declare(ptr %p, !61, !DIExpression(DW_OP_stack_value), !63)
    #dbg_value(i8 0, !62, !DIExpression(), !63)
    #dbg_label(!64, !63)
  call void @llvm.masked.store.v2i64.p0(<2 x i64> <i64 21, i64 31>, ptr %p, i32 8, <2 x i1> %mask)
  call void @consume(ptr %p), !dbg !63
  ret void
}

; Possible writes are not proof that every described byte was initialized.
; CHECK-LABEL: unknown_call:
; CHECK: #DEBUG_VALUE: unknown_call:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
; CHECK-NOT: range.state <- 1
; CHECK: .Lfunc_end
define rogcc void @unknown_call() !dbg !65 {
entry:
  %p = alloca <2 x i64>, align 16
    #dbg_declare(ptr %p, !66, !DIExpression(DW_OP_stack_value), !68)
    #dbg_value(i8 0, !67, !DIExpression(), !68)
    #dbg_label(!69, !68)
  call void @mutate(ptr %p)
  call void @consume(ptr %p), !dbg !68
  ret void
}

; Argument availability is positive-only: neither the attribute nor the
; entry descriptor is proof of a write before the real machine store.
; CHECK-LABEL: argument_store:
; CHECK: #DEBUG_VALUE: argument_store:available.state <- 0
; CHECK: callq checkpoint
; CHECK-NOT: available.state <- 1
; CHECK: movq $25,
; CHECK: #DEBUG_VALUE: argument_store:available.state <- 1
; CHECK: callq consume
define rogcc void @argument_store(ptr initializes((0, 8)) %p) !dbg !80 {
entry:
    #dbg_value(ptr %p, !81, !DIExpression(), !83)
  call void @checkpoint(), !dbg !83
  store i64 25, ptr %p, align 8
  call void @consume(ptr %p), !dbg !83
  ret void
}

; RS4GC conservatively drops ordinary call memory effects. Its independent
; check covers source-statepoint return proof without assuming those effects.
; SP-LABEL: caller_initialize:
; SP: #DEBUG_VALUE: caller_initialize:range.state <- 0
; SP: callq initialize
; SP: #DEBUG_VALUE: caller_initialize:range.state <- 1
; SP-LABEL: ambiguous_calls:
; SP-NOT: range.state <- 1
; SP: callq consume
; SP-LABEL: patchable_call:
; SP-NOT: range.state <- 1
; SP: callq consume
;
; A real source call to an initializing callee restores the caller's bytes
; only after that exact machine call has completed.
; FRESH-LABEL: DW_AT_name ("caller_initialize")
; FRESH: DW_AT_name ("ROGMemoryFreshLocal")
; FRESH-NEXT: DW_AT_const_value ("1")
; CHECK-LABEL: caller_initialize:
; CHECK: #DEBUG_VALUE: caller_initialize:range.state <- 0
; CHECK: callq initialize
; CHECK: #DEBUG_VALUE: caller_initialize:range.state <- 1
; CHECK: callq consume
define rogcc void @caller_initialize() gc "statepoint-example" !dbg !90 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !91, !DIExpression(DW_OP_stack_value), !93)
    #dbg_label(!94, !93)
  call rogcc void @initialize(ptr %p), !dbg !93
  call void @consume(ptr %p), !dbg !93
  ret void
}

; Same-callee calls with different actual objects are ambiguous at MI level.
; Neither call may be used as proof for this descriptor.
; CHECK-LABEL: ambiguous_calls:
; CHECK: #DEBUG_VALUE: ambiguous_calls:range.state <- 0
; CHECK: callq initialize
; CHECK-NOT: range.state <- 1
; CHECK: callq initialize
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @ambiguous_calls() gc "statepoint-example" !dbg !100 {
entry:
  %p = alloca i64, align 8
  %q = alloca i64, align 8
    #dbg_declare(ptr %p, !101, !DIExpression(DW_OP_stack_value), !103)
    #dbg_label(!104, !103)
  call rogcc void @initialize(ptr %p), !dbg !103
  call rogcc void @initialize(ptr %q), !dbg !103
  call void @consume(ptr %p), !dbg !103
  ret void
}

define rogcc void @initialize(ptr captures(none) initializes((0, 8)) %p) !dbg !95 {
entry:
  store i64 25, ptr %p, align 8
  ret void
}

; A patchable statepoint emits NOPs rather than executing its symbolic callee.
; CHECK-LABEL: patchable_call:
; CHECK: #DEBUG_VALUE: patchable_call:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @patchable_call() gc "statepoint-example" !dbg !110 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !111, !DIExpression(DW_OP_stack_value), !113)
    #dbg_label(!114, !113)
  %token = call rogcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 101, i32 5, ptr elementtype(void (ptr)) @initialize, i32 1, i32 0, ptr %p, i32 0, i32 0), !dbg !113
  call void @consume(ptr %p), !dbg !113
  ret void
}

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)
; initializes is not a substitute for a defined-memory postcondition.
; CHECK-LABEL: proof_empty:
; CHECK: #DEBUG_VALUE: proof_empty:range.state <- 0
; CHECK: callq initialize_empty
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @proof_empty() !dbg !200 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !201, !DIExpression(DW_OP_stack_value), !203)
    #dbg_label(!204, !203)
  call rogcc void @initialize_empty(ptr %p), !dbg !203
  call void @consume(ptr %p), !dbg !203
  ret void
}

define rogcc void @initialize_empty(ptr captures(none) initializes((0, 8)) %p) !dbg !205 {
entry:
  ret void
}

; initializes is not a substitute for a defined-memory postcondition.
; CHECK-LABEL: proof_undef:
; CHECK: #DEBUG_VALUE: proof_undef:range.state <- 0
; CHECK: callq initialize_undef
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @proof_undef() !dbg !210 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !211, !DIExpression(DW_OP_stack_value), !213)
    #dbg_label(!214, !213)
  call rogcc void @initialize_undef(ptr %p), !dbg !213
  call void @consume(ptr %p), !dbg !213
  ret void
}

define rogcc void @initialize_undef(ptr captures(none) initializes((0, 8)) %p) !dbg !215 {
entry:
  store i64 undef, ptr %p
  ret void
}

; initializes is not a substitute for a defined-memory postcondition.
; CHECK-LABEL: proof_poison:
; CHECK: #DEBUG_VALUE: proof_poison:range.state <- 0
; CHECK: callq initialize_poison
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @proof_poison() !dbg !220 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !221, !DIExpression(DW_OP_stack_value), !223)
    #dbg_label(!224, !223)
  call rogcc void @initialize_poison(ptr %p), !dbg !223
  call void @consume(ptr %p), !dbg !223
  ret void
}

define rogcc void @initialize_poison(ptr captures(none) initializes((0, 8)) %p) !dbg !225 {
entry:
  store i64 poison, ptr %p
  ret void
}

; initializes is not a substitute for a defined-memory postcondition.
; CHECK-LABEL: proof_dead:
; CHECK: #DEBUG_VALUE: proof_dead:range.state <- 0
; CHECK: callq initialize_dead
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @proof_dead() !dbg !230 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !231, !DIExpression(DW_OP_stack_value), !233)
    #dbg_label(!234, !233)
  call rogcc void @initialize_dead(ptr %p), !dbg !233
  call void @consume(ptr %p), !dbg !233
  ret void
}

define rogcc void @initialize_dead(ptr captures(none) dead_on_return initializes((0, 8)) %p) !dbg !235 {
entry:
  store i64 25, ptr %p
  ret void
}

; initializes is not a substitute for a defined-memory postcondition.
; CHECK-LABEL: proof_unknown_value:
; CHECK: #DEBUG_VALUE: proof_unknown_value:range.state <- 0
; CHECK: callq initialize_unknown_value
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @proof_unknown_value() !dbg !240 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !241, !DIExpression(DW_OP_stack_value), !243)
    #dbg_label(!244, !243)
  call rogcc void @initialize_unknown_value(ptr %p, i64 7), !dbg !243
  call void @consume(ptr %p), !dbg !243
  ret void
}

define rogcc void @initialize_unknown_value(ptr captures(none) initializes((0, 8)) %p, i64 %value) !dbg !245 {
entry:
  store i64 %value, ptr %p
  ret void
}

; initializes is not a substitute for a defined-memory postcondition.
; CHECK-LABEL: proof_one_path:
; CHECK: #DEBUG_VALUE: proof_one_path:range.state <- 0
; CHECK: callq initialize_one_path
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @proof_one_path() !dbg !250 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !251, !DIExpression(DW_OP_stack_value), !253)
    #dbg_label(!254, !253)
  call rogcc void @initialize_one_path(ptr %p, i1 false), !dbg !253
  call void @consume(ptr %p), !dbg !253
  ret void
}

define rogcc void @initialize_one_path(ptr captures(none) initializes((0, 8)) %p, i1 %condition) !dbg !255 {
entry:
  br i1 %condition, label %write, label %exit
write:
  store i64 25, ptr %p
  br label %exit
exit:
  ret void
}

; initializes is not a substitute for a defined-memory postcondition.
; CHECK-LABEL: proof_unknown_clobber:
; CHECK: #DEBUG_VALUE: proof_unknown_clobber:range.state <- 0
; CHECK: callq initialize_unknown_clobber
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @proof_unknown_clobber() !dbg !260 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !261, !DIExpression(DW_OP_stack_value), !263)
    #dbg_label(!264, !263)
  call rogcc void @initialize_unknown_clobber(ptr %p), !dbg !263
  call void @consume(ptr %p), !dbg !263
  ret void
}

define rogcc void @initialize_unknown_clobber(ptr captures(none) initializes((0, 8)) %p) !dbg !265 {
entry:
  store i64 25, ptr %p
  call void @mutate(ptr %p)
  ret void
}

; A complete MOV does not establish defined bytes from an unknown IR value.
; CHECK-LABEL: late_unknown:
; CHECK: #DEBUG_VALUE: late_unknown:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @late_unknown(i64 %value) !dbg !300 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !301, !DIExpression(DW_OP_stack_value), !303)
    #dbg_label(!304, !303)
  store i64 %value, ptr %p
  call void @consume(ptr %p), !dbg !303
  ret void
}

; A complete MOV does not establish defined bytes from an unknown IR value.
; CHECK-LABEL: late_undef:
; CHECK: #DEBUG_VALUE: late_undef:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @late_undef(i64 %value) !dbg !310 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !311, !DIExpression(DW_OP_stack_value), !313)
    #dbg_label(!314, !313)
  store i64 undef, ptr %p
  call void @consume(ptr %p), !dbg !313
  ret void
}

; A complete MOV does not establish defined bytes from an unknown IR value.
; CHECK-LABEL: late_poison:
; CHECK: #DEBUG_VALUE: late_poison:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @late_poison(i64 %value) !dbg !320 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !321, !DIExpression(DW_OP_stack_value), !323)
    #dbg_label(!324, !323)
  store i64 poison, ptr %p
  call void @consume(ptr %p), !dbg !323
  ret void
}

; A complete MOV does not establish defined bytes from an unknown IR value.
; CHECK-LABEL: late_overflow:
; CHECK: #DEBUG_VALUE: late_overflow:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @late_overflow(i64 %value) !dbg !330 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !331, !DIExpression(DW_OP_stack_value), !333)
    #dbg_label(!334, !333)
  %sum = add nsw i64 %value, 1
  store i64 %sum, ptr %p
  call void @consume(ptr %p), !dbg !333
  ret void
}

; CHECK-LABEL: positive_unknown:
; CHECK: #DEBUG_VALUE: positive_unknown:available.state <- 0
; CHECK-NOT: available.state <- 1
; CHECK: callq consume
define rogcc void @positive_unknown(ptr %p, i64 %value) !dbg !340 {
entry:
    #dbg_value(ptr %p, !341, !DIExpression(), !343)
  store i64 %value, ptr %p
  call void @consume(ptr %p), !dbg !343
  ret void
}

; No IR StoreInst maps this intrinsic's MMO, so no defined-store proof exists.
; CHECK-LABEL: late_memset:
; CHECK: #DEBUG_VALUE: late_memset:range.state <- 0
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @late_memset() !dbg !350 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !351, !DIExpression(DW_OP_stack_value), !353)
    #dbg_label(!354, !353)
  call void @llvm.memset.p0.i64(ptr %p, i8 25, i64 8, i1 false)
  call void @consume(ptr %p), !dbg !353
  ret void
}

; A possible write of unknown bytes invalidates a previous defined-write proof.
; CHECK-LABEL: late_masked_clobber:
; CHECK: movq $25,
; CHECK: #DEBUG_VALUE: late_masked_clobber:range.state <- 1
; CHECK: callq checkpoint
; CHECK: #DEBUG_VALUE: late_masked_clobber:range.state <- 0
; CHECK: callq consume
define rogcc void @late_masked_clobber(<2 x i64> %value, <2 x i1> %mask) "target-features"="+avx512f,+avx512vl" !dbg !360 {
entry:
  %p = alloca <2 x i64>, align 16
    #dbg_declare(ptr %p, !361, !DIExpression(DW_OP_stack_value), !363)
    #dbg_label(!364, !363)
  store i64 25, ptr %p
  call void @checkpoint(), !dbg !363
  call void @llvm.masked.store.v2i64.p0(<2 x i64> %value, ptr %p, i32 8, <2 x i1> %mask)
  call void @consume(ptr %p), !dbg !363
  ret void
}


; CHECK-LABEL: late_call_clobber:
; CHECK: movq $25,
; CHECK: #DEBUG_VALUE: late_call_clobber:range.state <- 1
; CHECK: callq checkpoint
; CHECK: callq mutate
; CHECK: #DEBUG_VALUE: late_call_clobber:range.state <- 0
; CHECK: callq consume
define rogcc void @late_call_clobber() !dbg !370 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !371, !DIExpression(DW_OP_stack_value), !373)
    #dbg_label(!374, !373)
  store i64 25, ptr %p
  call void @checkpoint(), !dbg !373
  call void @mutate(ptr %p), !dbg !373
  call void @consume(ptr %p), !dbg !373
  ret void
}

; CHECK-LABEL: argument_alias_clobber:
; CHECK: movq $25,
; CHECK: #DEBUG_VALUE: argument_alias_clobber:available.state <- 1
; CHECK: callq checkpoint
; CHECK: #DEBUG_VALUE: argument_alias_clobber:available.state <- 0
; CHECK: callq consume
define rogcc void @argument_alias_clobber(ptr %p, ptr %q, i64 %value) !dbg !380 {
entry:
    #dbg_value(ptr %p, !381, !DIExpression(), !383)
  store i64 25, ptr %p
  call void @checkpoint(), !dbg !383
  store i64 %value, ptr %q
  call void @consume(ptr %p), !dbg !383
  ret void
}

; Shared abstract variables must not inherit one concrete object's proof.
; FRESH-LABEL: DW_AT_name ("shared_descriptor")
; FRESH-NOT: ROGMemoryFreshLocal
; CHECK-LABEL: shared_descriptor:
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @shared_descriptor() !dbg !400 {
entry:
  %p = alloca i64, align 8
  %q = alloca i64, align 8
    #dbg_declare(ptr %p, !401, !DIExpression(DW_OP_stack_value), !403)
    #dbg_declare(ptr %q, !401, !DIExpression(DW_OP_stack_value), !403)
  call void @consume(ptr %p), !dbg !403
  call void @consume(ptr %q), !dbg !403
  ret void
}

; FRESH-LABEL: DW_AT_name ("future_escape")
; FRESH-NOT: ROGMemoryFreshLocal
; CHECK-LABEL: future_escape:
; CHECK: callq initialize
; CHECK-NOT: range.state <- 1
; CHECK: callq consume
define rogcc void @future_escape() !dbg !390 {
entry:
  %p = alloca i64, align 8
    #dbg_declare(ptr %p, !391, !DIExpression(DW_OP_stack_value), !393)
    #dbg_label(!394, !393)
  call rogcc void @initialize(ptr %p), !dbg !393
  call void @escape(ptr %p), !dbg !393
  call void @consume(ptr %p), !dbg !393
  ret void
}

declare void @escape(ptr)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1 immarg)
declare void @mutate(ptr captures(none))
declare void @llvm.masked.store.v2i64.p0(<2 x i64>, ptr, i32 immarg, <2 x i1>)
declare void @checkpoint() memory(inaccessiblemem: readwrite)
declare void @consume(ptr captures(none)) memory(argmem: read)

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_Go, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "memory.go", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "partial", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!7, !9, !12})
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = distinct !DILocalVariable(name: "range", scope: !4, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!8 = !DIBasicType(name: "uintptr", size: 64, encoding: DW_ATE_unsigned)
!9 = distinct !DILocalVariable(name: "range.state", scope: !4, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!10 = !DIBasicType(name: "uint8", size: 8, encoding: DW_ATE_unsigned)
!11 = !DILocation(line: 0, scope: !4)
!12 = distinct !DILabel(scope: !4, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!13 = !DILocation(line: 2, column: 1, scope: !4)
!14 = !DILocation(line: 3, column: 1, scope: !4)
!15 = !DILocation(line: 4, column: 1, scope: !4)
!16 = !DILocation(line: 5, column: 1, scope: !4)
!17 = distinct !DISubprogram(name: "one_path", scope: !1, file: !1, line: 10, type: !5, scopeLine: 10, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!18, !19, !21})
!18 = distinct !DILocalVariable(name: "range", scope: !17, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!19 = distinct !DILocalVariable(name: "range.state", scope: !17, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!20 = !DILocation(line: 0, scope: !17)
!21 = distinct !DILabel(scope: !17, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!22 = !DILocation(line: 11, column: 1, scope: !17)
!23 = !DILocation(line: 12, column: 1, scope: !17)
!24 = distinct !DISubprogram(name: "both_paths", scope: !1, file: !1, line: 20, type: !5, scopeLine: 20, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!25, !26, !28})
!25 = distinct !DILocalVariable(name: "range", scope: !24, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!26 = distinct !DILocalVariable(name: "range.state", scope: !24, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!27 = !DILocation(line: 0, scope: !24)
!28 = distinct !DILabel(scope: !24, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!29 = !DILocation(line: 21, column: 1, scope: !24)
!30 = !DILocation(line: 22, column: 1, scope: !24)
!31 = !DILocation(line: 23, column: 1, scope: !24)
!32 = !{!34}
!33 = !{!35}
!34 = !{!"ROGMemoryUnavailable", !"8"}
!35 = !{!"ROGMemoryState", !"range"}
!36 = distinct !DISubprogram(name: "lifetime", scope: !1, file: !1, line: 30, type: !5, scopeLine: 30, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!37, !38, !40, !41})
!37 = distinct !DILocalVariable(name: "range", scope: !36, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!38 = distinct !DILocalVariable(name: "range.state", scope: !36, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!39 = !DILocation(line: 0, scope: !36)
!40 = distinct !DILabel(scope: !36, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!41 = distinct !DILabel(scope: !36, name: "range.end.0", file: !1, line: 0, isArtificial: true)
!42 = !DILocation(line: 31, column: 1, scope: !36)
!50 = distinct !DISubprogram(name: "atomic_store", scope: !1, file: !1, line: 40, type: !5, scopeLine: 40, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!51, !52, !54})
!51 = distinct !DILocalVariable(name: "range", scope: !50, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!52 = distinct !DILocalVariable(name: "range.state", scope: !50, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!53 = !DILocation(line: 0, scope: !50)
!54 = distinct !DILabel(scope: !50, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!55 = distinct !DISubprogram(name: "conditional_store", scope: !1, file: !1, line: 40, type: !5, scopeLine: 40, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!56, !57, !59})
!56 = distinct !DILocalVariable(name: "range", scope: !55, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!57 = distinct !DILocalVariable(name: "range.state", scope: !55, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!58 = !DILocation(line: 0, scope: !55)
!59 = distinct !DILabel(scope: !55, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!60 = distinct !DISubprogram(name: "masked_store", scope: !1, file: !1, line: 40, type: !5, scopeLine: 40, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!61, !62, !64})
!61 = distinct !DILocalVariable(name: "range", scope: !60, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!62 = distinct !DILocalVariable(name: "range.state", scope: !60, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!63 = !DILocation(line: 0, scope: !60)
!64 = distinct !DILabel(scope: !60, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!65 = distinct !DISubprogram(name: "unknown_call", scope: !1, file: !1, line: 40, type: !5, scopeLine: 40, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!66, !67, !69})
!66 = distinct !DILocalVariable(name: "range", scope: !65, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!67 = distinct !DILocalVariable(name: "range.state", scope: !65, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!68 = !DILocation(line: 0, scope: !65)
!69 = distinct !DILabel(scope: !65, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!70 = distinct !DISubprogram(name: "vector_store", scope: !1, file: !1, line: 40, type: !5, scopeLine: 40, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!71, !72, !74})
!71 = distinct !DILocalVariable(name: "range", scope: !70, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!72 = distinct !DILocalVariable(name: "range.state", scope: !70, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!73 = !DILocation(line: 0, scope: !70)
!74 = distinct !DILabel(scope: !70, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!75 = distinct !DISubprogram(name: "inactive_join", scope: !1, file: !1, line: 50, type: !5, scopeLine: 50, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!76, !77, !79})
!76 = distinct !DILocalVariable(name: "range", scope: !75, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!77 = distinct !DILocalVariable(name: "range.state", scope: !75, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!78 = !DILocation(line: 0, scope: !75)
!79 = distinct !DILabel(scope: !75, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!80 = distinct !DISubprogram(name: "argument_store", scope: !1, file: !1, line: 60, type: !5, scopeLine: 60, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!81, !82})
!81 = distinct !DILocalVariable(name: "available", scope: !80, file: !1, type: !8, flags: DIFlagArtificial, annotations: !84)
!82 = distinct !DILocalVariable(name: "available.state", scope: !80, file: !1, type: !10, flags: DIFlagArtificial, annotations: !88)
!83 = !DILocation(line: 0, scope: !80)
!84 = !{!85, !86, !87}
!85 = !{!"ROGMemoryAvailable", !"8"}
!86 = !{!"ROGMemoryArgument", !"0"}
!87 = !{!"ROGMemoryOffset", !"0"}
!88 = !{!89}
!89 = !{!"ROGMemoryState", !"available"}
!90 = distinct !DISubprogram(name: "caller_initialize", scope: !1, file: !1, line: 70, type: !5, scopeLine: 70, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!91, !92, !94})
!91 = distinct !DILocalVariable(name: "range", scope: !90, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!92 = distinct !DILocalVariable(name: "range.state", scope: !90, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!93 = !DILocation(line: 0, scope: !90)
!94 = distinct !DILabel(scope: !90, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!95 = distinct !DISubprogram(name: "initialize", scope: !1, file: !1, line: 80, type: !5, scopeLine: 80, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!100 = distinct !DISubprogram(name: "ambiguous_calls", scope: !1, file: !1, line: 90, type: !5, scopeLine: 90, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!101, !102, !104})
!101 = distinct !DILocalVariable(name: "range", scope: !100, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!102 = distinct !DILocalVariable(name: "range.state", scope: !100, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!103 = !DILocation(line: 0, scope: !100)
!104 = distinct !DILabel(scope: !100, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!110 = distinct !DISubprogram(name: "patchable_call", scope: !1, file: !1, line: 100, type: !5, scopeLine: 100, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!111, !112, !114})
!111 = distinct !DILocalVariable(name: "range", scope: !110, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!112 = distinct !DILocalVariable(name: "range.state", scope: !110, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!113 = !DILocation(line: 0, scope: !110)
!114 = distinct !DILabel(scope: !110, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!200 = distinct !DISubprogram(name: "proof_empty", scope: !1, file: !1, line: 110, type: !5, scopeLine: 110, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!201, !202, !204})
!201 = distinct !DILocalVariable(name: "range", scope: !200, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!202 = distinct !DILocalVariable(name: "range.state", scope: !200, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!203 = !DILocation(line: 0, scope: !200)
!204 = distinct !DILabel(scope: !200, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!205 = distinct !DISubprogram(name: "initialize_empty", scope: !1, file: !1, line: 120, type: !5, scopeLine: 120, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!210 = distinct !DISubprogram(name: "proof_undef", scope: !1, file: !1, line: 110, type: !5, scopeLine: 110, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!211, !212, !214})
!211 = distinct !DILocalVariable(name: "range", scope: !210, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!212 = distinct !DILocalVariable(name: "range.state", scope: !210, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!213 = !DILocation(line: 0, scope: !210)
!214 = distinct !DILabel(scope: !210, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!215 = distinct !DISubprogram(name: "initialize_undef", scope: !1, file: !1, line: 120, type: !5, scopeLine: 120, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!220 = distinct !DISubprogram(name: "proof_poison", scope: !1, file: !1, line: 110, type: !5, scopeLine: 110, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!221, !222, !224})
!221 = distinct !DILocalVariable(name: "range", scope: !220, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!222 = distinct !DILocalVariable(name: "range.state", scope: !220, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!223 = !DILocation(line: 0, scope: !220)
!224 = distinct !DILabel(scope: !220, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!225 = distinct !DISubprogram(name: "initialize_poison", scope: !1, file: !1, line: 120, type: !5, scopeLine: 120, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!230 = distinct !DISubprogram(name: "proof_dead", scope: !1, file: !1, line: 110, type: !5, scopeLine: 110, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!231, !232, !234})
!231 = distinct !DILocalVariable(name: "range", scope: !230, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!232 = distinct !DILocalVariable(name: "range.state", scope: !230, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!233 = !DILocation(line: 0, scope: !230)
!234 = distinct !DILabel(scope: !230, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!235 = distinct !DISubprogram(name: "initialize_dead", scope: !1, file: !1, line: 120, type: !5, scopeLine: 120, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!240 = distinct !DISubprogram(name: "proof_unknown_value", scope: !1, file: !1, line: 110, type: !5, scopeLine: 110, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!241, !242, !244})
!241 = distinct !DILocalVariable(name: "range", scope: !240, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!242 = distinct !DILocalVariable(name: "range.state", scope: !240, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!243 = !DILocation(line: 0, scope: !240)
!244 = distinct !DILabel(scope: !240, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!245 = distinct !DISubprogram(name: "initialize_unknown_value", scope: !1, file: !1, line: 120, type: !5, scopeLine: 120, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!250 = distinct !DISubprogram(name: "proof_one_path", scope: !1, file: !1, line: 110, type: !5, scopeLine: 110, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!251, !252, !254})
!251 = distinct !DILocalVariable(name: "range", scope: !250, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!252 = distinct !DILocalVariable(name: "range.state", scope: !250, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!253 = !DILocation(line: 0, scope: !250)
!254 = distinct !DILabel(scope: !250, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!255 = distinct !DISubprogram(name: "initialize_one_path", scope: !1, file: !1, line: 120, type: !5, scopeLine: 120, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!260 = distinct !DISubprogram(name: "proof_unknown_clobber", scope: !1, file: !1, line: 110, type: !5, scopeLine: 110, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!261, !262, !264})
!261 = distinct !DILocalVariable(name: "range", scope: !260, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!262 = distinct !DILocalVariable(name: "range.state", scope: !260, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!263 = !DILocation(line: 0, scope: !260)
!264 = distinct !DILabel(scope: !260, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!265 = distinct !DISubprogram(name: "initialize_unknown_clobber", scope: !1, file: !1, line: 120, type: !5, scopeLine: 120, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!300 = distinct !DISubprogram(name: "late_unknown", scope: !1, file: !1, line: 130, type: !5, scopeLine: 130, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!301, !302, !304})
!301 = distinct !DILocalVariable(name: "range", scope: !300, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!302 = distinct !DILocalVariable(name: "range.state", scope: !300, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!303 = !DILocation(line: 0, scope: !300)
!304 = distinct !DILabel(scope: !300, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!310 = distinct !DISubprogram(name: "late_undef", scope: !1, file: !1, line: 130, type: !5, scopeLine: 130, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!311, !312, !314})
!311 = distinct !DILocalVariable(name: "range", scope: !310, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!312 = distinct !DILocalVariable(name: "range.state", scope: !310, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!313 = !DILocation(line: 0, scope: !310)
!314 = distinct !DILabel(scope: !310, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!320 = distinct !DISubprogram(name: "late_poison", scope: !1, file: !1, line: 130, type: !5, scopeLine: 130, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!321, !322, !324})
!321 = distinct !DILocalVariable(name: "range", scope: !320, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!322 = distinct !DILocalVariable(name: "range.state", scope: !320, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!323 = !DILocation(line: 0, scope: !320)
!324 = distinct !DILabel(scope: !320, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!330 = distinct !DISubprogram(name: "late_overflow", scope: !1, file: !1, line: 130, type: !5, scopeLine: 130, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!331, !332, !334})
!331 = distinct !DILocalVariable(name: "range", scope: !330, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!332 = distinct !DILocalVariable(name: "range.state", scope: !330, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!333 = !DILocation(line: 0, scope: !330)
!334 = distinct !DILabel(scope: !330, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!340 = distinct !DISubprogram(name: "positive_unknown", scope: !1, file: !1, line: 140, type: !5, scopeLine: 140, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!341, !342})
!341 = distinct !DILocalVariable(name: "available", scope: !340, file: !1, type: !8, flags: DIFlagArtificial, annotations: !84)
!342 = distinct !DILocalVariable(name: "available.state", scope: !340, file: !1, type: !10, flags: DIFlagArtificial, annotations: !88)
!343 = !DILocation(line: 0, scope: !340)
!350 = distinct !DISubprogram(name: "late_memset", scope: !1, file: !1, line: 150, type: !5, scopeLine: 150, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!351, !352, !354})
!351 = distinct !DILocalVariable(name: "range", scope: !350, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!352 = distinct !DILocalVariable(name: "range.state", scope: !350, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!353 = !DILocation(line: 0, scope: !350)
!354 = distinct !DILabel(scope: !350, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!360 = distinct !DISubprogram(name: "late_masked_clobber", scope: !1, file: !1, line: 160, type: !5, scopeLine: 160, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!361, !362, !364})
!361 = distinct !DILocalVariable(name: "range", scope: !360, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!362 = distinct !DILocalVariable(name: "range.state", scope: !360, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!363 = !DILocation(line: 0, scope: !360)
!364 = distinct !DILabel(scope: !360, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
!370 = distinct !DISubprogram(name: "late_call_clobber", scope: !1, file: !1, line: 370, type: !5, scopeLine: 370, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!371, !372, !374})
!371 = distinct !DILocalVariable(name: "range", scope: !370, file: !1, line: 370, type: !8, flags: DIFlagArtificial, annotations: !32)
!372 = !DILocalVariable(name: "range.state", scope: !370, file: !1, line: 370, type: !10, flags: DIFlagArtificial, annotations: !376)
!373 = !DILocation(line: 370, column: 1, scope: !370)
!374 = !DILabel(scope: !370, name: "range.invalid.0", file: !1, line: 370)
!376 = !{!377}
!377 = !{!"ROGMemoryState", !"range"}
!380 = distinct !DISubprogram(name: "argument_alias_clobber", scope: !1, file: !1, line: 380, type: !5, scopeLine: 380, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!381, !382, !384})
!381 = distinct !DILocalVariable(name: "available", scope: !380, file: !1, line: 380, type: !8, flags: DIFlagArtificial, annotations: !385)
!382 = !DILocalVariable(name: "available.state", scope: !380, file: !1, line: 380, type: !10, flags: DIFlagArtificial, annotations: !386)
!383 = !DILocation(line: 380, column: 1, scope: !380)
!384 = !DILabel(scope: !380, name: "available.invalid.0", file: !1, line: 380)
!386 = !{!387}
!387 = !{!"ROGMemoryState", !"available"}
!385 = !{!388, !86, !87}
!388 = !{!"ROGMemoryAvailable", !"8"}
!390 = distinct !DISubprogram(name: "future_escape", scope: !1, file: !1, line: 390, type: !5, scopeLine: 390, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!391, !392, !394})
!391 = distinct !DILocalVariable(name: "range", scope: !390, file: !1, line: 390, type: !8, flags: DIFlagArtificial, annotations: !32)
!392 = !DILocalVariable(name: "range.state", scope: !390, file: !1, line: 390, type: !10, flags: DIFlagArtificial, annotations: !396)
!393 = !DILocation(line: 390, column: 1, scope: !390)
!394 = !DILabel(scope: !390, name: "range.invalid.0", file: !1, line: 390)
!396 = !{!397}
!397 = !{!"ROGMemoryState", !"range"}
!400 = distinct !DISubprogram(name: "shared_descriptor", scope: !1, file: !1, line: 400, type: !5, scopeLine: 400, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !{!401, !402, !404})
!401 = distinct !DILocalVariable(name: "range", scope: !400, file: !1, type: !8, flags: DIFlagArtificial, annotations: !32)
!402 = distinct !DILocalVariable(name: "range.state", scope: !400, file: !1, type: !10, flags: DIFlagArtificial, annotations: !33)
!403 = !DILocation(line: 400, column: 1, scope: !400)
!404 = distinct !DILabel(scope: !400, name: "range.invalid.0", file: !1, line: 0, isArtificial: true)
