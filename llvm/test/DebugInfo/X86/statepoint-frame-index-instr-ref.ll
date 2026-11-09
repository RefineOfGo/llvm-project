; RUN: llc -O2 -verify-machineinstrs -stop-after=finalize-isel -o - %s | FileCheck %s --check-prefix=ISEL
; RUN: llc -O2 -verify-machineinstrs -stop-after=livedebugvalues -o - %s | FileCheck %s --check-prefix=LDV
;
; Expanding a statepoint's frame index must preserve instruction references to
; its return register, whose operand index moves past the expanded frame index.

target triple = "x86_64-unknown-linux-gnu"

; ISEL-LABEL: name: with_frame_index
; ISEL: debugValueSubstitutions:
; ISEL-NEXT: - { srcinst: [[#OLD:]], srcop: [[#SP:]], dstinst: [[#NEW:]],
; ISEL-SAME: dstop: [[#SP+2]], subreg: 0 }
; ISEL-NEXT: - { srcinst: [[#OLD]], srcop: [[#SSP:]], dstinst: [[#NEW]],
; ISEL-SAME: dstop: [[#SSP+2]], subreg: 0 }
; ISEL-NEXT: - { srcinst: [[#OLD]], srcop: [[#RET:]], dstinst: [[#NEW]],
; ISEL-SAME: dstop: [[#RET+2]], subreg: 0 }
; ISEL: STATEPOINT {{.*}}@produce, {{.*}}implicit-def $rsp, implicit-def $ssp, implicit-def $rax, debug-instr-number [[#NEW]],
; ISEL: DBG_INSTR_REF {{.*}}DW_OP_LLVM_fragment, 64, 64), dbg-instr-ref([[#OLD]], [[#RET]]),
; LDV-LABEL: name: with_frame_index
; LDV: $[[RESULT:r[a-z0-9]+]] = MOV64rr $rax
; LDV: DBG_VALUE_LIST ![[PAIR:[0-9]+]], !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_fragment, 64, 64), $[[RESULT]],
; LDV: CALL64pcrel32 target-flags(x86-plt) @checkpoint
; LDV: DBG_VALUE_LIST ![[PAIR]], !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_fragment, 64, 64), $rax,
define i64 @with_frame_index() gc "statepoint-example" !dbg !4 {
entry:
  %slot = alloca i64, align 8
  store i64 0, ptr %slot, align 8
  %token = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 1, i32 0, ptr elementtype(i64 ()) @produce, i32 0, i32 0, i32 0, i32 0) [ "deopt"(ptr %slot) ], !dbg !8
  %result = call i64 @llvm.experimental.gc.result.i64(token %token), !dbg !8
    #dbg_value(i64 %result, !7, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !8)
  call void @checkpoint(), !dbg !8
  ret i64 %result, !dbg !8
}

; A statepoint without frame indices keeps its original instruction identity.
; ISEL-LABEL: name: without_frame_index
; ISEL: debugValueSubstitutions: []
; ISEL: STATEPOINT {{.*}}@produce, {{.*}}implicit-def $rax, debug-instr-number [[#UNCHANGED:]],
; ISEL: DBG_INSTR_REF {{.*}}dbg-instr-ref([[#UNCHANGED]], {{[0-9]+}}),
; LDV-LABEL: name: without_frame_index
; LDV: DBG_VALUE_LIST {{.*}}DW_OP_LLVM_fragment, 64, 64), $rax,
define i64 @without_frame_index() gc "statepoint-example" !dbg !10 {
entry:
  %token = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 2, i32 0, ptr elementtype(i64 ()) @produce, i32 0, i32 0, i32 0, i32 0), !dbg !12
  %result = call i64 @llvm.experimental.gc.result.i64(token %token), !dbg !12
    #dbg_value(i64 %result, !11, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !12)
  ret i64 %result, !dbg !12
}

; Rebuilding an instruction without debug users must not allocate debug numbers.
; ISEL-LABEL: name: without_debug_info
; ISEL: debugValueSubstitutions: []
; ISEL: body:
; ISEL-NOT: debug-instr-number
; ISEL: ...
; LDV-LABEL: name: without_debug_info
define i64 @without_debug_info() gc "statepoint-example" {
entry:
  %slot = alloca i64, align 8
  store i64 0, ptr %slot, align 8
  %token = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 3, i32 0, ptr elementtype(i64 ()) @produce, i32 0, i32 0, i32 0, i32 0) [ "deopt"(ptr %slot) ]
  %result = call i64 @llvm.experimental.gc.result.i64(token %token)
  ret i64 %result
}

declare i64 @produce()
declare void @checkpoint()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare i64 @llvm.experimental.gc.result.i64(token)

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "statepoint.c", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "with_frame_index", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = !DILocalVariable(name: "pair", scope: !4, file: !1, line: 1, type: !9)
!8 = !DILocation(line: 1, column: 1, scope: !4)
!9 = !DIBasicType(name: "__int128", size: 128, encoding: DW_ATE_unsigned)
!10 = distinct !DISubprogram(name: "without_frame_index", scope: !1, file: !1, line: 2, type: !5, scopeLine: 2, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!11 = !DILocalVariable(name: "pair", scope: !10, file: !1, line: 2, type: !9)
!12 = !DILocation(line: 2, column: 1, scope: !10)
