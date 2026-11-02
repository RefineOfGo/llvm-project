; RUN: llc -O2 -verify-machineinstrs -experimental-debug-variable-locations=true -stop-after=finalize-isel -o - %s | FileCheck %s --check-prefixes=CHECK,REF
; RUN: llc -O2 -verify-machineinstrs -experimental-debug-variable-locations=false -stop-after=finalize-isel -o - %s | FileCheck %s --check-prefixes=CHECK,REG
; RUN: llc -O2 -verify-machineinstrs -experimental-debug-variable-locations=true -stop-after=livedebugvalues -o - %s | FileCheck %s --check-prefix=LIVE
; RUN: llc -O2 -verify-machineinstrs -experimental-debug-variable-locations=false -stop-after=livedebugvalues -o - %s | FileCheck %s --check-prefix=LOC
;
; CodeGenPrepare groups probes at block entry, leaving the debug updates next
; to each other. Opportunistic emission of a register value must not move it
; ahead of an earlier constant update, including an overlapping whole variable.

target triple = "x86_64-unknown-linux-gnu"

; CHECK-LABEL: name: with_probes
; CHECK: PSEUDO_PROBE
; CHECK: CALL64pcrel32 {{.*}}@produce
; CHECK: DBG_VALUE i128 0, $noreg, ![[PAIR:[0-9]+]], !DIExpression(),
; REF-NEXT: DBG_INSTR_REF ![[PAIR]], !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_fragment, 64, 64),
; REG-NEXT: DBG_VALUE %{{[0-9]+}}, $noreg, ![[PAIR]], !DIExpression(DW_OP_LLVM_fragment, 64, 64),
; CHECK: CALL64pcrel32 {{.*}}@checkpoint
; LIVE-LABEL: name: with_probes
; LIVE: $[[VALUE:r[a-z0-9]+]] = MOV64rr $rax
; LIVE: DBG_VALUE i128 0, $noreg, ![[LIVE_PAIR:[0-9]+]], !DIExpression(),
; LIVE: DBG_VALUE_LIST ![[LIVE_PAIR]], !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_fragment, 64, 64), $[[VALUE]],
; LIVE-NOT: DBG_VALUE i128 0
; LIVE: CALL64pcrel32 {{.*}}@checkpoint
; LOC-LABEL: name: with_probes
; LOC: $[[LOC_VALUE:r[a-z0-9]+]] = MOV64rr $rax
; LOC: DBG_VALUE $[[LOC_VALUE]], $noreg, {{.*}}!DIExpression(DW_OP_LLVM_fragment, 64, 64),
; LOC-NOT: DBG_VALUE i128 0
; LOC: CALL64pcrel32 {{.*}}@checkpoint
define i64 @with_probes() !dbg !4 {
  %value = call i64 @produce(), !dbg !8
  call void @llvm.pseudoprobe(i64 1, i64 1, i32 0, i64 -1), !dbg !8
    #dbg_value(i128 0, !7, !DIExpression(), !8)
  call void @llvm.pseudoprobe(i64 1, i64 2, i32 0, i64 -1), !dbg !8
    #dbg_value(i64 %value, !7, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !8)
  call void @llvm.pseudoprobe(i64 1, i64 3, i32 0, i64 -1), !dbg !8
  call void @checkpoint(), !dbg !8
  ret i64 %value, !dbg !8
}

; The same update ordering is required when no probes are present.
; CHECK-LABEL: name: without_probes
; CHECK: CALL64pcrel32 {{.*}}@produce
; CHECK: DBG_VALUE 0, $noreg, ![[NO_PROBE:[0-9]+]], !DIExpression(DW_OP_LLVM_fragment, 64, 64),
; REF-NEXT: DBG_INSTR_REF ![[NO_PROBE]], !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_fragment, 64, 64),
; REG-NEXT: DBG_VALUE %{{[0-9]+}}, $noreg, ![[NO_PROBE]], !DIExpression(DW_OP_LLVM_fragment, 64, 64),
; CHECK: CALL64pcrel32 {{.*}}@checkpoint
; LIVE-LABEL: name: without_probes
; LIVE: $[[NO_PROBE_VALUE:r[a-z0-9]+]] = MOV64rr $rax
; LIVE: DBG_VALUE 0, $noreg, ![[NO_PROBE_PAIR:[0-9]+]], !DIExpression(DW_OP_LLVM_fragment, 64, 64),
; LIVE: DBG_VALUE_LIST ![[NO_PROBE_PAIR]], !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_fragment, 64, 64), $[[NO_PROBE_VALUE]],
; LIVE-NOT: DBG_VALUE 0
; LIVE: CALL64pcrel32 {{.*}}@checkpoint
; LOC-LABEL: name: without_probes
; LOC: $[[NO_PROBE_LOC:r[a-z0-9]+]] = MOV64rr $rax
; LOC: DBG_VALUE $[[NO_PROBE_LOC]], $noreg, {{.*}}!DIExpression(DW_OP_LLVM_fragment, 64, 64),
; LOC-NOT: DBG_VALUE 0
; LOC: CALL64pcrel32 {{.*}}@checkpoint
define i64 @without_probes() !dbg !10 {
  %value = call i64 @produce(), !dbg !12
    #dbg_value(i64 0, !11, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !12)
    #dbg_value(i64 %value, !11, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !12)
  call void @checkpoint(), !dbg !12
  ret i64 %value, !dbg !12
}

declare i64 @produce()
declare void @checkpoint()
declare void @llvm.pseudoprobe(i64, i64, i32, i64)

!llvm.pseudo_probe_desc = !{!13}
!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "debug-value-order.c", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "with_probes", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = !DILocalVariable(name: "pair", scope: !4, file: !1, line: 1, type: !9)
!8 = !DILocation(line: 1, column: 1, scope: !4)
!9 = !DIBasicType(name: "__int128", size: 128, encoding: DW_ATE_unsigned)
!10 = distinct !DISubprogram(name: "without_probes", scope: !1, file: !1, line: 2, type: !5, scopeLine: 2, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!11 = !DILocalVariable(name: "pair", scope: !10, file: !1, line: 2, type: !9)
!12 = !DILocation(line: 2, column: 1, scope: !10)
!13 = !{i64 1, i64 1, !"with_probes"}
