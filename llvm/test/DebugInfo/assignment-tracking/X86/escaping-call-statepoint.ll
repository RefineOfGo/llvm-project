; RUN: llc %s -stop-after=finalize-isel -o - | FileCheck %s

;; Statepoints must expose the wrapped call's memory effects to assignment
;; tracking. Only actual call arguments escape, not deopt or GC roots. Readonly,
;; readnone and byval attributes may live on the wrapped callee or at the
;; corresponding statepoint call-argument position.
;;
;; An unlinked value assignment makes x's pre-call location the constant 42.
;; A potentially writing call must reinstate x's memory location afterwards;
;; the other calls must preserve that value location.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare void @clobber(ptr)
declare void @readonly_func(ptr readonly)
declare void @readnone_func(ptr readnone)
declare void @byval_func(ptr byval(i32))
declare void @only_reads(ptr) memory(read)
declare void @noargs()
declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)

; CHECK-LABEL: name: statepoint_write
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR0:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR0]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}@clobber
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR0]], !DIExpression(DW_OP_deref)
; CHECK:         RET 0

define void @statepoint_write() gc "statepoint-example" !dbg !10 {
entry:
  %x = alloca i32, align 4, !DIAssignID !13
    #dbg_assign(i1 poison, !11, !DIExpression(), !13, ptr %x, !DIExpression(), !12)
  store i32 1, ptr %x, align 4, !DIAssignID !14
    #dbg_assign(i32 1, !11, !DIExpression(), !14, ptr %x, !DIExpression(), !12)
    #dbg_value(i32 42, !11, !DIExpression(), !12)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) @clobber, i32 1, i32 0, ptr %x, i32 0, i32 0)
  ret void, !dbg !12
}

; CHECK-LABEL: name: statepoint_readonly_callee
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR1:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR1]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}@readonly_func
; CHECK-NOT:     DBG_VALUE
; CHECK:         RET 0

define void @statepoint_readonly_callee() gc "statepoint-example" !dbg !16 {
entry:
  %x = alloca i32, align 4, !DIAssignID !19
    #dbg_assign(i1 poison, !17, !DIExpression(), !19, ptr %x, !DIExpression(), !18)
  store i32 1, ptr %x, align 4, !DIAssignID !20
    #dbg_assign(i32 1, !17, !DIExpression(), !20, ptr %x, !DIExpression(), !18)
    #dbg_value(i32 42, !17, !DIExpression(), !18)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) @readonly_func, i32 1, i32 0, ptr %x, i32 0, i32 0)
  ret void, !dbg !18
}

; CHECK-LABEL: name: statepoint_readnone_callee
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR2:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR2]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}@readnone_func
; CHECK-NOT:     DBG_VALUE
; CHECK:         RET 0

define void @statepoint_readnone_callee() gc "statepoint-example" !dbg !22 {
entry:
  %x = alloca i32, align 4, !DIAssignID !25
    #dbg_assign(i1 poison, !23, !DIExpression(), !25, ptr %x, !DIExpression(), !24)
  store i32 1, ptr %x, align 4, !DIAssignID !26
    #dbg_assign(i32 1, !23, !DIExpression(), !26, ptr %x, !DIExpression(), !24)
    #dbg_value(i32 42, !23, !DIExpression(), !24)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) @readnone_func, i32 1, i32 0, ptr %x, i32 0, i32 0)
  ret void, !dbg !24
}

; CHECK-LABEL: name: statepoint_readonly_callsite
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR3:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR3]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}{{%[0-9]+}}
; CHECK-NOT:     DBG_VALUE
; CHECK:         RET 0

define void @statepoint_readonly_callsite(ptr %fn) gc "statepoint-example" !dbg !28 {
entry:
  %x = alloca i32, align 4, !DIAssignID !31
    #dbg_assign(i1 poison, !29, !DIExpression(), !31, ptr %x, !DIExpression(), !30)
  store i32 1, ptr %x, align 4, !DIAssignID !32
    #dbg_assign(i32 1, !29, !DIExpression(), !32, ptr %x, !DIExpression(), !30)
    #dbg_value(i32 42, !29, !DIExpression(), !30)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) %fn, i32 1, i32 0, ptr readonly %x, i32 0, i32 0)
  ret void, !dbg !30
}

; CHECK-LABEL: name: statepoint_readnone_callsite
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR4:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR4]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}{{%[0-9]+}}
; CHECK-NOT:     DBG_VALUE
; CHECK:         RET 0

define void @statepoint_readnone_callsite(ptr %fn) gc "statepoint-example" !dbg !34 {
entry:
  %x = alloca i32, align 4, !DIAssignID !37
    #dbg_assign(i1 poison, !35, !DIExpression(), !37, ptr %x, !DIExpression(), !36)
  store i32 1, ptr %x, align 4, !DIAssignID !38
    #dbg_assign(i32 1, !35, !DIExpression(), !38, ptr %x, !DIExpression(), !36)
    #dbg_value(i32 42, !35, !DIExpression(), !36)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) %fn, i32 1, i32 0, ptr readnone %x, i32 0, i32 0)
  ret void, !dbg !36
}

; CHECK-LABEL: name: statepoint_byval
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR5:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR5]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}@byval_func
; CHECK-NOT:     DBG_VALUE
; CHECK:         RET 0

define void @statepoint_byval() gc "statepoint-example" !dbg !40 {
entry:
  %x = alloca i32, align 4, !DIAssignID !43
    #dbg_assign(i1 poison, !41, !DIExpression(), !43, ptr %x, !DIExpression(), !42)
  store i32 1, ptr %x, align 4, !DIAssignID !44
    #dbg_assign(i32 1, !41, !DIExpression(), !44, ptr %x, !DIExpression(), !42)
    #dbg_value(i32 42, !41, !DIExpression(), !42)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) @byval_func, i32 1, i32 0, ptr byval(i32) %x, i32 0, i32 0)
  ret void, !dbg !42
}

; CHECK-LABEL: name: statepoint_readonly_function
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR6:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR6]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}@only_reads
; CHECK-NOT:     DBG_VALUE
; CHECK:         RET 0

define void @statepoint_readonly_function() gc "statepoint-example" !dbg !46 {
entry:
  %x = alloca i32, align 4, !DIAssignID !49
    #dbg_assign(i1 poison, !47, !DIExpression(), !49, ptr %x, !DIExpression(), !48)
  store i32 1, ptr %x, align 4, !DIAssignID !50
    #dbg_assign(i32 1, !47, !DIExpression(), !50, ptr %x, !DIExpression(), !48)
    #dbg_value(i32 42, !47, !DIExpression(), !48)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) @only_reads, i32 1, i32 0, ptr %x, i32 0, i32 0)
  ret void, !dbg !48
}

; CHECK-LABEL: name: statepoint_metadata_only
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR7:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR7]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}@noargs
; CHECK-NOT:     DBG_VALUE
; CHECK:         RET 0

define void @statepoint_metadata_only() gc "statepoint-example" !dbg !52 {
entry:
  %x = alloca i32, align 4, !DIAssignID !55
    #dbg_assign(i1 poison, !53, !DIExpression(), !55, ptr %x, !DIExpression(), !54)
  store i32 1, ptr %x, align 4, !DIAssignID !56
    #dbg_assign(i32 1, !53, !DIExpression(), !56, ptr %x, !DIExpression(), !54)
    #dbg_value(i32 42, !53, !DIExpression(), !54)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void ()) @noargs, i32 0, i32 0, i32 0, i32 0) [ "deopt"(ptr %x), "gc-live"(ptr %x) ]
  ret void, !dbg !54
}

; CHECK-LABEL: name: statepoint_indirect_write
; CHECK:       bb.0.entry:
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR8:[0-9]+]], !DIExpression(DW_OP_deref)
; CHECK:         MOV32mi %stack.0.x
; CHECK:         DBG_VALUE 42, $noreg, ![[VAR8]], !DIExpression()
; CHECK:         STATEPOINT {{.*}}{{%[0-9]+}}
; CHECK:         DBG_VALUE %stack.0.x, $noreg, ![[VAR8]], !DIExpression(DW_OP_deref)
; CHECK:         RET 0

define void @statepoint_indirect_write(ptr %fn) gc "statepoint-example" !dbg !58 {
entry:
  %x = alloca i32, align 4, !DIAssignID !61
    #dbg_assign(i1 poison, !59, !DIExpression(), !61, ptr %x, !DIExpression(), !60)
  store i32 1, ptr %x, align 4, !DIAssignID !62
    #dbg_assign(i32 1, !59, !DIExpression(), !62, ptr %x, !DIExpression(), !60)
    #dbg_value(i32 42, !59, !DIExpression(), !60)
  %sp = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(void (ptr)) %fn, i32 1, i32 0, ptr %x, i32 0, i32 0)
  ret void, !dbg !60
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "test.c", directory: "/tmp")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 5}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 7, !"debug-info-assignment-tracking", i1 true}
!6 = !DISubroutineType(types: !2)
!7 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)

!10 = distinct !DISubprogram(name: "statepoint_write", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!11 = !DILocalVariable(name: "x", scope: !10, file: !1, line: 2, type: !7)
!12 = !DILocation(line: 2, column: 1, scope: !10)
!13 = distinct !DIAssignID()
!14 = distinct !DIAssignID()

!16 = distinct !DISubprogram(name: "statepoint_readonly_callee", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!17 = !DILocalVariable(name: "x", scope: !16, file: !1, line: 2, type: !7)
!18 = !DILocation(line: 2, column: 1, scope: !16)
!19 = distinct !DIAssignID()
!20 = distinct !DIAssignID()

!22 = distinct !DISubprogram(name: "statepoint_readnone_callee", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!23 = !DILocalVariable(name: "x", scope: !22, file: !1, line: 2, type: !7)
!24 = !DILocation(line: 2, column: 1, scope: !22)
!25 = distinct !DIAssignID()
!26 = distinct !DIAssignID()

!28 = distinct !DISubprogram(name: "statepoint_readonly_callsite", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!29 = !DILocalVariable(name: "x", scope: !28, file: !1, line: 2, type: !7)
!30 = !DILocation(line: 2, column: 1, scope: !28)
!31 = distinct !DIAssignID()
!32 = distinct !DIAssignID()

!34 = distinct !DISubprogram(name: "statepoint_readnone_callsite", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!35 = !DILocalVariable(name: "x", scope: !34, file: !1, line: 2, type: !7)
!36 = !DILocation(line: 2, column: 1, scope: !34)
!37 = distinct !DIAssignID()
!38 = distinct !DIAssignID()

!40 = distinct !DISubprogram(name: "statepoint_byval", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!41 = !DILocalVariable(name: "x", scope: !40, file: !1, line: 2, type: !7)
!42 = !DILocation(line: 2, column: 1, scope: !40)
!43 = distinct !DIAssignID()
!44 = distinct !DIAssignID()

!46 = distinct !DISubprogram(name: "statepoint_readonly_function", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!47 = !DILocalVariable(name: "x", scope: !46, file: !1, line: 2, type: !7)
!48 = !DILocation(line: 2, column: 1, scope: !46)
!49 = distinct !DIAssignID()
!50 = distinct !DIAssignID()

!52 = distinct !DISubprogram(name: "statepoint_metadata_only", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!53 = !DILocalVariable(name: "x", scope: !52, file: !1, line: 2, type: !7)
!54 = !DILocation(line: 2, column: 1, scope: !52)
!55 = distinct !DIAssignID()
!56 = distinct !DIAssignID()

!58 = distinct !DISubprogram(name: "statepoint_indirect_write", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!59 = !DILocalVariable(name: "x", scope: !58, file: !1, line: 2, type: !7)
!60 = !DILocation(line: 2, column: 1, scope: !58)
!61 = distinct !DIAssignID()
!62 = distinct !DIAssignID()
