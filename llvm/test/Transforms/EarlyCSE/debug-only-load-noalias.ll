; RUN: opt -S -passes=early-cse %s | FileCheck %s --check-prefixes=CHECK,NOMSSA
; RUN: opt -S -passes='early-cse<memssa>' %s | FileCheck %s --check-prefixes=CHECK,MSSA
; RUN: opt -S -passes='early-cse<memssa>' -earlycse-mssa-optimization-cap=1 %s | FileCheck %s --check-prefixes=CHECK,MSSA
;
; Debug-only loads may cross a write to disjoint memory. Use MemorySSA to
; recover an existing value without charging the program's query budget.

; CHECK-LABEL: define i64 @disjoint_call(
; CHECK: %first = load i64, ptr %p
; CHECK: call void @write(ptr %q)
; NOMSSA: #dbg_value(i64 poison,
; NOMSSA: %second = load i64, ptr %p
; NOMSSA: %result = add i64 %first, %second
; MSSA: #dbg_value(i64 %first,
; MSSA-NOT: load i64
; MSSA: %result = add i64 %first, %first
; CHECK: ret i64 %result
define i64 @disjoint_call(ptr noalias %p, ptr noalias %q) !dbg !4 {
  %first = load i64, ptr %p, align 8
  call void @write(ptr %q)
  %debug.only = load i64, ptr %p, align 8
    #dbg_value(i64 %debug.only, !7, !DIExpression(), !8)
  %second = load i64, ptr %p, align 8
  %result = add i64 %first, %second
  ret i64 %result
}

; A call that can change the loaded location must still make the debug value
; unavailable, rather than reusing the pre-call snapshot as the new value.
; CHECK-LABEL: define i64 @aliasing_call(
; CHECK: %first = load i64, ptr %p
; CHECK: call void @write(ptr %p)
; CHECK-NOT: load i64
; CHECK: #dbg_value(i64 poison,
; CHECK: ret i64 %first
define i64 @aliasing_call(ptr %p) !dbg !9 {
  %first = load i64, ptr %p, align 8
  call void @write(ptr %p)
  %debug.only = load i64, ptr %p, align 8
    #dbg_value(i64 %debug.only, !10, !DIExpression(), !11)
  ret i64 %first
}

declare void @write(ptr) memory(argmem: readwrite)

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "debug-only-load-noalias.c", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "disjoint_call", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = !DILocalVariable(name: "value", scope: !4, file: !1, line: 1, type: !12)
!8 = !DILocation(line: 1, column: 1, scope: !4)
!9 = distinct !DISubprogram(name: "aliasing_call", scope: !1, file: !1, line: 2, type: !5, scopeLine: 2, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!10 = !DILocalVariable(name: "value", scope: !9, file: !1, line: 2, type: !12)
!11 = !DILocation(line: 2, column: 1, scope: !9)
!12 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
