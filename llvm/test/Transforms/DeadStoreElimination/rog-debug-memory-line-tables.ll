; RUN: opt -S -passes=dse %s -o %t.ll
; RUN: FileCheck %s --check-prefix=IR --implicit-check-not="#dbg_" --implicit-check-not=ROGMemory < %t.ll
; RUN: llc -mtriple=x86_64-unknown-linux-gnu -filetype=obj %t.ll -o %t.o
; RUN: llvm-dwarfdump --debug-info --debug-line %t.o | FileCheck %s --check-prefix=DWARF --implicit-check-not=DW_TAG_variable --implicit-check-not=DW_TAG_subprogram --implicit-check-not=ROGMemory
;
; Line-tables-only CUs must not acquire local-variable memory descriptors.
; Otherwise DwarfDebug collects the synthetic retained variables and asserts
; when it takes the no-inlined-subroutines line-table-only emission path.
;
; Both producers are covered: an eliminated stack store and a surviving write
; through an argument. The stores still receive ordinary DSE treatment.

target triple = "x86_64-unknown-linux-gnu"

; IR-LABEL: define rogcc void @stack_dead_store(
; IR-NOT: store i64 20
; IR: call void @checkpoint()
; IR: store i64 21, ptr %p
; IR: call void @consume(ptr %p)
; IR: ret void
define rogcc void @stack_dead_store() !dbg !4 {
entry:
  %p = alloca i64, align 8
  store i64 20, ptr %p, align 8, !dbg !7
  call void @checkpoint(), !dbg !8
  store i64 21, ptr %p, align 8, !dbg !9
  call void @consume(ptr %p), !dbg !10
  ret void, !dbg !10
}

; IR-LABEL: define rogcc void @argument_store(
; IR: store i64 25, ptr %p
; IR: call void @consume(ptr %p)
; IR: ret void
define rogcc void @argument_store(ptr %p) !dbg !11 {
entry:
  store i64 25, ptr %p, align 8, !dbg !12
  call void @consume(ptr %p), !dbg !13
  ret void, !dbg !13
}

declare void @checkpoint() memory(inaccessiblemem: readwrite)
declare void @consume(ptr) memory(argmem: read)

; DWARF: DW_TAG_compile_unit
; DWARF: DW_AT_stmt_list
; DWARF: .debug_line contents:
; DWARF: name: "memory-line-tables.go"
; DWARF: Address{{ +}}Line

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_Go, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: LineTablesOnly)
!1 = !DIFile(filename: "memory-line-tables.go", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "stack_dead_store", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = !DILocation(line: 2, column: 1, scope: !4)
!8 = !DILocation(line: 3, column: 1, scope: !4)
!9 = !DILocation(line: 4, column: 1, scope: !4)
!10 = !DILocation(line: 5, column: 1, scope: !4)
!11 = distinct !DISubprogram(name: "argument_store", scope: !1, file: !1, line: 10, type: !5, scopeLine: 10, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!12 = !DILocation(line: 11, column: 1, scope: !11)
!13 = !DILocation(line: 12, column: 1, scope: !11)
