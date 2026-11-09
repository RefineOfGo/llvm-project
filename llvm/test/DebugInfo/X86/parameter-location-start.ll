; RUN: llc -O2 -filetype=obj -experimental-debug-variable-locations=true %s -o %t
; RUN: llvm-dwarfdump --debug-info %t | FileCheck %s
; RUN: llc -O2 -filetype=obj -experimental-debug-variable-locations=false %s -o %t
; RUN: llvm-dwarfdump --debug-info %t | FileCheck %s
;
; A parameter's first surviving location need not describe its incoming value.
; Respect its recorded start, including constants and disjoint fragments. Real
; entry locations must remain available at entry without extending later ones.
;
; The late scalar starts after the first call (one push plus a five-byte call),
; and the second fragment starts only after the second call.
; CHECK: DW_TAG_subprogram
; CHECK: DW_AT_low_pc (0x[[#%.16x,SCALAR:]])
; CHECK: DW_AT_name ("later_scalar")
; CHECK: DW_TAG_formal_parameter
; CHECK: DW_AT_location
; CHECK-NEXT: [0x[[#%.16x,SCALAR+6]], {{.*}}): DW_OP_consts +21, DW_OP_stack_value)
; CHECK: DW_AT_name ("arg")
; CHECK: DW_TAG_subprogram
; CHECK: DW_AT_low_pc (0x[[#%.16x,FRAGMENTS:]])
; CHECK: DW_AT_name ("later_fragments")
; CHECK: DW_TAG_formal_parameter
; CHECK: DW_AT_location
; CHECK-NEXT: [0x[[#%.16x,FRAGMENTS+6]], 0x[[#%.16x,FRAGMENTS+11]]): DW_OP_consts +21, DW_OP_stack_value, DW_OP_piece 0x8
; CHECK-NEXT: [0x[[#%.16x,FRAGMENTS+11]], {{.*}}): DW_OP_consts +21, DW_OP_stack_value, DW_OP_piece 0x8, DW_OP_consts +23, DW_OP_stack_value, DW_OP_piece 0x8)
; CHECK: DW_AT_name ("arg")
; CHECK: DW_AT_name ("entry_register")
; CHECK: DW_TAG_formal_parameter
; CHECK-NEXT: DW_AT_location (DW_OP_reg5 RDI)
; CHECK: DW_AT_name ("arg")
; CHECK: DW_AT_name ("entry_constant")
; CHECK: DW_TAG_formal_parameter
; CHECK-NEXT: DW_AT_const_value (7)
; CHECK: DW_AT_name ("arg")
; CHECK: DW_AT_name ("entry_null")
; CHECK: DW_TAG_formal_parameter
; CHECK-NEXT: DW_AT_const_value (0)
; CHECK: DW_AT_name ("arg")
; CHECK: DW_TAG_subprogram
; CHECK: DW_AT_low_pc (0x[[#%.16x,ENTRY:]])
; CHECK: DW_AT_name ("entry_fragments")
; CHECK: DW_TAG_formal_parameter
; CHECK: DW_AT_location
; CHECK-NEXT: [0x[[#%.16x,ENTRY]], {{.*}}): DW_OP_consts +7, DW_OP_stack_value, DW_OP_piece 0x8, DW_OP_consts +9, DW_OP_stack_value, DW_OP_piece 0x8)
; CHECK: DW_AT_name ("arg")

target triple = "x86_64-unknown-linux-gnu"

define void @later_scalar(i64 %arg) !dbg !4 {
  call void @checkpoint(), !dbg !8
    #dbg_value(i64 21, !7, !DIExpression(), !9)
  call void @checkpoint(), !dbg !9
  ret void, !dbg !9
}

define void @later_fragments(i64 %arg) !dbg !10 {
  call void @checkpoint(), !dbg !12
    #dbg_value(i64 21, !11, !DIExpression(DW_OP_LLVM_fragment, 0, 64), !13)
  call void @checkpoint(), !dbg !13
    #dbg_value(i64 23, !11, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !14)
  call void @checkpoint(), !dbg !14
  ret void, !dbg !14
}

define i64 @entry_register(i64 %arg) !dbg !15 {
    #dbg_value(i64 %arg, !16, !DIExpression(), !17)
  %sum = add i64 %arg, 1, !dbg !18
  ret i64 %sum, !dbg !18
}

; Vararg register saves can move an actual entry constant past the prologue.
; Its IR entry record still proves that it is valid at the function entry.
define void @entry_constant(i64 %arg, ...) !dbg !19 {
    #dbg_value(i64 7, !20, !DIExpression(), !21)
  call void @llvm.va_start(ptr undef), !dbg !22
  call void @checkpoint(), !dbg !22
  ret void, !dbg !22
}

define void @entry_null(ptr %arg, ...) !dbg !28 {
    #dbg_value(ptr null, !29, !DIExpression(), !30)
  call void @llvm.va_start(ptr undef), !dbg !31
  call void @checkpoint(), !dbg !31
  ret void, !dbg !31
}

define void @entry_fragments(i64 %arg) !dbg !23 {
    #dbg_value(i64 7, !24, !DIExpression(DW_OP_LLVM_fragment, 0, 64), !25)
    #dbg_value(i64 9, !24, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !25)
  call void @checkpoint(), !dbg !26
  ret void, !dbg !26
}

declare void @checkpoint()
declare void @llvm.va_start(ptr)

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "parameters.c", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "later_scalar", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !{})
!6 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!7 = !DILocalVariable(name: "arg", arg: 1, scope: !4, file: !1, line: 1, type: !6)
!8 = !DILocation(line: 2, column: 1, scope: !4)
!9 = !DILocation(line: 3, column: 1, scope: !4)
!10 = distinct !DISubprogram(name: "later_fragments", scope: !1, file: !1, line: 5, type: !5, scopeLine: 5, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!11 = !DILocalVariable(name: "arg", arg: 1, scope: !10, file: !1, line: 5, type: !27)
!12 = !DILocation(line: 6, column: 1, scope: !10)
!13 = !DILocation(line: 7, column: 1, scope: !10)
!14 = !DILocation(line: 8, column: 1, scope: !10)
!15 = distinct !DISubprogram(name: "entry_register", scope: !1, file: !1, line: 10, type: !5, scopeLine: 10, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!16 = !DILocalVariable(name: "arg", arg: 1, scope: !15, file: !1, line: 10, type: !6)
!17 = !DILocation(line: 10, column: 1, scope: !15)
!18 = !DILocation(line: 11, column: 1, scope: !15)
!19 = distinct !DISubprogram(name: "entry_constant", scope: !1, file: !1, line: 13, type: !5, scopeLine: 13, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!20 = !DILocalVariable(name: "arg", arg: 1, scope: !19, file: !1, line: 13, type: !6)
!21 = !DILocation(line: 13, column: 1, scope: !19)
!22 = !DILocation(line: 14, column: 1, scope: !19)
!23 = distinct !DISubprogram(name: "entry_fragments", scope: !1, file: !1, line: 16, type: !5, scopeLine: 16, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!24 = !DILocalVariable(name: "arg", arg: 1, scope: !23, file: !1, line: 16, type: !27)
!25 = !DILocation(line: 16, column: 1, scope: !23)
!26 = !DILocation(line: 17, column: 1, scope: !23)
!27 = !DIBasicType(name: "__int128", size: 128, encoding: DW_ATE_signed)
!28 = distinct !DISubprogram(name: "entry_null", scope: !1, file: !1, line: 19, type: !5, scopeLine: 19, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!29 = !DILocalVariable(name: "arg", arg: 1, scope: !28, file: !1, line: 19, type: !32)
!30 = !DILocation(line: 19, column: 1, scope: !28)
!31 = !DILocation(line: 20, column: 1, scope: !28)
!32 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !6, size: 64)
