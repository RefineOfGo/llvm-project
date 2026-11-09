; RUN: opt -S -passes='function(declare-to-assign),default<O3>' %s -o %t.ll
; RUN: llc -O3 -filetype=obj %t.ll -o %t.o
; RUN: llvm-nm --defined-only %t.o > %t.dump
; RUN: llvm-dwarfdump --debug-info %t.o >> %t.dump
; RUN: FileCheck %s < %t.dump
;
; MemCpyOpt eliminates the middle 120-byte memcpy using its source memset. The
; assignment identity must follow the write so the initialized key stays available.
; In particular, its location at key_ready must not have a 120-byte empty piece.
; Earlier, partially initialized ranges may legitimately contain empty pieces.
; The zero-byte asm label identifies this exact program point without depending
; on instruction encodings or frame size when checking the DWARF location range.
;
; CHECK: [[#%.16x,READY:]] t key_ready
; CHECK: DW_TAG_subprogram
; CHECK: DW_AT_high_pc (0x[[#%.16x,END:]])
; CHECK: DW_AT_name ("bigkey")
; CHECK: DW_TAG_variable
; CHECK: DW_AT_location
; CHECK: [0x[[#%.16x,READY]], 0x[[#%.16x,END]]): DW_OP_breg7 RSP+0)
; CHECK-NEXT: DW_AT_name ("key")

target triple = "x86_64-unknown-linux-gnu"

define void @bigkey() !dbg !4 {
entry:
  %tmp = alloca [17 x i64], align 8
  %key = alloca [17 x i64], align 8
    #dbg_declare(ptr %key, !7, !DIExpression(), !10)
  call void @llvm.memset.p0.i64(ptr %tmp, i8 0, i64 136, i1 false), !dbg !10
  store i64 1, ptr %tmp, align 8, !dbg !11
  %last = getelementptr [17 x i64], ptr %tmp, i64 0, i64 16
  store i64 17, ptr %last, align 8, !dbg !11
  call void @llvm.memcpy.p0.p0.i64(ptr %key, ptr %tmp, i64 136, i1 false), !dbg !11
  call void asm sideeffect "key_ready:", "~{memory}"(), !dbg !12
  call void @consume(ptr %key), !dbg !12
  call void @checkpoint(), !dbg !13
  call void @consume(ptr %key), !dbg !13
  ret void, !dbg !13
}

declare void @llvm.memset.p0.i64(ptr captures(none) writeonly, i8, i64, i1 immarg)
declare void @llvm.memcpy.p0.p0.i64(ptr captures(none) writeonly, ptr captures(none) readonly, i64, i1 immarg)
declare void @consume(ptr captures(none) readonly) memory(argmem: read)
declare void @checkpoint() memory(inaccessiblemem: readwrite)

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "generic control", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "bigkey.c", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "bigkey", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !{})
!6 = !DIBasicType(name: "uint64_t", size: 64, encoding: DW_ATE_unsigned)
!7 = !DILocalVariable(name: "key", scope: !4, file: !1, line: 2, type: !8)
!8 = !DICompositeType(tag: DW_TAG_array_type, baseType: !6, size: 1088, elements: !9)
!9 = !{!14}
!10 = !DILocation(line: 2, column: 1, scope: !4)
!11 = !DILocation(line: 3, column: 1, scope: !4)
!12 = !DILocation(line: 4, column: 1, scope: !4)
!13 = !DILocation(line: 5, column: 1, scope: !4)
!14 = !DISubrange(count: 17)
