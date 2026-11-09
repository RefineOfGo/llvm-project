; RUN: opt -passes=memcpyopt -verify-each -S %s | FileCheck %s --check-prefixes=SAME,OFFSET,DYNAMIC,SHORT,SHIFT,CALLSAME,CALLPART,CALLNOID,CALLSHARED,CONST

@zero_block = private constant [16 x i8] zeroinitializer

; A replacement that writes the same destination range performs the memcpy's
; assignment, not the preceding source memset's assignment. Keep their IDs
; distinct and check that the destination marker remains linked.
define void @same_range(ptr noalias %dst, ptr noalias %src) !dbg !10 {
; SAME-LABEL: define void @same_range(
; SAME: call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 16, i1 false), !DIAssignID ![[SAME_SRC:[0-9]+]]
; SAME-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[SAME_SRC]], ptr %src, !DIExpression(), !{{[0-9]+}})
; SAME-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 16, i1 false), !DIAssignID ![[SAME_DST:[0-9]+]]
; SAME-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[SAME_DST]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; SAME-NEXT: ret void
  call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 16, i1 false), !DIAssignID !13
    #dbg_assign(i1 poison, !11, !DIExpression(), !13, ptr %src, !DIExpression(), !15)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false), !DIAssignID !14
    #dbg_assign(i1 poison, !12, !DIExpression(), !14, ptr %dst, !DIExpression(), !15)
  ret void
}

; A positive offset into the initialized source does not change the destination
; range. Requiring equal source pointers would unnecessarily lose this link.
define void @positive_source_offset(ptr noalias %dst, ptr noalias %src) !dbg !20 {
; OFFSET-LABEL: define void @positive_source_offset(
; OFFSET: call void @llvm.memset.p0.i64(ptr %src, i8 7, i64 32, i1 false), !DIAssignID ![[OFFSET_SRC:[0-9]+]]
; OFFSET-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[OFFSET_SRC]], ptr %src, !DIExpression(), !{{[0-9]+}})
; OFFSET-NEXT: %offset = getelementptr i8, ptr %src, i64 8
; OFFSET-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 7, i64 16, i1 false), !DIAssignID ![[OFFSET_DST:[0-9]+]]
; OFFSET-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[OFFSET_DST]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; OFFSET-NEXT: ret void
  call void @llvm.memset.p0.i64(ptr %src, i8 7, i64 32, i1 false), !DIAssignID !23
    #dbg_assign(i1 poison, !21, !DIExpression(), !23, ptr %src, !DIExpression(), !25)
  %offset = getelementptr i8, ptr %src, i64 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %offset, i64 16, i1 false), !DIAssignID !24
    #dbg_assign(i1 poison, !22, !DIExpression(), !24, ptr %dst, !DIExpression(), !25)
  ret void
}

; The identical SSA length is sufficient even when it is not a constant.
define void @dynamic_same_range(ptr noalias %dst, ptr noalias %src, i64 %len) !dbg !30 {
; DYNAMIC-LABEL: define void @dynamic_same_range(
; DYNAMIC: call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 %len, i1 false), !DIAssignID ![[DYNAMIC_SRC:[0-9]+]]
; DYNAMIC-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[DYNAMIC_SRC]], ptr %src, !DIExpression(), !{{[0-9]+}})
; DYNAMIC-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 %len, i1 false), !DIAssignID ![[DYNAMIC_DST:[0-9]+]]
; DYNAMIC-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[DYNAMIC_DST]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; DYNAMIC-NEXT: ret void
  call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 %len, i1 false), !DIAssignID !33
    #dbg_assign(i1 poison, !31, !DIExpression(), !33, ptr %src, !DIExpression(), !35)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 %len, i1 false), !DIAssignID !34
    #dbg_assign(i1 poison, !32, !DIExpression(), !34, ptr %dst, !DIExpression(), !35)
  ret void
}

; The trailing four source bytes are undef, so the optimization may drop them.
; Keep the optimization, but do not claim the new 12-byte store performs the
; original full 16-byte assignment. Its marker must remain unlinked.
define void @shortened_range(ptr %dst) !dbg !40 {
; SHORT-LABEL: define void @shortened_range(
; SHORT-NEXT: %src = alloca [16 x i8], align 1
; SHORT-NEXT: call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 12, i1 false), !DIAssignID ![[SHORT_SRC:[0-9]+]]
; SHORT-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(DW_OP_LLVM_fragment, 0, 96), ![[SHORT_SRC]], ptr %src, !DIExpression(), !{{[0-9]+}})
; SHORT-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 12, i1 false){{$}}
; SHORT-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[SHORT_DST:[0-9]+]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; SHORT-NEXT: ret void
  %src = alloca [16 x i8], align 1
  call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 12, i1 false), !DIAssignID !43
    #dbg_assign(i1 poison, !41, !DIExpression(DW_OP_LLVM_fragment, 0, 96), !43, ptr %src, !DIExpression(), !45)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false), !DIAssignID !44
    #dbg_assign(i1 poison, !42, !DIExpression(), !44, ptr %dst, !DIExpression(), !45)
  ret void
}

; A negative source offset skips undef leading bytes by moving the destination.
; Every successful such conversion also shortens the constant copy length:
; NewSize = max(OldSize - DestinationShift, 0). A dynamic negative-offset case
; is rejected by the optimizer, so this path cannot isolate only the address
; guard while keeping the original nonzero length.
define void @shifted_range(ptr %dst) !dbg !50 {
; SHIFT-LABEL: define void @shifted_range(
; SHIFT-NEXT: %src = alloca [20 x i8], align 1
; SHIFT-NEXT: %initialized = getelementptr i8, ptr %src, i64 4
; SHIFT-NEXT: call void @llvm.memset.p0.i64(ptr %initialized, i8 0, i64 16, i1 false), !DIAssignID ![[SHIFT_SRC:[0-9]+]]
; SHIFT-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(DW_OP_LLVM_fragment, 32, 128), ![[SHIFT_SRC]], ptr %initialized, !DIExpression(), !{{[0-9]+}})
; SHIFT-NEXT: [[SHIFTED:%.*]] = getelementptr i8, ptr %dst, i64 4
; SHIFT-NEXT: call void @llvm.memset.p0.i64(ptr [[SHIFTED]], i8 0, i64 16, i1 false){{$}}
; SHIFT-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[SHIFT_DST:[0-9]+]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; SHIFT-NEXT: ret void
  %src = alloca [20 x i8], align 1
  %initialized = getelementptr i8, ptr %src, i64 4
  call void @llvm.memset.p0.i64(ptr %initialized, i8 0, i64 16, i1 false), !DIAssignID !53
    #dbg_assign(i1 poison, !51, !DIExpression(DW_OP_LLVM_fragment, 32, 128), !53, ptr %initialized, !DIExpression(), !55)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 20, i1 false), !DIAssignID !54
    #dbg_assign(i1 poison, !52, !DIExpression(), !54, ptr %dst, !DIExpression(), !55)
  ret void
}

; A writable destination and an alloca source used only by the memset and
; memcpy permit call-slot forwarding: retarget the original memset rather than
; creating a second one. The source assignment must become unlinked, while the
; equal-range destination assignment becomes linked to the retargeted store.
define void @callslot_same_range(ptr noalias writable dereferenceable(16) %dst) !dbg !70 {
; CALLSAME-LABEL: define void @callslot_same_range(
; CALLSAME-NEXT: %src = alloca [16 x i8], align 1
; CALLSAME-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 16, i1 false), !DIAssignID ![[CALLSAME_DST:[0-9]+]]
; CALLSAME-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[CALLSAME_SRC:[0-9]+]], ptr %src, !DIExpression(), !{{[0-9]+}})
; CALLSAME-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[CALLSAME_DST]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; CALLSAME-NEXT: ret void
  %src = alloca [16 x i8], align 1
  call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 16, i1 false), !DIAssignID !73
    #dbg_assign(i1 poison, !71, !DIExpression(), !73, ptr %src, !DIExpression(), !75)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false), !DIAssignID !74
    #dbg_assign(i1 poison, !72, !DIExpression(), !74, ptr %dst, !DIExpression(), !75)
  ret void
}

; Call-slot forwarding also accepts a shorter memset because the remaining
; source bytes were undef. Its 12-byte write cannot inherit the 16-byte memcpy
; assignment, and it no longer performs the original source assignment either.
define void @callslot_partial_range(ptr noalias writable dereferenceable(16) %dst) !dbg !80 {
; CALLPART-LABEL: define void @callslot_partial_range(
; CALLPART-NEXT: %src = alloca [16 x i8], align 1
; CALLPART-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 12, i1 false){{$}}
; CALLPART-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(DW_OP_LLVM_fragment, 0, 96), ![[CALLPART_SRC:[0-9]+]], ptr %src, !DIExpression(), !{{[0-9]+}})
; CALLPART-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[CALLPART_DST:[0-9]+]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; CALLPART-NEXT: ret void
  %src = alloca [16 x i8], align 1
  call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 12, i1 false), !DIAssignID !83
    #dbg_assign(i1 poison, !81, !DIExpression(DW_OP_LLVM_fragment, 0, 96), !83, ptr %src, !DIExpression(), !85)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false), !DIAssignID !84
    #dbg_assign(i1 poison, !82, !DIExpression(), !84, ptr %dst, !DIExpression(), !85)
  ret void
}

; Without a destination assignment ID, forwarding must still clear the source
; ID: the retargeted memset no longer initializes the named source variable.
define void @callslot_no_destination_id(ptr noalias writable dereferenceable(16) %dst) !dbg !90 {
; CALLNOID-LABEL: define void @callslot_no_destination_id(
; CALLNOID-NEXT: %src = alloca [16 x i8], align 1
; CALLNOID-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 16, i1 false){{$}}
; CALLNOID-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[CALLNOID_SRC:[0-9]+]], ptr %src, !DIExpression(), !{{[0-9]+}})
; CALLNOID-NEXT: ret void
  %src = alloca [16 x i8], align 1
  call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 16, i1 false), !DIAssignID !93
    #dbg_assign(i1 poison, !91, !DIExpression(), !93, ptr %src, !DIExpression(), !95)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false)
  ret void
}

; If source and destination share an ID, simply keeping it would also keep the
; obsolete source marker linked. Conservatively unlink the retargeted store
; without changing either marker's ID or address.
define void @callslot_shared_id(ptr noalias writable dereferenceable(16) %dst) !dbg !100 {
; CALLSHARED-LABEL: define void @callslot_shared_id(
; CALLSHARED-NEXT: %src = alloca [16 x i8], align 1
; CALLSHARED-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 16, i1 false){{$}}
; CALLSHARED-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[CALLSHARED_ID:[0-9]+]], ptr %src, !DIExpression(), !{{[0-9]+}})
; CALLSHARED-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[CALLSHARED_ID]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; CALLSHARED-NEXT: ret void
  %src = alloca [16 x i8], align 1
  call void @llvm.memset.p0.i64(ptr %src, i8 0, i64 16, i1 false), !DIAssignID !103
    #dbg_assign(i1 poison, !101, !DIExpression(), !103, ptr %src, !DIExpression(), !105)
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false), !DIAssignID !103
    #dbg_assign(i1 poison, !102, !DIExpression(), !103, ptr %dst, !DIExpression(), !105)
  ret void
}

; Copying a constant byte pattern is another memcpy-to-memset path with an
; unchanged destination range. It must preserve the destination assignment too.
define void @constant_source(ptr %dst) !dbg !120 {
; CONST-LABEL: define void @constant_source(
; CONST-NEXT: call void @llvm.memset.p0.i64(ptr %dst, i8 0, i64 16, i1 false), !DIAssignID ![[CONST_ID:[0-9]+]]
; CONST-NEXT: #dbg_assign(i1 poison, !{{[0-9]+}}, !DIExpression(), ![[CONST_ID]], ptr %dst, !DIExpression(), !{{[0-9]+}})
; CONST-NEXT: ret void
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr @zero_block, i64 16, i1 false), !DIAssignID !123
    #dbg_assign(i1 poison, !121, !DIExpression(), !123, ptr %dst, !DIExpression(), !125)
  ret void
}

declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!7, !8, !9}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "test", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "assignment-tracking-memcpy-to-memset.c", directory: "/")
!2 = !DISubroutineType(types: !3)
!3 = !{null}
!4 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!5 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 128, elements: !6)
!6 = !{!60}
!7 = !{i32 2, !"Dwarf Version", i32 4}
!8 = !{i32 2, !"Debug Info Version", i32 3}
!9 = !{i32 7, !"debug-info-assignment-tracking", i1 true}

!10 = distinct !DISubprogram(name: "same_range", scope: !1, file: !1, line: 1, type: !2, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!11 = !DILocalVariable(name: "src", scope: !10, file: !1, line: 1, type: !5)
!12 = !DILocalVariable(name: "dst", scope: !10, file: !1, line: 1, type: !5)
!13 = distinct !DIAssignID()
!14 = distinct !DIAssignID()
!15 = !DILocation(line: 1, column: 1, scope: !10)

!20 = distinct !DISubprogram(name: "positive_source_offset", scope: !1, file: !1, line: 2, type: !2, scopeLine: 2, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!21 = !DILocalVariable(name: "src", scope: !20, file: !1, line: 2, type: !61)
!22 = !DILocalVariable(name: "dst", scope: !20, file: !1, line: 2, type: !5)
!23 = distinct !DIAssignID()
!24 = distinct !DIAssignID()
!25 = !DILocation(line: 2, column: 1, scope: !20)

!30 = distinct !DISubprogram(name: "dynamic_same_range", scope: !1, file: !1, line: 3, type: !2, scopeLine: 3, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!31 = !DILocalVariable(name: "src", scope: !30, file: !1, line: 3, type: !5)
!32 = !DILocalVariable(name: "dst", scope: !30, file: !1, line: 3, type: !5)
!33 = distinct !DIAssignID()
!34 = distinct !DIAssignID()
!35 = !DILocation(line: 3, column: 1, scope: !30)

!40 = distinct !DISubprogram(name: "shortened_range", scope: !1, file: !1, line: 4, type: !2, scopeLine: 4, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!41 = !DILocalVariable(name: "src", scope: !40, file: !1, line: 4, type: !5)
!42 = !DILocalVariable(name: "dst", scope: !40, file: !1, line: 4, type: !5)
!43 = distinct !DIAssignID()
!44 = distinct !DIAssignID()
!45 = !DILocation(line: 4, column: 1, scope: !40)

!50 = distinct !DISubprogram(name: "shifted_range", scope: !1, file: !1, line: 5, type: !2, scopeLine: 5, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!51 = !DILocalVariable(name: "src", scope: !50, file: !1, line: 5, type: !64)
!52 = !DILocalVariable(name: "dst", scope: !50, file: !1, line: 5, type: !64)
!53 = distinct !DIAssignID()
!54 = distinct !DIAssignID()
!55 = !DILocation(line: 5, column: 1, scope: !50)

!60 = !DISubrange(count: 16)
!61 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 256, elements: !62)
!62 = !{!63}
!63 = !DISubrange(count: 32)
!64 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 160, elements: !65)
!65 = !{!66}
!66 = !DISubrange(count: 20)

!70 = distinct !DISubprogram(name: "callslot_same_range", scope: !1, file: !1, line: 6, type: !2, scopeLine: 6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!71 = !DILocalVariable(name: "src", scope: !70, file: !1, line: 6, type: !5)
!72 = !DILocalVariable(name: "dst", scope: !70, file: !1, line: 6, type: !5)
!73 = distinct !DIAssignID()
!74 = distinct !DIAssignID()
!75 = !DILocation(line: 6, column: 1, scope: !70)

!80 = distinct !DISubprogram(name: "callslot_partial_range", scope: !1, file: !1, line: 7, type: !2, scopeLine: 7, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!81 = !DILocalVariable(name: "src", scope: !80, file: !1, line: 7, type: !5)
!82 = !DILocalVariable(name: "dst", scope: !80, file: !1, line: 7, type: !5)
!83 = distinct !DIAssignID()
!84 = distinct !DIAssignID()
!85 = !DILocation(line: 7, column: 1, scope: !80)

!90 = distinct !DISubprogram(name: "callslot_no_destination_id", scope: !1, file: !1, line: 8, type: !2, scopeLine: 8, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!91 = !DILocalVariable(name: "src", scope: !90, file: !1, line: 8, type: !5)
!93 = distinct !DIAssignID()
!95 = !DILocation(line: 8, column: 1, scope: !90)

!100 = distinct !DISubprogram(name: "callslot_shared_id", scope: !1, file: !1, line: 9, type: !2, scopeLine: 9, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!101 = !DILocalVariable(name: "src", scope: !100, file: !1, line: 9, type: !5)
!102 = !DILocalVariable(name: "dst", scope: !100, file: !1, line: 9, type: !5)
!103 = distinct !DIAssignID()
!105 = !DILocation(line: 9, column: 1, scope: !100)

!120 = distinct !DISubprogram(name: "constant_source", scope: !1, file: !1, line: 11, type: !2, scopeLine: 11, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!121 = !DILocalVariable(name: "dst", scope: !120, file: !1, line: 11, type: !5)
!123 = distinct !DIAssignID()
!125 = !DILocation(line: 11, column: 1, scope: !120)

; Separate definition matches also reject accidentally merging the source and
; destination IDs while preserving both markers.
; SAME-DAG: ![[SAME_SRC]] = distinct !DIAssignID()
; SAME-DAG: ![[SAME_DST]] = distinct !DIAssignID()
; OFFSET-DAG: ![[OFFSET_SRC]] = distinct !DIAssignID()
; OFFSET-DAG: ![[OFFSET_DST]] = distinct !DIAssignID()
; DYNAMIC-DAG: ![[DYNAMIC_SRC]] = distinct !DIAssignID()
; DYNAMIC-DAG: ![[DYNAMIC_DST]] = distinct !DIAssignID()
; CALLSAME-DAG: ![[CALLSAME_SRC]] = distinct !DIAssignID()
; CALLSAME-DAG: ![[CALLSAME_DST]] = distinct !DIAssignID()
