; RUN: opt -S -passes=dse %s | FileCheck %s
; RUN: opt -S -passes='dse,dse' %s | FileCheck %s
; RUN: opt -S -passes='default<O3>' %s | FileCheck %s --check-prefix=PIPELINE
; RUN: opt -passes='default<O3>' %s | llc -filetype=obj -o %t
; RUN: llvm-dwarfdump --debug-info %t | FileCheck %s --check-prefix=DWARF
;
; Full-pipeline lowering must retain the actual stack address, including scalar
; allocas. Promoting the address descriptor into the pointee value 21 is invalid.
; PIPELINE-LABEL: define rogcc void @scalar(
; PIPELINE: #dbg_declare(ptr %p,
; PIPELINE-SAME: !DIExpression(DW_OP_stack_value)
; PIPELINE: call void @checkpoint()
; DWARF: DW_AT_location (DW_OP_fbreg
; DWARF-SAME: DW_OP_stack_value)
; DWARF-NEXT: DW_AT_name ("__rog_memory_0")
;
; Source bytes from an eliminated store must not be presented as initialized
; memory. The range descriptor describes the real stack address, not its value.
target triple = "x86_64-unknown-linux-gnu"

; CHECK-LABEL: define rogcc void @scalar(
; CHECK: #dbg_declare(ptr %p, ![[ADDR:[0-9]+]], !DIExpression(DW_OP_stack_value),
; CHECK-NOT: store i64 20
; CHECK: #dbg_value(i8 0, ![[STATE:[0-9]+]], !DIExpression(),
; CHECK: #dbg_label(
; CHECK: call void @checkpoint()
; CHECK: store i64 21, ptr %p
; CHECK: call void @consume(ptr %p)
; CHECK: #dbg_label(
; CHECK: call void @llvm.lifetime.end
define rogcc void @scalar() !dbg !4 {
entry:
  %p = alloca i64, align 8
  store i64 20, ptr %p, align 8, !dbg !7
  call void @checkpoint(), !dbg !8
  store i64 21, ptr %p, align 8, !dbg !9
  call void @consume(ptr %p), !dbg !10
  call void @llvm.lifetime.end.p0(ptr %p)
  ret void, !dbg !10
}

; Non-ROG code must not acquire this debugger protocol.
; CHECK-LABEL: define void @ordinary(
; CHECK-NOT: #dbg_
; CHECK-NOT: store i64 20
; CHECK: call void @checkpoint()
; CHECK: store i64 21, ptr %p
; CHECK: ret void
define void @ordinary() !dbg !11 {
entry:
  %p = alloca i64, align 8
  store i64 20, ptr %p, align 8
  call void @checkpoint()
  store i64 21, ptr %p, align 8
  call void @consume(ptr %p)
  ret void
}

; Repeated invalidations of one range share its address and state variables.
; CHECK-LABEL: define rogcc void @repeated(
; CHECK: #dbg_declare(ptr %p, ![[REPEATADDR:[0-9]+]],
; CHECK-NOT: #dbg_declare
; CHECK: #dbg_value(i8 0, ![[REPEATSTATE:[0-9]+]],
; CHECK: call void @checkpoint()
; CHECK-NOT: #dbg_declare
; CHECK: #dbg_value(i8 0, ![[REPEATSTATE]],
; CHECK: call void @checkpoint()
; CHECK: store i64 21, ptr %p
; CHECK: ret void
define rogcc void @repeated() !dbg !12 {
entry:
  %p = alloca i64, align 8
  store i64 10, ptr %p, align 8
  call void @checkpoint()
  store i64 20, ptr %p, align 8
  call void @checkpoint()
  store i64 21, ptr %p, align 8
  call void @consume(ptr %p)
  ret void
}

; Even the ROG calling convention must not modify non-Go debug information.
; CHECK-LABEL: define rogcc void @foreign(
; CHECK-NOT: #dbg_
; CHECK: call void @checkpoint()
; CHECK: store i64 21, ptr %p
; CHECK: ret void
define rogcc void @foreign() !dbg !14 {
entry:
  %p = alloca i64, align 8
  store i64 20, ptr %p, align 8
  call void @checkpoint()
  store i64 21, ptr %p, align 8
  call void @consume(ptr %p)
  ret void
}

; A positive-only argument descriptor must not claim that a later store has
; executed. An inferred initializes summary is not an execution-time proof.
; CHECK-LABEL: define rogcc void @callee(
; CHECK: #dbg_value(ptr %p, ![[CALLEEADDR:[0-9]+]], !DIExpression(),
; CHECK-NOT: #dbg_value(i8 1
; CHECK: call void @checkpoint()
; CHECK: store i64 25, ptr %p
; CHECK: call void @consume(ptr %p)
define rogcc void @callee(ptr initializes((0, 8)) %p) !dbg !15 {
entry:
  call void @checkpoint()
  store i64 25, ptr %p, align 8
  call void @consume(ptr %p)
  ret void
}

; CHECK-LABEL: define rogcc void @unscoped_callee(
; CHECK: #dbg_value(ptr %p,
; CHECK-NOT: #dbg_value(i8 1
; CHECK: store i64 25
; CHECK: ret void
define rogcc void @unscoped_callee(ptr %p) !dbg !16 {
entry:
  store i64 25, ptr %p, align 8
  call void @consume(ptr %p)
  ret void
}

; A physical store of a potentially poison value is not an availability proof.
; CHECK-LABEL: define rogcc void @unknown_value_callee(
; CHECK-NOT: #dbg_
; CHECK: store i64 %value
; CHECK: ret void
define rogcc void @unknown_value_callee(ptr %p, i64 %value) !dbg !17 {
entry:
  store i64 %value, ptr %p
  call void @consume(ptr %p)
  ret void
}

declare void @checkpoint() memory(inaccessiblemem: readwrite)
declare void @consume(ptr) memory(argmem: read)
declare void @llvm.lifetime.end.p0(ptr)

; CHECK: ![[ADDR]] = distinct !DILocalVariable(name: "__rog_memory_0",
; CHECK-SAME: flags: DIFlagArtificial, annotations: ![[ADDRANN:[0-9]+]])
; CHECK: ![[ADDRANN]] = !{![[ADDRPAIR:[0-9]+]]}
; CHECK: ![[ADDRPAIR]] = !{!"ROGMemoryUnavailable", !"8"}
; CHECK: ![[STATE]] = distinct !DILocalVariable(name: "__rog_memory_0.state",
; CHECK-SAME: flags: DIFlagArtificial, annotations: ![[STATEANN:[0-9]+]])
; CHECK: ![[STATEANN]] = !{![[STATEPAIR:[0-9]+]]}
; CHECK: ![[STATEPAIR]] = !{!"ROGMemoryState", !"__rog_memory_0"}

; CHECK: ![[CALLEEADDR]] = distinct !DILocalVariable(name: "__rog_memory_available_0",
; CHECK-SAME: annotations: ![[CALLEEANN:[0-9]+]])
; CHECK: ![[CALLEEANN]] = !{![[AVAILABLE:[0-9]+]],
; CHECK: ![[AVAILABLE]] = !{!"ROGMemoryAvailable", !"8"}

!llvm.dbg.cu = !{!0, !13}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_Go, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "memory.go", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "scalar", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = !DILocation(line: 2, column: 1, scope: !4)
!8 = !DILocation(line: 3, column: 1, scope: !4)
!9 = !DILocation(line: 4, column: 1, scope: !4)
!10 = !DILocation(line: 5, column: 1, scope: !4)
!11 = distinct !DISubprogram(name: "ordinary", scope: !1, file: !1, line: 10, type: !5, scopeLine: 10, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!12 = distinct !DISubprogram(name: "repeated", scope: !1, file: !1, line: 20, type: !5, scopeLine: 20, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!13 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!14 = distinct !DISubprogram(name: "foreign", scope: !1, file: !1, line: 30, type: !5, scopeLine: 30, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !13)
!15 = distinct !DISubprogram(name: "callee", scope: !1, file: !1, line: 40, type: !5, scopeLine: 40, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!16 = distinct !DISubprogram(name: "unscoped_callee", scope: !1, file: !1, line: 50, type: !5, scopeLine: 50, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!17 = distinct !DISubprogram(name: "unknown_value_callee", scope: !1, file: !1, line: 60, type: !5, scopeLine: 60, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
