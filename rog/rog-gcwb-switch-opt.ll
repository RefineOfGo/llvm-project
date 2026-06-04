; RUN: opt -passes=rog-gcwb-switch-opt -S %s | FileCheck %s --check-prefix=OPT
; RUN: opt -passes=rog-gcwb-switch-opt -rog-gcwb-switch-opt-budget=3 -S %s | FileCheck %s --check-prefix=AGE
; RUN: opt -disable-output -debug-pass-manager -passes='lto<O1>' %s 2>&1 | FileCheck %s --check-prefix=LTO-O1-PM
; RUN: opt -disable-output -debug-pass-manager -passes='lto<O2>' %s 2>&1 | FileCheck %s --check-prefix=LTO-PM
; RUN: opt -disable-output -debug-pass-manager -passes='thinlto<O2>' %s 2>&1 | FileCheck %s --check-prefix=THINLTO-PM

; LTO-O1-PM: Running pass: ROGGCWBSwitchOptPass
; LTO-PM: Running pass: ROGGCWBSwitchOptPass
; THINLTO-PM: Running pass: ROGGCWBSwitchOptPass

@ROG_GCWB_SWITCH = external global i8

declare void @callee()

define i16 @same_block() gc "rog" {
; OPT-LABEL: define i16 @same_block(
; OPT: %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT-NOT: %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH
; OPT: %z1 = zext i8 %l0 to i16
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z0 = zext i8 %l0 to i16
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z1 = zext i8 %l1 to i16
  %sum = add i16 %z0, %z1
  ret i16 %sum
}

define i16 @prefer_nearest_same_block() gc "rog" {
; OPT-LABEL: define i16 @prefer_nearest_same_block(
; OPT: %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT-NOT: load atomic i8, ptr @ROG_GCWB_SWITCH
; OPT: %z1 = zext i8 %l0 to i16
; OPT-NOT: load atomic i8, ptr @ROG_GCWB_SWITCH
; OPT: %z2 = zext i8 %l0 to i16
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z0 = zext i8 %l0 to i16
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z1 = zext i8 %l1 to i16
  %l2 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z2 = zext i8 %l2 to i16
  %sum0 = add i16 %z0, %z1
  %sum1 = add i16 %sum0, %z2
  ret i16 %sum1
}

define i16 @too_old_load_is_kept() gc "rog" {
; AGE-LABEL: define i16 @too_old_load_is_kept(
; AGE: %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; AGE: %x1 = add i16 %x0, 1
; AGE: %x2 = add i16 %x1, 1
; AGE: %x3 = add i16 %x2, 1
; AGE: %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %x0 = zext i8 %l0 to i16
  %x1 = add i16 %x0, 1
  %x2 = add i16 %x1, 1
  %x3 = add i16 %x2, 1
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z1 = zext i8 %l1 to i16
  %sum = add i16 %x3, %z1
  ret i16 %sum
}

define i16 @branch_join(i1 %cond) gc "rog" {
; OPT-LABEL: define i16 @branch_join(
; OPT: %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT-NOT: %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH
; OPT: merge:
; OPT-NEXT: %z1 = zext i8 %l0 to i16
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  br i1 %cond, label %then, label %else

then:
  br label %merge

else:
  br label %merge

merge:
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z1 = zext i8 %l1 to i16
  ret i16 %z1
}

define i16 @call_on_path_blocks_reuse(i1 %cond) gc "rog" {
; OPT-LABEL: define i16 @call_on_path_blocks_reuse(
; OPT: %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT: call void @callee()
; OPT: %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT: %z1 = zext i8 %l1 to i16
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  br i1 %cond, label %then, label %else

then:
  call void @callee()
  br label %merge

else:
  br label %merge

merge:
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z1 = zext i8 %l1 to i16
  ret i16 %z1
}

define i16 @side_exit_call_does_not_block(i1 %cond) gc "rog" {
; OPT-LABEL: define i16 @side_exit_call_does_not_block(
; OPT: %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT: call void @callee()
; OPT-NOT: %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH
; OPT: %z1 = zext i8 %l0 to i16
entry:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  br i1 %cond, label %exit, label %cont

exit:
  call void @callee()
  ret i16 0

cont:
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z1 = zext i8 %l1 to i16
  ret i16 %z1
}

define i16 @non_dominating_load_kept(i1 %cond) gc "rog" {
; OPT-LABEL: define i16 @non_dominating_load_kept(
; OPT: then:
; OPT-NEXT: %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT: merge:
; OPT: %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
; OPT: %z1 = zext i8 %l1 to i16
entry:
  br i1 %cond, label %then, label %merge

then:
  %l0 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z0 = zext i8 %l0 to i16
  br label %merge

merge:
  %base = phi i16 [ %z0, %then ], [ 0, %entry ]
  %l1 = load atomic i8, ptr @ROG_GCWB_SWITCH seq_cst, align 1
  %z1 = zext i8 %l1 to i16
  %sum = add i16 %base, %z1
  ret i16 %sum
}
