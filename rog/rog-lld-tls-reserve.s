# RUN: llvm-mc -filetype=obj -triple=x86_64-unknown-linux %s -o %t.o
# RUN: ld.lld %t.o -o %t.no-reserve -e _start
# RUN: llvm-readelf -S -l %t.no-reserve | FileCheck %s --check-prefix=NORESERVE
# RUN: llvm-mc -filetype=obj -triple=x86_64-unknown-linux -defsym ROG_MARKER=1 %s -o %t.marker.o
# RUN: ld.lld %t.marker.o -o %t.reserve -e _start
# RUN: llvm-readelf -S -l %t.reserve | FileCheck %s --check-prefix=RESERVE
# RUN: llvm-mc -filetype=obj -triple=x86_64-unknown-freebsd -defsym ROG_MARKER=1 %s -o %t.freebsd.o
# RUN: ld.lld %t.freebsd.o -o %t.freebsd -e _start
# RUN: llvm-readelf -S -l %t.freebsd | FileCheck %s --check-prefix=NORESERVE

# NORESERVE:      .tbss
# NORESERVE-SAME: 000008
# NORESERVE:      Program Headers:
# NORESERVE:      TLS
# NORESERVE-SAME: 0x000000 0x000008

# RESERVE:      .tbss
# RESERVE-SAME: 000028
# RESERVE:      Program Headers:
# RESERVE:      TLS
# RESERVE-SAME: 0x000000 0x000028

.section .text,"ax",@progbits
.globl _start
_start:
  retq

.section .tbss,"awT",@nobits
.p2align 3
tls_byte:
  .zero 8

.ifdef ROG_MARKER
.weak __rog_lld_tls_reserve
.hidden __rog_lld_tls_reserve
.set __rog_lld_tls_reserve, 0
.endif
