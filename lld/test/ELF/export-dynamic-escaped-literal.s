# REQUIRES: x86
# These RUN lines rely on Unix command-line backslash/quote handling.
# UNSUPPORTED: system-windows
# RUN: llvm-mc -filetype=obj -triple=x86_64 %s -o %t.o
# RUN: ld.lld -pie %t.o -o %t \
# RUN:   --export-dynamic-symbol 'literal\*' \
# RUN:   --export-dynamic-symbol 'literal\?' \
# RUN:   --export-dynamic-symbol 'literal\[\]' \
# RUN:   --export-dynamic-symbol 'slash\\\*' \
# RUN:   --export-dynamic-symbol 'slashsep\\b\*' \
# RUN:   --export-dynamic-symbol 'tag\[\]"json"' \
# RUN:   --export-dynamic-symbol 'common\*' \
# RUN:   --export-dynamic-symbol 'undefined\*' \
# RUN:   --export-dynamic-symbol 'missing\*' \
# RUN:   --export-dynamic-symbol 'glob\[*' \
# RUN:   --export-dynamic-symbol 'class[ab]' \
# RUN:   --export-dynamic-symbol '"quoted*"'
# RUN: llvm-nm -D --just-symbol-name %t | FileCheck %s --match-full-lines

## Escaped metacharacters are literal, including backslashes and embedded
## quotes used in Go type metadata. A backslash is only a backslash, never also
## a path separator. Real globs and quoted exact patterns keep their existing
## semantics. Undefined symbols are not turned into exports.
# CHECK-NOT: {{.}}
# CHECK: classa
# CHECK-NEXT: classb
# CHECK-NEXT: common*
# CHECK-NEXT: glob[one
# CHECK-NEXT: glob[two
# CHECK-NEXT: literal*
# CHECK-NEXT: literal?
# CHECK-NEXT: literal[]
# CHECK-NEXT: quoted*
# CHECK-NEXT: slash\*
# CHECK-NEXT: slashsep\b*
# CHECK-NEXT: tag[]"json"
# CHECK-NOT: {{.}}

## Do not bypass validation when trying to decode escaped literals.
# RUN: not ld.lld -pie %t.o --export-dynamic-symbol 'literal\*[' -o /dev/null 2>&1 | FileCheck %s --check-prefix=BAD
# BAD: invalid glob pattern, unmatched '['

.globl _start
_start:
  ret

.globl "literal*", "literal?", "literal[]", "literalX"
"literal*":
"literal?":
"literal[]":
"literalX":
  ret

.globl "slash\\*", "tag[]\"json\""
"slash\\*":
"tag[]\"json\"":
  ret

.globl "slashsep\\b*", "slashsep/b*"
"slashsep\\b*":
"slashsep/b*":
  ret

.comm "common*", 8, 8
.globl "undefined*"

.globl "glob[one", "glob[two", classa, classb, classc
"glob[one":
"glob[two":
classa:
classb:
classc:
  ret

.globl "quoted*", quotedX
"quoted*":
quotedX:
  ret
