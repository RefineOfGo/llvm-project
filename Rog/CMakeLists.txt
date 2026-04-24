# ROG test suite – included as an LLVM subdirectory via llvm/test/CMakeLists.txt.
# All LLVM build-tree variables (LLVM_SOURCE_DIR, LLVM_BINARY_DIR, etc.) are
# inherited from the parent scope; no find_package needed.

set(ROG_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(ROG_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}")

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
  PATHS
  "LLVM_SOURCE_DIR"
  "LLVM_BINARY_DIR"
  "LLVM_TOOLS_DIR"
  "SHLIBDIR"
  "ROG_SOURCE_DIR"
  "ROG_BINARY_DIR"
  )

add_lit_testsuite(check-rog "Running the ROG regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  EXCLUDE_FROM_CHECK_ALL
  DEPENDS ${LLVM_TEST_DEPENDS_COMMON} opt
  )
set_target_properties(check-rog PROPERTIES FOLDER "LLVM/Tests")
