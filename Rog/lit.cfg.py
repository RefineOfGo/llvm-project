# -*- Python -*-

# Configuration file for the 'lit' test runner for the Rog regression tests.
#
# This is intentionally minimal: the Rog tests only rely on `opt` and
# `FileCheck`, so we avoid pulling in the full LLVM substitution set (which
# otherwise produces a large amount of "Did not find ..." notes for tools that
# are not built for this suite).

import os

import lit.formats
from lit.llvm import llvm_config

# name: The name of this test suite.
config.name = "Rog"

# testFormat: The test format to use to interpret tests.
config.test_format = lit.formats.ShTest(not llvm_config.use_lit_shell)

# suffixes: A list of file extensions to treat as test files.
config.suffixes = [".ll"]

# excludes: A list of files/directories to exclude from the testsuite.
config.excludes = ["CMakeLists.txt"]

# test_source_root: The root path where tests are located.
config.test_source_root = os.path.dirname(__file__)

# test_exec_root: The root path where tests should be run.
config.test_exec_root = config.rog_obj_root

# Tweak the PATH to include the tools dir.
llvm_config.with_environment("PATH", config.llvm_tools_dir, append_path=True)

# Only register the tools actually used by the Rog tests.
tools = ["opt", "FileCheck"]
llvm_config.add_tool_substitutions(tools, config.llvm_tools_dir)
