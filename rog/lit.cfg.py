# -*- Python -*-

# Configuration file for the 'lit' test runner for the rog regression tests.
#
# Register the tools used by this suite without loading the complete LLVM
# test configuration.

import os

import lit.formats
from lit.llvm import llvm_config

# name: The name of this test suite.
config.name = "rog"

# testFormat: The test format to use to interpret tests.
config.test_format = lit.formats.ShTest(False)

# suffixes: A list of file extensions to treat as test files.
config.suffixes = [".ll", ".mir", ".s"]

# excludes: A list of files/directories to exclude from the testsuite.
config.excludes = ["CMakeLists.txt"]

# test_source_root: The root path where tests are located.
config.test_source_root = os.path.dirname(__file__)

# test_exec_root: The root path where tests should be run.
config.test_exec_root = config.rog_obj_root

# Tweak the PATH to include the tools dir.
llvm_config.with_environment("PATH", config.llvm_tools_dir, append_path=True)

# Only register the tools actually used by the rog tests.
tools = ["ld.lld", "llc", "llvm-mc", "llvm-readelf", "llvm-readobj", "opt", "FileCheck"]
llvm_config.add_tool_substitutions(tools, config.llvm_tools_dir)
