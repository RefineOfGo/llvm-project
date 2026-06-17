add_llvm_component_library(LLVMRogTransforms
  ROGArgumentLoadFreezeOpt.cpp
  ROGBulkWriteBarrierDecompose.cpp
  ROGBulkWriteBarrierSimplify.cpp
  ROGGCWBSwitchOpt.cpp
  ROGGCWriteBarrierOpt.cpp
  ROGScalarWriteBarrierSimplify.cpp
  ROGWriteBarrierUtils.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms/Rog

  DEPENDS
  intrinsics_gen

  COMPONENT_NAME
  Rog

  LINK_COMPONENTS
  Analysis
  Core
  InstCombine
  Support
  )
