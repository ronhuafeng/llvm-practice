cmake_minimum_required(VERSION 3.4.3)

project(llvm-practice)

if("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  message(FATAL_ERROR "Do not set the build directory equal to the source directory!")
endif()

find_package(LLVM 8.0 REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in ${LLVM_DIR}")
message(STATUS "LLVM Root: ${LLVM_TOOLS_BINARY_DIR}")
message(STATUS "LLVM Include dirs: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM Definitions: ${LLVM_DEFINITIONS}")

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# add_executable(Hello src/hello.cpp)

add_executable(LLVMHello src/llvm-hello.cpp)

target_link_libraries(LLVMHello
  # LLVMX86AsmParser # MC, MCParser, Support, X86Desc, X86Info
  # LLVMX86Desc # MC, Support, X86AsmPrinter, X86Info
  # LLVMX86AsmPrinter # MC, Support, X86Utils
  # LLVMX86Info # MC, Support, Target
  # LLVMX86Utils # Core, Support
  # LLVMipo
  # LLVMScalarOpts
  # LLVMInstCombine
  # LLVMTransformUtils
  # LLVMipa
  # LLVMAnalysis
  # LLVMTarget
  # LLVMOption # Support
  # LLVMMCParser # MC, Support
  # LLVMMC # Object, Support
  # LLVMObject # BitReader, Core, Support
  # LLVMBitReader # Core, Support
  LLVMCore # Support
  # LLVMSupport
)
