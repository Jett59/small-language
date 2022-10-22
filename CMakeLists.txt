cmake_minimum_required(VERSION 3.15)

project(small-language CXX C)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_BUILD_TYPE release)

set(CMAKE_CONFIGURATION_TYPES release;debug)

if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zi")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
endif()

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

add_executable(sl
    src/main.cpp
    src/lexer.cpp
    src/parser.cpp
    src/ast.cpp
    src/type.cpp
    src/codegen.cpp
)

set(llvm_arch_libraries ${LLVM_TARGETS_TO_BUILD})

set(required_llvm_libraries support core ${llvm_arch_libraries})

execute_process(COMMAND "${LLVM_TOOLS_BINARY_DIR}/llvm-config" --libnames ${required_llvm_libraries}
    OUTPUT_VARIABLE llvm_libs
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY
)
string(REPLACE " " ";" llvm_libs "${llvm_libs}")

message(STATUS "LLVM libs: \"${llvm_libs}\"")

execute_process(COMMAND "${LLVM_TOOLS_BINARY_DIR}/llvm-config" --libdir
    OUTPUT_VARIABLE llvm_libdir
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY
)

message(STATUS "LLVM libdir: \"${llvm_libdir}\"")

target_link_directories(sl PRIVATE "${llvm_libdir}")

target_link_libraries(sl PRIVATE ${llvm_libs})
