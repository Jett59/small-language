cmake_minimum_required(VERSION 3.15)

project(small-language CXX C)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_BUILD_TYPE release)

if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zi")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
endif()

find_package(LLVM REQUIRED CONFIG)
find_package(BISON REQUIRED)

bison_target(parser src/parser.y "${CMAKE_CURRENT_BINARY_DIR}/parser.cc")

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

add_executable(sl
    src/main.cpp
    src/lexer.cpp
    "${CMAKE_CURRENT_BINARY_DIR}/parser.cc"
    src/ast.cpp
    src/type.cpp
    src/codegen.cpp
)

# Ref: https://stackoverflow.com/a/60890947/11553216
# /Zc:__cplusplus is required to make __cplusplus accurate
# /Zc:__cplusplus is available starting with Visual Studio 2017 version 15.7
# (according to https://docs.microsoft.com/en-us/cpp/build/reference/zc-cplusplus)
# That version is equivalent to _MSC_VER==1914
# (according to https://docs.microsoft.com/en-us/cpp/preprocessor/predefined-macros?view=vs-2019)
# CMake's ${MSVC_VERSION} is equivalent to _MSC_VER
# (according to https://cmake.org/cmake/help/latest/variable/MSVC_VERSION.html#variable:MSVC_VERSION)
if((MSVC) AND(MSVC_VERSION GREATER_EQUAL 1914))
    target_compile_options(sl PUBLIC "/Zc:__cplusplus")

    # Remove the many warnings we get in the LLVM headers.
    # Ref: https://groups.google.com/g/llvm-dev/c/ypIeUw2p274/m/mi6vzhfyDAAJ
    target_compile_options(sl PUBLIC "/experimental:external" "/external:anglebrackets" "/external:W0")
endif()

set(llvm_arch_libraries ${LLVM_TARGETS_TO_BUILD})

set(required_llvm_libraries support core passes ${llvm_arch_libraries})

message(STATUS "Using llvm-config: ${LLVM_TOOLS_BINARY_DIR}/llvm-config")

execute_process(COMMAND "${LLVM_TOOLS_BINARY_DIR}/llvm-config" --libnames ${required_llvm_libraries}
    OUTPUT_VARIABLE llvm_libs
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY
)
string(REPLACE " " ";" llvm_libs "${llvm_libs} ")

message(STATUS " LLVM libs: \"${llvm_libs}\" ")

execute_process(COMMAND "${LLVM_TOOLS_BINARY_DIR}/llvm-config" --libdir
    OUTPUT_VARIABLE llvm_libdir
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY
)

message(STATUS " LLVM libdir: \"${llvm_libdir}\" ")

target_link_directories(sl PRIVATE "${llvm_libdir}")

target_link_libraries(sl PRIVATE ${llvm_libs})

target_include_directories(sl PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/src"
    "${CMAKE_BINARY_DIR}"
)
