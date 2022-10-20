cmake_minimum_required(VERSION 3.15)

project(small-language CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zi")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
endif()

add_executable(sl
src/main.cpp
src/lexer.cpp
src/parser.cpp
src/ast.cpp
src/type.cpp
)
