cmake_minimum_required(VERSION 3.15)

project(small-language CXX)

set(CMAKE_CXX_STANDARD 20)

add_executable(sl
src/main.cpp
src/lexer.cpp
)
