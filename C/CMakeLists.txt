cmake_minimum_required(VERSION 3.27)
project(wacc-c C)

set(CMAKE_C_STANDARD 11)

include_directories(.)

add_executable(wacc-c
        main.c
        WACC.c
        WACC.h)
