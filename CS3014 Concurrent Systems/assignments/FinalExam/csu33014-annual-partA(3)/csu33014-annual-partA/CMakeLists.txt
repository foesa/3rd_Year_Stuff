cmake_minimum_required(VERSION 3.16)
project(csu33014_annual_partA C)

set(CMAKE_C_STANDARD 11)

include_directories(.)

add_executable(csu33014_annual_partA
        csu33014-annual-partA-code.c
        csu33014-annual-partA-code.h
        csu33014-annual-partA-main.c)
