cmake_minimum_required(VERSION 3.16)
project(csu33014_annual_partB C)

set(CMAKE_C_STANDARD 11)

include_directories(.)

add_executable(csu33014_annual_partB
        csu33014-annual-partB-code.c
        csu33014-annual-partB-code.h
        csu33014-annual-partB-main.c
        csu33014-annual-partB-person.c
        csu33014-annual-partB-person.h)
