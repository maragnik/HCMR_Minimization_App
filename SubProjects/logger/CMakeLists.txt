cmake_minimum_required(VERSION 3.6)

set(logger_sources loguru.cpp)
set(logger_headers loguru.h)

# add the executable
add_library(loguru_logger ${logger_sources} ${logger_headers})






