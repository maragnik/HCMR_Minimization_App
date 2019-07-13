cmake_minimum_required(VERSION 3.6)

set(test_sources src/MyTest.cpp)

set(test_headers includes/MyTest.h)

# add the executable
add_library(HCMRMinimizationLibTest ${test_sources} ${test_headers})

target_link_libraries(HCMRMinimizationLibTest PUBLIC HCMRMinimizationLib)

target_include_directories (HCMRMinimizationLibTest PUBLIC includes)




