cmake_minimum_required(VERSION 3.6)

set(test_sources MyTest.cpp)
set(test_headers MyTest.h)

# add the executable
add_executable(HCMRMinimizationLib_Test ${test_sources} ${test_headers})

target_link_libraries(HCMRMinimizationLib_Test PUBLIC HCMRMinimizationLib) 




