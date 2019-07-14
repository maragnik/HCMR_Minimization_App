cmake_minimum_required(VERSION 3.6)

set(HCMRMinimization_sources 
		HCMRMinimization.cpp
		HCMRDatabase.cpp
		HCMRUnitaryInput.cpp)

set(HCMRMinimization_headers 
		HCMRMinimization.h
		HCMRDatabase.h
		HCMRUnitaryInput.h)

# add the executable
add_library(HCMRMinimizationLib ${HCMRMinimization_sources} ${HCMRMinimization_headers})
target_link_libraries(HCMRMinimizationLib PUBLIC Minuit2)






