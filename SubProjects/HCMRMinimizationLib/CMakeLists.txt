cmake_minimum_required(VERSION 3.6)

set(HCMRMinimization_sources 
		HCMRMinimization.cpp
		HCMRSpectrum.cpp
		HCMRModel.cpp
		HCMRData.cpp
		HCMRParser.cpp
		HCMRPeekFinder.cpp)

set(HCMRMinimization_headers 
        common_definitions.h
		HCMRMinimization.h
		HCMRSpectrum.h
		HCMRModel.h
		HCMRData.h
		HCMRParser.h
		HCMRPeekFinder.h
		IPlotter.h
		IHCMRListener.h)

# add the executable
add_library(HCMRMinimizationLib ${HCMRMinimization_sources} ${HCMRMinimization_headers})
target_link_libraries(HCMRMinimizationLib PUBLIC loguru_logger)
target_link_libraries(HCMRMinimizationLib PUBLIC Minuit2)






