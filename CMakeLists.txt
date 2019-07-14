cmake_minimum_required(VERSION 3.6)

project(HCMR_Minimization)

# find Qt5 and required components
find_package(Qt5 COMPONENTS REQUIRED Core Gui Widgets PrintSupport)

# need this to include the auto generated ui_mainwindow.h
# file from the qt5_wrap_ui call below.
set(CMAKE_INCLUDE_CURRENT_DIR ON)

include_directories (HCMR_Minimization PUBLIC Sources)
include_directories (HCMR_Minimization PUBLIC SubProjects/HCMRMinimizationLib)

add_subdirectory(SubProjects/Minuit2)
add_subdirectory(SubProjects/HCMRMinimizationLib)
add_subdirectory(SubProjects/test)

set(project_sources 
		Sources/main.cpp 
		Sources/mainwindow.cpp 
		Sources/qcustomplot.cpp)
		
set(project_headers 
		Sources/mainwindow.h 
		Sources/qcustomplot.h)
		
set(project_ui Sources/mainwindow.ui)

# wrap the ui file to a c++ header
qt5_wrap_ui(ui_wrap ${project_ui})
qt5_wrap_cpp(moc_sources ${project_headers})

# add the executable
add_executable(HCMR_Minimization 
	${project_sources} 
	${project_headers}
	${ui_wrap} 
	${moc_sources})

target_link_libraries(HCMR_Minimization 
	PUBLIC
	Qt5::Core
	Qt5::Gui 
	Qt5::Widgets
	Qt5::PrintSupport
	HCMRMinimizationLib)

# auto copy dlls if on windows
if(WIN32)
	add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Core> $<TARGET_FILE_DIR:${PROJECT_NAME}>
		COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Gui> $<TARGET_FILE_DIR:${PROJECT_NAME}>
		COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Widgets> $<TARGET_FILE_DIR:${PROJECT_NAME}>
		COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::PrintSupport> $<TARGET_FILE_DIR:${PROJECT_NAME}> )
		
set_property(DIRECTORY PROPERTY VS_STARTUP_PROJECT HCMR_Minimization)
endif(WIN32)





