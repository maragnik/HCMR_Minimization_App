@echo OFF
set CURRENT_FOLDER=%~dp0
set "CURRENT_FOLDER=%CURRENT_FOLDER:\=/%"


set GENERATOR="Visual Studio 16 2019"
set PATH_TO_EXTERNALS=%CURRENT_FOLDER%Externals


echo.
echo.
echo ###############################
echo # Setting mantadory varables  #
echo ###############################
echo.
echo GENERATOR         = %GENERATOR%
echo PATH_TO_EXTERNALS = %PATH_TO_EXTERNALS%
echo.
cmake -G %GENERATOR% -D PATH_TO_EXTERNALS=%PATH_TO_EXTERNALS% -S "./" -B "./_build"
