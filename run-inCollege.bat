@echo off
setlocal
cd /d "%~dp0"

echo Compiling inCollege.cob...
cobc -x -o inCollege.exe inCollege.cob
if errorlevel 1 (
    echo.
    echo Compilation failed. The executable was not run.
    pause
    exit /b 1
)

echo.
echo Running inCollege.exe...
echo.
if "%~1"=="" (
    inCollege.exe
) else (
    inCollege.exe < "%~1"
)
set "exitCode=%errorlevel%"

echo.
pause
exit /b %exitCode%
