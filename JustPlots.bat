@REM This script assumes that the execs are compiled and that the initial
@REM states and recruits have been extracted. 
@REM - It then runs the sim ScallopPopDensity
@REM - Interpolates the results: ProcessResults
@REM - Plots and saves the results to PDF: PlotAllByYear

@echo off
.\SRC\ScallopPopDensity.exe Scallop.cfg %1 %2 %3
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in ScallopPopDensity Stopping[0m
    exit /b
)

python .\PythonScripts\Process%3Results.py %1 %2
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in Process%3Results.py. Stopping[0m
    exit /b
)

.\PlotAllByYear.bat %1 %2 %3
