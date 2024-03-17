@echo off
.\SRC\ScallopPopDensity.exe Scallop.cfg %1 %2 %3
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in ScallopPopDensity Stopping[0m
    exit /b
)

if "%3" == "MA" (
python .\PythonScripts\ProcessMAResults.py %1 %2
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in ProcessMAResults.py. Stopping[0m
    exit /b
)
) else (
python .\PythonScripts\ProcessGBResults.py %1 %2
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in ProcessGBResults.py. Stopping[0m
    exit /b
)
)

.\PlotAllByYear.bat %1 %2 %3
