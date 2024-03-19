@REM This scripts calls up each CSV file to have matlab plot the results and save to PDF
@REM At present the sim is running with 13 timestep per year. The Survey results have
@REM as saved at each timestep so the ts_per_year is used to plot annual results on the 
@REM survey data grid

@echo off
del Results\*%3*.pdf
set ts_per_year=13

set gridList=Grid_EBMS Grid_LAND Grid_LPUE Grid_RECR Grid_FEFF ^
Grid_Trend_EBMS Grid_Trend_LAND Grid_Trend_LPUE Grid_Trend_RECR Grid_Trend_FEFF

set survList=Surv_EBMS Surv_LAND Surv_LPUE Surv_RECR Surv_FEFF

for %%p in (%gridList%) do (
    matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_%%p_%3_%1_%2', %1, '%3', 1); exit;"
    if ERRORLEVEL 1 (
        @echo [31mError in MATLAB "PlotLatLonGrid('Results/Lat_Lon_%%p_%3_%1_%2', %1, '%3', 1)" Stopping[0m
        exit /b
    )
)

for %%p in (%survList%) do (
    matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_%%p_%3', %ts_per_year%, %1); exit;"
    if ERRORLEVEL 1 (
        @echo [31mError in MATLAB "PlotLatLonSurvey('Results/Lat_Lon_%%p_%3', %ts_per_year%, %1)" Stopping[0m
        exit /b
    )
)