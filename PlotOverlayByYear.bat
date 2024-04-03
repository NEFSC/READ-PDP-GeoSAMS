@REM This scripts calls up each CSV file to have matlab plot the results and save to PDF
@REM At present the sim is running with 13 timestep per year. The Survey results have
@REM as saved at each timestep so the ts_per_year is used to plot annual results on the 
@REM survey data grid

@echo off
del Results\*%3*.pdf
set ts_per_year=13

@REM set gridList=EBMS LAND LPUE RECR Grid_FEFF ^
@REM Trend-EBMS Trend-LAND Trend-LPUE Trend-RECR Trend-FEFF
@REM set survList=EBMS LAND LPUE RECR FEFF

set gridList=EBMS LPUE RECR

for %%p in (%gridList%) do (
    matlab.exe -batch "PlotLatLonGridSurvey('Results/Lat_Lon_Surv_%%p_%3', 'Results/Lat_Lon_Grid_%%p_%3_%1_%2', %1, %ts_per_year%, '%3')
    if ERRORLEVEL 1 (
        @echo [31mError in MATLAB "PlotLatLonGridSurvey()" Stopping[0m
        exit /b
    )
)

