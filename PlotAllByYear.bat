del Results\*%3*.pdf
set ts_per_year=13

if "%3" == "MA" (
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_EBMS_MA_%1_%2', %1, 'MA', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_LAND_MA_%1_%2', %1, 'MA', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_LPUE_MA_%1_%2', %1, 'MA', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_RECR_MA_%1_%2', %1, 'MA', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_FEFF_MA_%1_%2', %1, 'MA', 1); exit;"

matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_EBMS_MA', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_LAND_MA', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_LPUE_MA', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_RECR_MA', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_FEFF_MA', %ts_per_year%, %1); exit;"

) else (

matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_EBMS_GB_%1_%2', %1, 'GB', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_LAND_GB_%1_%2', %1, 'GB', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_LPUE_GB_%1_%2', %1, 'GB', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_RECR_GB_%1_%2', %1, 'GB', 1); exit;"
matlab.exe -batch "PlotLatLonGrid('Results/Lat_Lon_Grid_FEFF_GB_%1_%2', %1, 'GB', 1); exit;"

matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_EBMS_GB', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_LAND_GB', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_LPUE_GB', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_RECR_GB', %ts_per_year%, %1); exit;"
matlab.exe -batch "PlotLatLonSurvey('Results/Lat_Lon_Surv_FEFF_GB', %ts_per_year%, %1); exit;"
)