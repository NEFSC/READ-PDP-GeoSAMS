del Results\*$3*.pdf
tsPerYear=13

if [ "$3" == "MA" ]
then
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_EBMS_MA_$1_$2', $1, 'MA', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_LAND_MA_$1_$2', $1, 'MA', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_LPUE_MA_$1_$2', $1, 'MA', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_RECR_MA_$1_$2', $1, 'MA', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_FEFF_MA_$1_$2', $1, 'MA', 1); exit;"

octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_EBMS_MA', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_LAND_MA', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_LPUE_MA', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_RECR_MA', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_FEFF_MA', $tsPerYear, $1); exit;"

else

octave "PlotLatLonGrid('Results/Lat_Lon_Grid_EBMS_GB_$1_$2', $1, 'GB', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_LAND_GB_$1_$2', $1, 'GB', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_LPUE_GB_$1_$2', $1, 'GB', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_RECR_GB_$1_$2', $1, 'GB', 1); exit;"
octave "PlotLatLonGrid('Results/Lat_Lon_Grid_FEFF_GB_$1_$2', $1, 'GB', 1); exit;"

octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_EBMS_GB', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_LAND_GB', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_LPUE_GB', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_RECR_GB', $tsPerYear, $1); exit;"
octave "PlotLatLonSurvey('Results/Lat_Lon_Surv_FEFF_GB', $tsPerYear, $1); exit;"
fi