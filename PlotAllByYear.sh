rm Results/*$3*.pdf
tsPerYear=13

gridList="Grid_EBMS Grid_LAND Grid_LPUE Grid_RECR Grid_FEFF "\
"Grid_Trend-EBMS Grid_Trend-LAND Grid_Trend-LPUE Grid_Trend-RECR Grid_Trend-FEFF"

survList="Surv_EBMS Surv_LAND Surv_LPUE Surv_RECR Surv_FEFF"

 for p in $gridList
 do
     echo Results/Lat_Lon_$p\_$3\_$1\_$2
     octave mfiles/PlotLatLonGrid.m Results/Lat_Lon_$p\_$3\_$1\_$2 $1 $3
     if [ $? != 0 ]; then
         echo [31m"PlotLatLonGrid('Results/Lat_Lon_$p_$3_$1_$2', $1, '$3')" Stopping[0m
         exit
     fi
 done

for p in $survList
do
    octave mfiles/PlotLatLonSurvey.m Results/Lat_Lon_$p\_$3 $tsPerYear $1
    if [ $? != 0 ]; then
        echo [31m"PlotLatLonSurvey('Results/Lat_Lon_$p_$3', $tsPerYear, $1)" Stopping[0m
        exit
fi
done
