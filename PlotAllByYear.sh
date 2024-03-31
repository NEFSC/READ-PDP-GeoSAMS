rm Results/*$3*.pdf
tsPerYear=13

#gridList="Grid_EBMS Grid_LAND Grid_LPUE Grid_RECR Grid_FEFF "\
#"Grid_Trend-EBMS Grid_Trend-LAND Grid_Trend-LPUE Grid_Trend-RECR Grid_Trend-FEFF"
gridList="EBMS LPUE RECR "\
"Trend-EBMS Trend-LPUE Trend-RECR"

survList="EBMS LPUE RECR"

 for p in $gridList
 do
     echo Results/Lat_Lon_$p\_$3\_$1\_$2
     octave mfiles/PlotLatLonGrid.m Results/Lat_Lon_Grid_$p\_$3\_$1\_$2 $1 $3
     if [ $? != 0 ]; then
         echo [31m"PlotLatLonGrid('Results/Lat_Lon_Grid_$p_$3_$1_$2', $1, '$3')" Stopping[0m
         exit
     fi
 done

for p in $survList
do
    octave mfiles/PlotLatLonSurvey.m Results/Lat_Lon_Surv_$p\_$3 $tsPerYear $1
    if [ $? != 0 ]; then
        echo [31m"PlotLatLonSurvey('Results/Lat_Lon_Surv_$p_$3', $tsPerYear, $1)" Stopping[0m
        exit
fi
done
