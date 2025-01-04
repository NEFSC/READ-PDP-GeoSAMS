%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Typical fileName: 'Lat_Lon_Grid_ABUN....'
%              or   'Lat_Lon_Survey_ABUN...'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function units = GetUnits(fileName)
% Called from PlotLatLonGridSurvey
%
    units ='';
    k=strfind(fileName,'ABUN');
    if k>0, units = 'scallops/m^2';end

    k=strfind(fileName,'BIOM');
    if k>0, units = 'g/m^2';end

    k=strfind(fileName,'EBMS');
    if k>0, units = 'metric tons';end

    k=strfind(fileName,'FEFF');
    if k>0, units = 'unitless';end

    k=strfind(fileName,'FMOR');
    if k>0, units = 'unitless';end

    k=strfind(fileName,'LAND');
    if k>0, units = 'scallops';end

    k=strfind(fileName,'LNDW');
    if k>0, units = 'grams';end

    k=strfind(fileName,'LPUE');
    if k>0, units = 'landings/day';end

    k=strfind(fileName,'RECR');
    if k>0, units = 'scallops/m^2';end
end
