% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonStrata(fname, C)
D=readtable(fname,"FileType","spreadsheet");

lat=table2array(D(:,4));
lon=table2array(D(:,5));
strata=table2array(D(:,6)) / 100.;


%figure('Name',fname)
s=geoscatter(lat, lon, strata, '.', C);
hold on
geobasemap bluegreen
