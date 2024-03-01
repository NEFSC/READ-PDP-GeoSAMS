% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonStrata(fname, C)
D=readtable(fname,"FileType","spreadsheet");
[r, c]=size(D);
c = c - 2; % deduct for lat, lon

lat=table2array(D(:,4));
lon=table2array(D(:,5));
strata=table2array(D(:,6)) / 100.;


%figure('Name',fname)
s=geoscatter(lat, lon, strata, '.', C);
hold on
geobasemap streets
