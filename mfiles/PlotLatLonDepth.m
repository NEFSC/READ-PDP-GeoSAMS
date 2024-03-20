% Reads in data From DNxyzLatLon and converts it to a table
% Then plots depth using color grid

function PlotLatLonDepth(fname)
D=readtable(fname,"FileType","spreadsheet");

lat=array2table(table2array(D(:,4)),'VariableNames',{'Latitude'});
lon=array2table(table2array(D(:,5)),'VariableNames',{'Longitude'});
depth=array2table(table2array(D(:,3)),'VariableNames',{'Depth'});
tbl = [lat, lon, depth];

%figure('Name',fname)
s=geoscatter(tbl,"Latitude", "Longitude", "filled");
hold on
geobasemap streets

s.SizeData = 20; % size of dots
s.ColorVariable = "Depth";
c = colorbar;
c.Label.String = "Depth";