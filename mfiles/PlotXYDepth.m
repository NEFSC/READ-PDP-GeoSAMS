% Reads in data From DNxyzLatLon and converts it to a table
% Then plots depth using color grid

function PlotXYDepth(fname)
D=readtable(fname,"FileType","spreadsheet");

X=table2array(D(:,2));
Y=table2array(D(:,3));
[lat, lon] = utm2ll(X,Y,19);
depth=array2table(table2array(D(:,5)),'VariableNames',{'Depth'});
lon = array2table(lon,'VariableNames',{'Longitude'});
lat = array2table(lat,'VariableNames',{'Latitude'});
tbl = [lat, lon, depth];

%figure('Name',fname)
s=geoscatter(tbl,"Latitude", "Longitude", "filled");
hold on
geobasemap darkwater;

s.SizeData = 5; % size of dots
s.ColorVariable = "Depth";
c=hot(100);
colormap(c);
c = colorbar;
c.Label.String = "Depth";