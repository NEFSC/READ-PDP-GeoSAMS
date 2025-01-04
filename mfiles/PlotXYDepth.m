% Reads in data From DNRegionGrid and converts it to a table
% Then plots depth using color grid

function PlotXYDepth(fname)
D=readtable(fname,"FileType","spreadsheet");

depth_t=array2table(table2array(D(:,3)),'VariableNames',{'Depth'});
lat_t = array2table(table2array(D(:,4)),'VariableNames',{'Longitude'});
lon_t = array2table(table2array(D(:,5)),'VariableNames',{'Latitude'});
tbl = [lat_t, lon_t, depth_t];

figure('Name',fname)
s=geoscatter(tbl,"Longitude", "Latitude", "filled");
hold on
geobasemap darkwater;
s.SizeData = 5; % size of dots
s.ColorVariable = "Depth";
c=hot(100);
colormap(c);
c = colorbar;
c.Label.String = "Depth";
