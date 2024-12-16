% Reads in data From fname and converts it to a table
% Then plots depth using color grid. Assumes
%  * latitude is in column 1
%  * longitude is in column 2
%  * col: user provides column wherein lies the data to be plotted
%  * tag: user provides a namve for the data
function PlotLatLonError(fname, col, tag)
D=readtable(fname,"FileType","spreadsheet");

lat=array2table(table2array(D(:,1)),'VariableNames',{'Latitude'});
lon=array2table(table2array(D(:,2)),'VariableNames',{'Longitude'});
err=array2table(table2array(D(:,col)),'VariableNames',{tag});
tbl = [lat, lon, err];

figure('Name',fname)
s=geoscatter(tbl,"Latitude", "Longitude", "filled");
hold on
geobasemap bluegreen

s.SizeData = 5; % size of dots
s.ColorVariable = tag;
c=hot(100);
colormap(c);
c = colorbar;
c.Label.String = tag;
title(fname, 'Interpreter','none'); % Interpreter, none is so that '_' are kept intact
