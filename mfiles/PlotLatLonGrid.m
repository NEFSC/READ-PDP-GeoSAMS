% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonGrid(fname, yrStart)
%cArray=['r', 'k', 'm', 'g', 'b', 'y', 'c'];
cArray=['y', 'b', 'r', 'g', 'm', 'k', 'c'];

D=readtable(fname,"FileType","spreadsheet");
[r, c]=size(D);
c = c - 2; % deduct for lat, lon
param=NaN(r,c); % pre-allocate

lat=table2array(D(:,1));
lon=table2array(D(:,2));
for k=1:c
    param(:,k) =table2array(D(:,k+2))/1000.;
end

% normalize the data
for k=1:c
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    % assume anything > 1e12 is an outlier
    if param(n,k)<=0 ; param(n,k) = 1e-6; end
    % saturate values
    if param(n,k)> 100; param(n,k) = 100; end
end
end
max(param,[],"all")
% scale data 0+ to 100
m=max(param,[],"all") / 100.;
param = param ./ m;
max(param,[],"all")

h=figure('Name',fname);

geoscatter(lat, lon, param(:,1), 'o', cArray(1));
hold on
for i=2:c
    geoscatter(lat, lon, param(:,i), 'o', cArray(i));
end
leg = {strcat(int2str(yrStart), ' start')};
for i=2:c
    leg{i} = int2str(yrStart+i-2);
end
legend(leg)
geobasemap streets
