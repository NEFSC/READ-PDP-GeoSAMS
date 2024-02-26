% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonData(fname)
D=readtable(fname,"FileType","spreadsheet");
[r, c]=size(D);
c = c - 2; % deduct for lat, lon
param=NaN(r,c); % pre-allocate

lat=table2array(D(:,1));
lon=table2array(D(:,2));
param=table2array(D(:,3));


% geoscatter does not accept 0.0, must be positive or NaN
for n=1:r
    if param(n)==0; param(n,k)=1e-6;end
end

figure('Name',fname)
s=geoscatter(lat, lon, param, '.', 'b');
hold on
geobasemap streets
