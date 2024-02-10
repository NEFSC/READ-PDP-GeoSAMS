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
for k=1:c
    param(:,k) =table2array(D(:,k+2));
end

% geoscatter does not accept 0.0, must be positive or NaN
for k=1:c
for n=1:r
    if param(n,k)==0; param(n,k)=1e-6;end
end
end

figure('Name',fname)
s=geoscatter(lat, lon, param(:,1), 'o', 'r');
hold on
s=geoscatter(lat, lon, param(:,14), 'o', 'black');
s=geoscatter(lat, lon, param(:,27), 'o', 'm');
s=geoscatter(lat, lon, param(:,40), 'o', 'g');
s=geoscatter(lat, lon, param(:,53), 'o', 'b');
legend('2005 start','2005', '2006','2007', '2008')
geobasemap streets
