% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonGrid(fname, yrStart)
%cArray=['r', 'k', 'm', 'g', 'b', 'y', 'c'];
cArray=['b', 'y', 'm', 'g', 'c', 'r', 'k'];

D=readtable(fname,"FileType","spreadsheet");
[r, c]=size(D);
c = c - 2; % deduct for lat, lon
param=NaN(r,c); % pre-allocate

lat=table2array(D(:,1));
lon=table2array(D(:,2));
for k=1:c
    param(:,k) =table2array(D(:,k+2))/100.;
end

j = isoutlier(param);
% geoscatter does not accept 0.0, must be positive or NaN
for k=1:c
for n=1:r
    %if param(n,k)<=0 || param(n,k) > 100; param(n,k)=1e-6;end
    %if j(n,k) || param(n,k)<=0 || param(n,k) > 1000; param(n,k) = 1e-6; end
    if param(n,k)<=0 || param(n,k) > 100; param(n,k) = 1e-6; end
end
end

h=figure('Name',fname);
s=geoscatter(lat, lon, param(:,1), 'o', cArray(1));
hold on
for i=2:c
    s=geoscatter(lat, lon, param(:,i), 'o', cArray(i));
end
leg = {strcat(int2str(yrStart), ' start')};
for i=2:c
    leg{i} = int2str(yrStart+i-2);
end
legend(leg)
geobasemap streets
