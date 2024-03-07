% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonGrid(fname, yrStart)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

cArray=['y', 'b', 'r', 'g', 'm', 'k', 'c'];

if isOctave
	D=csvreadK(fname);
else
	D=readtable(fname,"FileType","spreadsheet");
end

[r, c]=size(D);
c = c - 2; % deduct for lat, lon
param=NaN(r,c); % pre-allocate

if isOctave
	lat=D(:,1);
	lon=D(:,2);
	for k=1:c
    	param(:,k) = D(:,k+2);
	end
else
	lat=table2array(D(:,1));
	lon=table2array(D(:,2));
	for k=1:c
    	param(:,k) =table2array(D(:,k+2));
	end
end

% normalize the data
for k=1:c
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    % assume anything > 1e12 is an outlier
    if param(n,k)<=0 ; param(n,k) = 1e-6; end
    % saturate values
    if param(n,k)> 1e8; param(n,k) = 1e8; end
end
end
max(max(param))
% scale data 0+ to 100
m = max(max(param)) / 200.;
param = param ./ m;
max(max(param))

figure('Name',fname);

if isOctave 
    scatter(lon, lat, param(:,1), cArray(1));
else
    geoscatter(lat, lon, param(:,1), 'o', cArray(1));
end
hold on
for i=2:c
    if isOctave
	    clr=cArray(i);
    	scatter(lon, lat, param(:,i), clr);
	else
    	geoscatter(lat, lon, param(:,i), 'o', cArray(i));
	end
end
leg = {strcat(int2str(yrStart), ' start')};
for i=2:c
    leg{i} = int2str(yrStart+i-2);
end
legend(leg)
if ~isOctave; geobasemap streets; end
