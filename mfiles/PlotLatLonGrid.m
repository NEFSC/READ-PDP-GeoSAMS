% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN
% Extension is added to fname in script
function PlotLatLonGrid(fname, yrStart, domain)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

cArray=['y', 'b', 'r', 'g', 'm', 'k', 'c'];

if isOctave
	D=csvreadK([fname '.csv']);
else
	D=readtable([fname '.csv'],"FileType","spreadsheet");
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

edges = [0,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10];
h=histcounts(param,edges);
% looking for % of data
a=0.9 * r * c;
total = 0;
saturate = 1e10;
for n=1:size(h,2)
    total = total + h(n);
    if total > a
        saturate = 10^n
        break
    end
end

% clear data limits of 0+ to saturate negative and zero data
for k=1:c
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    % assume anything > 1e12 is an outlier
    if param(n,k)<=0 ; param(n,k) = 1e-6; end
    % saturate values
    if param(n,k)> saturate; param(n,k) = 1e-6; end
end
end
% scale data 0+ to 200
m = max(max(param)) / 200.;
param = param ./ m;
max(max(param))

h=figure('Name',[fname '_' int2str(saturate)]);

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

% enlarge figure
if strcmp(domain, 'GB')
    h.OuterPosition = [1963.4 -221.4 1500 1087.2];
else
    h.OuterPosition = [1963.4 -221.4 1000 1087.2];
end

saveas(gcf,[fname '_' int2str(saturate) '.pdf'])
