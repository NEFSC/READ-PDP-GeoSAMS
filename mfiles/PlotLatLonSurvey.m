% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonSurvey(fname, tsPerYear, yearStart)

n=size(fname,2);
domain = fname(n-1:n);

D=readtable([fname '.csv'],"FileType","spreadsheet");
[r, c]=size(D);
c = c - 2; % deduct for lat, lon

% We are only going to plot annual data, so every tsPerYear columns
numCol = floor(c/tsPerYear) + 1;
param=NaN(r,numCol); % pre-allocate

lat=table2array(D(:,1));
lon=table2array(D(:,2));
n=1;
for k=1:tsPerYear:c
    param(:,n) =table2array(D(:,k+2));
    n=n+1;
end

edges = [0,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10];
h=histcounts(param,edges);
% looking for % of data
a=0.9 * r * numCol;
total = 0;
saturate = 1e10;
for n=1:size(h,2)
    total = total + h(n);
    if total > a
        saturate = 10^n;
        break
    end
end

% clear data limits of 0+ to saturate negative and zero data
for k=1:numCol
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    if param(n,k)<=0 ; param(n,k) = 1e-6; end
    % saturate values
    if param(n,k)> saturate; param(n,k) = 1e-6; end
end
end
% scale data 0+ to 50
m = max(max(param)) / 50.;
param = param ./ m;

for i=1:numCol
	year = yearStart + i - 2;
	h = figure('Name',[fname '_' int2str(year) '_' int2str(saturate)]);
    geoscatter(lat, lon, param(:,i), 'o', 'b');
	legend(int2str(year))
	geobasemap streets;

    % enlarge figure
    if strcmp(domain, 'GB')
		h.OuterPosition = [1963.4 -221.4 1500 1087.2];
    else
	    h.OuterPosition = [1963.4 -221.4 1000 1087.2];
    end

    p = gcf();
    p.PaperSize = [11 17];
    p.PaperType  = "tabloid";
    if strcmp(domain, 'GB')
        p.PaperOrientation = "landscape";
    else
        p.PaperOrientation = "portrait";
        p.PaperPosition = [.1 .1 10 16]; 
    end

	saveas(gcf,[fname int2str(year) '_' int2str(saturate) '.pdf'])
end

