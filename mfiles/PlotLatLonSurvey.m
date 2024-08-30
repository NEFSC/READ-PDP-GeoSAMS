% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN

function PlotLatLonSurvey(fname, tsPerYear, yearStart)
isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui');
        fname = cell2mat(arg_list(1));
        tsPerYear = str2num(cell2mat(arg_list(2)));
        yearStart = str2num(cell2mat(arg_list(3)));
    end
end

n=size(fname,2);
domain = fname(n-1:n);

if isOctave
    D=csvreadK([fname '.csv']);
else
    D=readtable([fname '.csv'],"FileType","spreadsheet");
end
[r, c]=size(D);
c = c - 2; % deduct for lat, lon

% We are only going to plot annual data, so every tsPerYear columns
numCol = floor(c/tsPerYear) + 1;
field=NaN(r,numCol); % pre-allocate

if isOctave
    lat=D(:,1);
    lon=D(:,2);
    n=1;
    for k=1:tsPerYear:c
        field(:,n) = D(:,k+2);
        n=n+1;
    end
else
    lat=table2array(D(:,1));
    lon=table2array(D(:,2));
    n=1;
    for k=1:tsPerYear:c
        field(:,n) =table2array(D(:,k+2));
        n=n+1;
    end
end
edges = [0,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10];
if isOctave
    hh=histc(field,edges);
    len = size(hh,1);
    h = zeros(1,len);
    for n=1:len
        h(n) = sum(hh(n,:));
    end
else
    h=histcounts(field,edges);
end
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
    if field(n,k)<=0 ; field(n,k) = NaN; end
    % saturate values
    if field(n,k)> saturate; field(n,k) = NaN; end
end
end

for i=1:numCol
	year = yearStart + i - 2;
    if isOctave
        f = figure('Name',[fname '_' int2str(year) '_' int2str(saturate)]);
        scatter(lon, lat, field(:,i), 'b');
        legend(int2str(year))
        % enlarge figure
        figure(f,"position",get(0,"screensize"))
        % now save it to pdf
        p = gcf();
        set(p, 'papersize', [11 17]);
        set(p, 'papertype', "tabloid");
        if strcmp(domain, 'GB')
            set(p, 'paperorientation', "landscape");
        else
            set(p, 'paperorientation', "portrait");
            set(p, 'paperposition', [.1 .1 10 16]);
        end
    else
        f = figure('Name',[fname '_' int2str(year) '_' int2str(saturate)]);
        s=geoscatter(lat, lon, field(:,i), field(:,i), "filled");
        geobasemap bluegreen;
        title([fname '_' int2str(year)], 'Interpreter', 'none');
        s.SizeData = 5; % size of dots
        c=hot(100);
        colormap(c);
        c = colorbar;
        c.Label.String = "Field";


        p = gcf();
        p.PaperSize = [11 17];
        p.PaperType  = "tabloid";
        if strcmp(domain, 'GB')
            p.PaperOrientation = "landscape";
        else
            p.PaperOrientation = "portrait";
            p.PaperPosition = [.1 .1 10 16]; 
        end
    end
	saveas(gcf,[fname int2str(year) '_' int2str(saturate) '.pdf'])
end

