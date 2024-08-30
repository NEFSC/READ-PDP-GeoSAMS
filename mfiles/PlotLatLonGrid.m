% Assumes that data is arranged as
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN
% Extension is added to fname in script
function PlotLatLonGrid(fname, yrStart, domain)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui');
        fname = cell2mat(arg_list(1));
        yrStart = str2num(cell2mat(arg_list(2)));
        domain = cell2mat(arg_list(3));
    end
end

% Expected name is in the form:
% Lat_Lon_Grid_FIELD_DN_YYST_YYSP
s=strfind(fname,'_');
useTitle = fname(1:s(5));

if isOctave
    D=csvreadK([fname '.csv']);
else
    D=readtable([fname '.csv'],"FileType","spreadsheet");
end
[r, c]=size(D);
c = c - 2; % deduct for lat, lon
field=NaN(r,c); % pre-allocate

if isOctave
    lat=D(:,1);
    lon=D(:,2);
    for k=1:c
        field(:,k) = D(:,k+2);
    end
else
    lat=table2array(D(:,1));
    lon=table2array(D(:,2));
    for k=1:c
        field(:,k) =table2array(D(:,k+2));
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

% looking for 90% of data
a=0.9 * r * c;
total = 0;
saturate = 1e10;
for n=1:size(h,2)
   total = total + h(n);
   if total > a
      saturate = 10^n;
      break
   end
end

%
for k=1:c
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    if field(n,k)<=0 ; field(n,k) = NaN; end
    % saturate values
    if field(n,k)> saturate; field(n,k) = saturate; end
end
end

for i=1:c
    year = yrStart + i - 2;
    thisTitle = [useTitle int2str(year) '_' int2str(saturate)];
    if isOctave
        f = figure('Name', thisTitle);
        s=scatter(lon, lat, field(:,i), field(:,i), "filled");
        %set(gca, 'color', [193 245 247]/255) %RGB as a fraction
        % however background color does not save to pdf
        set(s,'sizedata',5)

        c=cool(100);
        colormap(c);
        c = colorbar;

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
        f = figure('Name', thisTitle);
        s=geoscatter(lat, lon, field(:,i), field(:,i), "filled");
        geobasemap bluegreen;
        title([useTitle int2str(year)], 'Interpreter', 'none');
        s.SizeData = 5; % size of dots
        c=hot(100);
        colormap(c);
        c = colorbar;
        c.Label.String = "Field";
        % enlarge figure
        if strcmp(domain, 'GB')
            f.OuterPosition = [1963.4 -221.4 1500 1087.2];
        else
            f.OuterPosition = [1963.4 -221.4 1000 1087.2];
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
    end

    saveas(gcf,[thisTitle '.pdf'])
end

