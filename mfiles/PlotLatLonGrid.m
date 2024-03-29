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
        byYear = str2num(cell2mat(arg_list(4)));
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
    if field(n,k)<=0 ; field(n,k) = 1e-6; end
    % saturate values
    if field(n,k)> saturate; field(n,k) = saturate; end
end
end

if isOctave
    % Matlab is using colorbar so is more legible than varying circles
    % scale data 0+ to 50
    m = max(max(field)) / 50.; %using max(max()) to be compatible w/octave
    field = field ./ m;
    % data is ready now plot all years or individually
else
    N=saturate;
    % White to Magenta
    %Color1 = [ones(N,1), (N-1:-1:0)'/(N-1),ones(N,1)];
    % White to black
    %Color1 = [(N-1:-1:0)'/(N-1), (N-1:-1:0)'/(N-1), (N-1:-1:0)'/(N-1)];
    % Lime to Blue
    Color1 = [ones(N,1)/4, (N-1:-1:0)'/(N-1), 3*ones(N,1)/4];

    lat_t=array2table(lat,'VariableNames',{'Latitude'});
    lon_t=array2table(lon,'VariableNames',{'Longitude'});
end

for i=1:c
    year = yrStart + i - 2;
    if isOctave
        f = figure('Name',[useTitle int2str(year) '_' int2str(saturate)]);
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
        field_t=array2table(field(:,i),'VariableNames',{'Field'});
        tbl = [lat_t, lon_t, field_t]; 
        f = figure('Name',[useTitle int2str(year) '_' int2str(saturate)]);
        colormap(Color1)
        s=geoscatter(tbl,"Latitude", "Longitude", "filled");
        geobasemap streets
        s.SizeData = 20; % size of dots
        s.ColorVariable = "Field";
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

    saveas(gcf,[useTitle int2str(year) '_' int2str(saturate) '.pdf'])
end

