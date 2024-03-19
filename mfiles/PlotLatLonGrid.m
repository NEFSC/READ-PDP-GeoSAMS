% Assumes that data is arranged as
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN
% Extension is added to fname in script
function PlotLatLonGrid(fname, yrStart, domain, byYear)

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

cArray=['b', 'k', 'r', 'y', 'm', 'g', 'c'];

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
if isOctave
    hh=histc(param,edges);
    len = size(hh,1);
    h = zeros(1,len);
    for n=1:len
        h(n) = sum(hh(n,:));
    end
else
   h=histcounts(param,edges);
end

% looking for % of data
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


% clear data limits of 0+ to saturate negative and zero data
for k=1:c
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    if param(n,k)<=0 ; param(n,k) = 1e-6; end
    % saturate values
    if param(n,k)> saturate; param(n,k) = 1e-6; end
end
end
% scale data 0+ to 200
m = max(max(param)) / 50.;
param = param ./ m;

% data is ready now plot all years or individually

if byYear
    s=strfind(fname,'_');
    useTitle = fname(1:s(5));

    for i=1:c
        year = yrStart + i - 2;
        if isOctave
            f = figure('Name',[useTitle int2str(year) '_' int2str(saturate)]);
            scatter(lon, lat, param(:,i), 'b');
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
            f = figure('Name',[useTitle int2str(year) '_' int2str(saturate)]);
            geoscatter(lat, lon, param(:,i), 'o', 'b');
            legend(int2str(year))

            geobasemap streets;

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

else

    f=figure('Name',[fname '_' int2str(saturate)]);

    if isOctave
        figure(f,"position",get(0,"screensize"))
        scatter(lon, lat, param(:,1), cArray(1));
    else
        geoscatter(lat, lon, param(:,1), 'o', cArray(1));
    end
    hold on
    leg = {strcat(int2str(yrStart), ' start')};
    for i=2:c
        if isOctave
            clr=cArray(i);
            scatter(lon, lat, param(:,i), clr);
        else
            geoscatter(lat, lon, param(:,i), 'o', cArray(i));
        end
        leg{i} = int2str(yrStart+i-2);
    end
    legend(leg)

    if isOctave
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
        geobasemap streets;
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

    saveas(gcf,[fname '_' int2str(saturate) '.pdf'])
end % if byYear
