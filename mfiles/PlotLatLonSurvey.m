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
    else
        yearStart = str2num(yearStart);
        tsPerYear = str2num(tsPerYear);
    end
end

n=size(fname,2);
domain = fname(n-1:n);
param = fname(n-6:n-3);

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
        if strcmp(param,'RECR')
            % Recruitment is precomputed and stored for each timestep
            % Need to sum for the year to get complete view of recruitment.
            % k = 1, 14, 27, ...
            % field(:,1) = D(:,3);
            % field(:,2) = sum(D(:,4:16));
            % field(:,3) = sum(D(:,17:29));
            % ...
            if k == 1
                field(:,n) = D(:,k+2);
            else
                field(:,n) = sum(D(:,k-10:k+2),2);
            end
        else
            field(:,n) = D(:,k+2);
        end
        n=n+1;
    end
else
    lat=table2array(D(:,1));
    lon=table2array(D(:,2));
    n=1;
    for k=1:tsPerYear:c
        if strcmp(param,'RECR')
            % Recruitment is precomputed and stored for each timestep
            % Need to sum for the year to get complete view of recruitment.
            % k = 1, 14, 27, ...
            % field(:,1) =table2array(D(:,3));
            % field(:,2) =sum(table2array(D(:,4:16));
            % field(:,3) =sum(table2array(D(:,17:29));
            % ...
            if k == 1
                field(:,n) = table2array(D(:,k+2));
            else
                field(:,n) = sum(table2array(D(:,k-10:k+2)),2);
            end
        else
            field(:,n) = table2array(D(:,k+2));
        end
        n=n+1;
    end
end
% clear data limits of 0+ to saturate negative and zero data
for k=1:numCol
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    if field(n,k)<=0 ; field(n,k) = 1e-99; end
end
end

for i=1:numCol
	year = yearStart + i - 2;
    if isOctave
        f = figure('Name',[fname '_' int2str(year) ]);
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
        f = figure('Name',[fname '_' int2str(year)]);
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
	saveas(gcf,[fname int2str(year) '.pdf'])
end

