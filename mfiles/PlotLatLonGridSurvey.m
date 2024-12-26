% Assumes that data is arranged as
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN
% Extension is added to gridFname in script
%
% Typically, the processing covers a span of years. But this script will handle a single year
% surveyFname: File name containing survey lat, lon and processed data
% gridFname:   File name containing lat, lon coordinates to which survey data was interpolated
% yrStart:     Starting year of span
% tsPerYear:   Number of time samples per year
% yrSelect:    Used to identify a single year

function PlotLatLonGridSurvey(surveyFname, gridFname, yrStart, tsPerYear, domain)

%------------------SHAPE DATA --------------------------------------------
shapeMA = shaperead('ShapeFiles/MAB_Region/MAB_Est_Areas_2024_UTM18_Habcam_GeoSAMS.shp');
shapeGB = shaperead('ShapeFiles/GB_Region/GB_Est_Areas_2024_UTM19_Habcam_GeoSAMS.shp');


isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui')
        surveyFname = cell2mat(arg_list(1));
        gridFname = cell2mat(arg_list(2));
        yrStart = str2num(cell2mat(arg_list(3)));
        tsPerYear = str2num(cell2mat(arg_list(4)));
        domain = cell2mat(arg_list(5));
    else
        yrStart = str2num(yrStart);
        tsPerYear = str2num(tsPerYear);
    end
end
units = ['Grid: ' GetUnits(gridFname)];

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
% Gather Grid Data
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% Expected name is in the form:
% Lat_Lon_Grid_FIELD_DN_YYST_YYSP
s=strfind(gridFname,'_');

if size(s,2) < 5
    % plotting a single year
    useTitle = gridFname;
else
    useTitle = gridFname(1:s(4));
end

if isOctave
    D=csvreadK([gridFname '.csv']);
else
    D=readtable([gridFname '.csv'],'NumHeaderLines',1);
    % remove NaN
    D=fillmissing(D,'constant',0.0);
end

[r, c]=size(D);
c = c - 5; % deduct for lat, lon, X, Y, Zone
grid=NaN(r,c); % pre-allocate

if isOctave
    latGrid=D(:,1);
    lonGrid=D(:,2);
    for k=1:c
        grid(:,k) = D(:,k+5);
    end
else
    latGrid=table2array(D(:,1));
    lonGrid=table2array(D(:,2));
    for k=1:c
        grid(:,k)=table2array(D(:,k+5)); % data starts in column 6
    end
end

edges = [0,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10];
if isOctave
    hh=histc(grid,edges);
    len = size(hh,1);
    h = zeros(1,len);
    for n=1:len
        h(n) = sum(hh(n,:));
    end
else
    h=histcounts(grid,edges);
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
    if grid(n,k)<=0 ; grid(n,k) = realmin(); end
    % saturate values
    if grid(n,k)> saturate; grid(n,k) = saturate; end
end
end

% Split MA
cutNS = 39.0;
% Split AJ
cutWE = -70.5;

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%----------------------------------------------------------------------------
% Gather Survey Data
%----------------------------------------------------------------------------
n=size(surveyFname,2);
param = surveyFname(n-6:n-3);

if isOctave
    D=csvreadK([surveyFname '.csv']);
else
    D=readtable([surveyFname '.csv']);
end
[rSurvey, cSurvey]=size(D);
cSurvey = cSurvey - 2; % deduct for lat, lon

% We are only going to plot annual data, so every tsPerYear columns
numCol = floor(cSurvey/tsPerYear) + 1;
survey=NaN(rSurvey,numCol); % pre-allocate

if isOctave
    latSurvey=D(:,1);
    lonSurvey=D(:,2);
    n=1;
    for k=1:tsPerYear:cSurvey
        if strcmp(param,'RECR')
            % Recruitment is precomputed and stored for each timestep
            % Need to sum for the year to get complete view of recruitment.
            % k = 1, 14, 27, ...
            % field(:,1) = (D(:,3));
            % field(:,2) =sum( (D(:,4:16));
            % field(:,3) =sum( (D(:,17:29));
            % ...
            if k == 1
                survey(:,n) = D(:,k+2);
            else
                survey(:,n) = sum(D(:,k-10:k+2),2);
            end
        else
            survey(:,n) = D(:,k+2);
        end
        n=n+1;
    end
else
    latSurvey=table2array(D(:,1));
    lonSurvey=table2array(D(:,2));
    n=1;
    for k=1:tsPerYear:cSurvey
        if strcmp(param,'RECR')
            % Recruitment is precomputed and stored for each timestep
            % Need to sum for the year to get complete view of recruitment.
            % k = 1, 14, 27, ...
            % field(:,1) =table2array(D(:,3));
            % field(:,2) =sum(table2array(D(:,4:16));
            % field(:,3) =sum(table2array(D(:,17:29));
            % ...
            if k == 1
                survey(:,n) = table2array(D(:,k+2));
            else
                survey(:,n) = sum(table2array(D(:,k-10:k+2)),2);
            end
        else
            survey(:,n) = table2array(D(:,k+2));
        end
        n=n+1;
    end
end

for k=1:numCol
for n=1:rSurvey
    % geoscatter does not accept 0.0, must be positive or NaN
    if survey(n,k)<=0 ; survey(n,k) = realmin(); end
end
end

% Split MA
if or(strcmp(domain, 'MA'), strcmp(domain, 'AL'))
    N = 0;
    S = 0;
    NE = 0;
    for n=1:rSurvey
        if lonSurvey(n) > cutWE
            NE = NE + 1;
            latSurvey_NE(NE) = latSurvey(n);
            lonSurvey_NE(NE) = lonSurvey(n);
            survey_NE(NE,:) = survey(n,:);
        elseif latSurvey(n) > cutNS
            N = N + 1;
            latSurvey_N(N) = latSurvey(n);
            lonSurvey_N(N) = lonSurvey(n);
            survey_N(N,:) = survey(n,:);
        else
            S = S + 1;
            latSurvey_S(S) = latSurvey(n);
            lonSurvey_S(S) = lonSurvey(n);
            survey_S(S,:) = survey(n,:);
        end
    end
end

%----------------------------------------------------------------------------

%
% Scale grid data proportional to survey data
%
if or(strcmp(domain, 'MA'), strcmp(domain, 'AL'))
    N = 0;
    S = 0;
    NE = 0;
    for n=1:r
        if lonGrid(n) > cutWE
            NE = NE + 1;
            latGrid_NE(NE) = latGrid(n);
            lonGrid_NE(NE) = lonGrid(n);
            grid_NE(NE,:) = grid(n,:);
        elseif latGrid(n) > cutNS
            N = N + 1;
            latGrid_N(N) = latGrid(n);
            lonGrid_N(N) = lonGrid(n);
            grid_N(N,:) = grid(n,:);
        else
            S = S + 1;
            latGrid_S(S) = latGrid(n);
            lonGrid_S(S) = lonGrid(n);
            grid_S(S,:) = grid(n,:);
        end
    end

    if strcmp(domain, 'AL')
        mxSurvey = max(survey_NE);
        mxGrid = max(grid_NE);
        mx_NE = mxGrid ./ mxSurvey;
        grid_NE = grid_NE ./ mx_NE;
    end

    mxSurvey = max(survey_N);
    mxGrid = max(grid_N);
    mx_N = mxGrid ./ mxSurvey;
    grid_N = grid_N ./ mx_N;

    mxSurvey = max(survey_S);
    mxGrid = max(grid_S);
    mx_S = mxGrid ./ mxSurvey;
    grid_S = grid_S ./ mx_S;
else
    mxSurvey = max(survey);
    mxGrid = max(grid);
    mx = mxGrid ./ mxSurvey;
    grid = grid ./ mx;
end


%
% Ready to plot
%

% plot figure for each year
for i=1:c
    year = yrStart + i - 1;

    if or(strcmp(domain, 'MA'), strcmp(domain, 'AL'))
        thisTitle = [useTitle int2str(year) '_MA_North'];
        unitStr = [units, ' X ', num2str(mx_N(i),4)];
        surveyLon = lonSurvey_N;
        surveyLat = latSurvey_N;
        surveyData = survey_N(:,i);
        gridLon = lonGrid_N;
        gridLat = latGrid_N;
        gridData = grid_N(:,i);
    else
        thisTitle = [useTitle int2str(year) ];
        unitStr = [units, ' X ', num2str(mx(i),4)];
        surveyLon = lonSurvey;
        surveyLat = latSurvey;
        surveyData = survey(:,i);
        gridLon = lonGrid;
        gridLat = latGrid;
        gridData = grid(:,i);
    end
    PlotGrid(domain, thisTitle, isOctave, surveyLon, surveyLat, surveyData, gridLon, gridLat, gridData, unitStr)
    PlotRegion(isOctave, 'MA_North', shapeMA, cutNS, 1)
    SetColorbar(isOctave)
    SizePaper(domain, isOctave)
    saveas(gcf,[thisTitle '.pdf'])
    if ~isOctave, saveas(gcf,['MFigures/' thisTitle '.fig']); end

    % Plot lower data
    if or(strcmp(domain, 'MA'), strcmp(domain, 'AL'))
        thisTitle = [useTitle int2str(year) '_MA_South'];
        PlotGrid(domain, thisTitle, isOctave, lonSurvey_S, latSurvey_S, survey_S(:,i), ...
            lonGrid_S, latGrid_S, grid_S(:,i), [units, ' X ', num2str(mx_S(i),4)])
        PlotRegion(isOctave, 'MA_South', shapeMA, cutNS, 1)
        SetColorbar(isOctave)
        SizePaper(domain, isOctave)
        saveas(gcf,[thisTitle '.pdf'])
        if ~isOctave, saveas(gcf,['MFigures/' thisTitle '.fig']); end
    end

    % Plot NE, i.e. GB data
    if strcmp(domain, 'AL')
        thisTitle = [useTitle int2str(year) '_GB'];
        PlotGrid(domain, thisTitle, isOctave, lonSurvey_NE, latSurvey_NE, survey_NE(:,i), ...
            lonGrid_NE, latGrid_NE, grid_NE(:,i), [units, ' X ', num2str(mx_NE(i),4)])
        PlotRegion(isOctave, 'GB', shapeGB, cutNS, 0)
        SetColorbar(isOctave)
        SizePaper(domain, isOctave)
        saveas(gcf,[thisTitle '.pdf'])
        if ~isOctave, saveas(gcf,['MFigures/' thisTitle '.fig']); end
    end
end % for i=1:c
end % function PlotLatLonGridSurvey

function  SizePaper(domain, isOctave)
p = gcf();
if isOctave
    if strcmp(domain, 'GB')
        set(p, 'papertype', "tabloid");
        set(p, 'papersize', [11 17]);
        set(p, 'paperorientation', "landscape");
    else
        set(p, 'papertype', "usletter");
        set(p, 'papersize', [8.5 11]);
        set(p, 'paperorientation', "portrait");
        set(p, 'paperposition', [.1 .1 8 10.5]);
    end
else
    if strcmp(domain, 'GB')
        p.PaperType  = "tabloid";
        p.PaperSize = [11 17];
        p.PaperOrientation = "landscape";
    else
        p.PaperType  = "usletter";
        p.PaperSize = [8.5 11];
        p.PaperOrientation = "portrait";
        p.PaperPosition = [.1 .1 8 10.5];
    end
end %if isOctave
end % function

function SetColorbar(isOctave)
if isOctave
    set(gca, 'color', [193 245 247]/255);     %RGB as a fraction
    c=cool(100);
else
    geobasemap bluegreen
    c=cool(100);
end
colormap(c);
colorbar;
end % function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function PlotGrid(domain, thisTitle, isOctave, ...
                  surveyLon, surveyLat, surveyData, ...
                  gridLon, gridLat, gridData, unitsStr)
gridPtSize = 3;
survPtSize = 12;
f = figure('Name', thisTitle);

if isOctave
    pSurvey = scatter(surveyLon, surveyLat, surveyData, surveyData, "filled");
    set(pSurvey, 'sizedata', survPtSize); % size of dots
    hold on

    pGrid = scatter(gridLon, gridLat, gridData, gridData, "filled");
    set(pGrid, 'sizedata', gridPtSize); % size of dots

    % enlarge figure
    figure(f,"position",get(0,"screensize"))
    daspect([1,cos(mean(gridLat)*pi/180)]);
else
    pSurvey = geoscatter(surveyLat, surveyLon, surveyData, surveyData, "filled");
    pSurvey.SizeData = survPtSize; % size of dots
    hold on

    pGrid = geoscatter(gridLat, gridLon, gridData, gridData, "filled");
    pGrid.SizeData = gridPtSize; % size of dots
    % enlarge figure
    if strcmp(domain, 'GB')
        f.OuterPosition = [1963.4 -221.4 1500 1087.2];
    else
        f.OuterPosition = [1963.4 -221.4 1000 1087.2];
    end
end
% correct scale when data is all 0
if max(gridData) == realmin(), clim([realmin(), 0.001]); end

% Using this approach as octave, axes properties does not support 'Subtitle'
title([thisTitle newline() unitsStr], 'Interpreter', 'none');
end
