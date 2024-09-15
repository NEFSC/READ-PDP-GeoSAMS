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

function PlotLatLonGridSurvey(surveyFname, gridFname, yrStart, tsPerYear, domain, yrSelect)

%------------------SHAPE DATA --------------------------------------------
shapeMA = shaperead('ShapeFiles/MAB_Estimation_Areas_2024_UTM18_PDT.shp');
shapeGB = shaperead('ShapeFiles/GB_Estimation_Areas_2024_UTM19_PDT.shp');

shapeMAlen = length(shapeMA);
for k=1:shapeMAlen, [shapeMA(k).lat,shapeMA(k).lon] = utm2ll(shapeMA(k).X,shapeMA(k).Y,18); end

shapeGBlen = length(shapeGB);
for k=1:shapeGBlen, [shapeGB(k).lat,shapeGB(k).lon] = utm2ll(shapeGB(k).X,shapeGB(k).Y,19); end
%-------------------------------------------------------------------------

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
        yrSelect = str2num(yrSelect);
    end
end

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
    useTitle = gridFname(1:s(5));
end

if isOctave
    D=csvreadK([gridFname '.csv']);
else
    D=readtable([gridFname '.csv']);
    % remove NaN
    D=fillmissing(D,'constant',0.0);
end

[r, c]=size(D);
c = c - 2; % deduct for lat, lon
grid=NaN(r,c); % pre-allocate

if isOctave
    latGrid=D(:,1);
    lonGrid=D(:,2);
    for k=1:c
        grid(:,k) = D(:,k+2);
    end
else
    latGrid=table2array(D(:,1));
    lonGrid=table2array(D(:,2));
    for k=1:c
        grid(:,k)=table2array(D(:,k+2));
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
    if grid(n,k)<=0 ; grid(n,k) = NaN; end
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
    if survey(n,k)<=0 ; survey(n,k) = NaN; end
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
        mx = mxGrid ./ mxSurvey;
        grid_NE = grid_NE ./ mx;
    end

    mxSurvey = max(survey_N);
    mxGrid = max(grid_N);
    mx = mxGrid ./ mxSurvey;
    grid_N = grid_N ./ mx;

    mxSurvey = max(survey_S);
    mxGrid = max(grid_S);
    mx = mxGrid ./ mxSurvey;
    grid_S = grid_S ./ mx;
else
    mxSurvey = max(survey);
    mxGrid = max(grid);
    mx = mxGrid ./ mxSurvey;
    grid = grid ./ mx;
end


%
% Ready to plot
%
if c == 1
    % then grid file is only a single year
    % select offset for survey data
    offset = yrSelect - yrStart + 1;
else
    offset = 0;
end


% plot figure for each year
for i=1:c
    year = yrStart + i - 2 + offset;
    thisTitle = [useTitle int2str(year) '_' int2str(saturate)];
    if or(strcmp(domain, 'MA'), strcmp(domain, 'AL')); thisTitle = [thisTitle '_MA_North']; end

    if or(strcmp(domain, 'MA'), strcmp(domain, 'AL'))
        surveyLon = lonSurvey_N;
        surveyLat = latSurvey_N;
        if isOctave
            surveyData = survey_N(:,i+offset);
        else
            surveyData = survey_N(:,i);
        end
        gridLon = lonGrid_N;
        gridLat = latGrid_N;
        gridData = grid_N(:,i);
    else
        surveyLon = lonSurvey;
        surveyLat = latSurvey;
        if isOctave
            surveyData = survey(:,i+offset);
        else
            surveyData = survey(:,i);
        end
        gridLon = lonGrid;
        gridLat = latGrid;
        gridData = grid(:,i);
    end
    PlotGrid(domain, thisTitle, isOctave, surveyLon, surveyLat, surveyData, gridLon, gridLat, gridData)
    PlotRegion(shapeMA, shapeMAlen, 'MA_North', cutNS)
    if or(strcmp(domain, 'MA'), strcmp(domain, 'AL'))
        title([useTitle int2str(year) '_MA_North'], 'Interpreter', 'none');
    else
        title([useTitle int2str(year)], 'Interpreter', 'none');
    end
    SetColorbar(isOctave)
    SizePaper(domain, isOctave)
    saveas(gcf,[thisTitle '.pdf'])

    % Plot lower data
    if or(strcmp(domain, 'MA'), strcmp(domain, 'AL'))
        thisTitle = [useTitle int2str(year) '_' int2str(saturate) '_MA_South'];
        PlotGrid(domain, thisTitle, isOctave, lonSurvey_S, latSurvey_S, survey_S(:,i), lonGrid_S, latGrid_S, grid_S(:,i))
        PlotRegion(shapeMA, shapeMAlen, 'MA_South', cutNS)
        title([useTitle int2str(year) '_MA_South'], 'Interpreter', 'none');
        SetColorbar(isOctave)
        SizePaper(domain, isOctave)
        saveas(gcf,[thisTitle '.pdf'])
    end

    % Plot NE, i.e. GB data
    if strcmp(domain, 'AL')
        thisTitle = [useTitle int2str(year) '_' int2str(saturate) '_GB'];
        PlotGrid(domain, thisTitle, isOctave, lonSurvey_NE, latSurvey_NE, survey_NE(:,i), lonGrid_NE, latGrid_NE, grid_NE(:,i))
        PlotRegion(shapeGB, shapeGBlen, 'GB', cutNS)
        title([useTitle int2str(year) '_GB'], 'Interpreter', 'none');
        SetColorbar(isOctave)
        SizePaper(domain, isOctave)
        saveas(gcf,[thisTitle '.pdf'])
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
    c=rainbow(100);
else
    geobasemap bluegreen
    c=hot(100);
end
colormap(c);
colorbar;
end % function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function PlotGrid(domain, thisTitle, isOctave, surveyLon, surveyLat, surveyData, gridLon, gridLat, gridData)
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
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function PlotRegion(shape, shapeLen, region, cutNS)
if strcmp(region, 'GB')
    for k=1:shapeLen
        geoplot(shape(k).lat,shape(k).lon,'r');
        maxLon = max(shape(k).lon);
        minLon = min(shape(k).lon);
        maxLat = max(shape(k).lat);
        minLat = min(shape(k).lat);
        posnLon = (maxLon + minLon) / 2.0;
        posnLat = (maxLat + minLat) / 2.0;
        text(posnLat, posnLon, shape(k).SAMS,'Color',"#A2142F",'FontSize',8,'FontWeight', 'bold');
    end
else
    for k=1:shapeLen
        maxLon = -360.0;
        minLon = 360.0;
        maxLat = -360.0;
        minLat = 360.0;
        for n=1:length(shape(k).lat)-1
            if ...
            strcmp(region, 'MA_South') && shape(k).lat(n) < cutNS && shape(k).lat(n+1) < cutNS ...
            || ...
            strcmp(region, 'MA_North') && shape(k).lat(n) >= cutNS && shape(k).lat(n+1) >= cutNS
                geoplot(shape(k).lat(n:n+1),shape(k).lon(n:n+1),'r');
                if shape(k).lat(n) > maxLat, maxLat=shape(k).lat(n); end
                if shape(k).lat(n) < minLat, minLat=shape(k).lat(n); end
                if shape(k).lon(n) > maxLon, maxLon=shape(k).lon(n); end
                if shape(k).lon(n) < minLon, minLon=shape(k).lon(n); end
            end
        end
        %plot to edge, a straight line above cutNS to below cutNS
        if k==4
            if strcmp(region, 'MA_North')
                p1 = [shape(k).lat(89), cutNS];
                p2 = [shape(k).lon(89), cutNS-112];
                geoplot(p1, p2, 'r')
            else
                p1 = [shape(k).lat(91), cutNS];
                p2 = [shape(k).lon(91), cutNS-112];
                geoplot(p1, p2, 'r')
            end
            if p1(2) > maxLat, maxLat=p1(2); end
            if p1(2) < minLat, minLat=p1(2); end
            if p2(2) > maxLon, maxLon=p2(2); end
            if p2(2) < minLon, minLon=p2(2); end            
        end

        if maxLon + minLon ~= 0.0
            posnLon = (maxLon + minLon) / 2.0;
            posnLat = (maxLat + minLat) / 2.0;
            if length(shape(k).SAMS)>3
                text(posnLat-0.05, posnLon, shape(k).SAMS,'Color',"#A2142F",'FontSize',8,'FontWeight', 'bold');
            else
                text(posnLat, posnLon, shape(k).SAMS,'Color',"#A2142F",'FontSize',8,'FontWeight', 'bold');
            end
        end
    end
end
end % function
