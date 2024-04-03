% Assumes that data is arranged as
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN
% Extension is added to gridFname in script
function PlotLatLonGridSurvey(surveyFname, gridFname, yrStart, tsPerYear, domain)

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
% Gather Grid Data
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% Expected name is in the form:
% Lat_Lon_Grid_FIELD_DN_YYST_YYSP
s=strfind(gridFname,'_');
useTitle = gridFname(1:s(5));

D=readtable([gridFname '.csv'],"FileType","spreadsheet");

[r, c]=size(D);
c = c - 2; % deduct for lat, lon
grid=NaN(r,c); % pre-allocate

lat_g=table2array(D(:,1));
lon_g=table2array(D(:,2));
for k=1:c
    grid(:,k)=table2array(D(:,k+2));
end

edges = [0,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10];
h=histcounts(grid,edges);

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
    if grid(n,k)<=0 ; grid(n,k) = 1e-6; end
    % saturate values
    if grid(n,k)> saturate; grid(n,k) = saturate; end
end
end

% Split MA
cut = ceil((max(lat_g) + min(lat_g)) / 2);

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%----------------------------------------------------------------------------
% Gather Survey Data
%----------------------------------------------------------------------------
D=readtable([surveyFname '.csv'],"FileType","spreadsheet");
[r_s, c_s]=size(D);
c_s = c_s - 2; % deduct for lat, lon

% We are only going to plot annual data, so every tsPerYear columns
numCol = floor(c_s/tsPerYear) + 1;
survey=NaN(r_s,numCol); % pre-allocate

lat_s=table2array(D(:,1));
lon_s=table2array(D(:,2));
n=1;
for k=1:tsPerYear:c_s
    survey(:,n) =table2array(D(:,k+2));
    n=n+1;
end

for k=1:numCol
for n=1:r_s
    % geoscatter does not accept 0.0, must be positive or NaN
    if survey(n,k)<=0 ; survey(n,k) = 1e-6; end
end
end

% Split MA
if strcmp(domain, 'MA')
    u = 0;
    l = 0;
    for n=1:r_s
        if lat_s(n) > cut
            u = u + 1;
            lat_s_u(u) = lat_s(n);
            lon_s_u(u) = lon_s(n);
            survey_u(u,:) = survey(n,:);
        else
            l = l + 1;
            lat_s_l(l) = lat_s(n);
            lon_s_l(l) = lon_s(n);
            survey_l(l,:) = survey(n,:);
        end
    end
end

%----------------------------------------------------------------------------

%
% Scale grid data proportional to survey data
%
if strcmp(domain, 'MA')
    u = 0;
    l = 0;
    for n=1:r
        if lat_g(n) > cut
            u = u + 1;
            lat_g_u(u) = lat_g(n);
            lon_g_u(u) = lon_g(n);
            grid_u(u,:) = grid(n,:);
        else
            l = l + 1;
            lat_g_l(l) = lat_g(n);
            lon_g_l(l) = lon_g(n);
            grid_l(l,:) = grid(n,:);
        end
    end
    mx_s = max(survey_u);
    mx_g = max(grid_u);
    mx = mx_g ./ mx_s;
    grid_u = grid_u ./ mx;

    mx_s = max(survey_l);
    mx_g = max(grid_l);
    mx = mx_g ./ mx_s;
    grid_l = grid_l ./ mx;
else
    mx_s = max(survey);
    mx_g = max(grid);
    mx = mx_g ./ mx_s;
    grid = grid ./ mx;
end


%
% Ready to plot
%

% plot figure for each year
for i=1:c
    year = yrStart + i - 2;
   
    thisTitle = [useTitle int2str(year) '_' int2str(saturate)];

    if strcmp(domain, 'MA'); thisTitle = [thisTitle '_Upper']; end

    f = figure('Name', thisTitle);
    if strcmp(domain, 'MA')
        p_surv = geoscatter(lat_s_u, lon_s_u, survey_u(:,i), survey_u(:,i), "filled");
    else
        p_surv = geoscatter(lat_s, lon_s, survey(:,i), survey(:,i), "filled");
    end
    p_surv.SizeData = 10; % size of dots
    hold on

    if strcmp(domain, 'MA')
        p_grid = geoscatter(lat_g_u, lon_g_u, grid_u(:,i), grid_u(:,i), "filled");
    else
        p_grid = geoscatter(lat_g, lon_g, grid(:,i), grid(:,i), "filled");
    end
    p_grid.SizeData = 3; % size of dots
    title([useTitle int2str(year)], 'Interpreter', 'none');
    SetColorbar()
    SizePaper(domain)
    saveas(gcf,[thisTitle '.pdf'])

    % Plot lower data
    if strcmp(domain, 'MA')
        thisTitle = [useTitle int2str(year) '_' int2str(saturate) '_Lower'];

        f = figure('Name', thisTitle);
        p_surv = geoscatter(lat_s_l, lon_s_l, survey_l(:,i), survey_l(:,i), "filled");
        p_surv.SizeData = 10; % size of dots
        hold on
    
        p_grid = geoscatter(lat_g_l, lon_g_l, grid_l(:,i), grid_l(:,i), "filled");
        p_grid.SizeData = 3; % size of dots
        title([useTitle int2str(year) '_Lower'], 'Interpreter', 'none');
        SetColorbar()
        SizePaper(domain)
        saveas(gcf,[thisTitle '.pdf'])
    end
end
end % function PlotLatLonGridSurvey

function  SizePaper(domain)
p = gcf();
p.PaperType  = "tabloid";
if strcmp(domain, 'GB')
    p.PaperSize = [11 17];
    p.PaperOrientation = "landscape";
else
    p.PaperSize = [8.5 11];
    p.PaperOrientation = "portrait";
    p.PaperPosition = [.1 .1 8 10.5];
end
end % function

function SetColorbar()
geobasemap bluegreen;
c=hot(100);
colormap(c);
c = colorbar;
c.Label.String = "Field";
end % function