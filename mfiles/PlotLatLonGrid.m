% Assumes that data is arranged as
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , pN
% Extension is added to fname in script
function PlotLatLonGrid(param, yrStart, yrEnd, dispNames)

%------------------SHAPE DATA --------------------------------------------
shapeMA = shaperead('ShapeFiles/MAB_Region/MAB_Est_Areas_2024_UTM18_Habcam_GeoSAMS.shp');
shapeGB = shaperead('ShapeFiles/GB_Region/GB_Est_Areas_2024_UTM19_Habcam_GeoSAMS.shp');

shapeMAlen = length(shapeMA);
for k=1:shapeMAlen, [shapeMA(k).lat,shapeMA(k).lon] = utm2ll(shapeMA(k).X,shapeMA(k).Y,18); end

shapeGBlen = length(shapeGB);
for k=1:shapeGBlen, [shapeGB(k).lat,shapeGB(k).lon] = utm2ll(shapeGB(k).X,shapeGB(k).Y,19); end

region=['GB';'MA'];

%-------------------------------------------------------------------------
isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui');
        param = cell2mat(arg_list(1));
        yrStart = str2num(cell2mat(arg_list(2)));
        yrEnd = str2num(cell2mat(arg_list(3)));
        dispNames = str2num(cell2mat(arg_list(4)));
    end
end

% Expected name is in the form:
% Lat_Lon_Grid_FEFF_AL2026_MA_REGION_KRIGE
for r=1:2
    domain = region(r,:);
    for year=yrStart:yrEnd
        fname = ['Results/Lat_Lon_Grid_',param,'AL', int2str(year), '_',domain, '_REGION_KRIGE.csv'];
        useTitle = [param, domain];

        if isOctave
            D=csvreadK(fname);
        else
            D=readtable(fname,"FileType","spreadsheet");
        end

        if isOctave
            lat=D(:,4);
            lon=D(:,5);
            field = D(:,17);
        else
            lat=table2array(D(:,4));
            lon=table2array(D(:,5));
            field=table2array(D(:,17));
        end

        %
        for n=1:size(field,1)
            % geoscatter does not accept 0.0, must be positive or NaN
            if field(n)<=0 ; field(n) = NaN; end
        end

        units = ['Grid: ' GetUnits(param)];
        thisTitle = [useTitle int2str(year)];
        f = figure();

        if isOctave
            s=scatter(lon, lat, field, field, "filled");
            % Using this approach as octave, axes properties does not support 'Subtitle'
            title([thisTitle newline() units], 'Interpreter', 'none');
            %set(gca, 'color', [193 245 247]/255) %RGB as a fraction
            % however background color does not save to pdf
            set(s,'sizedata',5)

            c=jet(500);
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
            s=geoscatter(lat, lon, field, field, "filled");
            % Using this approach as octave, axes properties does not support 'Subtitle'
            title([thisTitle newline() units], 'Interpreter', 'none');
            geobasemap darkwater;
            %title([useTitle int2str(year)], 'Interpreter', 'none');
            s.SizeData = 5; % size of dots
            c=jet(500);
            colormap(c);
            c = colorbar;
    %        c.Label.String = param;
            % enlarge figure
            if strcmp(domain, 'GB')
                % if multiple montitors, otherwise [0 0 ...]
                %f.OuterPosition = [1963.4 -221.4 1500 1087.2];
                f.OuterPosition = [0 0 1500 1087.2];
            else
%                f.OuterPosition = [1963.4 -221.4 1000 1087.2];
                f.OuterPosition = [0 0 1000 1087.2];
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
        hold on
        if strcmp(domain, 'GB')
            PlotRegion(isOctave, 'GB', shapeGB, 0, 0, dispNames)
        else
            PlotRegion(isOctave, 'MA', shapeMA, 0, 0, dispNames)
        end
        fname = ['Results/', thisTitle '.pdf'];
        saveas(gcf,fname)
    end
end
