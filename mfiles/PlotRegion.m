% Assumes a figure has already been opened
function PlotRegion(isOctave, region, shape, cutNS, split)

if strcmp(region, 'GB')
    shapeLen = length(shape);
    for k=1:shapeLen, [shape(k).lat,shape(k).lon] = utm2ll(shape(k).X,shape(k).Y,19); end
    for k=1:shapeLen
        if isOctave
            plot(shape(k).lon,shape(k).lat,'color','k');
        else
            geoplot(shape(k).lat,shape(k).lon,'r');
        end
        maxLon = max(shape(k).lon);
        minLon = min(shape(k).lon);
        maxLat = max(shape(k).lat);
        minLat = min(shape(k).lat);
        posnLon = (maxLon + minLon) / 2.0;
        posnLat = (maxLat + minLat) / 2.0;
        if isOctave
            text(posnLon, posnLat, shape(k).Zone,'Color','k','FontSize',10,'FontWeight', 'bold');
        else
            text(posnLat, posnLon, shape(k).Zone,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
        end
    end
else
    shapeLen = length(shape);
    for k=1:shapeLen, [shape(k).lat,shape(k).lon] = utm2ll(shape(k).X,shape(k).Y,18); end
    for k=1:shapeLen
        if split
            maxLon = -360.0;
            minLon = 360.0;
            maxLat = -360.0;
            minLat = 360.0;
            for n=1:length(shape(k).lat)-1
                if ...
                strcmp(region, 'MA_South') && shape(k).lat(n) < cutNS && shape(k).lat(n+1) < cutNS ...
                || ...
                strcmp(region, 'MA_North') && shape(k).lat(n) >= cutNS && shape(k).lat(n+1) >= cutNS
                    if isOctave
                        plot(shape(k).lon(n:n+1),shape(k).lat(n:n+1),'color','k');
                    else
                        geoplot(shape(k).lat(n:n+1),shape(k).lon(n:n+1),'r');
                    end
                    if shape(k).lat(n) > maxLat, maxLat=shape(k).lat(n); end
                    if shape(k).lat(n) < minLat, minLat=shape(k).lat(n); end
                    if shape(k).lon(n) > maxLon, maxLon=shape(k).lon(n); end
                    if shape(k).lon(n) < minLon, minLon=shape(k).lon(n); end
                end
            end
            %plot to edge, a straight line above cutNS to below cutNS
            if k==4
                %if strcmp(region, 'MA_North')
                %    p1 = [shape(k).lat(89), cutNS];
                %    p2 = [shape(k).lon(89), cutNS-112];
                %else
                %    p1 = [shape(k).lat(91), cutNS];
                %    p2 = [shape(k).lon(91), cutNS-112];
                %end
                %if isOctave
                %    plot(p2, p1, 'color', 'k')
                %else
                %    geoplot(p1, p2, 'r')
                %end
                %if p1(2) > maxLat, maxLat=p1(2); end
                %if p1(2) < minLat, minLat=p1(2); end
                %if p2(2) > maxLon, maxLon=p2(2); end
                %if p2(2) < minLon, minLon=p2(2); end
            end
    
            if maxLon + minLon ~= 0.0
                posnLon = (maxLon + minLon) / 2.0;
                posnLat = (maxLat + minLat) / 2.0;
                if isOctave
                    if length(shape(k).Zone)>3
                        text(posnLon, posnLat-0.05, shape(k).Zone,'Color','k','FontSize',10,'FontWeight', 'bold');
                    else
                        text(posnLon, posnLat, shape(k).Zone,'Color','k','FontSize',10,'FontWeight', 'bold');
                    end
                else
                    if length(shape(k).Zone)>3
                        text(posnLat-0.05, posnLon, shape(k).Zone,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
                    else
                        text(posnLat, posnLon, shape(k).Zone,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
                    end
                end
            end
        else
            if isOctave
                plot(shape(k).lon,shape(k).lat,'color','k');
            else
                geoplot(shape(k).lat,shape(k).lon,'r');
            end
            maxLon = max(shape(k).lon);
            minLon = min(shape(k).lon);
            maxLat = max(shape(k).lat);
            minLat = min(shape(k).lat);
            posnLon = (maxLon + minLon) / 2.0;
            posnLat = (maxLat + minLat) / 2.0;
            if isOctave
                text(posnLon, posnLat, shape(k).Zone,'Color','k','FontSize',10,'FontWeight', 'bold');
            else
                text(posnLat, posnLon, shape(k).Zone,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
            end
        end
    end % k=1:shapeLen
end % strcmp(region, 'GB')
end % function
