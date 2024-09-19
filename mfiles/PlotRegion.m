% Assumes a figure has already been opened
function PlotRegion(isOctave, region, cutNS, split)

if strcmp(region, 'GB')
    shapeGB = shaperead('ShapeFiles/GB_Estimation_Areas_2024_UTM19_PDT.shp');
    shapeGBlen = length(shapeGB);
    for k=1:shapeGBlen, [shapeGB(k).lat,shapeGB(k).lon] = utm2ll(shapeGB(k).X,shapeGB(k).Y,19); end
    for k=1:shapeGBlen
        if isOctave
            plot(shapeGB(k).lon,shapeGB(k).lat,'color','k');
        else
            geoplot(shapeGB(k).lat,shapeGB(k).lon,'r');
        end
        maxLon = max(shapeGB(k).lon);
        minLon = min(shapeGB(k).lon);
        maxLat = max(shapeGB(k).lat);
        minLat = min(shapeGB(k).lat);
        posnLon = (maxLon + minLon) / 2.0;
        posnLat = (maxLat + minLat) / 2.0;
        if isOctave
            text(posnLon, posnLat, shapeGB(k).NewSAMS,'Color','k','FontSize',10,'FontWeight', 'bold');
        else
            text(posnLat, posnLon, shapeGB(k).NewSAMS,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
        end
    end
else
    shapeMA = shaperead('ShapeFiles/MAB_Estimation_Areas_2024_UTM18_PDT.shp');
    shapeMAlen = length(shapeMA);
    for k=1:shapeMAlen, [shapeMA(k).lat,shapeMA(k).lon] = utm2ll(shapeMA(k).X,shapeMA(k).Y,18); end
    for k=1:shapeMAlen
        if split
            maxLon = -360.0;
            minLon = 360.0;
            maxLat = -360.0;
            minLat = 360.0;
            for n=1:length(shapeMA(k).lat)-1
                if ...
                strcmp(region, 'MA_South') && shapeMA(k).lat(n) < cutNS && shapeMA(k).lat(n+1) < cutNS ...
                || ...
                strcmp(region, 'MA_North') && shapeMA(k).lat(n) >= cutNS && shapeMA(k).lat(n+1) >= cutNS
                    if isOctave
                        plot(shapeMA(k).lon(n:n+1),shapeMA(k).lat(n:n+1),'color','k');
                    else
                        geoplot(shapeMA(k).lat(n:n+1),shapeMA(k).lon(n:n+1),'r');
                    end
                    if shapeMA(k).lat(n) > maxLat, maxLat=shapeMA(k).lat(n); end
                    if shapeMA(k).lat(n) < minLat, minLat=shapeMA(k).lat(n); end
                    if shapeMA(k).lon(n) > maxLon, maxLon=shapeMA(k).lon(n); end
                    if shapeMA(k).lon(n) < minLon, minLon=shapeMA(k).lon(n); end
                end
            end
            %plot to edge, a straight line above cutNS to below cutNS
            if k==4
                if strcmp(region, 'MA_North')
                    p1 = [shapeMA(k).lat(89), cutNS];
                    p2 = [shapeMA(k).lon(89), cutNS-112];
                else
                    p1 = [shapeMA(k).lat(91), cutNS];
                    p2 = [shapeMA(k).lon(91), cutNS-112];
                end
                if isOctave
                    plot(p2, p1, 'color', 'k')
                else
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
                if isOctave
                    if length(shapeMA(k).NewSAMS)>3
                        text(posnLon, posnLat-0.05, shapeMA(k).NewSAMS,'Color','k','FontSize',10,'FontWeight', 'bold');
                    else
                        text(posnLon, posnLat, shapeMA(k).NewSAMS,'Color','k','FontSize',10,'FontWeight', 'bold');
                    end
                else
                    if length(shapeMA(k).NewSAMS)>3
                        text(posnLat-0.05, posnLon, shapeMA(k).NewSAMS,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
                    else
                        text(posnLat, posnLon, shapeMA(k).NewSAMS,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
                    end
                end
            end
        else
            if isOctave
                plot(shapeMA(k).lon,shapeMA(k).lat,'color','k');
            else
                geoplot(shapeMA(k).lat,shapeMA(k).lon,'r');
            end
            maxLon = max(shapeMA(k).lon);
            minLon = min(shapeMA(k).lon);
            maxLat = max(shapeMA(k).lat);
            minLat = min(shapeMA(k).lat);
            posnLon = (maxLon + minLon) / 2.0;
            posnLat = (maxLat + minLat) / 2.0;
            if isOctave
                text(posnLon, posnLat, shapeMA(k).NewSAMS,'Color','k','FontSize',10,'FontWeight', 'bold');
            else
                text(posnLat, posnLon, shapeMA(k).NewSAMS,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold');
            end
        end
    end % k=1:shapeMAlen
end % strcmp(region, 'GB')
end % function
