% Assumes a figure has already been opened
function PlotRegion(isOctave, region, shape, cutNS, split, dispText)

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
        PaintNames(isOctave, dispText, posnLon, posnLat, shape(k).Zone)
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
    
            posnLon = (maxLon + minLon) / 2.0;
            posnLat = (maxLat + minLat) / 2.0;
            if length(shape(k).Zone)>3
                PaintNames(isOctave, dispText, posnLon, posnLat-0.05, shape(k).Zone);
            else
                PaintNames(isOctave, dispText, posnLon, posnLat, shape(k).Zone);
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
            PaintNames(isOctave, dispText, posnLon, posnLat, shape(k).Zone);
        end
    end % k=1:shapeLen
end % strcmp(region, 'GB')
end % function

function PaintNames( isOctave, dispText, lon, lat, zone)
    if isOctave
        if dispText; text(lon, lat, zone,'Color','k','FontSize',10,'FontWeight', 'bold'); end
    else
        if dispText; text(lat, lon, zone,'Color',"#A2142F",'FontSize',10,'FontWeight', 'bold'); end
    end
end