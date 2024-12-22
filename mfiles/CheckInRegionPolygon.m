function [zone, region]= CheckInRegionPolygon(isOctave, X_t, Y_t, shape)
% Takes X,Y UTM coordinates and determins which, if any
% Zone region geopoint is located.
% Returns Zone name and alphabetical index

zone = 'NA';
region = 'NA';

if isOctave
    X = X_t;
    Y = Y_t;
else
    X = table2array(X_t);
    Y = table2array(Y_t);
end

for i=1:length(shape)
    if inpolygon(X, Y, shape(i).X, shape(i).Y)
        zone = string(shape(i).Zone);
        if strcmp(zone,'VIR'); zone = 'NA';end
        if strcmp(zone,'NA')
            region = 'NA';
        else
            region = string(shape(i).Region);
        end
        break
    end
end
