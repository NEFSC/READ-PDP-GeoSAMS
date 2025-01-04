function x = GetRegion(isOctave, lat_t, lon_t, stratum_t)
% Called from HabCamData5mmbin and TrawlData5mmbin
%
% Determines which region, as defined by stratum, the point lat_t, lon_t is in

region_none=0;
regionGB_N=1;
regionGB_S=2;
regionGB_SW=3;
regionGB_W=4;
regionMA_N=5;
regionMA_S=6;

if (isOctave)
    lat = lat_t;
    lon = lon_t;
    stratum = stratum_t;
else
    lat = table2array(lat_t);
    lon = table2array(lon_t);
    stratum = table2array(stratum_t);
end

if (isnan(stratum) || (stratum == 0))
    x = region_none;
else
    if (stratum < 6400)
        if (stratum < 6290)
            x = regionMA_S;
        else
            x = regionMA_N;
        end
    elseif( (stratum < 6460) || (stratum == 6652) || (stratum == 6662) )
        x = region_none;
    elseif (stratum < 6490)
        if ((lat > 40.7) && (lon > -69.35))
            x = regionGB_W;
        else
            x = regionGB_SW;
        end
    elseif (stratum < 6530)
        x = regionGB_W;
    elseif (stratum < 6560)
        x = regionGB_N;
    elseif (stratum < 6610)
        x = regionGB_S;
    elseif (stratum < 6622)
        x = regionGB_S;
    elseif (stratum < 6651)
        x = region_none;
    elseif (stratum < 6680)
        x = regionGB_N;
    elseif (stratum < 6710)
        x = region_none;
    elseif (stratum < 6730)
        x = regionGB_N;
    elseif (stratum < 6740)
        x = region_none;
    elseif (stratum < 6960)
        if (stratum == 6740)
            if (lat > 41.5)
                if lon < -67.14
                    x = region_none;
                else
                    x = regionGB_N;
                end
            else
                x = regionGB_S;
            end
        else
            x = region_none;
        end
    else
        x = region_none;
    end
end
end

