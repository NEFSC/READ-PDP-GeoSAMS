function x = GetRegion(isOctave, lat_t, lon_t, stratum_t)

region_none=0;
region_N=1;
region_S=2;
region_SW=3;
region_W=4;
region_MA=5;

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
        x = region_MA;
    elseif( (stratum < 6460) || (stratum == 6652) || (stratum == 6662) )
        x = region_none;
    elseif (stratum < 6490)
        if ((lat > 40.7) && (lon > -69.35))
            x = region_W;
        else
            x = region_SW;
        end
    elseif (stratum < 6530)
        x = region_W;
    elseif (stratum < 6560)
        x = region_N;
    elseif (stratum < 6610)
        x = region_S;
    elseif (stratum < 6622)
        x = region_S;
    elseif (stratum < 6651)
        x = region_none;
    elseif (stratum < 6680)
        x = region_N;
    elseif (stratum < 6710)
        x = region_none;
    elseif (stratum < 6730)
        x = region_N;
    elseif (stratum < 6740)
        x = region_none;
    elseif (stratum < 6960)
        if (stratum == 6740)
            if (lat > 41.5)
                if lon < -67.14
                    x = region_none;
                else
                    x = region_N;
                end
            else
                x = region_S;
            end
        else
            x = region_none;
        end
    else
        x = region_none;
    end
end
end

