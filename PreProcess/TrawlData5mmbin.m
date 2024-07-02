% src
%   NMFS_ALB ==> 1111
%   CANADIAN ==> 2222
%   F/V_TRAD ==> 3333
%   VIMSRSA ==> 4444
%   NMFSSHRP ==> 5555
%   ALL ==> 0
function TrawlData5mmbin(yrStart, yrEnd, src, domain)

domList = {'MA', 'GB', 'AL'};

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui')
	    yrStart = str2num(cell2mat(arg_list(1)));
	    yrEnd = str2num(cell2mat(arg_list(2)));
	    src = str2num(cell2mat(arg_list(3)));
	    domain = cell2mat(arg_list(4));
    end
end
srcText = src;

if sum(ismember(domList, domain)) == 0
    fprintf( 'Invalid Domain %s\n',  domain);
    fprintf( "Use: 'MA' or 'GB' or 'AL'\n" )
    error( "Use: 'MA' or 'GB' or 'AL'\n" )
end

if isOctave

    M=csvreadK('OriginalData/dredgetowbysize7917.csv');
    mon=M(:,5);
    j=find(mon>0);
    M=M(j,:);
    %------- new M table ----------------------

    if ~strcmp(domain, 'AL')
        lon=-M(:,19);
        if strcmp(domain, 'GB')
            j = lon>-70.5;%GB
            zone=19;
        else
            j = lon<-70.5;%MA
            zone=18;
        end

        %------- new M with just MA or GB ----------------------
        M = M(j,:);
        lat=table2array(M(:,18));
        lon=-table2array(M(:,19));
        [xx,yy]=ll2utm(lat,lon,zone);

    else % working with AL
        lat=M(:,18);
        lon=-M(:,19);
        % preallocate
        xx=lon;
        yy=lat;
        n = size(lon,1);
        for i = 1:n
            if lon(i)>-70.5
                zone=19;
            else
                zone=18;
            end
            if mod(i,10000) == 0; fprintf('i : %d of %d\n', i, n); end
            [xx(i),yy(i)]=ll2utm(lat(i),lon(i),zone);
        end
    end

    year = M(:,4);
    sg = M(:,27);
    dataSrc = M(:,37);
else % NOT Octave
    M = readtable('OriginalData/dredgetowbysize7917.csv',"FileType","text");

    mon=table2array(M(:,5));
    j=mon>0;
    M=M(j,:);
    %------- new M table ----------------------

    if ~strcmp(domain, 'AL')
        lon = -table2array(M(:,19));
        if strcmp(domain, 'GB')
            j = lon>-70.5;
            zone=19;
        else
            j = lon<=-70.5;
            zone=18;
        end

        %------- new M with just MA or GB ----------------------
        M = M(j,:);
        lat=table2array(M(:,18));
        lon=-table2array(M(:,19));
        [xx,yy]=ll2utm(lat,lon,zone);

    else % working with AL
        lat=table2array(M(:,18));
        lon=-table2array(M(:,19));
        % preallocate
        xx=lon;
        yy=lat;
        for i = 1:size(lon,1)
            if lon(i)>-70.5
                zone=19;
            else
                zone=18;
            end
            [xx(i),yy(i)]=ll2utm(lat(i),lon(i),zone);
        end
    end

    year = table2array(M(:,4));
    sg = table2array(M(:,27));
    dataSrc = table2array(M(:,37));
end
Detect=.4;

%  standard tow length of 1 nautical mile by 8 ft, or 2.4384 m, wide dredge
nautMile_m = 1852.;
towArea_sqm = nautMile_m * 2.438;
countPerSqm = 1. / (towArea_sqm * Detect);

%for yr=1979:2017
for yr=yrStart:yrEnd
    X=[];
    %j=and(year==yr,sg==3.);
    if srcText == 0
        j= year==yr & sg==3.;
    else
        j= dataSrc==srcText & year==yr & sg==3.;
    end

    if sum(j) == 0
        % no data found
        fprintf( 'Skipping %s Year %d\n',  domain, yr);
        msg = sprintf( 'No Data for %s Year %d',  domain, yr);
        errorStruct.message = msg;
        errorStruct.identifier = 'myComponent:inputError';
        error(errorStruct)
    else
        fprintf( 'Working on %s Year %d\n',  domain, yr);

        if isOctave
            stratum_t = M(j,8);
            is_closed_t = double(M(j,17)>0);
            lat_t = M(j,18);
            lon_t = -(M(j,19));
            mon = M(j,5);
            day = M(j,6);
            yd = 0 * day;
            for k=1:length(day)
                yd(k) = yearday(mon(k),day(k),0);
            end
            DecYr_t = year(j) + yd/365.25;
            z_t = cast(M(j,21),"double");
            x_t = xx(j);
            y_t = yy(j);
        else
            stratum_t = M(j,8);
            is_closed_t = array2table(int8(table2array(M(j,17)>0)),'VariableNames',{'isClosed'});
            lat_t = M(j,18);
            lon_t = array2table(-table2array(M(j,19)),'VariableNames',{'lon'});
            mon = table2array(M(j,5));
            day = table2array(M(j,6));
            yd = 0 * day;
            for k=1:length(day)
                yd(k) = yearday(mon(k),day(k),0);
            end
            DecYr_t = array2table(year(j) + yd/365.25,'VariableNames',{'DecYr'});
            z_t = M(j,21);
            x_t = array2table(xx(j),'VariableNames',{'x'});
            y_t = array2table(yy(j),'VariableNames',{'y'});
        end

        n=find(and(year==yr,sg==3.));
        for k=1:numel(lat_t)
            % bring in the surv_n data from size group 3 to 18, centimeters
            % which is in the 30 rows following sc==3
            % gather size data 3 - 14.5
            %               n(k) - n(k+23)
            % accumulate 15 to 18
            % sum n(k+24) - n(k+30) into k=25
            region = Get_Region(isOctave, lat_t(k,:), lon_t(k,:), stratum_t(k,:));
            if region>0
                if isOctave
                    k25 = sum((M(n(k)+24:n(k)+30,30)));
                    density = [(M(n(k):n(k)+23,30));k25];
                    density= density * countPerSqm;
                    X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), is_closed_t(k,:), stratum_t(k,:), transpose(density)];
                else
                    k25   = sum(table2array(M(n(k)+24:n(k)+30,30)));
                    density = [table2array(M(n(k):n(k)+23,30));k25];
                    density= rows2vars(array2table( density * countPerSqm));
                    X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), is_closed_t(k,:), stratum_t(k,:), density(1,2:end)];
                end
            end
        end
        flnm=strcat('Data/bin5mm',int2str(yr),domain,'.csv');
        fprintf('Size of grid %d\n', size(X,1))
        if isOctave
            csvwrite(flnm,X);
        else
            writetable(X,flnm,'WriteVariableNames',0);
        end
    end % if sum(j) == 0
end  % for yr=yrStart:yrEnd
end

function x = Get_Region(isOctave, lat_t, lon_t, stratum_t)
region_none=0;
region_N=1;
region_S=2;
region_SW=3;
region_W=4;
region_MA=5;

if (isOctave)
    lat = lat_t;
    lon = lon_t;
    stratum = lon_t;
else
    lat = table2array(lat_t);
    lon = table2array(lon_t);
    stratum = table2array(stratum_t);
end

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

