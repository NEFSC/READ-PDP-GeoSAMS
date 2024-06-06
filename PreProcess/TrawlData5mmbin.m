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
    if ~strcmp(arg_list(1), '--gui');
	    yrStart = str2num(cell2mat(arg_list(1)));
	    yrEnd = str2num(cell2mat(arg_list(2)));
	    src = str2num(cell2mat(arg_list(3)));
	    domain = cell2mat(arg_list(4));
    else
	    yrStart = str2num(yrStart);
	    yrEnd = str2num(yrEnd);
	    src = str2num(src);
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
        for i = 1:size(lon,1)
            if lon(i)>-70.5
                zone=19;
            else
                zone=18;
            end
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
towArea_sqm = 4516.; % nautMile_m * 2.438;
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
        flnm=strcat('Data/bin5mm',int2str(yr),domain,'.csv');
        if isOctave
            csvwrite(flnm,X);
        else
            writetable(X,flnm,'WriteVariableNames',0);
        end
    end % if sum(j) == 0
end  % for yr=yrStart:yrEnd
