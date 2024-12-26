% The assumption here is that this script is called first before any
% HabCam scripts. This script would create the Data/bin5mm files and
% HabCam would append to them
% DEPRECATED
% src
%   NMFS_ALB ==> 1111
%   CANADIAN ==> 2222
%   F/V_TRAD ==> 3333
%   VIMSRSA ==> 4444
%   NMFSSHRP ==> 5555
%   ALL ==> 0
function TrawlData5mmbin(refYear, domain)

domList = {'MA', 'GB', 'AL'};

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui')
        refYear = str2num(cell2mat(arg_list(1)));
        domain = cell2mat(arg_list(2));
    else
        refYear = str2num(refYear);
    end
end

dredgeFile = getenv('DredgeFile');
if strcmpi(dredgeFile, 'NONE')
    %Clear files and return
    flnm=strcat('Data/bin5mm',int2str(refYear),domain,'.csv');
    % if file exists remove it. We may not have any data to add.
    % Nor do we want HabCam to append to an existing file, i.e.
    % its own previously generated data
    if exist(flnm, 'file')==2
        delete(flnm)
    end
    return
end
dataFile = ['OriginalData/', dredgeFile,'.csv'];

if sum(ismember(domList, domain)) == 0
    fprintf( 'Invalid Domain %s\n',  domain);
    fprintf( "Use: 'MA' or 'GB' or 'AL'\n" )
    error( "Use: 'MA' or 'GB' or 'AL'\n" )
end

if isOctave
    M=csvreadK(dataFile);
    mon=M(:,5);
    j=find(mon>0);
    M=M(j,:);

    %------- new M table ----------------------
    if ~strcmp(domain, 'AL')
        lon=M(:,19);
        if strcmp(domain, 'GB')
            j = lon>-70.5;%GB
        else
            j = lon<=-70.5;%MA
        end

        %------- new M with just MA or GB ----------------------
        M = M(j,:);
    end

    year = M(:,4);
    sg = M(:,27);
    dataSrc = M(:,37);
else % NOT Octave
    M = readtable(dataFile,"FileType","text");
    mon=table2array(M(:,5));
    j=mon>0;
    M=M(j,:);

    %------- new M table ----------------------
    if ~strcmp(domain, 'AL')
        lon = table2array(M(:,19));
        if strcmp(domain, 'GB')
            j = lon>-70.5;
        else
            j = lon<=-70.5;
        end

        %------- new M with just MA or GB ----------------------
        M = M(j,:);
    end

    year = table2array(M(:,4));
    sg = table2array(M(:,27));
    dataSrc = table2array(M(:,37));
end
detect=.4;

%  standard tow length of 1 nautical mile by 8 ft, or 2.4384 m, wide dredge
% nautMile_m = 1852.;
towArea_sqm = 4516.; % nautMile_m * 2.438;
countPerSqm = 1.0 / (towArea_sqm * detect);

yr=refYear;
flnm=strcat('Data/bin5mm',int2str(yr),domain,'.csv');
% if file exists remove it. We may not have any data to add.
if exist(flnm, 'file')==2
    delete(flnm)
end
X=[];
%j=and(year==yr,sg==3.);
% was j= dataSrc==srcText & year==yr & sg==3.;
j= year==yr & sg==3.;

% Write header
WriteHeader(flnm);

if sum(j) == 0
    % no data found
    fprintf('Skipping %s Year %d\n', domain, yr);
else
    fprintf('Working on %s Year %d\n', domain, yr);

    if isOctave
        stratum_t = M(j,8);
        is_closed_t = double(M(j,17)>0);
        lat_t = M(j,18);
        lon_t = M(j,19);
        mon = M(j,5);
        day = M(j,6);
        yd = 0 * day;
        for k=1:length(day)
            yd(k) = yearday(mon(k),day(k),0);
        end
        DecYr_t = year(j) + yd/365.25;
        z_t = cast(M(j,21),"double");
        x_t = M(j,45);
        y_t = M(j,46);
    else
        stratum_t = M(j,8);
        is_closed_t = array2table(int8(table2array(M(j,17)>0)),'VariableNames',{'isClosed'});
        lat_t = M(j,18);
        lon_t = M(j,19);
        mon = table2array(M(j,5));
        day = table2array(M(j,6));
        yd = 0 * day;
        for k=1:length(day)
            yd(k) = yearday(mon(k),day(k),0);
        end
        DecYr_t = array2table(year(j) + yd/365.25,'VariableNames',{'DecYr'});
        z_t = M(j,21);
        x_t = M(j,45);
        y_t = M(j,46);
    end

    n=find(and(year==yr,sg==3.));
    % Initialize shape data
    shape = GetShapeData();

    % Create table for Zone and Region
    sz = size(lat_t);
    if isOctave
        zone_t = cell(sz,1);
        region_t = cell(sz,1);
    else
        varTypes = "string";
        varNames = "Zone";
        zone_t = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
        varNames = "Region";
        region_t = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
    end
    for k=1:numel(lat_t)
        % bring in the surv_n data from size group 3 to 18, centimeters
        % which is in the 30 rows following sg==3
        % gather size data 3 - 14.5
        %               n(k) - n(k+23)
        % accumulate 15 to 18
        % sum n(k+24) - n(k+30) into k=25
        region = GetRegion(isOctave, lat_t(k,:), lon_t(k,:), stratum_t(k,:));
        [zoneStr, regionStr] = CheckInRegionPolygon(isOctave, x_t(k,:), y_t(k,:), shape);
        if isOctave
            zone_t(k,:) = zoneStr;
            region_t(k,:) = regionStr;
        else
            zone_t(k,:) = mat2cell(zoneStr,1);
            region_t(k,:) = mat2cell(regionStr,1);
        end

        if region>0
            if isOctave
                k25 = sum((M(n(k)+24:n(k)+30,30)));
                density = [(M(n(k):n(k)+23,30));k25];
                density= density * countPerSqm;
                X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), ...
                   is_closed_t(k,:), stratum_t(k,:), zone_t(k,:), region_t(k,:), transpose(density)];
            else
                k25   = sum(table2array(M(n(k)+24:n(k)+30,30)));
                density = [table2array(M(n(k):n(k)+23,30));k25];
                density= rows2vars(array2table( density * countPerSqm));
                X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), ...
                   is_closed_t(k,:), stratum_t(k,:), zone_t(k,:), region_t(k,:), density(1,2:end)];
            end
        end
    end
    fprintf('Size of grid %d\n', size(X,1))
    if isOctave
        %dlmwrite(flnm, X, "-append")
        % Open a file for writing
        fid = fopen(flnm, "a");

        % Iterate through the matrix and write each row to the file
        for i = 1:size(X, 1)
            fprintf(fid, "%f,%f,%f,%f,%f,%f,%i,%f,%s,%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n", X{i,1:8}, X{i,9:10}, X{i,11});
        end

        % Close the file
        fclose(fid);
    else
        writetable(X,flnm,'WriteVariableNames',0,'WriteMode','append');
    end

end % if sum(j) == 0
end
