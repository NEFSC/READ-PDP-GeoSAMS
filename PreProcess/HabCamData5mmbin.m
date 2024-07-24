function HabCamData5mmbin(yrStart, yrEnd, domain)

dataFile = 'OriginalData/Habcam_BySegment_2000_2014-2020.csv';

header = { "year","month","day","station","lat","lon","xutm","yutm","setdpth","sizegrp","surv_n","SQM","NImages","area","stratum","clop"};
yearCol    = find(strcmpi("year", header), 1);
monCol     = find(strcmpi("month", header), 1);
dayCol     = find(strcmpi("day", header), 1);
latCol     = find(strcmpi("lat"    , header), 1);
lonCol     = find(strcmpi("lon"    , header), 1);
utmxCol    = find(strcmpi("xutm"   , header), 1);
utmyCol    = find(strcmpi("yutm"   , header), 1);
zCol       = find(strcmpi("setdpth", header), 1);
sgCol      = find(strcmpi("sizegrp", header), 1);
svCol      = find(strcmpi("surv_n" , header), 1);
sqmCol     = find(strcmpi("SQM"    , header), 1);
areaCol    = find(strcmpi("area"   , header), 1);
stratumCol = find(strcmpi("stratum", header), 1);
clopCol    = find(strcmpi("clop"   , header), 1);

domList = {'MA', 'GB', 'AL'};

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui')
        yrStart = str2num(cell2mat(arg_list(1)));
        yrEnd = str2num(cell2mat(arg_list(2)));
        domain = cell2mat(arg_list(3));
    else
        yrStart = str2num(yrStart);
        yrEnd = str2num(yrEnd);
    end
end

if sum(ismember(domList, domain)) == 0
    fprintf( 'Invalid Domain %s\n',  domain);
    fprintf( "Use: 'MA' or 'GB' or 'AL'\n" )
    error( "Use: 'MA' or 'GB' or 'AL'\n" )
end

if isOctave
    M=csvreadK(dataFile);
    if ~strcmp(domain, 'AL')
        area = M(:,15);
        if strcmp(domain, 'GB')
            j = area == 'GBK';
        else
            j = area == 'MAB';
        end

        %------- new M with just MA or GB ----------------------
        M = M(j,:);
    end

    year = M(:,yearCol);
    sg = M(:,sgCol);
else % NOT Octave
    M = readtable(dataFile,"FileType","text");
    if ~strcmp(domain, 'AL')
        area = table2array(M(:,areaCol));
        if strcmp(domain, 'GB')
            j = strcmp(area, 'GBK');
        else
            j = strcmp(area, 'MAB');
        end
        %------- new M with just MA or GB ----------------------
        M = M(j,:);
    end

    year = table2array(M(:,yearCol));
    sg = table2array(M(:,sqmCol));
end
Detect=.4;

for yr=yrStart:yrEnd
    X=[];
    j= year==yr & sg==3.;

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
            stratum_t = M(j,stratumCol);
            is_closed_t = M(j,clopCol);
            lat_t = M(j,latCol);
            lon_t = M(j,lonCol);
            mon = M(j,monCol);
            day = M(j,dayCol);
            yd = 0 * day;
            for k=1:length(day)
                yd(k) = yearday(mon(k),day(k),0);
            end
            DecYr_t = year(j) + yd/365.25;
            z_t = M(j,zCol);
            xutm_t = M(j,utmxCol);
            yutm_t = M(j,utmyCol);
            area_t = M(j,areaCol);
            % Compute density
            M(j,svCol) = M(j,svCol) ./ M(j,sqmCol);
        else
            stratum_t = M(j,stratumCol);
            is_closed_t = M(j,clopCol);
            lat_t = M(j,latCol);
            lon_t = M(j,lonCol);
            mon = M(j,monCol);
            day = M(j,dayCol);
            yd = 0 * day;
            for k=1:length(day)
                yd(k) = yearday(mon(k),day(k),0);
            end
            DecYr_t = array2table(year(j) + yd/365.25,'VariableNames',{'DecYr'});
            z_t = M(j,zCol);
            xutm_t = M(j,utmxCol);
            yutm_t = M(j,utmyCol);
            area_t = M(j,areaCol);
            % Compute density
            M(j,svCol) = M(j,svCol) ./ M(j,sqmCol);
        end

        n=find(and(year==yr,sg==3.));
        for k=1:numel(lat_t)
            % bring in the surv_n data from size group 3 to 18, centimeters
            % which is in the 30 rows following sc==3
            % gather size data 3 - 14.5
            %               n(k) - n(k+23)
            % accumulate 15 to 18
            % sum n(k+24) - n(k+30) into k=25
            region = GetRegion(isOctave, lat_t(k,:), lon_t(k,:), stratum_t(k,:));
            if region>0
                if isOctave
                    k25 = sum((M(n(k)+24:n(k)+30, svCol)));
                    density = [(M(n(k):n(k)+23, svCol));k25];
                    X=[X;DecYr_t(k,:), xutm_t(k,:), yutm_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), is_closed_t(k,:), stratum_t(k,:), transpose(density)];
                else
                    k25   = sum(table2array(M(n(k)+24:n(k)+30, svCol)));
                    density = [table2array(M(n(k):n(k)+23, svCol));k25];
                    %        A           B            C            D             E         F          G                    H             I
                    X=[X;DecYr_t(k,:), xutm_t(k,:), yutm_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), is_closed_t(k,:), stratum_t(k,:), density(1,2:end)];
                end
            end
        end
        flnm=strcat('Data/HCbin5mm',int2str(yr),domain,'.csv');
        fprintf('Size of grid %d\n', size(X,1))
        if isOctave
            csvwrite(flnm,X);
        else
            writetable(X,flnm,'WriteVariableNames',0);
        end
    end % if sum(j) == 0
end  % for yr=yrStart:yrEnd
end


