function HabCamData5mmbin(refYear, domain, appendResults)

habCamFile = getenv('HabCamFile');
if strcmpi(habCamFile, 'NONE')
    %nothing to do
    return
end
dataFile = ['OriginalData/', habCamFile, '.csv'];

header = { 'year','month','day','station','lat','lon','xutm','yutm','setdpth','sizegrp','surv_n','SQM','NImages','area','stratum','clop'};
yearCol    = find(strcmpi('year', header), 1);
monCol     = find(strcmpi('month', header), 1);
dayCol     = find(strcmpi('day', header), 1);
latCol     = find(strcmpi('lat'    , header), 1);
lonCol     = find(strcmpi('lon'    , header), 1);
utmxCol    = find(strcmpi('xutm'   , header), 1);
utmyCol    = find(strcmpi('yutm'   , header), 1);
zCol       = find(strcmpi('setdpth', header), 1);
sgCol      = find(strcmpi('sizegrp', header), 1);
svCol      = find(strcmpi('surv_n' , header), 1);
sqmCol     = find(strcmpi('SQM'    , header), 1);
areaCol    = find(strcmpi('area'   , header), 1);
stratumCol = find(strcmpi('stratum', header), 1);
clopCol    = find(strcmpi('clop'   , header), 1);

domList = {'MA', 'GB', 'AL'};

detect=.4;

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui')
        refYear = str2num(cell2mat(arg_list(1)));
        domain = cell2mat(arg_list(2));
        appendResults = cell2mat(arg_list(3));
    else
        refYear = str2num(refYear);
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
        lon = M(:,lonCol);
        if strcmp(domain, 'GB')
            j = lon > -70.5
        else
            j = lon <= -70.5;
        end

        %------- new M with just MA or GB ----------------------
        M = M(j,:);
    end

    year = M(:,yearCol);
    sg = M(:,sgCol);
    % convert count to density
    M(:,svCol) = (detect * M(:,svCol)) ./ M(:,sqmCol);
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

    % convert count to density
    svData = table2array(M(:,svCol));
    svArea = table2array(M(:,sqmCol));
    svData = (detect * svData) ./ svArea;
    M(:,svCol) = array2table(svData,'VariableNames',{'surv_n'});
    year = table2array(M(:,yearCol));
    sg = table2array(M(:,sgCol));
end

% 
yr=refYear;
X=[];
j= year==yr & sg==3.;

if sum(j) == 0
    % no data found
    fprintf( 'Skipping %s Year %d\n',  domain, yr);
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
    else
        stratum_t = M(j,stratumCol);
        is_closed_t = M(j,clopCol);
        lat_t = M(j,latCol);
        lon_t = M(j,lonCol);
        mon = table2array(M(j,monCol));
        day = table2array(M(j,dayCol));
        yd = 0 * day;
        for k=1:length(day)
            yd(k) = yearday(mon(k),day(k),0);
        end
        DecYr_t = array2table(year(j) + yd/365.25,'VariableNames',{'DecYr'});
        z_t = M(j,zCol);
        xutm_t = M(j,utmxCol);
        yutm_t = M(j,utmyCol);
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
                density_t = rows2vars(array2table(density,'VariableNames',{'density'}));
                %        A           B            C            D             E         F          G                    H             I
                X=[X;DecYr_t(k,:), xutm_t(k,:), yutm_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), is_closed_t(k,:), stratum_t(k,:), density_t(1,2:end)];
            end
        end
    end
    flnm=strcat('Data/bin5mm',int2str(yr),domain,'.csv');
    fprintf('Size of grid %d\n', size(X,1))
    if isOctave
        if strcmp(appendResults(1), 'T')
            dlmwrite(flnm, X, "-append")
        else
            dlmwrite(flnm,X);
        end
    else
        if strcmp(appendResults(1), 'T')
            writetable(X,flnm,'WriteVariableNames',0,'WriteMode','append');
        else
            writetable(X,flnm,'WriteVariableNames',0);
        end
    end
end % if sum(j) == 0
end


