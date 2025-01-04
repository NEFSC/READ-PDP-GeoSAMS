% for each location it sums together the scallop density from shell length 3cm to 6 cm,
% inclusive. It then adds this value as a new column along with the current data for size
% grp 4 as a single row for the location and writes this out to "OriginalData/NewRecruits.csv".
% DEPRECATED
% src
% NMFS_ALB ==> 1111
% CANADIAN ==> 2222
% F/V_TRAD ==> 3333
% VIMSRSA ==> 4444
% NMFSSHRP ==> 5555
% ALL ==> 0
function PullOutRecruitData(useHabCam, appendResults)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui');
        useHabCam = cell2mat(arg_list(1));
        appendResults = cell2mat(arg_list(2));
    end
end

useHC = strcmp(useHabCam, 'T');

% Need these header data used by ProcessRecruitData
% Year Month Day Lat Lon Z recr
if useHC
    header = { 'year','month','day','station','lat','lon','xutm','yutm','setdpth','sizegrp','surv_n',...
                'SQM','NImages','area','stratum','clop'};
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
    dataFile = getenv('HabCamFile');
    flnm = ['OriginalData/',dataFile,'.csv'];
else
    dataFile = getenv('DredgeFile');
    if strcmpi(dataFile, 'NONE')
        %nothing to do
        % remove data file so HabCam does not append to old data
        flnm = 'OriginalData/NewRecruits.csv';
        if exist(flnm, 'file')==2
            delete(flnm)
        end
        return
    end
    header = { 'area','subarea','cruise6','year','month','day','time','stratum','tow','station',...
               'statype','SVGEAR','haul','gearcon','sdefid','newstra','clop','lat','lon','tnms',...
               'setdpth','bottemp','dopdisb','distused','towadj','towdur','sizegrp','catchnu',...
               'catchwt','surv_n','partrecn','fullrecn','surv_b','partrecb','fullrecb','postow',...
               'datasource','lwarea','SETLW','SVSPP','PropChains','AREAKIND','STRATMAP','SQNM', 'UTM X', 'UTM Y'};
    yearCol    = find(strcmpi('year', header), 1);
    monCol     = find(strcmpi('month', header), 1);
    dayCol     = find(strcmpi('day',   header), 1);
    latCol     = find(strcmpi('lat'    , header), 1);
    lonCol     = find(strcmpi('lon'    , header), 1);
    utmxCol    = find(strcmpi('UTM X'   , header), 1);
    utmyCol    = find(strcmpi('UTM Y'   , header), 1);
    zCol       = find(strcmpi('setdpth', header), 1);
    sgCol      = find(strcmpi('sizegrp', header), 1);
    svCol      = find(strcmpi('surv_n' , header), 1);
    srcCol     = find(strcmpi('datasource', header), 1);
    flnm = ['OriginalData/',dataFile,'.csv'];
end


fprintf('Reading from %s\n', flnm)

if isOctave
    F=csvreadK(flnm);
    size_grp = F(:,sgCol);
    survn = F(:,svCol);
    if (useHC)
        sqm = F(:,sqmCol);
    else
        dataSrc = F(:,srcCol);
    end
else
    F= readtable(flnm,"FileType","text");
    size_grp = table2array(F(:,sgCol));
    survn = table2array(F(:,svCol));
    if (useHC)
        sqm = table2array(F(:,sqmCol));
    else
        dataSrc = table2array(F(:,srcCol));
    end
end

% Area of survey is determined differently between HabCam and Dredge
% Dredge is a known size determined by width of dredge and tow length
% HabCam is determined from focal length and captured in the survey file in SQM column
if useHC
    survn = survn ./ sqm; % convert count to density
    n=find(size_grp==3);
    recr = zeros(1, numel(n));
    for k=1:numel(n)
        % sum 3cm to 6 cm
        recr(k) = sum(survn(n(k):n(k)+6));
    end
else
    towArea_sqm = 4516;
    T2M2=1./towArea_sqm;
    % was if srcText != 0 n=find(size_grp==3 & dataSrc==srcText);
    n=find(size_grp==3);
    recr = zeros(1, numel(n));
    for k=1:numel(n)
        % sum 3cm to 6 cm
        recr(k) = sum(survn(n(k):n(k)+6)) * T2M2; % convert count to density
    end
end

if isOctave
    recr_t = transpose(recr);
else
    recr_t = array2table(transpose(recr),'VariableNames',{'rec'});
end

% was if srcText != 0 j=size_grp==4 & dataSrc==srcText;
j=size_grp==4;

M = [F(j,yearCol) F(j,monCol) F(j,dayCol) F(j,latCol) F(j,lonCol) F(j, utmxCol) F(j, utmyCol) F(j,zCol) recr_t];

flnm = 'OriginalData/NewRecruits.csv';

if isOctave
    if strcmp(appendResults(1), 'T') && useHC
        dlmwrite(flnm, M, "-append")
    else
        dlmwrite(flnm,M);
    end
else
    if strcmp(appendResults(1), 'T') && useHC
        writetable(M,flnm,'WriteVariableNames',0,'WriteMode','append');
    else
        writetable(M,flnm,'WriteVariableNames',0);
    end
end
