% for each location it sums together the scallop density from shell length 3cm to 6 cm,
% inclusive. It then adds this value as a new column along with the current data for size
% grp 4 as a single row.
%
% Processing continues on this array.
% - Dredge Data, if chosend,  requires further processing to adjust for scallops detected vs actual 
%   and depth adjustments. HabCam needs no further adjustment.
%
% Dredge and HabCam data are combined into a single array and then sorted by year
% Written out to file names Data/RecruitsYYYYDN.csv
%
function PullOutProcessRecruitData(yrStart, yrEnd, domain)

maxDredge=0;
maxHC=0;

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
if isOctave
  % used if called by command line, or gui
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

if ~strcmp(domain, 'GB') && ~strcmp(domain, 'MA') && ~strcmp(domain, 'AL')
  fprintf( 'Invalid Domain %s\n',  domain);
  fprintf( "Use: 'MA' or 'GB' or 'AL'\n" )
  return;
end

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

%%%%%%%%%%%%%%%%%%%%%%%%%%% DREDGE DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% First consider dredge data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataFile = getenv('DredgeFile');
if strcmpi(dataFile, 'NONE')
    %nothing to do
    MDredge = [];
else
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

fprintf('Reading from %s\n', flnm)

if isOctave
    F=csvreadK(flnm);
    size_grp = F(:,sgCol);
    survn = F(:,svCol);
else
    F= readtable(flnm,"FileType","text");
    size_grp = table2array(F(:,sgCol));
    survn = table2array(F(:,svCol));
end

towArea_sqm = 4516;
T2M2=1./towArea_sqm;
n=find(size_grp==3);
recr = zeros(1, numel(n));
for k=1:numel(n)
    % sum 3cm to 6 cm
    recr(k) = sum(survn(n(k):n(k)+6)) * T2M2; % convert count to density
    if recr(k) > maxDredge, maxDredge = recr(k); end
end

if isOctave
    recr_t = transpose(recr);
else
    recr_t = array2table(transpose(recr),'VariableNames',{'rec'});
end

j=size_grp==4;

MDredge = [F(j,yearCol) F(j,monCol) F(j,dayCol) F(j,latCol) F(j,lonCol) F(j, utmxCol) F(j, utmyCol) F(j,zCol) recr_t];

fprintf('Max dredge before adjustment %f\n', maxDredge)
%
% We now have the recruit estimate information for Dredge data in recr_t as scallops/m^2
%
% Dredge data needs to have the recruit estimate adjusted based on detection and depth, rock information
%
detectScallop=.4;
detectRS=.27;

yearCol  = 1;
monCol   = 2;
dayCol   = 3;
latCol   = 4;
lonCol   = 5;
utmxCol  = 6;
utmyCol  = 7;
depthCol = 8;
recrCol  = 9;  % recruit units are scallops/m^2

if isOctave
    year= MDredge(:,yearCol);
    mon=  MDredge(:,monCol);
    day=  MDredge(:,dayCol);
    lat=  MDredge(:,latCol);
    lon=  MDredge(:,lonCol);
    utmx= MDredge(:,utmxCol);
    utmy= MDredge(:,utmyCol);
    recM2=MDredge(:,recrCol);
    depth=MDredge(:,depthCol);
else
    year= table2array(MDredge(:,yearCol));
    mon=  table2array(MDredge(:,monCol));
    day=  table2array(MDredge(:,dayCol));
    lat=  table2array(MDredge(:,latCol));
    lon=  table2array(MDredge(:,lonCol));
    utmx= table2array(MDredge(:,utmxCol));
    utmy= table2array(MDredge(:,utmyCol));
    recM2=table2array(MDredge(:,recrCol));
    depth=table2array(MDredge(:,depthCol));
end
% if NAN or blank, i.e. 0
j = find(isnan(mon));mon(j)=6;day(j)=21;% assign missing date to summer solstice
j = find(~(mon));mon(j)=6;day(j)=21;% assign missing date to summer solstice
  
len = length(year);
yd = zeros(len,1);
for n=1:len
    yd(n) = yearday(mon(n),day(n),0);%ignore leap years
end
yd=yd(:);
decYr=year(:)+( yd(:)/365.2425 );

MDredge = [decYr(:),lat(:),lon(:),utmx(:),utmy(:),depth(:),recM2(:)];
j = find(~isnan(sum(MDredge')));
MDredge = MDredge(j,:);

%XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
%adjust data for 40 percent detection rate,  27 percent in rock strata
%Rock Chain Strata:
%46 (only above 4050N, 40.83333N)
%47 (only above 4050N, 40.83333N)
%49 (entire strata)
%50 (entire strata)
%51 (entire strata)
%52 (entire strata)
%71 (only in CA2 portion of strata - East of 6720W, -67.333333 W)
%72 (only in CA2 portion of strata - East of 6720W, -67.333333 W)
%74 (only in CA2 portion of strata - East of 6720W, -67.333333 W)
%651 (only in CA2 portion of strata - East of 6720W, -67.333333 W)
%661 (only in CA2 portion of strata - East of 6720W, -67.333333 W)
%652 (don\u2019t survey anymore- Canada)
%662 (don\u2019t survey anymore- Canada)
%XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

rsi = [36,37,14,13,11,9 ,5 ,10,17, 4 , 2 , 3 , 1 ];
minLon = [-180*ones(1,6),-67.333333*ones(1,5),-180*ones(1,2)];
minLat = [40.83333,40.83333,zeros(1,11)];
nrs = length(rsi);

G = shaperead('ShapeFiles/Shellfish_Strata.shp');
isRock=zeros(size(lat));
for k=1:nrs
    [k,rsi(k),nrs];
    lonStrat=G(rsi(k)).X;
    latStrat=G(rsi(k)).Y;
    for n=1:len
        j0(n) = inside(lon(n), lat(n), lonStrat, latStrat);
    end
    j0 = find(j0==1);
    j1 = find(lon > minLon(k));
    j2 = find(lat > minLat(k));
    j = intersect(intersect(j0,j1), j2);
    isRock(j)=1;
end
recA = 0 * recM2(:);
j0 = find(isRock==0);
j1 = find(isRock==1);
recA(j0) = recM2(j0)/detectScallop;
recA(j1) = recM2(j1)/detectRS;
MDredge = [decYr(:), lat(:), lon(:), utmx(:), utmy(:), depth(:), recA(:)];
j=find(~isnan(sum(MDredge')));
MDredge = MDredge(j,:);

fprintf('Max dredge after adjustment %f\n', max(recA))

end % if strcmpi(dataFile, 'NONE')

%%%%%%%%%%%%%%%%%%%%%%%%%%% HABCAM DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now consider HabCam data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Need these header data used by ProcessRecruitData
% Year Month Day Lat Lon Z recr
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

fprintf('Reading from %s\n', flnm)

if isOctave
    F=csvreadK(flnm);
    size_grp = F(:,sgCol);
    survn = F(:,svCol);
    sqm = F(:,sqmCol);
else
    F= readtable(flnm,"FileType","text");
    size_grp = table2array(F(:,sgCol));
    survn = table2array(F(:,svCol));
    sqm = table2array(F(:,sqmCol));
end

% Area of survey is determined differently between HabCam and Dredge
% Dredge is a known size determined by width of dredge and tow length
% HabCam is determined from focal length and captured in the survey file in SQM column
survn = survn ./ sqm; % convert count to density
n=find(size_grp==3);
recr = zeros(1, numel(n));
for k=1:numel(n)
    % sum 3cm to 6 cm
    recr(k) = sum(survn(n(k):n(k)+6));
end

if isOctave
    recr_t = transpose(recr);
else
    recr_t = array2table(transpose(recr),'VariableNames',{'rec'});
end

% was if srcText != 0 j=size_grp==4 & dataSrc==srcText;
j=size_grp==4;

MHC = [F(j,yearCol) F(j,monCol) F(j,dayCol) F(j,latCol) F(j,lonCol) F(j, utmxCol) F(j, utmyCol) F(j,zCol) recr_t];

yearCol  = 1;
monCol   = 2;
dayCol   = 3;
latCol   = 4;
lonCol   = 5;
utmxCol  = 6;
utmyCol  = 7;
depthCol = 8;
recrCol  = 9; 

if isOctave
    year= MHC(:,yearCol);
    mon=  MHC(:,monCol);
    day=  MHC(:,dayCol);
    lat=  MHC(:,latCol);
    lon=  MHC(:,lonCol);
    utmx= MHC(:,utmxCol);
    utmy= MHC(:,utmyCol);
    depth=MHC(:,depthCol);
    recM2=MHC(:,recrCol);
else
    year= table2array(MHC(:,yearCol));
    mon=  table2array(MHC(:,monCol));
    day=  table2array(MHC(:,dayCol));
    lat=  table2array(MHC(:,latCol));
    lon=  table2array(MHC(:,lonCol));
    utmx= table2array(MHC(:,utmxCol));
    utmy= table2array(MHC(:,utmyCol));
    depth=table2array(MHC(:,depthCol));
    recM2=table2array(MHC(:,recrCol));
end

len = length(year);
yd = zeros(len,1);
for n=1:len
    yd(n) = yearday(mon(n),day(n),0);%ignore leap years
end
yd=yd(:);
decYr=year(:)+( yd(:)/365.2425 );
MHC=[decYr(:), lat(:), lon(:), utmx(:), utmy(:), depth(:), recM2(:)];

fprintf('Max HabCam %f\n', max(recM2))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Continue processing for both HabCam and Dredge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
M = [MDredge;MHC];
header='decimal year,latitude,longitude,utm x,utm y,bottom depth(m),recruits per sq m';
flnm=['Data/Recruits',domain,'.csv'];
writecsv(M,flnm,'%f, %f, %f, %f, %f, %f, %e',header);
fprintf('Writing to %s. Number of records %d\n', flnm, size(M,1))

yearCol  = 1;
latCol   = 2;
lonCol   = 3;
utmxCol  = 4;
utmyCol  = 5;
depthCol = 6;
recrCol  = 7; 
decYr= M(:,yearCol);
lat=  M(:,latCol);
lon=  M(:,lonCol);
utmx= M(:,utmxCol);
utmy= M(:,utmyCol);
depth=M(:,depthCol);
recM2=M(:,recrCol);

if ~strcmp(domain, 'AL')
    if strcmp(domain, 'GB')
        j = lon>-70.5;
    else
        j = lon<=-70.5;
    end
    M = [decYr(j), lat(j), lon(j), utmx(j), utmy(j), depth(j), recM2(j)];
else
    M = [decYr, lat, lon, utmx, utmy, depth, recM2];
end

yearMin = min(floor(decYr));
yearMax = max(floor(decYr));
header='decimal year,latitude,longitude,utm x,utm y,bottom depth(m),recruits per sq m';
if yrStart >= yearMin && yrEnd <= yearMax
    for yr=yrStart:yrEnd
        j = find(floor(decYr)==yr);
        Msave = M(j,:);
        flnm=['Data/Recruits',int2str(yr),domain,'.csv'];
        writecsv(Msave,flnm,'%f, %f, %f, %f, %f, %f, %e',header);
        fprintf('Writing to %s. Number of records %d\n', flnm, size(Msave,1))
    end
else
    fprintf('INPUT YEARS OUT OF RANGE: %i to %i : actual %i to %i\n', yearMin, yearMax, yrStart, yrEnd )
    msg = sprintf( 'INPUT YEARS OUT OF RANGE: %i to %i : actual %i to %i\n', yearMin, yearMax, yrStart, yrEnd);
    errorStruct.message = msg;
    errorStruct.identifier = 'myComponent:inputError';
    error(errorStruct)
end

close all
