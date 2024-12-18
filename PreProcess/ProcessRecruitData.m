%----------------------------------------------------------
% This only applies to data collected from a Dredge Survey
%----------------------------------------------------------
function ProcessRecruitData(yrStart, yrEnd, domain)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
if isOctave
  % used if called by command line, or gui
  arg_list=argv();
  if ~strcmp(arg_list(1), '--gui');
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

Detect=.4;
DetectRS=.27;

flnm = 'OriginalData/NewRecruits.csv';
yrCol    = 1;
monCol   = 2;
dayCol   = 3;
latCol   = 4;
lonCol   = 5;
utmxCol  = 6;
utmyCol  = 7;
depthCol = 8;
recrCol  = 9;  % recruit units are scallops/m^2
fprintf('Reading from %s\n', flnm)

if isOctave
  F=csvreadK(flnm);
  year=F(:,yrCol);
  mon=F(:,monCol);
  day=F(:,dayCol);
else
  F= readtable(flnm,"FileType","text");
  year=table2array(F(:,yrCol));
  mon=table2array(F(:,monCol));
  day=table2array(F(:,dayCol));
end
% if NAN or blank, i.e. 0
j=find(isnan(mon));mon(j)=6;day(j)=21;% assign missing date to summer solstice
j=find(~(mon));mon(j)=6;day(j)=21;% assign missing date to summer solstice

N=length(year);
for n=1:N
  yd(n) = yearday(mon(n),day(n),0);%ignore leap years
end
yd=yd(:);
DecYr=year(:)+( yd(:)/365.2425 );

if isOctave
  lat=F(:,latCol);
  lon=F(:,lonCol);
  utmx=F(:,utmxCol);
  utmy=F(:,utmyCol);
  RecM2=F(:,recrCol);
  Depth=F(:,depthCol);
else
  lat=table2array(F(:,latCol));
  lon=table2array(F(:,lonCol));
  utmx=table2array(F(:,utmxCol));
  utmy=table2array(F(:,utmyCol));
  RecM2=table2array(F(:,recrCol));
  Depth=table2array(F(:,depthCol));
end

M=[DecYr(:),lat(:),lon(:),utmx(:),utmy(:),Depth(:),RecM2(:)];
j=find(~isnan(sum(M')));
M=M(j,:);
flnm='Data/RecruitsUnadjusted.csv';
header='decimal year,latitude,longitude,UTM x,UTM y,bottom depth(m),recruits per m^2';
fprintf('Writing to %s\n\n', flnm)
writecsv(M,flnm,'%f, %f, %f, %f, %f, %f, %e',header);

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
%Hi Keston,
%I don't have a rock chain shapefile but I have the shellfish strata shapefile
%and that should do?! STRATUM is the strata number. To match the strata numbers
%in the shapefile with the rock chain strata numbers below - add a 6 in the front
%and if the strata number is two digits, add a 0 at the end. Let me know if you
%need anything else. Hope this helps.
%best,
%han

RS= [46,47,49,50,51,52,71,72,74,651,661,652,662];
RSI=[36,37,14,13,11,9 ,5 ,10,17, 4 , 2 , 3 , 1 ];
MinLon=[-180*ones(1,6),-67.333333*ones(1,5),-180*ones(1,2)];
MinLat=[40.83333,40.83333,zeros(1,11)];
NRS=length(RSI);

flnm='Data/RecruitsUnadjusted.csv';
fprintf('Reading from to %s\n', flnm)

if isOctave
    F=csvreadK(flnm);
else
    F=table2array(readtable(flnm,'PreserveVariableNames', true));
end

segments = strings(0);
remain = header;
while (remain ~= "")
   [token,remain] = strtok(remain, ',');
   segments = [segments ; token];
end

yearCol = find(strcmpi('decimal year', segments), 1);
latCol  = find(strcmpi('latitude', segments), 1);
lonCol  = find(strcmpi('longitude', segments), 1);
utmxCol = find(strcmpi('UTM x', segments), 1);
utmyCol = find(strcmpi('UTM y', segments), 1);
zCol    = find(strcmpi('bottom depth(m)', segments), 1);
recCol  = find(strcmpi('recruits per m^2', segments), 1);

[N,five]=size(F);
DecYr=F(:,yearCol);
lat=F(:,latCol);
lon=F(:,lonCol);
utmx=F(:,utmxCol);
utmy=F(:,utmyCol);
Depth=F(:,zCol);
Rec=F(:,recCol);

G = shaperead('ShapeFiles/Shellfish_Strata.shp');
IsRock=zeros(size(lat));
for k=1:NRS
    [k,RSI(k),NRS];
    lonStrat=G(RSI(k)).X;
    latStrat=G(RSI(k)).Y;
    for n=1:N
        j0(n)=inside(lon(n),lat(n),lonStrat,latStrat);
    end
    j0=find(j0==1);
    j1=find(lon>MinLon(k));
    j2=find(lat>MinLat(k));
    j=intersect(intersect(j0,j1),j2);
    IsRock(j)=1;
end
RecA=0*Rec(:);
j0=find(IsRock==0);
j1=find(IsRock==1);
RecA(j0)=Rec(j0)/Detect;
RecA(j1)=Rec(j1)/DetectRS;
M=[DecYr(:),lat(:),lon(:),utmx(:),utmy(:),Depth(:),Rec(:),IsRock(:),RecA(:)];
j=find(~isnan(sum(M')));
M=M(j,:);
flnm='Data/RecruitsRockStrataAdjustment.csv';
header='decimal year,latitude,longitude,UTM x,UTM y,bottom depth(m),recruits per sq m raw,Is Rock Strata,recruits per sq m adjusted';
writecsv(M,flnm,['%f, %f, %f, %f, %f, %f, %e, %i ,%e' ],header);
fprintf('Writing to %s\n\n', flnm)

close all;
%j0=find(IsRock==0);
%j1=find(IsRock==1);
%plot(lon(j0),lat(j0),'c.',lon(j1),lat(j1),'r.');
%hold on;
%for k=1:length(G)
%    lonStrat=G(k).X;
%    latStrat=G(k).Y;
%    plot(lonStrat,latStrat,'k');hold on;
%end
%for k=1:NRS
%    lonStrat=G(RSI(k)).X;
%    latStrat=G(RSI(k)).Y;
%    plot(lonStrat,latStrat,'r');
%end
%if isOctave % Octave did not need 3rd term
%    daspect([1,cos(mean(lat)*pi/180)]);
%else
%    daspect([1,cos(mean(lat)*pi/180), 1]);
%end
%print -djpeg RockStrata.

%XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

segments = strings(0);
remain = header;
while (remain ~= "")
   [token,remain] = strtok(remain, ',');
   segments = [segments ; token];
end

yearCol = find(strcmpi('decimal year', segments), 1);
latCol  = find(strcmpi('latitude', segments), 1);
lonCol  = find(strcmpi('longitude', segments), 1);
utmxCol = find(strcmpi('UTM x', segments), 1);
utmyCol = find(strcmpi('UTM y', segments), 1);
zCol    = find(strcmpi('bottom depth(m)', segments), 1);
recCol  = find(strcmpi('recruits per sq m adjusted', segments), 1);

flnm='Data/RecruitsRockStrataAdjustment.csv';
fprintf('Reading from %s\n', flnm)
if isOctave
    F=csvreadK(flnm);
    DecYr= F(:,yearCol);
    lat=   F(:,latCol);
    lon=   F(:,lonCol);
    utmx=  F(:,utmxCol);
    utmy=  F(:,utmyCol);
    Depth= F(:,zCol);
    rec=   F(:,recCol);
else
    F=readtable(flnm,"FileType","text");
    DecYr=table2array(F(:,yearCol));
    lat  =table2array(F(:,latCol));
    lon  =table2array(F(:,lonCol));
    utmx =table2array(F(:,utmxCol));
    utmy =table2array(F(:,utmyCol));
    Depth=table2array(F(:,zCol));
    rec  =table2array(F(:,recCol));
end

if ~strcmp(domain, 'AL')
    if strcmp(domain, 'GB')
        j = lon>-70.5;
    else
        j = lon<=-70.5;
    end
    M=[DecYr(j),lat(j),lon(j),utmx(j),utmy(j),Depth(j),rec(j)];
else
    M=[DecYr, lat, lon, utmx, utmy, Depth, rec];
end

flnm=['Data/Recruits', domain, '.csv'];
header='decimal year,latitude,longitude,utm x,utm y,bottom depth(m),recruits per sq m';
fprintf('Writing to %s\n', flnm)
writecsv(M,flnm,'%f, %f, %f, %f, %f, %f, %e',header);
fprintf('Reading from %s\n', flnm);
if isOctave
    F=csvreadK(flnm);
    DecYr=F(:,yearCol);
else
    F=readtable(flnm,"FileType","text");
    DecYr=table2array(F(:,yearCol));
end
yearMin = min(floor(DecYr));
yearMax = max(floor(DecYr));
if yrStart >= yearMin && yrEnd <= yearMax
    for yr=yrStart:yrEnd
        j=find(floor(DecYr)==yr);
        if isOctave
            M=F(j,:);
        else
            M=table2array(F(j,:));
        end
        flnm=['Data/Recruits',int2str(yr),domain,'.csv'];
        writecsv(M,flnm,'%f, %f, %f, %f, %f, %f, %e',header);
        fprintf('Writing to %s. Number of records %d\n', flnm, size(M,1))
    end
else
    fprintf('INPUT YEARS OUT OF RANGE: %i to %i : actual %i to %i\n', yearMin, yearMax, yrStart, yrEnd )
    msg = sprintf( 'INPUT YEARS OUT OF RANGE: %i to %i : actual %i to %i\n', yearMin, yearMax, yrStart, yrEnd);
    errorStruct.message = msg;
    errorStruct.identifier = 'myComponent:inputError';
    error(errorStruct)
end
