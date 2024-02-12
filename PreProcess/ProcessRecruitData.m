% "","area","subarea","cruise6","year","month","day","time","stratum","tow",
% "station","statype","SVGEAR","haul","gearcon","sdefid","newstra","clop","lat","lon",
%"tnms","setdpth","bottemp","dopdisb","distused","towadj","towdur","sizegrp","catchnu","catchwt",
%"surv_n","partrecn","fullrecn","surv_b","partrecb","fullrecb","postow","datasource","lwarea","SETLW",
%"SVSPP","PropChains","AREAKIND","STRATMAP","SQNM","ysta","gperiod","recruit","linf","K",
%"date","days_from_apr1","sh_apr1","upper","rec""
function ProcessRecruitData(yrStart, yrEnd)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

towArea_sqm = 4516;
T2M2=1./towArea_sqm;
 
Detect=.4;
DetectRS=.27;
DetectHD=.13;
DetectHDThreshold=2;%scallops/m^2

%flnm = 'OriginalData/recruitsv2.csv'; yr_col=5;
flnm = 'OriginalData/NewRecruits.csv'; yr_col=4;
%flnm = 'OriginalData/recruitsv2KWS.csv';
% data files differ by one column, using year column as reference
mon_col=yr_col+1;
day_col=mon_col+1;
lat_col=yr_col+14;
lon_col=yr_col+15;
depth_col=yr_col+17;
fprintf('Reading from %s\n', flnm)

if isOctave
  F=csvreadK(flnm);
  year=F(:,yr_col);mon=F(:,mon_col);day=F(:,day_col);
else
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F= readtable(flnm,"FileType","text");
  year=table2array(F(:,yr_col));mon=table2array(F(:,mon_col));day=table2array(F(:,day_col));
end
% if NAN or blank, i.e. 0
j=find(isnan(mon));mon(j)=6;day(j)=21;% assign missing date to summer solstice
j=find(~(mon));mon(j)=6;day(j)=21;% assign missing date to summer solstice

N=length(year);
for n=1:N
  yd(n) = yearday(mon(n),day(n),0);%ignore leap years
end
yd=yd(:);
DecYr=year(:)+( yd(:)/365.25 );

if isOctave
  lat=F(:,lat_col);lon=-F(:,lon_col);
  Rec=F(:,end);
  Depth=F(:,depth_col);
else
  lat=table2array(F(:,lat_col));lon=-table2array(F(:,lon_col));
  Rec=table2array(F(:,end));
  Depth=table2array(F(:,depth_col));
end

RecM2=Rec*T2M2;
M=[DecYr(:),lat(:),lon(:),Depth(:),RecM2(:)];
j=find(~isnan(sum(M')));M=M(j,:);
flnm='Data/RecruitsUnadjusted.csv';
header='"decmal year", "latitude", "longitude", "bottom depth(m)","recruits per m^2"';
fprintf('Writing to %s\n\n', flnm)
writecsv(M,flnm,['%g, %g, %g, %g, %e'],header);


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
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F=table2array(readtable(flnm,'PreserveVariableNames', true));
end

[N,five]=size(F);lat=F(:,2);lon=F(:,3);DecYr=F(:,1);Depth=F(:,4);Rec=F(:,5);
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
M=[DecYr(:),lat(:),lon(:),Depth(:),Rec(:),IsRock(:),RecA(:)];
j=find(~isnan(sum(M')));M=M(j,:);
flnm='Data/RecruitsRockStrataAdjustment.csv';
header='"decmal year", "latitude", "longitude", "bottom depth(m)","recruits per sq m raw","Is Rock Strata","recruits per sq m adjusted"';
writecsv(M,flnm,['%g, %g, %g, %g, %e, %i ,%e' ],header);
fprintf('Writing to %s\n\n', flnm)

close all;
j0=find(IsRock==0);
j1=find(IsRock==1);
plot(lon(j0),lat(j0),'c.',lon(j1),lat(j1),'r.');
hold on;
for k=1:length(G)
  lonStrat=G(k).X;
  latStrat=G(k).Y;
  plot(lonStrat,latStrat,'k');hold on;
end
for k=1:NRS
  lonStrat=G(RSI(k)).X;
  latStrat=G(RSI(k)).Y;
  plot(lonStrat,latStrat,'r');
end
if isOctave % Octave did not need 3rd term
  daspect([1,cos(mean(lat)*pi/180)]);
else
  daspect([1,cos(mean(lat)*pi/180), 1]);
end
print -djpeg RockStrata.jpg
%XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

%clear
%isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
flnm='Data/RecruitsRockStrataAdjustment.csv';
fprintf('Reading from %s\n', flnm)
if isOctave
  F=csvreadK(flnm);
  DecYr=F(:,1);
  lat=F(:,2);
  lon=F(:,3);
  Depth=F(:,4);
  rec=F(:,7);
else
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F=readtable(flnm,"FileType","text");
  DecYr=table2array(F(:,1));
  lat=table2array(F(:,2));
  lon=table2array(F(:,3));
  Depth=table2array(F(:,4));
  rec=table2array(F(:,7));
end
jMA=find(lon<-70.5);
jGB=find(lon>-70.5);
[xMA,yMA]=ll2utm(lat,lon,18);
[xGB,yGB]=ll2utm(lat,lon,19);

M=[DecYr(jMA),xMA(jMA),yMA(jMA),Depth(jMA),rec(jMA)];
flnm='Data/RecruitsMA.csv';
header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per sq m"';
fprintf('Writing to %s\n', flnm)
writecsv(M,flnm,'%g, %f, %f, %f, %e',header);

M=[DecYr(jGB),xGB(jGB),yGB(jGB),Depth(jGB),rec(jGB)];
flnm='Data/RecruitsGB.csv';
header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per sq m"';
fprintf('Writing to %s\n\n', flnm)
writecsv(M,flnm,'%g, %f, %f, %f, %e',header);
%XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
flnm='Data/RecruitsMA.csv';
fprintf('Reading from %s\n', flnm)
if isOctave
  F=csvreadK(flnm);
  DecYr=F(:,1);
else
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F=readtable(flnm,"FileType","text");
  DecYr=table2array(F(:,1));
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
      G = LumpDataDx (M,1000.);
      flnm=['Data/Recruits',int2str(yr),'MA.csv'];
      header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per sq m"';
      writecsv(G,flnm,'%g, %f, %f, %f, %e',header);
    end
    fprintf('Writing to Data/RecruitsYYYYMA.csv  From %i to %i\n\n', yrStart, yrEnd)
else
    fprintf('INPUT YEARS OUT OF RANGE: %i to %i', yearMin, yearMax)
end

%XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
flnm='Data/RecruitsGB.csv';
fprintf('Reading from %s\n', flnm)
if isOctave
  F=csvreadK(flnm);
  DecYr=F(:,1);
else
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F=readtable(flnm,"FileType","text");
  DecYr=table2array(F(:,1));
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
      G = LumpDataDx (M,1000.);
      flnm=['Data/Recruits',int2str(yr),'GB.csv'];
      header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per sq m"';
      writecsv(G,flnm,'%g, %f, %f, %f, %e',header);
    end
    fprintf('Writing to Data/RecruitsYYYYGB.csv  From %i to %i\n\n', yrStart, yrEnd)
else
    fprintf('INPUT YEARS OUT OF RANGE: %i to %i', yearMin, yearMax)
end    
%----------------------------------------------------------------------------
% DEPRECATE
%----------------------------------------------------------------------------
% %XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
% flnm='Data/RecruitsMA.csv';
% fprintf('Reading from %s\n', flnm)
% if isOctave
%   F=csvreadK(flnm);
% else
%   warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
%   F=table2array(readtable(flnm,"FileType","text"));
% end
% Fout = LumpData1NM(F);
% flnm='Data/RecruitsMALump1NM.csv';
% header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per sq m"';
% fprintf('Writing to %s\n\n', flnm)
% writecsv(Fout,flnm,['%g, %f, %f, %f, %e'],header);
% %XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
% flnm='Data/RecruitsGB.csv';
% fprintf('Reading from %s\n', flnm)
% if isOctave
%   F=csvreadK(flnm);
% else
%   warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
%   F=table2array(readtable(flnm,"FileType","text"));
% end
% Fout = LumpData1NM(F);
% flnm='Data/RecruitsGBLump1NM.csv';
% header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per sq m"';
% fprintf('Writing to %s\n\n', flnm)
% writecsv(Fout,flnm,['%g, %f, %f, %f, %e'],header);
%----------------------------------------------------------------------------
