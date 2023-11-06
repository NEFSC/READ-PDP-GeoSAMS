

clear

TowSqFt=6076.12*8
MpSqft=.3048^2;
T2M2=1/(MpSqft*TowSqFt)
 
Detect=.4;
DetectRS=.27;
DetectHD=.13;
DetectHDThreshold=2;%scallops/m^2

M=csvreadK('OrigonalData/dredgetowbysize7917.csv');

 mon=M(:,5);
 j=find(mon>0);
 M=M(j,:);
 year=M(:,4);
 sc=M(:,27);
 mon=M(:,5);
 day=M(:,6);
 
 lon=-M(:,19);%GB
 j=find(lon>-70.5);
 M=M(j,:);
 year=M(:,4);
 sc=M(:,27);
 mon=M(:,5);
 day=M(:,6);
 
 %RecM2=Rec*T2M2;
 
 yd=0*day;
 for k=1:length(day);
   yd(k)=yearday(mon(k),day(k),0);
end

DecYr=year(:)+( yd(:)/365.25 );

lat=M(:,18);
lon=-M(:,19);
[xx,yy]=ll2utm(lat,lon,19);

for yr=1979:2019
 X=[];
 j=find(and(year==yr,sc==3.));
 a=M(j,30);
 sci=M(j,27);
 lat=M(j,18);
 lon=-M(j,19);
 mon=M(j,5);
 day=M(j,6);
 z=M(j,21);
 x=xx(j);y=yy(j);
 sl=3:.5:18;
 for k=1:length(day);
   yd(k)=yearday(mon(k),day(k),0);
 end
 for k=1:length(j)
  n=j(k);
  X=[X;DecYr(k),x(k),y(k),lat(k),lon(k),z(k),M(n:n+30,30)'*T2M2/Detect];
 end
flnm=['Data/bin5mm',int2str(yr),'GB.csv']
csvwrite(flnm,X);


endfor




clear

TowSqFt=6076.12*8
MpSqft=.3048^2;
T2M2=1/(MpSqft*TowSqFt)
 
Detect=.4;
DetectRS=.27;
DetectHD=.13;
DetectHDThreshold=2;%scallops/m^2

M=csvreadK('OrigonalData/dredgetowbysize7917.csv');

 mon=M(:,5);
 j=find(mon>0);
 M=M(j,:);
 year=M(:,4);
 sc=M(:,27);
 mon=M(:,5);
 day=M(:,6);
 
 lon=-M(:,19);%MA
 j=find(lon<-70.5);
 M=M(j,:);
 year=M(:,4);
 sc=M(:,27);
 mon=M(:,5);
 day=M(:,6);
 
 %RecM2=Rec*T2M2;
 
 yd=0*day;
 for k=1:length(day);
   yd(k)=yearday(mon(k),day(k),0);
end

DecYr=year(:)+( yd(:)/365.25 );

lat=M(:,18);
lon=-M(:,19);
[xx,yy]=ll2utm(lat,lon,18);

for yr=1979:2019
 X=[];
 j=find(and(year==yr,sc==3.));
 a=M(j,30);
 sci=M(j,27);
 lat=M(j,18);
 lon=-M(j,19);
 mon=M(j,5);
 day=M(j,6);
 z=M(j,21);
 x=xx(j);y=yy(j);
 sl=3:.5:18;
 for k=1:length(day);
   yd(k)=yearday(mon(k),day(k),0);
 end
 for k=1:length(j)
  n=j(k);
  X=[X;DecYr(k),x(k),y(k),lat(k),lon(k),z(k),M(n:n+30,30)'*T2M2/Detect];
 end
flnm=['Data/bin5mm',int2str(yr),'MA.csv']
csvwrite(flnm,X);
endfor




