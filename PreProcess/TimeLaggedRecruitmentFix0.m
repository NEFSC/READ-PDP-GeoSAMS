 F=csvread('recruitsv2KWS.csv');
%F=csvread('gbdredgeKSMod.csv');
F=F(2:end,:);
year=F(:,5);mon=F(:,6);day=F(:,7);
j=find(~isnan(mon+day));
F=F(j,:);% remove records without date (336/20k)
year=F(:,5);mon=F(:,6);day=F(:,7);
lat=F(:,19);lon=-F(:,20);
j=find(lon>-70.5);%GB
F=F(j,:);% remove records without date (336/20k)
year=F(:,5);mon=F(:,6);day=F(:,7);
lat=F(:,19);lon=-F(:,20);

yd = yearday(mon,day,0+0*day);
yd0 = yearday(4,1,0);
dt=(yd-yd0)/365.25;

lat=F(:,19);lon=-F(:,20);
upper=F(:,end-1);
sh0=F(:,end-2);
dfApr1=F(:,end-3);
K=F(:,end-4);
Linf=F(:,end-5);
%recs=F(:,end)/.4;
recs=F(:,end);
Z=-F(:,22);


M=load('SAMS_GB_squares.txt');E=M(:,1:4);
M=csvread('SAMS_GB_nodes_utm_z_latlon.csv');
xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);
addpath /home/keston/mfiles/mapping-matlab-master/latlonutm/
[x,y]=ll2utm(lat,lon,19);
[zp,H] = SAMSgrid_interp (xg,yg,zg,E,x,y);
j=find(isnan(zp));zp(j)=Z(j)
j=find(zp<-250);
zp(j)=-250;

for yr=1979:2019
  j=find(yr==year);
  [xo,yo,zo,fo] = clean_scallop_data (x(j),y(j),zp(j),recs(j))
  %M=[x(j),y(j),zp(j),recs(j)];
  M=[xo,yo,zo,fo];
  flnm=['SAMSrecreuitLinear0GB',int2str(yr),'.csv']
  csvwrite (flnm, M);
endfor


 F=csvread('recruitsv2KWS.csv');
%F=csvread('gbdredgeKSMod.csv');
F=F(2:end,:);
year=F(:,5);mon=F(:,6);day=F(:,7);
j=find(~isnan(mon+day));
F=F(j,:);% remove records without date (336/20k)
year=F(:,5);mon=F(:,6);day=F(:,7);
lat=F(:,19);lon=-F(:,20);
j=find(lon<-70.5);%MA
F=F(j,:);% remove records without date (336/20k)
year=F(:,5);mon=F(:,6);day=F(:,7);
lat=F(:,19);lon=-F(:,20);

yd = yearday(mon,day,0+0*day);
yd0 = yearday(4,1,0);
dt=(yd-yd0)/365.25;

lat=F(:,19);lon=-F(:,20);
upper=F(:,end-1);
sh0=F(:,end-2);
dfApr1=F(:,end-3);
K=F(:,end-4);
Linf=F(:,end-5);
%recs=F(:,end)/.4;
recs=F(:,end);
Z=-F(:,22);

addpath /home/keston/mfiles/mapping-matlab-master/latlonutm/
[x,y]=ll2utm(lat,lon,18);

M=load('SAMS_MA_squares.txt');E=M(:,1:4);
M=csvread('SAMS_MA_nodes_utm_z_latlon.csv');
xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);
addpath /home/keston/mfiles/mapping-matlab-master/latlonutm/
[x,y]=ll2utm(lat,lon,18);
[zp,H] = SAMSgrid_interp (xg,yg,zg,E,x,y);
j=find(isnan(zp));zp(j)=Z(j)

j=find(zp<-250);
zp(j)=-250;
for yr=1979:2019
  j=find(yr==year);
  [xo,yo,zo,fo] = clean_scallop_data (x(j),y(j),zp(j),recs(j))
 % M=[x(j),y(j),zp(j),recs(j)];
  M=[xo,yo,zo,fo];
  flnm=['SAMSrecreuitLinear0MA',int2str(yr),'.csv']
  csvwrite (flnm, M);
endfor
