

F=csvreadK('OrigonalData/HabcamDataAggregated_All_2011-2021.csv');
lon=F(:,5);
j=find(lon<-70.5);F=F(j,:);
year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);
year=year+.5;
[x,y]=ll2utm(lat,lon,18);
M=[year,x,y,z,f];

for yr=2011:2021
  j=find(floor(M(:,1))==yr);
  F=M(j,:);
  Fu=unique(F,'rows');
  flnm=['Data/HabCamData',int2str(yr),'MA.csv']
  header='"decmal year", "x utm", "y utm", "bottom depth(m)","Scallops per sq m"';
  writecsv(Fu,flnm,['%g, %f, %f, %f, %e'],header);
end




F=csvreadK('OrigonalData/HabcamDataAggregated_All_2011-2021.csv');
lon=F(:,5);
j=find(lon>-70.5);F=F(j,:);
year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);
year=year+.5;
[x,y]=ll2utm(lat,lon,19);
M=[year,x,y,z,f];

for yr=2011:2021
  j=find(floor(M(:,1))==yr);
  F=M(j,:);
  Fu=unique(F,'rows');
  flnm=['Data/HabCamData',int2str(yr),'GB.csv']
  header='"decmal year", "x utm", "y utm", "bottom depth(m)","Scallops per sq m"';
  writecsv(Fu,flnm,['%g, %f, %f, %f, %e'],header);
end


