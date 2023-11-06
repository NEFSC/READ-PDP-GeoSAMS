function PlotRegionY(direct,Dom,StartYr,StopYr)

if (Dom=='MA'),
  load Data/ManagementAreasMA.mat
else
  load Data/ManagementAreasGB.mat
endif
whos

flnm=[direct,'ManagementRegion1.csv'];
F=csvread(flnm);
[n,m]=size(F)
nyr=(StopYr-StartYr+1)
tspy=n/nyr
dt=1/tspy
t=StartYr+dt*[1:n];

Ru=unique(R);
NRu=length(Ru)
G=zeros(NRu,25,m);
for k=1:NRu,
  flnm=[direct,'ManagementRegion',int2str(k),'.csv'];
  F=csvread(flnm);
  j=find(R==Ru(k));
  t=StartYr+dt*[1:length(F)];
  for yr=StartYr:StopYr
      n=find(floor(t)==yr);
      tmp=mean(F(n,:));
      G(k,yr-StartYr+1,:)=mean(F(n,:));
     l=3:.5:15;
  end
end


close all;

if (Dom=='MA'),
  load Data/ManagementAreasMA.mat Region
else
  load Data/ManagementAreasGB.mat Region
endif
for k=1:length(Ru);
  j=find(R);

  flnm=[direct,'ManagementRegion',int2str(k),'.csv'];
  F=csvread(flnm);
 t=StartYr+dt*[1:length(F)];
 l=3:.5:15;
cax=[0,3*max(max(F))/4];
cax=[0,2.7];
 clf; pcolor(t,l,F');
 shading interp;colormap('jet');%caxis(cax);pause(1);
 title(Region(k).Name);
 text(mean(t),1,'year')
 h=text(StartYr-2,5,'shell height (mm)')
 set(h,'rotation',90)
 colorbar;
 pause(1);  
 flnm=[direct,'ManagementRegionColor',int2str(k),'.jpg']
  eval(['print -djpeg ',flnm]); 
  system(['convert -trim ',flnm,' ',flnm]); 

end
if(0==1)
close all;
colormap('jet');caxis(cax)
h=colorbar('South')
set(gca,'visible','off');
set(h,'xtick',[0,1,2,2.7]);set(h,'xticklabel',[1,10,100,500]); 
 flmm=[direct,'ManagementRegionColorbar.jpg']
  eval(['print -djpeg ',flnm]); 
  system(['convert -trim ',flnm,' ',flnm]); 
end