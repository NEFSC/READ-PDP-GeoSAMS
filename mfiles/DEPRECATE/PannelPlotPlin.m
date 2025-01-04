function PannelPlotPlin (yr,Dom,lambda)
if strcmp(Dom,'GB')
    M=load('Grids/GBsquares.csv');E=M(:,1:4);
    M=csvread('Grids/GBxyzLatLon.csv');
    G = shaperead('ShapeFiles/GB_Est_Areas_2015_UTM19_PDT_Update_Habcam.shp');
    N=length(G);
    for k=1:N,
      [G(k).LAT,G(k).LON]=utm2ll(G(k).X,G(k).Y,19);
    end
    xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);
    xs=max(xg)-min(xg);
    ys=max(yg)-min(yg);ys=-ys;
    xst=-60000;
    yst=-25000;
endif

if strcmp(Dom,'MA')
    M=load('Grids/MAsquares.csv');E=M(:,1:4);
    M=csvread('Grids/MAxyzLatLon.csv');
    G = shaperead('ShapeFiles/MAB_Est_Areas_2015_UTM18_PDT_Habcam.shp');
    N=length(G);
    for k=1:N,
      [G(k).LAT,G(k).LON]=utm2ll(G(k).X,G(k).Y,18);
    end
    xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);
    xs=max(xg)-min(xg);
    ys=max(yg)-min(yg);ys=-ys;
    xst=-60000;
    yst=-25000;
endif

close all;
direct=['Output/Sim',Dom,'lin',int2str(yr),'/'];
%direct=['Output/Sim',Dom,'Clim',int2str(yr),'/'];


flnm=['Data/Recruits',int2str(yr),Dom,'.csv']
F=csvreadK(flnm);
x=F(:,2);y=F(:,3);fo=F(:,end);
flnm=[direct,'KrigingEstimate.txt'];
f=load(flnm);

cax=[0,max(fo.^lambda)]/2
cax=[0,max(f.^lambda)]
colorscatterp(x,y,fo.^lambda,6,2500);shading interp;colormap('jet');caxis(cax);
hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end
text(mean(xg)+xst,max(yg)+yst,'data');

flnm=[direct,'KrigingEstimate.txt'];
f=load(flnm);
patch(xg(E')+xs,yg(E'),f(E').^lambda);shading interp;colormap('jet');caxis(cax);
hold on;for k=1:N,plot(G(k).X+xs,G(k).Y,'k');end
text(mean(xg)+xst+xs,max(yg)+yst,'krig field');

flnm=[direct,'SpatialTrend.txt'];
ft=load(flnm);%ft=ft-mean(ft)+mean(f)

patch(xg(E'),yg(E')+ys,ft(E').^lambda);shading interp;colormap('jet');caxis(cax);
hold on;for k=1:N,plot(G(k).X,G(k).Y+ys,'k');end
text(mean(xg)+xst,max(yg)+yst+ys,'trend');

flnm=[direct,'Veps.txt'];
f=load(flnm);
%f=f-min(f);
patch(xg(E')+xs,yg(E')+ys,(2*f(E')).^lambda);shading interp;colormap('jet');caxis(cax);
hold on;for k=1:N,plot(G(k).X+xs,G(k).Y+ys,'k');end
text(mean(xg)+xst+xs,max(yg)+yst+ys,'2 \sigma');

if strcmp(Dom,'GB'),
  text(max(xg)-35000,max(yg)-7500,int2str(yr))
endif
if strcmp(Dom,'MA'),
  text(mean(xg)+xst,min(yg)+65000,int2str(yr))
endif

da=daspect()
set(gca,'visible','off')
daspect([max(da),max(da),1]);
flnm=[direct,'RecPanPlot.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);
pause(5)

clf
caxis(cax);
h=colorbar("north");colormap('jet')
set(gca,'visible','off');
r=get(h,'xtick');
p=r.^(1/lambda);
for k=1:length(p),
  if(p(k)>=2)
    p(k)=round(p(k))
  end
end
set(h,'xticklabel',p);
flnm=[direct,'ColorbarIntpH.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

clf
caxis(cax);
h=colorbar("west");colormap('jet')
set(gca,'visible','off');
r=get(h,'ytick');
p=r.^(1/lambda);
for k=1:length(p),
  if(p(k)>=2)
    p(k)=round(p(k))
  end
end
set(h,'yticklabel',p);
flnm=[direct,'ColorbarIntpV.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);


%addpath /home/keston/olddrive/usr/share/octave/3.6.4/m/statistics/distributions/
close all
f=load([direct,'OLSresidual.txt']);
n=length(f);
N=100000;
m=50;
mu=mean(f);
sig=std(f);
r=mu+sig*randn(N,1);
R=mu+sig*randn(n,m);
fsort=sort(f);
rsort=sort(r);
%plot(sort(f),[1:n]/n,'ro-',sort(r),[1:N]/N,'b');
plot(sort(R),[1:n]/n,'c--')
hold on;plot(sort(r),[1:N]/N,'k',"linewidth",2);
hold on;plot(sort(f),[1:n]/n,'r.-',"linewidth",3);
f1=interp1([1:N]/N,sort(r),[1:n]/n);
f0=interp1(rsort,[1:N]/N,fsort);
f1=f1(:);
f0=f0(:);
ks=abs(f0-[1:n]'/n);
%figure;plot([1:n]/n,fsort,'ro-',[1:n]/n,f1,'bx-')
[m,j]=max(ks);
%hold on;plot([fsort(j),fsort(j)],[f0(j),j/n],'k+-',"linewidth",5);text(fsort(j),[f0(j)+j/n]/2,num2str(m));
xlabel('log(c+RecruitAbundance)-trend');
ylabel('Cumultive Probability');
%eval(['print -djpeg RecKSPlot',Dom,int2str(yr),'.jpg']);
%swtest(f, .25)
flnm=[direct,'RecKSPlot.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

endfunction

