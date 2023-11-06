function PannelPlot (yr,Dom)
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
    cax=[-4,-1];
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
    cax=[-4,-1];
endif


close all;
direct=['Output/Sim',Dom,'lin',int2str(yr),'/'];
%direct=['Output/Sim',Dom,'Clim',int2str(yr),'/'];

A=10.^-6

flnm=['Data/Recruits',int2str(yr),Dom,'.csv']
F=csvreadK(flnm);
x=F(:,2);y=F(:,3);f=F(:,end);

cax=[min(log10(A+f)),max(log10(A+f))]
colorscatterp(x,y,log10(A+f),6,5000);shading interp;colormap('jet');caxis(cax);
hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end
text(mean(xg)+xst,max(yg)+yst,'data');
da=daspect()
set(gca,'visible','off')
daspect([max(da),max(da),1]);
flnm=[direct,'RecPanPlotA.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

flnm=[direct,'KrigingEstimate.txt'];
f=load(flnm);
patch(xg(E')+xs,yg(E'),log10(A+f(E')));shading interp;colormap('jet');caxis(cax);
hold on;for k=1:N,plot(G(k).X+xs,G(k).Y,'k');end
text(mean(xg)+xst+xs,max(yg)+yst,'krig field');
da=daspect()
set(gca,'visible','off')
daspect([max(da),max(da),1]);
flnm=[direct,'RecPanPlotB.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

flnm=[direct,'SpatialTrend.txt'];
ft=load(flnm);%ft=ft-mean(ft)+mean(f)

patch(xg(E')+xs+xs,yg(E'),log10(A+ft(E')));shading interp;colormap('jet');caxis(cax);
hold on;for k=1:N,plot(G(k).X+xs+xs,G(k).Y,'k');end
text(mean(xg)+xst+xs+xs,max(yg)+yst,'trend');


if strcmp(Dom,'GB'),
  text(max(xg)-35000,max(yg)-7500,int2str(yr))
endif
if strcmp(Dom,'MA'),
  text(mean(xg)+xst,min(yg)+65000,int2str(yr))
endif


da=daspect()
set(gca,'visible','off')
daspect([max(da),max(da),1]);
flnm=[direct,'RecPanPlotC.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

if(strcmp(Dom,'MA'))
  vtick=[1,10,100,500,1000,10.^max(cax)];
endif
if(strcmp(Dom,'GB'))
  vtick=[1,10,100,500,1000,5000,10.^max(cax)];
endif

cax=[-4,-1]
vtick=[-4:-1];
vticklabel=10.^vtick;

clf
cax=[-4,-1]
caxis(cax);
h=colorbar("north");colormap('jet')
set(gca,'visible','off');
set(h,'xtick',vtick);
set(h,'xticklabel',vticklabel);
flnm=['Output/Figures/ColorbarIntpH',Dom,'.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

clf
caxis(cax);
h=colorbar("west");colormap('jet')
set(gca,'visible','off');
set(h,'ytick',vtick);
set(h,'yticklabel',vticklabel);
flnm=['Output/Figures/ColorbarIntpV',Dom,'.jpg']
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

