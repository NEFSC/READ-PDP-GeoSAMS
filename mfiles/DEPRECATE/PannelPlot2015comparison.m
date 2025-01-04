function PannelPlot2015comparison

  Dom='MA'
  F=csvreadK('/home/keston/GeoSAMS/Data/Dredge_Grid_Estimates_2015.csv');
  flin=load('KrigingEstimate2015lin.txt');
  flog=load('KrigingEstimate2015log.txt');
  fobs=csvreadK('Data/Recruits2015MA.csv');
  
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
fobs=csvreadK('Data/Recruits2015MA.csv');
clf;colorscatterp(fobs(:,2),fobs(:,3),fobs(:,5),6,3000);colormap('jet');caxis([0,5]);shading interp;colorbar
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;
set(gca,'visible','off');
da=daspect();
daspect([max(da),max(da),1]);
flnm=['/home/keston/obs2015.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

F=csvreadK('/home/keston/GeoSAMS/Data/Dredge_Grid_Estimates_2015.csv');
clf;colorscatterp(F(:,1),F(:,2),F(:,6),6,2000);colormap('jet');caxis([0,5]);shading interp;colorbar
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;
set(gca,'visible','off')
da=daspect();
daspect([max(da),max(da),1]);
flnm=['/home/keston/GAM2015.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

f=load('KrigingEstimate2015log.txt');
clf;patch(xg(E'),yg(E'),f(E'));colormap('jet');;caxis([0,5]);shading interp;colorbar
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;
set(gca,'visible','off')
da=daspect();
daspect([max(da),max(da),1]);
flnm=['/home/keston/Interp2015log.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);

f=load('KrigingEstimate2015lin.txt');
clf;patch(xg(E'),yg(E'),f(E'));colormap('jet');;caxis([0,5]);shading interp;colorbar
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;
    set(gca,'visible','off')
da=daspect();
daspect([max(da),max(da),1]);
flnm=['/home/keston/Interp2015lin.jpg']
eval(['print -djpeg ',flnm]);
system(['convert -trim ',flnm,' ',flnm]);
