function FishingEffortSAplot (Dom,direct,StartYr,StopYr)
  
 g=1862.^2
if (Dom=='MA') 
  M=csvreadK('Grids/MAsquares.csv');E=M(:,1:4);
  M=csvreadK('Grids/MAxyzLatLon.csv');
  G = shaperead('ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');
end
if (Dom=='GB') 
  M=csvreadK('Grids/GBsquares.csv');E=M(:,1:4);
  M=csvreadK('Grids/GBxyzLatLon.csv');
  G = shaperead('ShapeFiles/GB_Estimation_Areas_2020_UTM19_PDT_NLSModified_022020.shp');
end
N=length(G);
xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);


  xs=max(xg)-min(xg);
  cax=[0,1]
  close all;
  ys=max(yg)-min(yg);ys=-ys;


close all;
cax=[0,1.];
 for yr=StartYr:StopYr,
    F=load([direct,'F',int2str(yr),'.txt']);
    clf;
    patch(xg(E'),yg(E'),F(E')/max(F));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)
    FL= log(.01+F);
    FL=(FL-min(FL))/(max(FL)-min(FL));
    patch(xg(E'),yg(E')+ys,FL(E') );
    shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)

    text(min(xg),max(yg)+10000,['F: ',int2str(yr)])
    text(min(xg),max(yg)+10000+ys,['log(F): ',int2str(yr)])
   set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    flnm=[direct,'FStockAss',Dom,int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
end
    