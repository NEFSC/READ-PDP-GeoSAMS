function FishingEffortSAplotTog (directGB,directMA,StartYr,StopYr)
  
 g=1862.^2
  M0=csvreadK('Grids/MAsquares.csv');E0=M0(:,1:4);
  M0=csvreadK('Grids/MAxyzLatLon.csv');
  G0 = shaperead('ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');
  
  M1=csvreadK('Grids/GBsquares.csv');E1=M1(:,1:4);
  M1=csvreadK('Grids/GBxyzLatLon.csv');
  G1 = shaperead('ShapeFiles/GB_Estimation_Areas_2020_UTM19_PDT_NLSModified_022020.shp');
N1=length(G1);
long1=M1(:,5);latg1=M1(:,4);
N0=length(G0);
long0=M0(:,5);latg0=M0(:,4);
latg=[latg0;latg1];
long=[long0;long1];

  xs=max(long)-min(long);
  cax=[0,1]
  close all;
  ys=max(latg)-min(latg);ys=-ys;

G=shaperead('/home/keston/work/GeoSAMS/ShapeFiles/EastCoast.shp');
N=length(G);
close all;
cax=[0,1.];
 for yr=StartYr:StopYr,
    F1=load([directGB,'F',int2str(yr),'.txt']);
    F0=load([directMA,'F',int2str(yr),'.txt']);
    F=[F0;F1];
    clf;
    patch(long0(E0'),latg0(E0'),F0(E0')/max(F));shading interp;colormap('jet');
    patch(long1(E1'),latg1(E1'),F1(E1')/max(F));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;
    axis([min(long),max(long),min(latg),max(latg)])
   text(-72,42,['F: ',int2str(yr)])
   set(gca,'visible','off')
   daspect([1,cos(mean([latg0;latg1])*pi/180)]);

    flnm=[directGB,'FStockAssTog',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
end
     
 for yr=StartYr:StopYr,
    F1=load([directGB,'F',int2str(yr),'.txt']);
    F0=load([directMA,'F',int2str(yr),'.txt']);
    F=log(.01+[F0;F1]);
    F0=log(.01+F0);
    F0=[F0-(min(F))]/[max(F)-min(F)];
    F1=log(.01+F1);
     F1=[F1-(min(F))]/[max(F)-min(F)];
   
    F=[F0;F1];
    clf;
    patch(long0(E0'),latg0(E0'),F0(E0')/max(F));shading interp;colormap('jet');
    patch(long1(E1'),latg1(E1'),F1(E1')/max(F));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;
    axis([min(long),max(long),min(latg),max(latg)])
   text(-72.5,42,['log scale F: ',int2str(yr)])
   set(gca,'visible','off')
   daspect([1,cos(mean([latg0;latg1])*pi/180)]);

    flnm=[directGB,'FStockAssTogLog',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
end
    