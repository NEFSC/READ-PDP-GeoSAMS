function DomainPlot 
  
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
  for k=1:N0,
    [G0(k).lat,G0(k).lon]=utm2ll(G0(k).X,G0(k).Y,18);
    end

  for k=1:N1,
    [G1(k).lat,G1(k).lon]=utm2ll(G1(k).X,G1(k).Y,19);
    end



long0=M0(:,5);latg0=M0(:,4);
zg0=M0(:,3);
zg1=M1(:,3);
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


   clf;
    patch(long0(E0'),latg0(E0'),-zg0(E0'));shading interp;colormap('jet');
    patch(long1(E1'),latg1(E1'),-zg1(E1'));shading interp;colormap('jet');
    axis([min(long),max(long),min(latg),max(latg)])
   %text(-72,42,['F: ',int2str(yr)])
   set(gca,'visible','off')
   daspect([1,cos(mean([latg0;latg1])*pi/180)]);
   hold on;for k=1:N0,plot(G0(k).lon,G0(k).lat,'k');end;
   hold on;for k=1:N1,plot(G1(k).lon,G1(k).lat,'k');end;
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;
    caxis([-130,-20])
 h=colorbar('west')
 set(h,'position',[.75,.1,.025,.5])
 flnm=['Documentation/Domain.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);

     