function PlotState


 g=1862.^2
 
  M=csvreadK('Grids/MAsquares.csv');E=M(:,1:4);
  M=csvreadK('Grids/MAxyzLatLon.csv');
  G = shaperead('ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');
  N=length(G);
xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);

 close all;
  xs=max(xg)-min(xg);
  cax=[0,1]
  for yr=2000:2019;
    F=csvreadK(['Output/Scallop/State',int2str(yr),'.csv']);
    f=sum(F')';
    %f=load(['Output/Scallop/IndvCaught',int2str(yr),'.txt']);
    clf;
    patch(xg(E'),yg(E'),f(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)

    text(min(xg),max(yg)+10000,['F: ',int2str(yr)])
   set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    flnm=['Output/Figures/State',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
end
    
    