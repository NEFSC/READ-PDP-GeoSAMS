function GridFieldPlot (flnm,Dom,LogTrans)
f=load(flnm);
if strcmp(Dom,'GB')
    M=load('Grids/GBsquares.csv');E=M(:,1:4);
    M=csvread('Grids/GBxyzLatLon.csv');
    G = shaperead('ShapeFiles/GB_Estimation_Areas_2020_UTM19_PDT_NLSModified_022020.shp');
    N=length(G);
    for k=1:N,
      [G(k).LAT,G(k).LON]=utm2ll(G(k).X,G(k).Y,19);
    end
    xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);
    xs=max(xg)-min(xg);
    ys=max(yg)-min(yg);ys=-ys;
    xst=-60000;
    yst=-25000;
    cax=[0,4];
end

if strcmp(Dom,'MA')
    M=load('Grids/MAsquares.csv');E=M(:,1:4);
    M=csvread('Grids/MAxyzLatLon.csv');
    G = shaperead('ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');
    N=length(G);
    for k=1:N,
      [G(k).LAT,G(k).LON]=utm2ll(G(k).X,G(k).Y,18);
    end
    xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);
    xs=max(xg)-min(xg);
    ys=max(yg)-min(yg);ys=-ys;
    xst=-60000;
    yst=-25000;
    cax=[0,3.5];
end
%close all
clf
if (LogTrans==1)
  A=10^-6
  patch(xg(E'),yg(E'),log10(A+f(E')));shading interp;colormap('jet');%caxis(cax);
else
  patch(xg(E'),yg(E'),f(E'));shading interp;colormap('jet');%caxis(cax);
end
  hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end
%text(mean(xg)+xst+xs,max(yg)+yst,'krig field');

da=daspect()
daspect([max(da),max(da),1]);
set(gca,'visible','off')
%flnm=[direct,'RecPanPlot.jpg']eval(['print -djpeg ',flnm]);system(['convert -trim ',flnm,' ',flnm]);

end
