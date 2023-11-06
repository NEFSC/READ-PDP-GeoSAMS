function GridFieldPlot (flnm,Dom,LogTrans)
f=load(flnm);
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
cax=[0,4];
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
cax=[0,3.5];
endif
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



endfunction
