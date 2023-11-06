
function SimSummaryMA(direct,StartYr,StopYr)
  Dom='MA';
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
  xs=max(xg)-min(xg);
  cax=[0,1]
  for yr=StartYr:StopYr,
    F=csvreadK([direct,'State',int2str(yr),'.csv']);
    f=sum(F')';
    %f=load(['Output/Scallop/IndvCaught',int2str(yr),'.txt']);
    clf;
    patch(xg(E'),yg(E'),f(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)

    text(min(xg),max(yg)+10000,['surv_n: ',int2str(yr)])
   set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    h=colorbar('west')
    set(h,'position',[.55,.2,.025,.37])
    flnm=[direct,'State',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
end

close all;
cax=[0,1.];
 for yr=StartYr:StopYr,
    F=load([direct,'F',int2str(yr),'.txt']);
    clf;
    patch(xg(E'),yg(E'),F(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)

    text(min(xg),max(yg)-10000,['F: ',int2str(yr)])
    set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    h=colorbar('west')
    set(h,'position',[.55,.2,.025,.37])
   flnm=[direct,'FMA',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
end
    

close all;
cax=[0,2.]
towarea=1852*4.5*(30*0.3048)
cax=cax*towarea
 for yr=StartYr:StopYr,
    %USD=load([direct,'Dollars',int2str(yr),'.txt']);
%   USD=load([direct,'USD',int2str(yr),'.txt']);
    USD=csvreadK([direct,'USD',int2str(yr),'.csv']);USD=USD(:,1);
    clf;
    patch(xg(E'),yg(E'),USD(E')*towarea);shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)

    text(min(xg),max(yg)-10000,['USD Per Tow ',int2str(yr)])
   set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    h=colorbar('west')
    set(h,'position',[.55,.2,.025,.37])
    flnm=[direct,'DollarsPerTow',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
    pause(1)
end
    

close all;
cax=[0,1.]
 for yr=StartYr:StopYr,
    USD=load([direct,'Abundance',int2str(yr),'.txt']);
    clf;
    patch(xg(E'),yg(E'),USD(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)

    text(min(xg),max(yg)-10000,['Abundance Per M^2 ',int2str(yr)])
   set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    h=colorbar('west')
    set(h,'position',[.55,.2,.025,.37])
   flnm=[direct,'Abundance',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
    pause(1)
end
    
l=30:5:150;
close all;
cax=[0,1]
 for yr=StartYr:StopYr,
%    S=csvreadK([direct,'Selectivity',int2str(yr),'.csv']);
    F=csvreadK([direct,'NaturalMortality',int2str(yr),'.csv']);
    S=csvreadK([direct,'State',int2str(yr),'.csv']);
   % r=load(['Output/Sim',Dom,int2str(yr),'/KrigingEstimate.txt']);
    r=sum(S(:,1:7)')';
    clf;
    fj=F(:,1);fa=F(:,end);
    %for k=1:length(xg)
    %  f(k)=interp1(S(k,:),l(:)',.5);
    %endfor
    patch(xg(E'),yg(E'),fj(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)
    patch(xg(E')+xs,yg(E'),r(E')/max(r));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X+xs,G(k).Y,'k');end;colormap('jet');caxis(cax)

    text(min(xg),max(yg)-10000,['Juvanile mortality ',int2str(yr)])
    text(min(xg)+xs+25000,max(yg)-10000,['Relative Juvaniles'])
   set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    
       h=colorbar('west')
    set(h,'position',[.725,.2,.025,.37])

    flnm=[direct,'MortalityJuv',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);
    pause(1)
end
    

if (Dom=='MA')
  CompHabCamDataFigMA(direct)
else
  CompHabCamDataFigGB(direct)
endif
  xs=max(xg)-min(xg);
  %cax=[0,1]
  close all;
  ys=max(yg)-min(yg);ys=-ys;
  cax=[-3,-1]
  for yr=StartYr:StopYr,
    clf;
    F=csvreadK([direct,'Cnts',int2str(yr),'.csv']);

    f=log10(.001+F(:,1));
    patch(xg(E'),yg(E'),f(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax)
    text(min(xg)+5000,max(yg)+10000,['U10: ',int2str(yr)])

    f=log10(.001+F(:,2));
    patch(xg(E')+xs,yg(E'),f(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X+xs,G(k).Y,'k');end;colormap('jet');caxis(cax)
    text(min(xg)+5000+xs,max(yg)+10000,['10-20: '])

    f=log10(.001+F(:,3));
    patch(xg(E'),yg(E')+ys,f(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X,G(k).Y+ys,'k');end;colormap('jet');caxis(cax)
    text(min(xg)+5000,max(yg)+10000+ys,['20-30: '])

    f=log10(.001+F(:,4));
    patch(xg(E')+xs,yg(E')+ys,f(E'));shading interp;colormap('jet');
    hold on;for k=1:N,plot(G(k).X+xs,G(k).Y+ys,'k');end;colormap('jet');caxis(cax)
    text(min(xg)+5000+xs,max(yg)+10000+ys,[' 30+ '])

 
 set(gca,'visible','off')
    d=daspect();daspect([max(d),max(d),1]);
    h=colorbar('west')
    set(h,'position',[.61,.2,.025,.25])
    flnm=[direct,'Cnts',int2str(yr),'.jpg']
    eval(['print -djpeg ',flnm]);
    system(['convert -trim ',flnm,' ',flnm]);

end

PlotRegionY(direct,Dom,StartYr,StopYr)

endfunction
