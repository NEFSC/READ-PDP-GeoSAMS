function CompHabCamDataFigMA(direct)

F=csvreadK('OriginalData/HabcamDataAggregated_All_2011-2021.csv');
lon=F(:,5);
j=find(lon<-70.5);F=F(j,:);
year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);
  
  M=csvreadK('Grids/MAsquares.csv');E=M(:,1:4);
  M=csvreadK('Grids/MAxyzLatLon.csv');
  G = shaperead('ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');

 N=length(G);

xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);

%units in feet for tow data 8' x 1 nm to (n per m^2)
ne=100;
cax=[-2,0]
ys=0.;
xs=max(xg)-min(xg)-100000
yt=mean(yg);
xt=min(xg)+.125*xs;
close all;
A=1/(1862*.3034)
for yr=2012:2021,
  F=csvreadK('OriginalData/HabcamDataAggregated_All_2011-2021.csv');
  lon=F(:,5);
  j=find(lon<-70.5);F=F(j,:);
  year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);
 
  j=find(year==yr);
  S=csvread([direct,'State',int2str(yr),'.csv']);
  S( find(isnan(S)) )=0;
  fm=sum(S')';
  
  clear fmi w;
  for k=1:length(j)
    xl=x(j(k));yl=y(j(k));
    D=sqrt(  (xg-xl).^2 + (yg-yl).^2  ).^4;
    w=1./D;
    [k,sum(w)];
    w=w/sum(w);
    fmi(k)=sum(w.*fm);
    [d,n]=min(D);
    fmi(k)=fm(n);
  endfor
  fmi=fmi(:);
  clf
  colorscatterp(x(j),y(j),log10(A+f(j)),6,1000);shading interp;
  hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax);text(xt,yt,'HabCam')

  colorscatterp(x(j)+xs,y(j),log10(A+fmi),6,1000);shading interp;
  hold on;for k=1:N,plot(G(k).X+xs,G(k).Y,'k');end; colormap('jet');caxis(cax);text(xt+xs,yt,'Model')
  
  d=daspect();daspect([max(d),max(d),1]);caxis(cax);
  title(int2str(yr));
  set(gca,'visible','off');
  h=colorbar('west');
  set(h,'position',[.65,.2,.025,.4]);
  %ticks=-[2,1,0];
  ticks=log10([.01,.05,.1,.5,1])
  set(h,'ytick',ticks)
  set(h,'yticklabel',10.^(ticks));

  flnm=[direct,'HabCamComp',int2str(yr),'.jpg'];
  eval(['print -djpeg ',flnm]) ;
     
  pause(5);
 
  system(['convert -trim ',flnm,' ',flnm])
endfor
