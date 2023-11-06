

F=csvread('/home/keston/Scallop/HabcamDataAggregated_All_2011-2021.csv');
lon=F(:,5);
j=find(lon<-70.5);F=F(j,:);
year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);


  M=load('SAMS_MA_squares.txt');E=M(:,1:4);
  M=csvread('SAMS_MA_nodes_utm_z_latlon.csv');
  G = shaperead('/home/keston/work/GeoSAMS/ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');
  N=length(G);

xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);

%units in feet for tow data 8' x 1 nm to (n per m^2)
sf=5280*8/(3.28^2);
ne=100;
cax=[1,4.5]
ys=0.;
xs=max(xg)-min(xg)-100000
yt=mean(yg);
xt=min(xg)+.125*xs;
for yr=2012:2021,
     F=csvread('/home/keston/Scallop/HabcamDataAggregated_All_2011-2021.csv');
    lon=F(:,5);
    j=find(lon<-70.5);F=F(j,:);
    year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);

     close all;
      j=find(year==yr);
      %S=csvread(['State',int2str(yr),'EnMean.csv']);
      S=csvread(['NoFishMort/State',int2str(yr),'.csv']);
      S( find(isnan(S)) )=0;
      fm=sum(S')';
      S=csvread(['FishMort/State',int2str(yr),'.csv']);
      S( find(isnan(S)) )=0;
      fmfm=sum(S')';
      
      clear fmi fmifm w;
      for k=1:length(j)
        xl=x(j(k));yl=y(j(k));
        D=sqrt(  (xg-xl).^2 + (yg-yl).^2  ).^4;

        [d,n]=min(D);
        fmi(k)=fm(n);
        fmifm(k)=fmfm(n);
        
      endfor
        fmi=fmi(:);
      f=f*sf;
      close all;
      colorscatterp(x(j),y(j),log10(.1+f(j)),6,1000);shading interp;
      hold on;for k=1:N,plot(G(k).X,G(k).Y,'k');end;colormap('jet');caxis(cax);text(xt,yt,'HabCam')

      colorscatterp(x(j)+xs,y(j),log10(fmi),6,1000);shading interp;
      hold on;for k=1:N,plot(G(k).X+xs,G(k).Y,'k');end; colormap('jet');caxis(cax);text(xt+xs,yt,'Model')
whos
      colorscatterp(x(j)+xs+xs,y(j),log10(fmifm),6,1000);shading interp;
      hold on;for k=1:N,plot(G(k).X+xs+xs,G(k).Y,'k');end; colormap('jet');caxis(cax);text(xt+xs+xs,yt,'Model-fishing feedback')

      colorscatterp(x(j)+xs+xs+xs,y(j),log10(fmifm(:))-log10(fmi(:)),6,1000);shading interp;
      hold on;for k=1:N,plot(G(k).X+xs+xs+xs,G(k).Y,'k');end; colormap('jet');caxis(cax);text(xt+xs+xs,yt,'Model-fishing feedback')
      
      d=daspect();daspect([max(d),max(d),1]);caxis(cax)
      title(int2str(yr));
      set(gca,'visible','off')
      eval(['print -djpeg HabCamComp',int2str(yr),'.jpg']) ;
       
      pause(5);
     
      system(['convert -trim HabCamComp',int2str(yr),'.jpg HabCamComp',int2str(yr),'c.jpg'])
endfor
clf
caxis([1,4.5]);
h=colorbar("south");colormap('jet')
set(gca,'visible','off');
set(h,'xtick',1:4);
set(h,'xticklabel',[floor(10.^(1:4))]);
print -djpeg colorbarLog.jpg
system('convert -trim colorbarLog.jpg colorbarLog.jpg ')
clf
caxis([1,4.5]);
h=colorbar("east");colormap('jet')
set(gca,'visible','off');
set(h,'ytick',1:4);
set(h,'yticklabel',[floor(10.^(1:4))]);
print -djpeg colorbarLogV.jpg
system('convert -trim colorbarLogV.jpg colorbarLogV.jpg ')
