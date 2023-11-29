
function SetUpICS(yr,Dom)

% Check if directories have been created
if ~exist('InitialCondition', 'dir')
  mkdir ('InitialCondition')
end
if ~exist(['InitialCondition' filesep 'Sim',Dom,int2str(yr)], 'dir')
  mkdir(['InitialCondition' filesep 'Sim',Dom,int2str(yr)])
end


%F=csvreadK(['Data/bin5mm',int2str(yr),Dom,'.csv']);
F = readtable(['Data/bin5mm',int2str(yr),Dom,'.csv'],"FileType","text");
lat=table2array(F(:,4));lon=table2array(F(:,5));
if strcmp(Dom,'MA')
  j=find(lon<-70.5);
else
  j=find(lon>-70.5);
end
F1=table2array(F(j,:));

F=F1;

[~,j]=sort(F(:,4));
F=F(j,:);

M=load(['Grids/',Dom,'squares.csv']);
E=M(:,1:4);

M=csvread(['Grids/',Dom,'xyzLatLon.csv']);
xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);
nn=length(xg);
l=3:.5:15;
G=zeros(nn,25);

% F0 = LumpDataDx (F,1000.);


lat=F(:,4);lon=F(:,5);z=F(:,5);
x=F(:,2);y=F(:,3);
DecYr=F(:,1);

[zp,~] = SAMSgrid_interp (xg,yg,zg,E,x,y);
j=find(isnan(zp));zp(j)=z(j);
j=find(zp>250);
zp(j)=250;
l=3:.5:15; 
%F=F(:,5:end);
F=F(:,7:end);
close all 
if strcmp(Dom,'MA')
    Gs = shaperead('ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');
    N=length(Gs);
    for k=1:N
        [Gs(k).LAT,Gs(k).LON]=utm2ll(Gs(k).X,Gs(k).Y,18);
    end
 else
    Gs = shaperead('ShapeFiles/GB_Estimation_Areas_2020_UTM19_PDT_NLSModified_022020.shp');
    N=length(Gs);
    for k=1:N
        [Gs(k).LAT,Gs(k).LON]=utm2ll(Gs(k).X,Gs(k).Y,19);
    end
 end

 
for k=1:length(l)

  if k>2 
    R=(F(:,k-2)+F(:,k-1)+F(:,k)+F(:,k+1)+F(:,k+2))/5;
  elseif(k>1)
    R=(F(:,k-1)+F(:,k)+F(:,k+1)+F(:,k+2))/4;
  elseif(k==1)
    R=(F(:,k)+F(:,k+1)+F(:,k+2))/3;
  end
  % j=find(R>0); xo=x(j);yo=y(j);zo=zp(j);fo=R(j);DecYro=DecYr(j);%exlude zeros
  xo=x(:);yo=y(:);zo=zp(:);fo=R(:);DecYro=DecYr(:);
  % [xo,yo,zo,fo] = clean_scallop_data (xo,yo,zo,fo);
  M=[DecYro,xo,yo,zo,fo];
  [~,~]=size(M);
  M = LumpDataDx (M,1000.);
  [~,~]=size(M);
  if (length(fo)>20)
     flnm='tmp.csv';
     %header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per sq m"';
     writecsv(M,flnm,'%g, %f, %f, %f, %e');
     %csvwrite(flnm,M)

     system(['./UKsrc/UK ',Dom,' tmp.csv']);
     f=load('KrigingEstimate.txt');
     sum(f)
  else
    
     f=zeros(nn,1);
     if (length(j)>1)
        f=zeros(nn,1)+mean(fo);
     end
  end
  G(:,k)=f(:); 
  cax=[-5,0];
  %close all;patch(long(E'),latg(E'),log10(1+f(E')));shading interp;colormap('jet');caxis(cax);hold on;
  close all;patch(long(E'),latg(E'),log10(1+f(E')));shading interp;colormap('jet');hold on;
  for kk=1:N,plot(Gs(kk).LON,Gs(kk).LAT,'k');end
  daspect([1,cos(mean(latg)*pi/180)]);
  colorbar
  title(num2str(l(k)))
  set(gca,'visible','off');
  pause(1)
end

flnm=['InitialCondition/Sim',Dom,int2str(yr),'/InitialCondition.csv'];
%header= ['"30 mm","35 mm","40 mm","45 mm","50 mm","55 mm","60 mm","65 mm","70 mm","75 mm","80mm","85 mm","90 mm","95 mm","100 mm","105 mm","110 mm","115 mm","120 mm","125 mm","130 mm","135mm","140mm","145mm","150mm"'];
writecsv(G,flnm,...
'%e, %e, %e, %e, %e,%e, %e, %e, %e, %e,%e, %e, %e, %e, %e,%e, %e, %e, %e, %e,%e, %e, %e, %e, %e');

%csvwrite (flnm, G);
    