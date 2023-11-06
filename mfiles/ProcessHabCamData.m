

F=csvread('/home/keston/Scallop/HabcamDataAggregated_All_2011-2021.csv');
lon=F(:,5);
j=find(lon<-70.5);F=F(j,:);
year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);


  M=load('SAMS_MA_squares.txt');E=M(:,1:4);
  M=csvread('SAMS_MA_nodes_utm_z_latlon.csv');
xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);

%units in feet for tow data 8' x 1 nm
sf=5280*8/(3.28^2);
ne=100;
for yr=2012:2021,
 j=find(year==yr);
  
  %S=csvread(['State',int2str(yr),'.csv']);
  S=csvread(['State',int2str(yr),'EnMean.csv']);
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
  endfor
  fmi=fmi(:);
  sfe=fmi\f(j);
  fmi=fmi/sf;
  subplot(3,4,yr-2010);
  plot(j,f(j),'k.-',j,fmi,'r.-');
  title(int2str(yr));
 %stx(yr-2010,:)=[sfe,  corr(f(j),fmi),corr(log(.1+f(j)),log(.1+fmi)),sqrt(mean( (f(j)-fmi ).^2) )./sqrt(mean( (f(j)).^2) ) ]
 disp([int2str(yr),' & ',num2str( corr(f(j),fmi)),' & ', num2str(corr(log(.1+f(j)),log(.1+fmi))),' \\ ' ])

endfor%units in feet for tow data 8' x 1 nm


sf=5280*8/(3.28^2);
ne=100;
F=csvread('/home/keston/Scallop/HabcamDataAggregated_All_2011-2021.csv');
lon=F(:,5);
j=find(lon<-70.5);F=F(j,:);
year=F(:,1);SFov=F(:,3);lon=F(:,5);lat=F(:,6);z=F(:,7);x=F(:,10);y=F(:,11);f=F(:,8);


  M=load('SAMS_MA_squares.txt');E=M(:,1:4);
  M=csvread('SAMS_MA_nodes_utm_z_latlon.csv');
xg=M(:,1);yg=M(:,2);long=M(:,5);latg=M(:,4);zg=M(:,3);

for yr=2012:2021,
 j=find(year==yr);
  
  %S=csvread(['State',int2str(yr),'.csv']);
  S=csvread(['State',int2str(yr),'EnMean.csv']);
  %S( find(isnan(S)) )=0;
  fm=sum(S')';
  clear fmi w;
  
  for k=1:length(j)
    xl=x(j(k));yl=y(j(k));
    D=sqrt(  (xg-xl).^2 + (yg-yl).^2  ).^4;
    w=1./D;
    [k,sum(w)];
    w=w/sum(w);
    fmi(k)=sum(w.*fm);
    [mmm,kk]=min(D);
    fmi(k)=fm(kk);
  endfor
  fmi=fmi(:);
  sfe=fmi\f(j);
  fmi=fmi/sf;
  subplot(3,4,yr-2010);
  plot(j,f(j),'k.-',j,fmi,'r.-');
  title(int2str(yr));
 %stx(yr-2010,:)=[sfe,  corr(f(j),fmi),corr(log(.1+f(j)),log(.1+fmi)),sqrt(mean( (f(j)-fmi ).^2) )./sqrt(mean( (f(j)).^2) ) ]
 %disp([int2str(yr),' & ',num2str(mean(fmi)),' & ', num2str(mean(f(j))) ])
disp([int2str(yr),' & ',num2str( corr(f(j),fmi)),' & ', num2str(corr(log(.1+f(j)),log(.1+fmi))),' \\ ' ])

endfor
