F=csvreadK('Data/Recruits2015MA.csv');
G=shaperead('ShapeFiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp');
x=F(:,2);y=F(:,3);
N=length(G);
for k=1:N
  X=G(k).X;  
  Y=G(k).Y;
  indx=inside(x,y,X,Y);
  j=find(indx==1);
  recmean(k)=mean(F(j,end))
end
mark=['r.';'c.';'b.';'y.';'g.';'m.';'ro';'co';'bo';'yo';'go';'mo'];
figure;
for k=1:N
  X=G(k).X;  
  Y=G(k).Y;
  indx=inside(x,y,X,Y);
  j=find(indx==1);
  recmean(k)=mean(F(j,end));
  
  plot(X,Y,'k',x(j),y(j),mark(k,:));hold on;
  text(mean(x(j)),mean(y(j)),num2str(recmean(k)) )
end
  
figure;
for k=1:N
  X=G(k).X;  
  Y=G(k).Y;
  
  indx=inside(x,y,X,Y);
  j=find(indx==1);
  recmean(k)=mean(F(j,end));
  
  plot(X,Y,'k');hold on;
  text(mean(x(j)),mean(y(j)),int2str(k) )
end
  
labels=['NYB';'LI ';'NYB';'ETo';'HCS';'DMV';'NYi';'NYi';'ETc';'BI ';'NYB';'VIR'];
for k=1:N
[labels(k,:),' ', num2str(recmean(k)) ],
end

clear
 F=csvreadK('Data/Recruits2015MA.csv');
G=shaperead('ShapeFiles/MAB_Est_Areas_2015_UTM18_PDT_Habcam.shp');
x=F(:,2);y=F(:,3);
N=length(G);
for k=1:N
  X=G(k).X;  
  Y=G(k).Y;
  indx=inside(x,y,X,Y);
  j=find(indx==1);
  recmean(k)=mean(F(j,end))
end
mark=['r.';'c.';'b.';'y.';'g.';'m.';'ro';'co';'bo';'yo';'go';'mo';'rx'];
figure;
for k=1:N
  X=G(k).X;  
  Y=G(k).Y;
  indx=inside(x,y,X,Y);
  j=find(indx==1);
  recmean(k)=mean(F(j,end));
  
  plot(X,Y,'k',x(j),y(j),mark(k,:));hold on;
  text(mean(x(j)),mean(y(j)),num2str(recmean(k)) )
  num2str(recmean(k))
end
  
%[1,11,3]=NYB
%[4,9]=ET
%[7,8]=NYBi


figure;
for k=1:N
  X=G(k).X;  
  Y=G(k).Y;
  
  indx=inside(x,y,X,Y);
  j=find(indx==1);
  recmean(k)=mean(F(j,end));
  
  plot(X,Y,'k');hold on;
  text(mean(x(j)),mean(y(j)),int2str(k) )
end
  
labels=['NYB';'LI ';'NYB';'ETo';'HCS';'DMV';'NYi';'NYi';'ETc';'BI ';'NYB';'VIR'];
for k=1:N
[labels(k,:),' ', num2str(recmean(k)) ],
end

