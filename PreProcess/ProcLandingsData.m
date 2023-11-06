%Hi Keston. Attached are landings (metric tons), value (thousand $) and price ($/lb) from 1998 to 2021. 
%The market categories are given in 
%NESPP4: 8002=U10, 8003 = 10-20, 8004 = 20-30, 8005 = 30-40, 8006=40-50, 8007=50-60,8008=60+, 8009 = unclassified


M=csvreadK('OrigonalData/landsum9821.csv');
yrs=M(:,2);
cl=M(:,3);
ucl=unique(cl)
yru=unique(yrs);

%"","year","NESPP4","land","value","price"

%8001   8002   8003   8004   8005   8006   8007   8008   8009
landings=[];
value=[];
price=[];


M=csvreadK('OrigonalData/landsum9821.csv');

m=0;
for yr=1998:2021
  m=m+1
  j=find(M(:,2)==yr);

  k=find(M(:,3)<8003);
  n=intersect(j,k);
  if length(n)>0,
    U10(m)=mean(M(n,6));
   else
    U10(m)=NaN
   endif
   
   k=find(M(:,3)==8003);
  n=intersect(j,k);
  if length(n)==1,
    U10to20(m)=M(n,6);
   else
    U10to20(m)=NaN;
   endif
     k=find(M(:,3)==8004);
  n=intersect(j,k);
  if length(n)==1,
    U20to30(m)=M(n,6);
   else
    U20to30(m)=NaN;
   endif
     k=find(M(:,3)>8004);
  n=intersect(j,k);
  if length(n)>0,
    U30plus(m)=mean(M(n,6));
   else
    U30plus(m)=NaN;
   endif
   year(m)=yr;
 end
 
 close all;
 plot(year,U10,'k',year,U10to20,'b',year,U20to30,'c',year,U30plus,'r')
 
 X=[year(:),U10(:),U10to20(:),U20to30(:),U30plus(:)];
 flnm='Data/ScallopPrice.csv'
header='"decmal year", "U10", "10-20", "20-30","30+"';
writecsv(X,flnm,['%g, %g, %g, %g, %g'],header);
