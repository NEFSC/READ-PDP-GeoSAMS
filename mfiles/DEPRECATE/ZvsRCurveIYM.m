
close all;
F=csvreadK('Data/RecruitsMA.csv');
yru=unique(floor(F(:,1)));
N=length(yru);
yr=floor(F(:,1));


for k=1:N,
  j=find(yr==yru(k));
  rsj(j)=F(j,5)/max(F(j,5) );
end
z=F(:,4);r=F(:,5);
r=rsj(:);
[z,j]=sort(z);r=r(j);
plot(z,r,'k.');

zc=60;
w=33;
a=.01;
b=1.;
n=6

fr=a+b*exp(- (( z-zc)/w).^n );
j=find(r>fr);
length(j)/length(z)
clf; plot(z,r,'k.',z,fr,'r',z(j),r(j),'rx');title(num2str(length(j)/length(z)))



close all;
F0=csvreadK('Data/RecruitsGB.csv');
F1=csvreadK('Data/RecruitsMA.csv');
%F=[F0;F1];
F=F0;
yru=unique(floor(F(:,1)));
N=length(yru);
yr=floor(F(:,1));
f

for k=1:N,
  j=find(yr==yru(k));
  rsj(j)=F(j,5)/max(F(j,5) );
end
z=F(:,4);r=F(:,5);
r=rsj(:);
[z,j]=sort(z);r=r(j);
plot(z,r,'k.');

zc=70;
w=50;
a=.01;
b=1.;
n=20

fr=a+b*exp(- (( z-zc)/w).^n );
j=find(r>fr);
length(j)/length(z)
clf; plot(z,r,'k.',z,fr,'r',z(j),r(j),'rx');title(num2str(length(j)/length(z)))


