
F=csvreadK('Data/RecruitsMA.csv');

z=F(:,4);r=F(:,5);
[z,j]=sort(z);r=r(j);
plot(z,r,'k.');
n=0;
for z0=0:250,
  n=n+1;
  k=find(and(z<z0+5,z>z0-5));
  if ~isempty(k)
    r0(n)=max(r(k));
  else
    r0(n)=NaN;
  endif
endfor
 z0=0:250;
 close all
clf; plot(z,r,'k.',z0,r0,'r');

zc=57;
w=15;
a=.01;
b=15;
fr=a+b*exp(-((z-zc)/w).^2);
j=find(r>fr);
length(j)/length(z)
clf; plot(z,r,'k.',z,fr,'r',z0,r0,'b',z(j),r(j),'rx');title(num2str(length(j)/length(z)))

figure

F=csvreadK('Data/RecruitsGB.csv');

z=F(:,4);r=F(:,5);
[z,j]=sort(z);r=r(j);
plot(z,r,'k.');
n=0;
for z0=0:250,
  n=n+1;
  k=find(and(z<z0+5,z>z0-5));
  if ~isempty(k)
    r0(n)=max(r(k));
  else
    r0(n)=NaN;
  endif
endfor
 z0=0:250;

clf; plot(z,r,'k.',z0,r0,'r');

zc=55;
w=15;
a=.01;
b=25;
fr=a+b*exp(-((z-zc)/w).^2);
j=find(z>zc);
w2=35;
fr(j)=a+b*exp(-((z(j)-zc)/w2).^2);

j=find(r>fr);
length(j)/length(z)
clf; plot(z,r,'k.',z,fr,'r',z0,r0,'b',z(j),r(j),'rx');title(num2str(length(j)/length(z)))



F0=csvreadK('Data/RecruitsGB.csv');
F1=csvreadK('Data/RecruitsMA.csv');
F=[F0;F1];


z=F(:,4);r=F(:,5);
[z,j]=sort(z);r=r(j);
plot(z,r,'k.');
n=0;
for z0=0:250,
  n=n+1;
  k=find(and(z<z0+5,z>z0-5));
  if ~isempty(k)
    r0(n)=max(r(k));
  else
    r0(n)=NaN;
  endif
endfor
 z0=0:250;
figure;
clf; plot(z,r,'k.',z0,r0,'r');

zc=55;
w=15;
a=.01;
b=25;
fr=a+b*exp(-((z-zc)/w).^2);
j=find(z>zc);
w2=30;
fr(j)=a+b*exp(-((z(j)-zc)/w2).^2);

j=find(r>fr);
length(j)/length(z)
clf; plot(z,r,'k.',z,fr,'r',z0,r0,'b',z(j),r(j),'rx');title(num2str(length(j)/length(z)))


