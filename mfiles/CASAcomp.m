 C=csvread('Data/CASA_MA_Est.csv');
 M=load('CASACompTable.txt');
 j=find(C(:,1)>M(1,1)-3);C=C(j,:);
 figure;
 subplot(3,1,1);plot(C(:,1),C(:,2),'k.-',M(:,1),M(:,2)/10^6,'r.-');ylabel('abundance');legend('CASA','GeoSAMS',"location", 'West')
 subplot(3,1,2);plot(C(:,1),C(:,6),'k.-',M(:,1),M(:,3),'r.-');ylabel('Bms')
 subplot(3,1,3);plot(C(:,1),C(:,8),'k.-',M(:,1),M(:,4),'r.-');ylabel('ExplBms')
 
 print -djpeg CASAcomp.jpg
 
 C=csvread('Data/CASA_GB_Close_Est.csv');
 M=load('CASACompClosedTable.txt');
 j=find(C(:,1)>M(1,1)-3);C=C(j,:);
 figure;
 subplot(3,1,1);plot(C(:,1),C(:,2),'k.-',M(:,1),M(:,2)/10^6,'r.-');ylabel('abundance');legend('CASA','GeoSAMS',"location", 'West')
 subplot(3,1,2);plot(C(:,1),C(:,6),'k.-',M(:,1),M(:,3),'r.-');ylabel('Bms')
 subplot(3,1,3);plot(C(:,1),C(:,8),'k.-',M(:,1),M(:,4),'r.-');ylabel('ExplBms')
 
 print -djpeg CASAcompClosed.jpg
 
 
 C=csvread('Data/CASA_MA_Open_Est.csv');
 M=load('CASACompOpenTable.txt');
 j=find(C(:,1)>M(1,1)-3);C=C(j,:);
 figure;
 subplot(3,1,1);plot(C(:,1),C(:,2),'k.-',M(:,1),M(:,2)/10^6,'r.-');ylabel('abundance');legend('CASA','GeoSAMS',"location", 'West')
 subplot(3,1,2);plot(C(:,1),C(:,6),'k.-',M(:,1),M(:,3),'r.-');ylabel('Bms')
 subplot(3,1,3);plot(C(:,1),C(:,8),'k.-',M(:,1),M(:,4),'r.-');ylabel('ExplBms')
 
 print -djpeg CASAcompClosed.jpg
 
 