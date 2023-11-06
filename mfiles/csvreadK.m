function F = csvreadK (flnm)
%function F = csvreadK (flnm);
%read csv file with header. header is tossed. 
%tcal%F=csvread(flnm,1,0);
F = readtable(flnm,"FileType","text")
[n,m]=size(F);
display(['number of original rows=',int2str(n)])
%BR=sum(  sum( F'==0 ) );
%%%%while( sum( F(1,:)==0 )==m )
%%%%  F=F(2:end,:);
%%%%end
%j=find(BR ==m );
%F=F(j,:);
%%%%[n,m]=size(F);
%%%%display(['number of final rows=',int2str(n)])
F=F(2:end,:)
end
