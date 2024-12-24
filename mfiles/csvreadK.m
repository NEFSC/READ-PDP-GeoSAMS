function F = csvreadK (flnm);
%function F = csvreadK (flnm);
%read csv file with header. header is tossed.
F=csvread(flnm);
[n,m]=size(F);
display(['number of original rows=',int2str(n)])
%BR=sum(  sum( F'==0 ) );
F=F(2:end,:);
%j=find(BR ==m );
%F=F(j,:);
[n,m]=size(F);
display(['number of final rows=',int2str(n)])
end
