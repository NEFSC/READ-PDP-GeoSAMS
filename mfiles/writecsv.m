function writecsv (M,flnm,fmt,header)
%function writecsv (M,flnm,header)

[n,m]=size(M);
fp=fopen(flnm,'w');
if(nargin>3)
  fprintf(fp,[header,'\n']);
end
if or( nargin<3,isempty(fmt))
  fmtstr=[];
  for k=1:m-1
    fmtstr;'%g, ';
    fmtstr=[fmtstr,'%f, '];
  end
  fmtstr=[fmtstr,'%f'];
else
  fmtstr=fmt;
end


if (nargin<3)
  fmtstr=[];
  for k=1:m-1,
    fmtstr,'%g, '
    fmtstr=[fmtstr,'%f, '];
  end
  fmtstr=[fmtstr,'%f'];
else
  fmtstr=fmt;
end
fmtstr=strcat(fmtstr,'\n');

for k=1:n
  fprintf(fp,fmtstr,M(k,:));
end
 fclose(fp);

end
