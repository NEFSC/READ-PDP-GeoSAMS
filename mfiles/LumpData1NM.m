
function Fout = LumpData1NM (Fin);
  dx=1852;
  z=Fin(:,2)+i*Fin(:,3);
  zf=floor(z/dx);
  zu=unique(zf);
  for n=1:length(zu)
    j=find(zf==zu(n));
    if length(j)>1,
      Fout(n,:)=mean(Fin(j,:));
     else
      Fout(n,:)=Fin(j,:);
    end
  end
end
