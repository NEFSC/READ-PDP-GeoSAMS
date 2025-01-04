function [f,H,h] = SAMSgrid_interp (xg,yg,fg,E,x,y)
%bilinear interpolation from fields f supported on (xg,yg) to (x,y)
%E is the quadrilateral element list (for xg,yg)
n=length(x);
[ng,m]=size(fg);
f=zeros(n,m);
h=zeros(n,1);
H=zeros(n,length(xg));
i=sqrt(-1);
for k=1:n
   [m,j]=min( abs( xg+i*yg - (x(k)+i*y(k)) ) );
   %if(m<1800*2)
   if(m<1800*2)
%    f(k,:)=fg(j,:);
    f(k)=fg(j);
    H(k,j)=1;
    %k
    h(k)=j;
  else
    f(k)=NaN;
    H(k,:)=NaN;
    end
end

if(0)
    [ng,m]=size(fg);
    xe=mean(xg(E'));
    ye=mean(yg(E'));
    n=length(x);
    f=zeros(n,m);
    for k=1:n
        [m,j]=min( abs(xe+i*ye - (x(k)+i*y(k)) ) );
        [x1,i1]=min(xg(E(j,:)) );[x2,i2]=max(xg(E(j,:)) );
        [y1,j1]=min(yg(E(j,:)) );[y2,j2]=max(yg(E(j,:)) );
        fl=fg(E(j,:),:);
        if inside(x(k),y(k),[x1,x2,x2,x1],[y1,y1,y2,y2])==1
            s=(x2-x1)*(y2-y1);
            w1=(x2-x(k))*(y2-y(k));  
            w2=(x(k)-x1)*(y2-y(k));  
            w4=(x2-x(k))*(y(k)-y1);  
            w3=(x(k)-x1)*(y(k)-y1); 
            f(k,:)=(w1*fl(1,:) + w2*fl(2,:)+ w3*fl(3,:)+ w4*fl(4,:))/s; 
            % f(k,:)=interp2(xg(E(j,:)),yg(E(j,:)),fg(E(j,:),:),x(k),y(k)); 
        else
            f(k,:)=NaN;
        end
    end
end
end
