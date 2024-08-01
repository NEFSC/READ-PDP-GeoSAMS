function AddXY()

M=dlmread('OriginalData/dredgetowbysize7917.csv');
lat=M(:,18);
lon=-M(:,19);
% preallocate
xx=lon;
yy=lat;
n = size(lon,1);
for i = 1:n
    if lon(i)>-70.5
        zone=19;
    else
        zone=18;
    end
    if mod(i,10000) == 0; fprintf('i : %d of %d\n', i, n); end
    [xx(i),yy(i)]=ll2utm(lat(i),lon(i),zone);
end

M = [M, xx, yy];
dlmwrite('OriginalData/dredgetowbysizeXY.csv', M);
end

