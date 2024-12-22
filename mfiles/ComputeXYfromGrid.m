% Assumes that data is arranged as
% X, Y, Z, LAT, LON
% Recomputes UTM X,Y from LAT,LON
% UTM Zone is determined by Lon
% CSV Extension is added to fname in script
function ComputeXYfromGrid(fname)
D=readtable([fname '.csv'],"FileType","spreadsheet",'NumHeaderLines', 1);
lon = table2array(D(:,5));
lat = table2array(D(:,4));

z = table2array(D(:,3));
sams = table2array(D(:,6));
stratum = table2array(D(:,7));
x=zeros(size(lon,1),1);
y=zeros(size(lon,1),1);
for i = 1:size(lon,1)
    if lon(i)>-70.5
        zone=19;
    else
        zone=18;
    end
    [x(i),y(i)]=ll2utm(lat(i),lon(i),zone);
end

M = [x, y, z, lat, lon, sams, stratum];

fNameWrite = [fname 'Mod' '.csv'];

writecsv(M, fNameWrite, '%f, %f, %f, %f, %f, %f, %f');

end % function

