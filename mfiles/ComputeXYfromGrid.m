% Assumes that data is arranged as
% X, Y, Z, LAT, LON
% Recomputes UTM X,Y from LAT,LON
% UTM Zone is determined by Lon
% CSV Extension is added to fname in script
function ComputeXYfromGrid(fname)
D=readtable([fname '.csv'],"FileType","spreadsheet",'NumHeaderLines', 0);
lon = table2array(D(:,5));
lat = table2array(D(:,4));

x = table2array(D(:,1));
y = table2array(D(:,2));
z = table2array(D(:,3));
sams = table2array(D(:,6));
stratum = table2array(D(:,7));
region = table2array(D(:,8));


for i = 1:size(lon,1)
    if lon(i)>-70.5
        zone=19;
    else
        zone=18;
    end
    [x(i),y(i)]=ll2utm(lat(i),lon(i),zone);
end
M = [x, y, z, lat, lon, sams, stratum, region];

fNameWrite = [fname 'Mod' '.csv'];

writecsv(M, fNameWrite, '%f, %f, %f, %f, %f, %f, %f, %f');

end % function

