% Assumes that data is arranged as 
% grid location, <--- grid parameter --->
% lat, lon, p1, p2, .... , 

%A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
%0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2
%1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6

function PlotLatLonHabCam
cArray=['y', 'b', 'r', 'g', 'm', 'k', 'c'];
fname1 = 'OriginalData/Habcam_Grid_Estimates_2015_Rec30_MAB.csv';
fname2 = 'OriginalData/Dredge_Grid_Estimates_2015_Rec30_MAB.csv';

D=readtable(fname1,"FileType","spreadsheet");
[r, c]=size(D);

recr = NaN(r,2);
lat = NaN(r,2);
lon = NaN(r,2);

lat(:,1)=table2array(D(:,16));
lon(:,1)=table2array(D(:,15));
recr(:,1)=table2array(D(:,14));

D=readtable(fname2,"FileType","spreadsheet");
[r, c]=size(D);

lat(:,2)=table2array(D(:,16));
lon(:,2)=table2array(D(:,15));
recr(:,2)=table2array(D(:,14));

for k=1:2
for n=1:r
    % geoscatter does not accept 0.0, must be positive or NaN
    if recr(n,k)<=0; recr(n,k) = 1e-6; end
end
end
% scale data 0+ to 100
m=max(recr,[],"all") / 500.;
recr = recr ./ m;

h=figure('Name','Recruits HabCam vs Dredge');

geoscatter(lat(:,1), lon(:,1), recr(:,1), 'o', 'r');
hold on
geobasemap streets
geoscatter(lat(:,2), lon(:,2), recr(:,2), 'o', 'b');
legend('HabCam', 'Dredge')
