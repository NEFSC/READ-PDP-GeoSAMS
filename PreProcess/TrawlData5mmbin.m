%-------------------------------
% GB
%-------------------------------
clear

M = readtable('OriginalData/dredgetowbysize7917.csv',"FileType","text");

mon=table2array(M(:,5));
j=mon>0;
M=M(j,:);
%------- new M table ----------------------
 
lon = -table2array(M(:,19));
% UTM Zone. Zone 18 if lon <= -72.0
%           Zone 19 if lon >  -72.0
% Zone is approx CEILING((180 + lon)/6,1), therefore not truly needed
utmZone = 19;
j = lon>-70.5;%GB

M = M(j,:);
%------- new M table ----------------------

lat=table2array(M(:,18));
lon=-table2array(M(:,19));
[xx,yy]=ll2utm(lat,lon,utmZone);

year = table2array(M(:,4));
sc = table2array(M(:,27));

nautMile_ft = 6076.12;
towArea_sqft = nautMile_ft * 8;
SqFtToSqM = .3048^2;
Detect=.4;

towArea_sqm = towArea_sqft * SqFtToSqM;
countPerSqm = towArea_sqm^(-1) * Detect^(-1);

for yr=1979:2017
    fprintf( 'Working on GB Year %d\n', yr);
    X=[];
    j=and(year==yr,sc==3.);
    lat_t = M(j,18);
    lon_t = array2table(-table2array(M(j,19)),'VariableNames',{'lon'});
    mon = table2array(M(j,5));
    day = table2array(M(j,6));
    yd = 0 * day;
    for k=1:length(day)
        yd(k) = yearday(mon(k),day(k),0);
    end
    DecYr_t = array2table(year(j) + yd/365.25,'VariableNames',{'DecYr'});
    z_t = M(j,21);
    x_t = array2table(xx(j),'VariableNames',{'x'});
    y_t = array2table(yy(j),'VariableNames',{'y'});
    %
    % At this point x_t, y_t would be the same as
    % lat=table2array(M(j,18));
    % lon=-table2array(M(j,19));
    % [xx,yy]=ll2utm(lat,lon,19);
    %
    n=find(and(year==yr,sc==3.));
    for k=1:numel(lat_t)
        % bring in the surv_n data from size group 3 to 18, 
        % which is in the 30 rows following sc==3
        density=rows2vars(array2table(table2array(M(n(k):n(k)+30,30)) * countPerSqm));
        X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), density(1,2:end)];
    end
    flnm=['Data/bin5mm',int2str(yr),'GB.csv'];
    writetable(X,flnm,'WriteVariableNames',0);
end

%-------------------------------
% MA
%-------------------------------
clear

M = readtable('OriginalData/dredgetowbysize7917.csv',"FileType","text");

mon=table2array(M(:,5));
j=mon>0;
M=M(j,:);
%------- new M table ----------------------

lon = -table2array(M(:,19));
% UTM Zone. Zone 18 if lon <= -72.0
%           Zone 19 if lon >  -72.0
% Zone is approx CEILING((180 + lon)/6,1), therefore not truly needed
j = lon<-70.5;%MA
utmZone = 18;
M = M(j,:);
%------- new M table ----------------------

year = table2array(M(:,4));
mon  = table2array(M(:,5));
day  = table2array(M(:,6));
 
yd = 0 * day;
for k=1:length(day)
    yd(k) = yearday(mon(k), day(k), 0);
end

DecYr_t = array2table(year(:) + (yd(:)/365.25 ),'VariableNames',{'DecYr'});

lat=table2array(M(:,18));
lon=-table2array(M(:,19));
[xx,yy]=ll2utm(lat,lon,utmZone);

sc = table2array(M(:,27));

nautMile_ft = 6076.12;
towArea_sqft = nautMile_ft * 8;
SqFtToSqM = .3048^2;
Detect=.4;

towArea_sqm = towArea_sqft * SqFtToSqM;
countPerSqm = towArea_sqm^(-1) * Detect^(-1);

for yr=1979:2017
    fprintf( 'Working on MA Year %d\n', yr);
    X=[];
    j=and(year==yr,sc==3.);
    lat_t = M(j,18);
    lon_t = array2table(-table2array(M(j,19)),'VariableNames',{'lon'});
    mon = table2array(M(j,5));
    day = table2array(M(j,6));
    yd = 0 * day;
    for k=1:length(day)
        yd(k) = yearday(mon(k),day(k),0);
    end
    DecYr_t = array2table(year(j) + yd/365.25,'VariableNames',{'DecYr'});
    z_t = M(j,21);
    x_t = array2table(xx(j),'VariableNames',{'x'});
    y_t = array2table(yy(j),'VariableNames',{'y'});
    n=find(and(year==yr,sc==3.));
    for k=1:numel(lat_t)
        % bring in the surv_n data from size group 3 to 18, 
        % which is in the 30 rows following sc==3
        density=rows2vars(array2table(table2array(M(n(k):n(k)+30,30)) * countPerSqm));
        X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), density(1,2:end)];
    end
    flnm=['Data/bin5mm',int2str(yr),'MA.csv'];
    writetable(X,flnm,'WriteVariableNames',0);
end
