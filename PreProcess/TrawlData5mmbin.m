function TrawlData5mmbin(yrStart, yrEnd, srcText)

domain = ['GB', 'MA'];
isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
% UTM Zone. Zone 18 if lon <= -72.0
%           Zone 19 if lon >  -72.0
% Zone is approx CEILING((180 + lon)/6,1), therefore not truly needed
utmZone = [19,18];

for d = 1:numel(size(domain))

  if isOctave

    % used if called by command line
    if (exist("yrStart", "var") == 0)
       arg_list = argv();
       yrStart=str2num(arg_list{1});
       yrEnd=str2num(arg_list{2});
    end
    
    M=csvreadK('OriginalData/dredgetowbysize7917.csv');

    mon=M(:,5);
    j=find(mon>0);
    M=M(j,:);
    %------- new M table ----------------------

    lon=-M(:,19);
    if d == 1
      j=find(lon>-70.5);%GB
    else
      j=find(lon<=-70.5);%MA
    end

    M = M(j,:);
    %------- new M table ----------------------

    lat=M(:,18);
    lon=-M(:,19);
    [xx,yy]=ll2utm(lat,lon,utmZone(d));

    year = M(:,4);
    sg = M(:,27);
    src = M(:,37);
  else
    M = readtable('OriginalData/dredgetowbysize7917.csv',"FileType","text");

    mon=table2array(M(:,5));
    j=mon>0;
    M=M(j,:);
    %------- new M table ----------------------

    lon = -table2array(M(:,19));
    if d == 1
      j = lon>-70.5;%GB
    else
      j = lon<-70.5;%MA
    end

    M = M(j,:);
    %------- new M table ----------------------

    lat=table2array(M(:,18));
    lon=-table2array(M(:,19));
    [xx,yy]=ll2utm(lat,lon,utmZone(d));

    year = table2array(M(:,4));
    sg = table2array(M(:,27));
    src = table2array(M(:,37));
  end
  Detect=.4;

  %  standard tow length of 1 nautical mile by 8 ft, or 2.4384 m, wide dredge
  nautMile_m = 1852.;
  towArea_sqm = 4516.; % nautMile_m * 2.438;
  countPerSqm = 1. / (towArea_sqm * Detect);

  %for yr=1979:2017
  for yr=yrStart:yrEnd
    fprintf( 'Working on %s Year %d\n',  domain(d*2-1:d*2), yr);
    X=[];
    %j=and(year==yr,sg==3.);
    j=strcmp(src, {srcText}) & year==2005 & sg==3.;
    if isOctave
      stratum_t = M(j,8);
      is_closed_t = double(M(j,17)>0);
      lat_t = M(j,18);
      lon_t = -(M(j,19));
      mon = M(j,5);
      day = M(j,6);
      yd = 0 * day;
      for k=1:length(day)
        yd(k) = yearday(mon(k),day(k),0);
      end
      DecYr_t = year(j) + yd/365.25;
      z_t = cast(M(j,21),"double");
      x_t = xx(j);
      y_t = yy(j);
    else
      stratum_t = M(j,8);
      is_closed_t = array2table(int8(table2array(M(j,17)>0)),'VariableNames',{'isClosed'});
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
    end
    %
    % At this point x_t, y_t would be the same as
    % lat=table2array(M(j,18));
    % lon=-table2array(M(j,19));
    % [xx,yy]=ll2utm(lat,lon,utmZone(d));
    %
    n=find(and(year==yr,sg==3.));
    for k=1:numel(lat_t)
      % bring in the surv_n data from size group 3 to 18, centimeters
      % which is in the 30 rows following sc==3
      % gather size data 3 - 14.5
      %               n(k) - n(k+23)
      % accumulate 15 to 18
      % sum n(k+24) - n(k+30) into k=25
      if isOctave
        k25   = sum((M(n(k)+24:n(k)+30,30)));
        density = [(M(n(k):n(k)+23,30));k25];
        density= density * countPerSqm;
        X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), is_closed_t(k,:), stratum_t(k,:), transpose(density)];
      else
        k25   = sum(table2array(M(n(k)+24:n(k)+30,30)));
        density = [table2array(M(n(k):n(k)+23,30));k25];
        density= rows2vars(array2table( density * countPerSqm));
        X=[X;DecYr_t(k,:), x_t(k,:), y_t(k,:), lat_t(k,:), lon_t(k,:), z_t(k,:), is_closed_t(k,:), stratum_t(k,:), density(1,2:end)];
      end
    end
    flnm=strcat('Data/bin5mm',int2str(yr),domain(d*2-1:d*2),'.csv');
    if isOctave
      csvwrite(flnm,X);
    else
      writetable(X,flnm,'WriteVariableNames',0);
    end
  end
end
