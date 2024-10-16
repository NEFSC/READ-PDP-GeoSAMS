function NearestNeighborRecInterp(yrStart, yrEnd, domain, refYear)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
  % used if called by command line, or gui
  arg_list=argv();
  if ~strcmp(arg_list(1), '--gui');
    yrStart = str2num(cell2mat(arg_list(1)));
    yrEnd = str2num(cell2mat(arg_list(2)));
    domain = cell2mat(arg_list(3));
    refYear = str2num(cell2mat(arg_list(4)));
  else
    yrStart = str2num(yrStart);
    yrEnd = str2num(yrEnd);
    refYear = str2num(refYear);
  end
end

if ~strcmp(domain, 'GB') && ~strcmp(domain, 'MA') && ~strcmp(domain, 'AL')
    fprintf( 'Invalid Domain %s\n',  domain);
    fprintf( "Use: 'MA' or 'GB' or 'AL'\n" )
    return;
end

if refYear == 0
    fl0=strcat('Data/Recruits',int2str(yrStart),domain,'.csv');
else
    fl0=strcat('Data/Recruits',int2str(refYear),domain,'.csv');
end
fl2=strcat('RecruitEstimates/RecruitEstimate',domain,int2str(yrStart),'.txt');
[status, msg, msgid] = fileattrib(fl2);
if msg.UserWrite == 0
    fprintf([char(27) '[33m' 'Skipping Nearest Neighbor - Files locked for testing\n' char(27) '[0m'])
    return
end

fl2CSV=strcat('RecruitEstimates/RecruitEstimate',domain,int2str(yrStart),'.csv');
latCol  = 2;
lonCol  = 3;
utmxCol = 4;
utmyCol = 5;
recCol  = 7;

fprintf('Reading from %s\n', fl0)

if isOctave
    D=csvreadK(fl0);
    lat0=D(:,latCol);
    lon0=D(:,lonCol);
    x0=D(:,utmxCol);
    y0=D(:,utmyCol);
    recs0=D(:,recCol);
    r = RandomizeOutput(recs0, yrStart);
    dlmwrite(fl2, r);
    dlmwrite(fl2CSV,[lat0, lon0, x0, y0, r]);
else
    D=readtable(fl0,"FileType","spreadsheet");
    lat0=table2array(D(:,latCol));
    lon0=table2array(D(:,lonCol));
    x0=table2array(D(:,utmxCol));
    y0=table2array(D(:,utmyCol));
    recs0=table2array(D(:,recCol));
    r = RandomizeOutput(recs0, yrStart);
    writematrix(r, fl2)
    M = array2table([lat0, lon0, x0, y0, r],'VariableNames',{'LAT','LON','X','Y','RECR'});
    writetable(M,fl2CSV,'WriteVariableNames',0);
end
fprintf('Writing to %s. Number of records %d\n', fl2, size(r,1))

ngp=length(x0);

for yr=yrStart+1:yrEnd
    fl=strcat('Data/Recruits',int2str(yr),domain,'.csv');
    if isOctave
        D=csvreadK(fl);
        x=D(:,utmxCol);
        y=D(:,utmyCol);
        recs=D(:,recCol);
    else
        D=readtable(fl,"FileType","spreadsheet");
        x=table2array(D(:,utmxCol));
        y=table2array(D(:,utmyCol));
        recs=table2array(D(:,recCol));
    end
    recs05=zeros(size(x0));
    for k=1:ngp
        dist=abs( x0(k)+1i*y0(k) - x - 1i*y);
        [m,j]=min(dist);
        recs05(k)=recs(j);
    end
    fl2=strcat('RecruitEstimates/RecruitEstimate',domain,int2str(yr),'.txt');
    fl2CSV=strcat('RecruitEstimates/RecruitEstimate',domain,int2str(yr),'.csv');
    r = RandomizeOutput(recs05, yr);
    fprintf('Writing to %s. Number of records %d\n', fl2, size(r,1))
    if isOctave
        dlmwrite(fl2, r);
        dlmwrite(fl2CSV,[lat0, lon0, x0, y0, r]);
    else
        writematrix(r, fl2)
        M = array2table([lat0, lon0, x0, y0, r],'VariableNames',{'LAT','LON','X','Y','RECR'});
        writetable(M,fl2CSV,'WriteVariableNames',0);
    end
end % yr=yrStart+1:yrEnd

end % function

function x = RandomizeOutput(M, yr)
    mu = 0.0;
    sigma = 0.5;
    r = random('norm',mu,sigma,size(M));
    x = exp(log(M) + r) * exp(-sigma^2/2.0);

    fprintf( 'Before: Stats for Year: %d   Mean: %f   STD: %f  Min: %f Max: %f\n', yr, mean(M), std(M), min(M), max(M))
    fprintf( 'Rand:   Stats for Year: %d   Mean: %f   STD: %f  Min: %f Max: %f\n', yr, mean(r), std(r), min(r), max(r))
    fprintf( 'After:  Stats for Year: %d   Mean: %f   STD: %f  Min: %f Max: %f\n', yr, mean(x), std(x), min(x), max(x))
end
