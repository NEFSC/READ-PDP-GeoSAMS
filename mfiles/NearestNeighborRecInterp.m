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

if ~strcmp(domain, 'GB') & ~strcmp(domain, 'MA') & ~strcmp(domain, 'AL')
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
if isOctave
    D=csvreadK(fl0);
    x0=D(:,2);
    y0=D(:,3);
    recs0=D(:,5);
    r = RandomizeOutput(recs0, yrStart);
    fid=fopen(fl2,'w');
    dlmwrite(fid, r);
    fclose(fid);
else
    D=readtable(fl0,"FileType","spreadsheet");
    x0=table2array(D(:,2));
    y0=table2array(D(:,3));
    recs0=table2array(D(:,5));
    r = RandomizeOutput(recs0, yrStart);
    writematrix(r, fl2)
end
fprintf('Writing to %s. Number of records %d\n', fl2, size(r,1))

ngp=length(x0);

for yr=yrStart+1:yrEnd
    fl=strcat('Data/Recruits',int2str(yr),domain,'.csv');
    if isOctave
        D=csvreadK(fl);
        x=D(:,2);
        y=D(:,3);
        z=D(:,4);
        recs=D(:,5);
    else
        D=readtable(fl,"FileType","spreadsheet");
        x=table2array(D(:,2));
        y=table2array(D(:,3));
        z=table2array(D(:,4));
        recs=table2array(D(:,5));
    end
    recs05=zeros(size(x0));
    for k=1:ngp
        dist=abs( x0(k)+1i*y0(k) - x - 1i*y);
        [m,j]=min(dist);
        recs05(k)=recs(j);
    end
    fl2=strcat('RecruitEstimates/RecruitEstimate',domain,int2str(yr),'.txt');
    r = RandomizeOutput(recs05, yr);
    fprintf('Writing to %s. Number of records %d\n', fl2, size(r,1))
    if isOctave
        fid=fopen(fl2,'w');
        dlmwrite(fid, r);
        fclose(fid);
    else
        writematrix(r, fl2)
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
