function NearestNeighborRecInterp(yrStart, yrEnd, domain)
isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
  % used if called by command line, or gui
  arg_list=argv();
  if ~strcmp(arg_list(1), '--gui');
    yrStart = str2num(cell2mat(arg_list(1)));
    yrEnd = str2num(cell2mat(arg_list(2)));
    domain = cell2mat(arg_list(3));
  else
    yrStart = str2num(yrStart);
    yrEnd = str2num(yrEnd);
  end
end

if ~strcmp(domain, 'GB') & ~strcmp(domain, 'MA')
    fprintf( 'Invalid Domain %s\n',  domain);
    return;
end
  
fl0=strcat('Data/Recruits',int2str(yrStart),domain,'.csv');
fl2=strcat('KrigingEstimates/KrigingEstimate',domain,int2str(yrStart),'.txt');
if isOctave
    D=csvreadK(fl0);
    dyr0=D(:,1);
    x0=D(:,2);
    y0=D(:,3);
    z0=D(:,4);
    recs0=D(:,5);
    fprintf('Writing to %s\n', fl2)
    fid=fopen(fl2,'w');
    dlmwrite(fid, recs0);
    fclose(fid);
else
    warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
    D=readtable(fl0,"FileType","spreadsheet");
    dyr0=table2array(D(:,1));
    x0=table2array(D(:,2));
    y0=table2array(D(:,3));
    z0=table2array(D(:,4));
    recs0=table2array(D(:,5));
    fprintf('Writing to %s\n', fl2)
    writematrix(recs0,fl2)
end

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
    recs05=NaN(size(x0));
    for k=1:ngp
        dist=abs( x0(k)+1i*y0(k) - x - 1i*y);
        [m,j]=min(dist);
        recs05(k)=recs(j);
    end
    fl2=strcat('KrigingEstimates/KrigingEstimate',domain,int2str(yr),'.txt');
    fprintf('Writing to %s\n', fl2)
    if isOctave
        fid=fopen(fl2,'w');
        dlmwrite(fid, recs0);
        fclose(fid);
    else
        writematrix(recs05,fl2)
    end
end
