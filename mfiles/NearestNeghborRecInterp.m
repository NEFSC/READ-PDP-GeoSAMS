isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
reg='MA';
fl0='Data/Recruits2005MA.csv';
if isOctave
    D=csvreadK(fl0);
    dyr0=D(:,1);
    x0=D(:,2);
    y0=D(:,3);
    z0=D(:,4);
else
    warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
    D=readtable(fl0,"FileType","spreadsheet");
    dyr0=table2array(D(:,1));
    x0=table2array(D(:,2));
    y0=table2array(D(:,3));
    z0=table2array(D(:,4));
end
ngp=length(x0);
for yr=1979:2018
    fl=['Data/Recruits',int2str(yr),reg,'.csv'];
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
    fl=['Data/NNRecs',int2str(yr),'For05Survey.csv'];
    DNN=[dyr0,x0,y0,z0,recs05];
    %header=['nearest neighbor interpolation of recruits to 2005 survey points for year ',int2str(yr)]
    header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per m^2"';
    writecsv (DNN,fl,"%f, %f, %f, %f, %f",header)
end
        
    
