function NearestNeighborRecInterp(yrStart, yrEnd)
isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

domain = ['GB', 'MA'];
for dd = 1:2
    dom=domain(dd*2-1:dd*2);
    fl0=strcat('Data/Recruits',int2str(yrStart),dom,'.csv');
    if isOctave
        D=csvreadK(fl0);
        dyr0=D(:,1);
        x0=D(:,2);
        y0=D(:,3);
        z0=D(:,4);
        recs0=D(:,5);
    else
        warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
        D=readtable(fl0,"FileType","spreadsheet");
        dyr0=table2array(D(:,1));
        x0=table2array(D(:,2));
        y0=table2array(D(:,3));
        z0=table2array(D(:,4));
        recs0=table2array(D(:,5));
    end
    fl2=strcat('KrigingEstimates/Sim',dom,int2str(yrStart),'/KrigingEstimate.txt');
    fprintf('Writing to %s\n', fl2)
    writematrix(recs0,fl2)
    
    ngp=length(x0);
    for yr=yrStart+1:yrEnd
        fl=strcat('Data/Recruits',int2str(yr),dom,'.csv');
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
    %    fl=['Data/NNRecs',int2str(yr),'For05Survey.csv'];
    %    DNN=[dyr0,x0,y0,z0,recs05];
        %header=['nearest neighbor interpolation of recruits to 2005 survey points for year ',int2str(yr)]
    %    header='"decmal year", "x utm", "y utm", "bottom depth(m)","recruits per m^2"';
    %    writecsv (DNN,fl,"%f, %f, %f, %f, %f",header)
        fl2=strcat('KrigingEstimates/Sim',dom,int2str(yr),'/KrigingEstimate.txt');
        fprintf('Writing to %s\n', fl2)
        writematrix(recs05,fl2)
    end
end    