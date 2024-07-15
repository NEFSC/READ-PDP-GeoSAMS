% This script will read in the GeoSAMS simulation output file
%    Lat_Lon_Surv_PARAM_DN.csv
% That is organized 
% number of survey grids rows, by timesteps * number of years + 1 colums
% The +1 if for the first column that provides the initial state.
%
% The script is used to either 
%  - Compute an annual sum for data such as Landings that pull scallops 
%    from the ocean
%  - Compute an annual mean such as Biomass that is based on all scallops
%    remaining in the ocean
function ProcessSumMean(domain, startYear, endYear, param, ts, useMean)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);
if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui')
        domain = cell2mat(arg_list(1));
        startYear = cell2mat(arg_list(2));
        endYear = str2num(cell2mat(arg_list(3)));
        param = arg_list(4);
        endYear = str2num(cell2mat(arg_list(5)));
        useMean = arg_list(6);
    end
end

prefix = 'Results/Lat_Lon_Surv_';
nyears = endYear - startYear + 1;
flin = [prefix param '_' domain '.csv'];


M=readtable(flin);
%    divisor = float(ts) if isMean else 1.0
if (useMean == 'T'); divisor = double(ts); else; divisor = 1.0; end
nrows = size(M,1);
sums = zeros(nrows, nyears+1);
for k=1:nrows
    sums(k,1) = table2array(M(k,3));
end

%    for k in range(1, nrows):
%        # initial state is in col2, 'LAT, LON, DATA, ...
%        for y in range(nyears):
%            sums[k-1][y+1] = _sum(arr[k][y*ts+3:y*ts+16]) / divisor
pArray = zeros(ts);
for k=787:nrows
    for y=1:nyears
        iStart = (y-1)*ts+4;
        iEnd = iStart + ts - 1;
        pArray = table2array(M(k,iStart:iEnd));
        sums(k,y+1) = sum(pArray) / divisor;
    end
end

fprintf('%f\n',sums(787,2))
fprintf('%f\n',sums(787,3))
fprintf('%f\n',sums(787,4))



