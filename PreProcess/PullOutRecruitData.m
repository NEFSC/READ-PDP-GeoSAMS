% for each location it sums together the scallop density from shell length 3cm to 6 cm,
% inclusive. It then adds this value as a new column along with the current data for size
% grp 4 as a single row for the location and writes this out to "OriginalData/NewRecruits.csv".
% src
% NMFS_ALB ==> 1111
% CANADIAN ==> 2222
% F/V_TRAD ==> 3333
% VIMSRSA ==> 4444
% NMFSSHRP ==> 5555
% ALL ==> 0
function PullOutRecruitData(src, useHabCam)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui');
        src = str2num(cell2mat(arg_list(1)));
        useHabCam = cell2mat(arg_list(2))
    else
        src = str2num(src);
    end
end
srcText = src;

useHC = strcmp(useHabCam, 'T');

% Need these header data used by ProcessRecruitData
% Year Month Day Lat Lon Z recr
if useHC
    header = { "year","month","day","station","lat","lon","xutm","yutm","setdpth","sizegrp","surv_n",...
                "SQM","NImages","area","stratum","clop"};
    yearCol    = find(strcmpi("year", header), 1);
    monCol     = find(strcmpi("month", header), 1);
    dayCol     = find(strcmpi("day", header), 1);
    latCol     = find(strcmpi("lat"    , header), 1);
    lonCol     = find(strcmpi("lon"    , header), 1);
    zCol       = find(strcmpi("setdpth", header), 1);
    sgCol      = find(strcmpi("sizegrp", header), 1);
    svCol      = find(strcmpi("surv_n" , header), 1);
    sqmCol     = find(strcmpi("SQM"    , header), 1);
    srcText = 0;
    srcCol = 16;  % not used for HabCam data, srcText force to 0
    flnm = 'OriginalData/Habcam_BySegment_2000_2014-2020.csv';
else
    header = { "area","subarea","cruise6","year","month","day","time","stratum","tow","station",...
               "statype","SVGEAR","haul","gearcon","sdefid","newstra","clop","lat","lon","tnms",...
               "setdpth","bottemp","dopdisb","distused","towadj","towdur","sizegrp","catchnu",...
               "catchwt","surv_n","partrecn","fullrecn","surv_b","partrecb","fullrecb","postow",...
               "datasource","lwarea","SETLW","SVSPP","PropChains","AREAKIND","STRATMAP","SQNM"};
    yearCol    = find(strcmpi("year", header), 1);
    monCol     = find(strcmpi("month", header), 1);
    dayCol     = find(strcmpi("day",   header), 1);
    latCol     = find(strcmpi("lat"    , header), 1);
    lonCol     = find(strcmpi("lon"    , header), 1);
    zCol       = find(strcmpi("setdpth", header), 1);
    sgCol      = find(strcmpi("sizegrp", header), 1);
    svCol      = find(strcmpi("surv_n" , header), 1);
    srcCol     = find(strcmpi("datasource", header), 1);
    flnm = 'OriginalData/dredgetowbysize7917.csv';
end


fprintf('Reading from %s\n', flnm)

if isOctave
  F=csvreadK(flnm);
  size_grp = F(:,sgCol);
  survn = F(:,svCol);
  dataSrc = F(:,srcCol);
else
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F= readtable(flnm,"FileType","text");
  size_grp = table2array(F(:,sgCol));
  survn = table2array(F(:,svCol));
  dataSrc = table2array(F(:,srcCol));
end

if srcText == 0
    n=find(size_grp==3);
else
    n=find(size_grp==3 & dataSrc==srcText);
end
recr = zeros(1, numel(n));
for k=1:numel(n)
    % sum 3cm to 6 cm
    recr(k) = sum(survn(n(k)+1:n(k)+6));
end

% Need to format file so that it has the same data regardless of source
if isOctave
    recr_t = transpose(recr);
else
    recr_t = array2table(transpose(recr),'VariableNames',{'rec'});
end
if srcText == 0
    j=size_grp==4;
else
    j=size_grp==4 & dataSrc==srcText;
end
if ~useHC
    F(j,lonCol) = -F(j,lonCol);
end
M = [F(j,yearCol) F(j,monCol) F(j,dayCol) F(j,latCol) F(j,lonCol) F(j,zCol) recr_t];

flnm = 'OriginalData/NewRecruits.csv';

if isOctave
  csvwrite(flnm, M);
else
  writetable(M,flnm);
end
