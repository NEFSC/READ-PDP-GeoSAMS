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
function PullOutRecruitData(src)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
    % used if called by command line
    arg_list=argv();
    if ~strcmp(arg_list(1), '--gui');
        src = str2num(cell2mat(arg_list(1)));
    else
        src = str2num(src);
    end
end
srcText = src;

flnm = 'OriginalData/dredgetowbysize7917.csv';
fprintf('Reading from %s\n', flnm)

if isOctave
  F=csvreadK(flnm);
  size_grp = F(:,27);
  survn = F(:,30);
  src = F(:,37);
else
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F= readtable(flnm,"FileType","text");
  size_grp = table2array(F(:,27));
  survn = table2array(F(:,30));
  src = table2array(F(:,37));
end

if srcText == 0
    n=find(size_grp==3);
else
    n=find(size_grp==3 & src==srcText);
end
for k=1:numel(n)
    % sum 3cm to 6 cm
    recr(k) = sum(survn(n(k)+1:n(k)+6));
end

if isOctave
    recr_t = transpose(recr);
else
    recr_t = array2table(transpose(recr),'VariableNames',{'rec'});
end
if srcText == 0
    j=size_grp==4; M = [F(j,:) recr_t];
else
    j=size_grp==4 & src==srcText; M = [F(j,:) recr_t];
end

flnm = 'OriginalData/NewRecruits.csv';

if isOctave
  csvwrite(flnm, M);
else
  writetable(M,flnm);
end
