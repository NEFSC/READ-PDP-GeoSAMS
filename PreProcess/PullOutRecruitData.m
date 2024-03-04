% for each location it sums together the scallop density from shell length 3cm to 6 cm,
% inclusive. It then adds this value as a new column along with the current data for size
% grp 4 as a single row for the location and writes this out to "OriginalData/NewRecruits.csv".
function PullOutRecruitData(src)
% It seems to be confusing trying to pass a string in batch files or command line. 
% "text" becomes 'text'
% Therefore, converting passed parameter to desired string
switch src
case 0
    srcText = "ALL";
case 1
    srcText = "NMFS_ALB";
case 2
    srcText = "VIMSRSA";
otherwise
    srcText = "ALL";
end

flnm = 'OriginalData/dredgetowbysize7917.csv';
fprintf('Reading from %s\n', flnm)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

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

if srcText == "ALL"
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

if srcText == "ALL"
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
end