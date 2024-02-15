% for each location it sums together the scallop density from shell length 3cm to 6 cm,
% inclusive. It then adds this value as a new column along with the current data for size
% grp 4 as a single row for the location and writes this out to "OriginalData/NewRecruits.csv".

flnm = 'OriginalData/dredgetowbysize7917.csv';
fprintf('Reading from %s\n', flnm)

isOctave = (exist('OCTAVE_VERSION', 'builtin') ~= 0);

if isOctave
  F=csvreadK(flnm);
  size_grp = F(:,27);
  survn = F(:,30);
else
  warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
  F= readtable(flnm,"FileType","text");
  size_grp = table2array(F(:,27));
  survn = table2array(F(:,30));
end

n=find(size_grp==3);
for k=1:numel(n)
    % sum 3cm to 6 cm
    recr(k) = sum(survn(n(k)+1:n(k)+6));
end

if isOctave
    recr_t = transpose(recr);
else
    recr_t = array2table(transpose(recr),'VariableNames',{'recr'});
end

j=find(size_grp==4); M = [F(j,:) recr_t];
flnm = 'OriginalData/NewRecruits.csv';

if isOctave
  csvwrite(flnm, M);
else
  writetable(M,flnm);
end
