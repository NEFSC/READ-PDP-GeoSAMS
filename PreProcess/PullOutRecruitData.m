flnm = 'OriginalData/dredgetowbysize7917.csv';
fprintf('Reading from %s\n', flnm)

warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames')
F= readtable(flnm,"FileType","text");
size_grp = table2array(F(:,27));
survn = table2array(F(:,30));

n=find(size_grp==3);
for k=1:numel(n)
    % sum 3cm to 6 cm
    recr(k) = sum(survn(n(k)+1:n(k)+6));
end
recr_t = array2table(transpose(recr),'VariableNames',{'recr'});

j=find(size_grp==4); M = [F(j,:) recr_t];
flnm = 'OriginalData/NewRecruits.csv';
writetable(M,flnm);
