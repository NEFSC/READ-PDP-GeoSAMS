% Used to write a consistent header row, overwriting headers used by survey data
function WriteHeader(filename)
c =['DecYr  ';'UTM-x  ';'UTM-y  ';'Lat    ';'Lon    ';'Depth  ';'IsClose';'Stratum';'Zone   '; 'Region ';
    'Var01  ';'Var02  ';'Var03  ';'Var04  ';'Var05  ';'Var06  ';'Var07  ';'Var08  ';'Var09  ';
    'Var10  ';'Var11  ';'Var12  ';'Var13  ';'Var14  ';'Var15  ';'Var16  ';'Var17  ';
    'Var18  ';'Var19  ';'Var20  ';'Var21  ';'Var22  ';'Var23  ';'Var24  ';'Var25  '];

fid = fopen (filename, "w");
for i = 1:size(c,1)-1
    fputc(fid, strtrim(c(i,:)));
    fputc(fid, ',');
end
fputc(fid, strtrim(c(i+1,:)));
fclose(fid);
