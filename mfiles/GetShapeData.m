function x = GetShapeData()
% Called from HabCamData5mmbin and TrawlData5mmbin
%
% reads in shape files named by environment variables
% Concatenates MAB and GB structures into a single structure
% getZone is true if the region data is desired
% otherwise the buffer data is read.

maShapeFile = getenv('MAShapeFile');
gbShapeFile = getenv('GBShapeFile');
maShapeDir  = ['MAB_Region', filesep()];
gbShapeDir  = ['GB_Region', filesep()];

dataFile = ['Shapefiles', filesep(), gbShapeDir, gbShapeFile];
G = shaperead(dataFile);
shapeG = struct('X',{G(:).X}, 'Y',{G(:).Y}, 'Zone', {G(:).Zone}, 'Region', {G(:).Region}, 'Area', {G(:).Area});

dataFile = ['Shapefiles',filesep(), maShapeDir, maShapeFile];
M = shaperead(dataFile);
shapeM = struct('X',{M(:).X}, 'Y',{M(:).Y}, 'Zone', {M(:).Zone}, 'Region', {M(:).Region}, 'Area', {M(:).Area});

x = [shapeM, shapeG];