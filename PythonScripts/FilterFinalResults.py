#########################################################################################
###    STAND ALONG FILE TO TEST CODING FOR
###    # Process CSV files to combine the years
###    In GeoSams.py
#########################################################################################
import sys
import os
import pandas as pd
from ReadSimConfigFile import *

dn = sys.argv[1]
year_start = int(sys.argv[2])
year_end = int(sys.argv[3])
simCfgFile = sys.argv[4]
years = range(year_start, year_end + 1) # year_start is initial state

# Used while concatenating files
# number of colums in csv file, starting at 0
# lat, lon, initial data
ncols = year_end - year_start + 3
# number of years plus initial state
nyears = year_end - year_start + 2

simFile = os.path.join('Configuration', 'Simulation', simCfgFile)
[paramStr, tsInYear] = ReadSimConfigFile(simFile)

if dn=='GB':
    # rgn = ['_SW', '_N', '_S', '_W']
    rgn = ['_GB']
elif dn=='MA':
    rgn = ['_MA']
else:
    # This would be AL
    #rgn = ['_SW', '_N', '_S', '_W', '_MA']
    rgn = ['_GB', '_MA']

pfix = 'Results/Lat_Lon_Grid_'

for pStr in paramStr:
    final = pfix + pStr + dn + '_'  + str(year_start) + '_' + str(year_end) + '.csv'
    for r in rgn:
        for year in years:
            # Lat_Lon_Grid_BIOM_AL2026_MA_REGION_KRIGE
            flin = pfix + pStr + dn + str(year) + r + '_REGION_KRIGE.csv'
            df = pd.read_csv(flin, usecols=['UTM_X', 'UTM_Y', 'LAT','LON', 'ZONE', 'FINALPREDICT'])

            if year == year_start:
                dfFinal = df.reindex(columns=['LAT','LON', 'UTM_X', 'UTM_Y', 'ZONE', 'FINALPREDICT'])
                dfFinal.rename(columns={ 'FINALPREDICT':str(year)}, inplace=True)
            else:
                dfFinal[str(year)] = df['FINALPREDICT']

        if r == rgn[0]: 
            dfFinal.to_csv(final, index=False)
        else:
            dfFinal.to_csv(final, mode='a', index=False,header=False)

############################################################################
#### Testing code for SortByRegionFrame.py
############################################################################
print("Use this name: ", final)
df = pd.read_csv(final)
dfAgg = pd.DataFrame()
#uniqueValues = df['ZONE'].unique()
for year in years:
    # print(df.groupby('ZONE').agg({str(year):'sum'})) #print sum by region
    dfAgg[str(year)] = df.groupby('ZONE').agg({str(year):'sum'})

dfAgg.to_csv('temp.csv')

counts = df['2022'].value_counts()
try:
    countNonZero = len(df['2022']) - counts[0]
    print(countNonZero)
except:
    countNonZero = len(df['2022'])
    print(countNonZero, len(df['2022']))

counts = df['ZONE'].value_counts().get('2022',0)
print(counts)

# df = pd.read_csv('Grids\GBRegionGrid.csv',na_filter=False)
# zonesGB=sorted(df['ZONE'].unique())
# zonesGB = list(filter(lambda x: (x!='NA'),zonesGB))

# df = pd.read_csv('Grids\MARegionGrid.csv',na_filter=False)
# zonesMA=sorted(df['ZONE'].unique())
# zonesMA = list(filter(lambda x: (x!='NA'),zonesMA))

# zones = sorted(zonesGB + zonesMA)

