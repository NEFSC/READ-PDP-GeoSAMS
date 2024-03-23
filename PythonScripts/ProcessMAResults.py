#subprocess.run([ex, dn, flin])

import subprocess
import socket
import os
import sys
import csv

from collections import defaultdict

if (len(sys.argv) != 3):
    print ("  Missing command line arguments. Expecting: ")
    print ("  $ ProcessMAResults.py StartYear EndYear")
    print()
    quit()
    
dataDir = 'Data/'
year_start = int(sys.argv[1])
year_end = int(sys.argv[2])
dn = 'MA'
print(year_start, year_end, dn)

years = range(year_start, year_end+1)

# Used whil concatenating files
# number of colums in csv file, starting at 0
# lat, lon, initial data
ncols = year_end - year_start + 3
# number of years plus initial state
nyears = year_end - year_start + 2

# set configuration file name for UK.exe
cfgFile = 'UK_MA.cfg'
ex = os.path.join('UKsrc', 'UK')

#paramStr = ['EBMS_', 'LAND_', 'LPUE_', 'FEFF_','RECR_']
paramStr = ['EBMS_', 'LPUE_', 'RECR_']
prefix = ['Results/Lat_Lon_Grid_', 'Results/Lat_Lon_Grid_Trend-']
#          , 'Temp/Lat_Lon_Grid_', 'Temp/Lat_Lon_Grid_Trend-']

for pStr in paramStr:
    if (pStr == 'RECR_'):
        zArg = '70.0'
    else:
        zArg = ''

    # UK expects input data files to be in subdir Data/
    # UK writes results to subdir Results/
    # using the same file name as provide for observation data.
    #
    #       Config File  Domain  ObservFile              ZF0Max(optional)
    # .\UKsrc\UK UK.cfg  MA      X_Y_EBMS_MA2005_0.csv   0.0 | 70.0
    #  arg#        1      2              3                  4

    #                                                 | --- optional but need both -----|
    #        ConfigFile Domain  ObservFile                GridFile            ZF0Max
    # [ex,   cfgFile,   dn,     obsFile,                  gridFile,           value
    # .\UKsrc\UK UK.cfg GB      X_Y_EBMS_GB2005_0_SW.csv  GBxyzLatLonSW.csv   0.0 | 70.0
    #  arg#        1     2              3                  4                   5

    obsFile = 'X_Y_' + pStr + dn + str(year_start) + '_0.csv'
    cmd = [ex, cfgFile, dn, obsFile, zArg]
    print(cmd)
    result = subprocess.run(cmd)
    if (result.returncode != 0):
        print('[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m')
        sys.exit(result.returncode)
    os.remove(dataDir + obsFile)

    for year in years:
        obsFile = 'X_Y_' + pStr + dn + str(year) + '.csv'
        cmd = [ex, cfgFile, dn, obsFile, zArg]
        result = subprocess.run(cmd)
        print(cmd)
        if (result.returncode != 0):
            print('[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m')
            sys.exit(result.returncode)
        os.remove(dataDir + obsFile)

    for pfix in prefix:
        ###########################################################################################
        # subprocess.run takes in Data/X_Y_* and creates both 
        #    Results/Lat_Lon_Grid* 
        #    Results/Lat_Lon_Grid_Trend*
        # Concatenate individual year files into a single file
        ###########################################################################################
        col = []
        col = [defaultdict(list) for i in range(nyears)]
        k = 0
        
        flin = pfix + pStr + dn + str(year_start) + '_0' + '.csv'
        with open(flin) as f:
            reader = csv.reader(f)
            for row in reader:
                for (i,v) in enumerate(row):
                    col[k][i].append(v)
            f.close()
        os.remove(flin)

        # append remaining years as additional columns to first data set
        for year in years:
            k  += 1
            flin = pfix + pStr + dn + str(year) + '.csv'
            with open(flin) as f:
                reader = csv.reader(f)
                for row in reader:
                    for (i,v) in enumerate(row):
                        col[k][i].append(v)
                f.close()
            os.remove(flin)

            for i in range (len(col[0][0])):    
                col[0][k + 2].append(col[k][2][i])

        # brute force write out results
        flout = open(pfix + pStr + dn + '_' + str(year_start) + '_' + str(year_end) + '.csv', 'w')
        for row in range(len(col[0][0])):
            for c in range(ncols):
                flout.write(col[0][c][row])
                flout.write(',')
            flout.write(col[0][ncols][row])
            flout.write('\n')

        print('Files concatenated to: ',flout.name)
        flout.close()
    # end for pfix
# end for pStr
sys.exit(0)
