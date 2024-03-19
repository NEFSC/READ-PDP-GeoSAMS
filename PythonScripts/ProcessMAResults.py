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

paramStr = ['EBMS_', 'LAND_', 'LPUE_', 'FEFF_','RECR_']
prefix = ['Results/Lat_Lon_Grid_', 'Results/Lat_Lon_Grid_Trend-']

for pStr in paramStr:
    # UK expects input data files to be in subdir Data/
    # UK writes results to subdir Results/
    # using the same file name as provide for observation data.
    flin = 'X_Y_' + pStr + dn + str(year_start) + '_0.csv'
    result = subprocess.run([ex, cfgFile, dn, flin, ' F'])
    print([ex, cfgFile, dn, flin, ' F'])
    if (result.returncode != 0):
        print('[31m' + flin + ' error: ' + hex(result.returncode) + '[0m')
        sys.exit(result.returncode)
    os.remove(dataDir + flin)

    for year in years:
        flin = 'X_Y_' + pStr + dn + str(year) + '.csv'
        # output all data, proc_recruits='F'
        result = subprocess.run([ex, cfgFile, dn, flin, ' F'])
        print([ex, cfgFile, dn, flin, ' F'])
        if (result.returncode != 0):
            print('[31m' + flin + ' error: ' + hex(result.returncode) + '[0m')
            sys.exit(result.returncode)
        os.remove(dataDir + flin)

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
