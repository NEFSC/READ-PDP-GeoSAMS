#subprocess.run([ex, dn, flin])

import subprocess
import socket
import os
import sys
import csv

from collections import defaultdict


if (len(sys.argv) != 3):
    print ("  Missing command line arguments. Expecting: ")
    print ("  $ ProcessGBResults.py StartYear EndYear")
    print()
    quit()

dataDir = 'Data/'
dn = 'GB'
year_start = int(sys.argv[1])
year_end = int(sys.argv[2])
years = range(year_start, year_end + 1)

# Used whil concatenating files
# number of colums in csv file, starting at 0
# lat, lon, initial data
ncols = year_end - year_start + 3

# set configuration file name for UK.exe
cfgFile = 'UK.cfg'
ex = os.path.join('UKsrc', 'UK')

paramStr = ['EBMS_', 
            'LAND_', 
            'LPUE_', 
            'FEFF_',
            'RECR_']

for pStr in paramStr:

    # Process multiple GB region files
    # .\UKsrc\UK UK.cfg GB X_Y_EBMS_GB2005_0_SW.csv GBxyzLatLonSW.csv F
    rgn = ['_SW', '_N', '_S', '_W']
    for r in rgn:
        flin = 'X_Y_' + pStr + dn + str(year_start) + '_0' + r + '.csv'
        gridFile = 'GBxyzLatLon' + r + '.csv'
        result = subprocess.run([ex, cfgFile, dn, flin, gridFile, ' F'])
        if (result.returncode != 0):
            print('[31m' + flin + ' error: ' + hex(result.returncode) + '[0m')
            sys.exit(result.returncode)
        print([ex, cfgFile, dn, flin, gridFile, ' F'])
        os.remove(dataDir + flin)

        for year in years:
            flin = 'X_Y_' + pStr + dn + str(year) + r + '.csv'
            result = subprocess.run([ex, cfgFile, dn, flin, gridFile, ' F'])
            if (result.returncode != 0):
                print('[31m' + flin + ' error: ' + hex(result.returncode) + '[0m')
                sys.exit(result.returncode)
            print([ex, cfgFile, dn, flin, gridFile, ' F'])
            os.remove(dataDir + flin)

        # subprocess.run takes in Data/X_Y_* and creates Results/Lat_Lon_Grid*
        # Concatenate individual year files into a single file
        col = []
        col = [defaultdict(list) for i in range(5)]
        k = 0
        
        flin = 'Results/Lat_Lon_Grid_' + pStr + dn + str(year_start) + '_0' + r + '.csv'
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
            flin = 'Results/Lat_Lon_Grid_' + pStr + dn + str(year) + r + '.csv'
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
        flout = open('Results/Lat_Lon_Grid_' + pStr + dn + r + '.csv', 'w')
        for row in range(len(col[0][0])):
            for c in range(ncols):
                flout.write(col[0][c][row])
                flout.write(',')
            flout.write(col[0][ncols][row])
            flout.write('\n')

        flout.close()
    # end for rgn

    # now combine all region files into one file
    flout = 'Results/Lat_Lon_Grid_' + pStr + dn + '_' + str(year_start) + '_' + str(year_end) + '.csv'
    wrFile = open(flout, 'w')
    for r in rgn:
        flin = 'Results/Lat_Lon_Grid_' + pStr + dn + r + '.csv'
        rdFile = open(flin, 'r')
        lines = rdFile.readlines()
        wrFile.writelines(lines)
        rdFile.close()
        os.remove(flin)
    wrFile.close()

    print('Files concatenated to: ',flout)
# end for pStr