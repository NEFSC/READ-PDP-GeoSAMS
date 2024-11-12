#subprocess.run([ex, dn, flin])
# GUI specifies 2022 to 2025
# - X_Y_BIOM_2022_DN  Initial state as of June 1, 2022 @ 00:00, i.e. May 31, 2022 @ 24:00
# - X_Y_BIOM_2023_DN  Growth state as of May 31, 2023 @ 24:00, results for 1st year growth
# - X_Y_BIOM_2024_DN  Growth state as of May 31, 2024 @ 24:00, results for 2nd year growth
# - X_Y_BIOM_2025_DN  Growth state as of May 31, 2025 @ 24:00, results for 3rd year growth
# - X_Y_BIOM_2026_DN  Growth state as of May 31, 2026 @ 24:00, results for 4th year growth

import subprocess
import os
import sys
import csv
import platform
import shutil

from collections import defaultdict
from ReadSimConfigFile import *

nargs = len(sys.argv)
if (nargs != 6):
    print ('[31m' + "  Missing command line arguments. Expecting: "+ '[0m')
    print ("  $ ProcessResults.py Domain StartYear EndYear Scallop.cfg UK.cfg")
    print()
    quit()

dataDir = 'Data/'
dn = sys.argv[1]
year_start = int(sys.argv[2])
year_end = int(sys.argv[3])
simCfgFile = sys.argv[4]
ukCfgFile = sys.argv[5]
# year_start is initial state, yearStop+2 represents the last year of simulation output, actually '+1'
years = range(year_start, year_end+2)

# Used while concatenating files
# number of colums in csv file, starting at 0
# lat, lon, initial data
ncols = year_end - year_start + 3
# number of years plus initial state
nyears = year_end - year_start + 2

# set configuration file name for UK.exe
ex = os.path.join('UKsrc', 'UK')
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

prefix = ['Results/Lat_Lon_Grid_'] #, 'Results/Lat_Lon_Grid_Trend-']

for pStr in paramStr:

    if (pStr == 'RECR_'):
        zArg = '70.0'
    else:
        zArg = '0.0'

    # Process multiple GB region files
    #                                                 | --- optional but need both -----|
    #        ConfigFile Domain  ObservFile                GridFile            ZF0Max
    # [ex,   ukCfgFile,   dn,     obsFile,                  gridFile,           value
    # .\UKsrc\UK UK.cfg GB      X_Y_EBMS_GB2005_0_SW.csv  GBxyzLatLonSW.csv   0.0 | 70.0
    #  arg#        1     2              3                  4                   5
    for r in rgn:
        if dn == 'AL':
            # ALxyzLatLon_MA uses the same grid file as MA
            #     'MA'+'xyzLatLon' + '' + '.csv'
            # ALxyzLatLon_SW to ALxyzLatLon_W use the same grid files as GB
            # 'GBxyzLatLon' + r + '.csv'
            #
            # These data files also need to use separate spatial functions
            # Override command line argument
            if r == '_MA':
                gridFile = 'MAxyzLatLonRgn.csv'
                ukCfgFile = 'UK_MA.cfg'
            else:
                #gridFile = 'GBxyzLatLon' + r + '.csv'
                gridFile = 'GBxyzLatLonRgn.csv'
                ukCfgFile = 'UK_GB.cfg'
        else:
            # DEPRECATE: if we no longer need to separate GB into sub regions
            #gridFile = self.domainName+'xyzLatLon' + r + '.csv'
            gridFile = dn+'xyzLatLonRgn.csv'

        for year in years:
            obsFile = 'X_Y_' + pStr + dn + str(year) + r + '.csv'
            cmd = [ex, ukCfgFile, dn, obsFile, gridFile, zArg]
            result = subprocess.run(cmd)
            if (result.returncode != 0):
                print('[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m')
                sys.exit(result.returncode)
            print( 'Just Finished: ', cmd)
###            os.remove(dataDir + obsFile)

        for pfix in prefix:
            ###########################################################################################
            # subprocess.run takes in Data/X_Y_* and creates
            #    Results/Lat_Lon_Grid* 
            # Concatenate individual year files into a single file
            ###########################################################################################
            col = []
            col = [defaultdict(list) for _ in range(nyears)]
            k = 0
            
            # append remaining years as additional columns to first data set
            for year in years:
                flin = pfix + pStr + dn + str(year) + r + '.csv'
                with open(flin) as f:
                    reader = csv.reader(f)
                    for row in reader:
                        for (i,v) in enumerate(row):
                            col[k][i].append(v)
                    f.close()
###                os.remove(flin)

                for i in range (len(col[0][0])):    
                    col[0][k + 2].append(col[k][2][i])
                k  += 1

            # brute force write out results
            flout = open(pfix + pStr + dn + r + '.csv', 'w')
            for row in range(len(col[0][0])):
                for c in range(ncols):
                    flout.write(col[0][c][row])
                    flout.write(',')
                flout.write(col[0][ncols][row])
                flout.write('\n')
            flout.close()
        # end for pfix
    # end for rgn

    # now combine all region files into one file
    for pfix in prefix:
        flout = pfix + pStr + dn + '_' + str(year_start) + '_' + str(year_end+1) + '.csv'
        wrFile = open(flout, 'w')
        for r in rgn:
            flin = pfix + pStr + dn + r + '.csv'
            rdFile = open(flin, 'r')
            lines = rdFile.readlines()
            wrFile.writelines(lines)
            rdFile.close()
###            os.remove(flin)
        wrFile.close()
        print('Files concatenated to: ',flout)
    # end for pfix
# end for pStr

# We have needed output paramters so lets plot data and save to pdf files
print('Plotting Results')
p = platform.platform()

for pStr in paramStr:
    str1 = 'Results/Lat_Lon_Surv_' + pStr + dn
    str2 = 'Results/Lat_Lon_Grid_' + pStr + dn+'_'+str(year_start) + '_' + str(year_end+1)

    if p[0:3] == 'Win':
        cmd = ['matlab.exe', '-batch', 'PlotLatLonGridSurvey('+"'"+str1+"','"+str2+"', "+str(year_start)+','+str(tsInYear)+", '"+dn+"')"]
    else: 
        cmd = ['octave', 'mfiles/PlotLatLonGridSurvey.m', str1, str2, str(year_start), str(tsInYear), dn]

    result = subprocess.run(cmd)
    if (result.returncode != 0):
        print('[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m')
        sys.exit(result.returncode)

sys.exit(0)
