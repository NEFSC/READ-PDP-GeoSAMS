#subprocess.run([ex, dn, flin])

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
    print ("  Missing command line arguments. Expecting: ")
    print ("  $ ProcessResults.py Domain StartYear EndYear Scallop.cfg UK.cfg")
    print()
    quit()

dataDir = 'Data/'
dn = sys.argv[1]
year_start = int(sys.argv[2])
year_end = int(sys.argv[3])
simCfgFile = sys.argv[4]
ukCfgFile = sys.argv[5]
years = range(year_start, year_end + 1)

# Used while concatenating files
# number of colums in csv file, starting at 0
# lat, lon, initial data
ncols = year_end - year_start + 3
# number of years plus initial state
nyears = year_end - year_start + 2

# set configuration file name for UK.exe
ex = os.path.join('UKsrc', 'UK')
simFile = os.path.join('Configuration', 'Simulation', simCfgFile)
[paramStr, tsInYear, savedByStratum] = ReadSimConfigFile(simFile)
if dn=='MA': savedByStratum = False
if savedByStratum:
    # Only GB and AL[L] use savedByStratum
    if dn=='GB':
        rgn = ['_SW', '_N', '_S', '_W']
    else:
        # This would be AL
        rgn = ['_SW', '_N', '_S', '_W', '_MA']
else:
    # This would be just MA
    rgn = ['']

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
        obsFile = 'X_Y_' + pStr + dn + str(year_start) + '_0' + r + '.csv'
        if dn == 'AL':
            # ALxyzLatLon_MA uses the same grid file as MA
            #     'MA'+'xyzLatLon' + '' + '.csv'
            # ALxyzLatLon_SW to ALxyzLatLon_W use the same grid files as GB
            # 'GBxyzLatLon' + r + '.csv'
            #
            # These data files also need to use separate spatial functions
            # Override command line argument
            if r == '_MA':
                gridFile = 'MAxyzLatLon.csv'
                ukCfgFile = 'UK_MA.cfg'
            else:
                gridFile = 'GBxyzLatLon' + r + '.csv'
                ukCfgFile = 'UK_GB.cfg'
        else:
            gridFile = dn+'xyzLatLon' + r + '.csv'
        cmd = [ex, ukCfgFile, dn, obsFile, gridFile, zArg]
        result = subprocess.run(cmd)
        if (result.returncode != 0):
            print('[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m')
            sys.exit(result.returncode)
        print( 'Just Finished: ', cmd)
        os.remove(dataDir + obsFile)

        for year in years:
            obsFile = 'X_Y_' + pStr + dn + str(year) + r + '.csv'
            cmd = [ex, ukCfgFile, dn, obsFile, gridFile, zArg]
            result = subprocess.run(cmd)
            if (result.returncode != 0):
                print('[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m')
                sys.exit(result.returncode)
            print( 'Just Finished: ', cmd)
            os.remove(dataDir + obsFile)

        for pfix in prefix:
            ###########################################################################################
            # subprocess.run takes in Data/X_Y_* and creates both 
            #    Results/Lat_Lon_Grid* 
            #    Results/Lat_Lon_Grid_Trend* DEPRECATED
            # Concatenate individual year files into a single file
            ###########################################################################################
            col = []
            col = [defaultdict(list) for i in range(nyears)]
            k = 0
            
            flin = pfix + pStr + dn + str(year_start) + '_0' + r + '.csv'
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
                flin = pfix + pStr + dn + str(year) + r + '.csv'
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
            if savedByStratum:
                flout = open(pfix + pStr + dn + r + '.csv', 'w')
            else:
                flout = open(pfix + pStr + dn + '_' + str(year_start) + '_' + str(year_end) + '.csv', 'w')
            for row in range(len(col[0][0])):
                for c in range(ncols):
                    flout.write(col[0][c][row])
                    flout.write(',')
                flout.write(col[0][ncols][row])
                flout.write('\n')
            flout.close()
        # end for pfix
    # end for rgn

    if savedByStratum:
        # now combine all region files into one file
        for pfix in prefix:
            flout = pfix + pStr + dn + '_' + str(year_start) + '_' + str(year_end) + '.csv'
            wrFile = open(flout, 'w')
            for r in rgn:
                flin = pfix + pStr + dn + r + '.csv'
                rdFile = open(flin, 'r')
                lines = rdFile.readlines()
                wrFile.writelines(lines)
                rdFile.close()
                os.remove(flin)
            wrFile.close()
            print('Files concatenated to: ',flout)
        # end for pfix
    else:
        print('Files concatenated to: ',flout.name)

    # end savedByStratum
# end for pStr

# We have needed output paramters so lets plot data and save to pdf files
print('Plotting Results')
p = platform.platform()

for pStr in paramStr:
    str1 = 'Results/Lat_Lon_Surv_' + pStr + dn
    str2 = 'Results/Lat_Lon_Grid_' + pStr + dn+'_'+str(year_start) + '_' + str(year_end)

    if p[0:3] == 'Win':
        cmd = ['matlab.exe', '-batch', 'PlotLatLonGridSurvey('+"'"+str1+"','"+str2+"', "+str(year_start)+','+str(tsInYear)+", '"+dn+"')"]
    else: 
        cmd = ['octave', 'mfiles/PlotLatLonGridSurvey.m', str1, str2, str(year_start), str(tsInYear), dn]

    result = subprocess.run(cmd)
    if (result.returncode != 0):
        print('[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m')
        sys.exit(result.returncode)

sys.exit(0)
