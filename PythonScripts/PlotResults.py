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
years = range(year_start-1, year_end + 1) # year_start-1 is initial state

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
