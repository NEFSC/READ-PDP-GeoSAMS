#subprocess.run([ex, dn, flin])

import subprocess
import socket
import os
import sys

if (len(sys.argv) < 4):
    print ("  Missing command line arguments. Expecting: ")
    print ("  $ EstimateRecruitFields.py StartYear EndYear Domain")
    print()
    quit()
    
year_start = int(sys.argv[1])
year_end = int(sys.argv[2])
dn = sys.argv[3]
print(year_start, year_end, dn)

years = range(year_start, year_end+1)
MC = range(1, 11)

# set configuration file name for UK.exe
cfgFile = 'UK.cfg'
ex = os.path.join('UKsrc', 'UK')

xyString = ['X_Y_EBMS_', 'X_Y_LAND_']

for xyStr in xyString:
    # UK expects input data files to be in subdir Data/
    # UK writes results to subdir Results/
    # using the same file name as provide for observation data.
    flin = xyStr+dn+str(year_start)+'_0.csv'
    result = subprocess.run([ex, cfgFile, dn, flin, ' F'])
    print([ex, cfgFile, dn, flin, ' F'])
    if (result.returncode != 0):
        sys.exit(result.returncode)

    for year in years:
        flin = xyStr+dn+str(year)+'.csv'
        # output all data, proc_recruits='F'
        result = subprocess.run([ex, cfgFile, dn, flin, ' F'])
        print([ex, cfgFile, dn, flin, ' F'])
        if (result.returncode != 0):
            sys.exit(result.returncode)

sys.exit(0)
