#subprocess.run([ex, dn, flin])

import subprocess
import socket
import os
import sys

if (len(sys.argv) < 3):
    print ("  Missing command line arguments. Expecting: ")
    print ("  $ EstimateRecruitFields.py StartYear EndYear")
    print()
    quit()
    
year_start = int(sys.argv[1])
year_end = int(sys.argv[2])
print(year_start, year_end)
years = range(year_start, year_end+1)
MC = range(1, 11)

# set configuration file name for UK.exe
cfgFile = 'UK.cfg'
ex = os.path.join('UKsrc', 'UK')

domainName = ['MA', 'GB']

for dn in domainName:
    # UK expects input data files to be in subdir Data/
    # UK writes results to subdir Results/
    # using the same file name as provide for observation data.
    flin = 'X_Y_EBMS_'+dn+str(year_start)+'_0.csv'
    subprocess.run([ex, cfgFile, dn, flin, ' F'])
    print([ex, cfgFile, dn, flin, ' F'])
    for year in years:
        flin = 'X_Y_EBMS_'+dn+str(year)+'.csv'
        # output all data, proc_recruits='F'
        subprocess.run([ex, cfgFile, dn, flin, ' F'])
        print([ex, cfgFile, dn, flin, ' F'])
