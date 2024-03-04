#subprocess.run([ex, dn, flin])

import subprocess
import socket
import os
import sys
import csv

# Results/X_Y_EBMS_MA2005_0.csv

from collections import defaultdict

if (len(sys.argv) < 3):
    print ("  Missing command line arguments. Expecting: ")
    print ("  $ EstimateRecruitFields.py StartYear EndYear")
    print()
    quit()
    
year_start = int(sys.argv[1])
year_end = int(sys.argv[2])
# number of colums in csv file, starting at 0
# lat, lon, initial data
ncols = year_end - year_start + 3
print(year_start, year_end)
years = range(year_start, year_end+1)

domainName = ['MA', 'GB']
xyString = ['Results/X_Y_EBMS_', 'Results/X_Y_LAND_']

for dn in domainName:
    for xyStr in xyString:
        col = []
        col = [defaultdict(list) for i in range(ncols-1)]
        k = 0
        print( 'Concat ', dn)
        with open(xyStr+dn+str(year_start)+'_0.csv') as f:
            reader = csv.reader(f)
            for row in reader:
                for (i,v) in enumerate(row):
                    col[k][i].append(v)
            f.close()
            os.remove(f.name)

        # append remaining years as additional columns to first data set
        for year in years:
            k += 1
            with open(xyStr+dn+str(year)+'.csv') as f:
                reader = csv.reader(f)
                for row in reader:
                    for (i,v) in enumerate(row):
                        col[k][i].append(v)
                f.close()
                os.remove(f.name)

            for i in range (len(col[0][0])):    
                col[0][k+2].append(col[k][2][i])

        # brute force write out results
        file1 = open(xyStr+dn+'_'+str(year_start)+'_'+str(year_end)+'.csv', 'w')
        for r in range(len(col[0][0])):
            for c in range(ncols):
                file1.write(col[0][c][r])
                file1.write(',')
            file1.write(col[0][ncols][r])
            file1.write('\n')

        file1.close()
