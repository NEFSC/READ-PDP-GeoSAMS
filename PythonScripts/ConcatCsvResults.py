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
print(year_start, year_end)
years = range(year_start, year_end+1)
domainName = ['MA', 'GB']
for dn in domainName:
    col = []
    col = [defaultdict(list) for i in range(5)]
    k = 0
    print( 'Concat ', dn)
    with open('Results/X_Y_EBMS_'+dn+str(year_start)+'_0.csv') as f:
        reader = csv.reader(f)
        for row in reader:
            for (i,v) in enumerate(row):
                col[k][i].append(v)

    # append remaining years as additional columns to first data set
    for year in years:
        k += 1
        with open('Results/X_Y_EBMS_'+dn+str(year)+'.csv') as f:
            reader = csv.reader(f)
            for row in reader:
                for (i,v) in enumerate(row):
                    col[k][i].append(v)

        for i in range (len(col[0][0])):    
            col[0][k+2].append(col[k][2][i])

        # with open('Results/X_Y_EBMS_MA2006.csv') as f:
        #             col[2][i].append(v)

        # for i in range (len(col[0][0])):    
        #     col[0][4].append(col[2][2][i])

        # with open('Results/X_Y_EBMS_MA2007.csv') as f:
        #             col[3][i].append(v)

        # for i in range (len(col[0][0])):    
        #     col[0][5].append(col[3][2][i])

        # with open('Results/X_Y_EBMS_MA2008.csv') as f:
        #             col[4][i].append(v)

        # for i in range (len(col[0][0])):    
        #     col[0][6].append(col[4][2][i])

    # brute force write out results
    file1 = open('Results/X_Y_EBMS_'+dn+'.csv', 'w')
    for r in range(len(col[0][0])):
        for c in range(6):
            file1.write(col[0][c][r])
            file1.write(',')
        file1.write(col[0][6][r])
        file1.write('\n')

    file1.close()
