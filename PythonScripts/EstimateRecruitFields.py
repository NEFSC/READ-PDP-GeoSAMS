#subprocess.run([ex, DomainName, flin])

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
MC = range(1, 101)

# set configuration file name for UK.exe
cfgFile = 'UK.cfg'
ex = os.path.join('UKsrc', 'UK')
DomainName = "MA"
if (os.name == 'nt'):
    move = 'move'
else:
    move = 'mv'


for year in years:
    flin = 'Recruits'+str(year)+'MA.csv'
    subprocess.run([ex, cfgFile, DomainName, flin])
    print([ex, ' ', DomainName, ' ', flin])
    outdir = os.path.join('KrigingEstimates', 'SimMA'+str(year), '')
    
    subprocess.run(["mkdir", outdir], shell = True)
    subprocess.run([move, "SpatialFunctions.csv", outdir], shell = True)
    subprocess.run([move, "KrigingEstimate.txt", outdir], shell = True)
    subprocess.run([move, "trendRF.csv", outdir], shell = True)
    subprocess.run([move, "noiseRF.csv", outdir], shell = True)
    subprocess.run([move, "totalRF.csv", outdir], shell = True)
    subprocess.run([move, "NLSFpar.csv", outdir], shell = True)
    subprocess.run([move, "SIntV.txt", outdir], shell = True)
    subprocess.run([move, "DIntV.txt", outdir], shell = True)
    subprocess.run([move, "GammaIntV.txt", outdir], shell = True)
    subprocess.run([move, "KRIGpar.txt", outdir], shell = True)
    subprocess.run([move, "beta.txt", outdir], shell = True)
    subprocess.run([move, "epsilon.txt", outdir], shell = True)
    subprocess.run([move, "residuals.csv", outdir], shell = True)
    subprocess.run([move, "OLSresidual.txt", outdir], shell = True)
    subprocess.run([move, "VSpFn.txt", outdir], shell = True)
    subprocess.run([move, "Veps.txt", outdir], shell = True)
    subprocess.run([move, "SpatialTrend.txt", outdir], shell = True)
    subprocess.run([move, "KrigSTD.txt", outdir], shell = True)
    subprocess.run([move, "CovBeta.csv", outdir], shell = True)
    #subprocess.run([move, "RandomField.csv", outdir])
    for j in MC:
        fl = "RandomField"+str(j)+".txt"
        subprocess.run([move, fl, outdir], shell = True)
        
DomainName = "GB"

for year in years:
    flin = 'Recruits'+str(year)+'GB.csv'
    subprocess.run([ex, cfgFile, DomainName, flin])
    print([ex, ' ', DomainName, ' ', flin])
    outdir = os.path.join('KrigingEstimates', 'SimGB'+str(year), '')
    
    subprocess.run(["mkdir", outdir], shell = True)
    subprocess.run([move, "SpatialFunctions.csv", outdir], shell = True)
    subprocess.run([move, "KrigingEstimate.txt", outdir], shell = True)
    subprocess.run([move, "trendRF.csv", outdir], shell = True)
    subprocess.run([move, "noiseRF.csv", outdir], shell = True)
    subprocess.run([move, "totalRF.csv", outdir], shell = True)
    subprocess.run([move, "NLSFpar.csv", outdir], shell = True)
    subprocess.run([move, "SIntV.txt", outdir], shell = True)
    subprocess.run([move, "DIntV.txt", outdir], shell = True)
    subprocess.run([move, "GammaIntV.txt", outdir], shell = True)
    subprocess.run([move, "KRIGpar.txt", outdir], shell = True)
    subprocess.run([move, "beta.txt", outdir], shell = True)
    subprocess.run([move, "epsilon.txt", outdir], shell = True)
    subprocess.run([move, "residuals.csv", outdir], shell = True)
    subprocess.run([move, "OLSresidual.txt", outdir], shell = True)
    subprocess.run([move, "VSpFn.txt", outdir], shell = True)
    subprocess.run([move, "Veps.txt", outdir], shell = True)
    subprocess.run([move, "SpatialTrend.txt", outdir], shell = True)
    subprocess.run([move, "KrigSTD.txt", outdir], shell = True)
    subprocess.run([move, "CovBeta.csv", outdir], shell = True)
    for j in MC:
        fl = "RandomField"+str(j)+".txt"
        subprocess.run([move, fl, outdir], shell = True)
    #subprocess.run([move, "RandomField.csv", outdir])
