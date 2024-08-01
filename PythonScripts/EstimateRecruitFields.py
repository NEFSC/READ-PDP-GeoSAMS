#subprocess.run([ex, dn, flin])

import subprocess
import socket
import os
import sys

def MoveFiles(move, outdir):
    subprocess.run(["mkdir", outdir], shell = True)
    subprocess.run([move, "beta.txt", outdir], shell = True)
    subprocess.run([move, "CBeta0.csv", outdir], shell = True)
    subprocess.run([move, "CovBeta.csv", outdir], shell = True)
    subprocess.run([move, "data.txt", outdir], shell = True)
    subprocess.run([move, "DIntV.txt", outdir], shell = True)
    subprocess.run([move, "epsilon.txt", outdir], shell = True)
    subprocess.run([move, "Fglsa0.csv", outdir], shell = True)
    subprocess.run([move, "GammaIntV.txt", outdir], shell = True)
    subprocess.run([move, "KrigingEstimate.txt", outdir], shell = True)
    subprocess.run([move, "KRIGpar.txt", outdir], shell = True)
    subprocess.run([move, "KrigSTD.txt", outdir], shell = True)
    subprocess.run([move, "NLSF_Class.csv", outdir], shell = True)
    subprocess.run([move, "obs.txt", outdir], shell = True)
    subprocess.run([move, "OLSresidual.txt", outdir], shell = True)
    subprocess.run([move, "residuals.csv", outdir], shell = True)
    subprocess.run([move, "SIntV.txt", outdir], shell = True)
    subprocess.run([move, "SpatialFunctions.csv", outdir], shell = True)
    subprocess.run([move, "SpatialTrend.txt", outdir], shell = True)
    for j in MC:
        fl = "RandomField"+str(j)+".txt"
        subprocess.run([move, fl, outdir], shell = True)


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
if (os.name == 'nt'):
    move = 'move'
else:
    move = 'mv'

domainName = ['MA', 'GB']
for dn in domainName:
    for year in years:
        flin = 'Recruits'+str(year)+dn+'.csv'
        # output all data, save_data='T'
        subprocess.run([ex, cfgFile, dn, flin, ' T'])
        print([ex, ' ', dn, ' ', flin, ' T'])
        outdir = os.path.join('KrigingEstimates', 'Sim'+dn+str(year), '')
        MoveFiles(move, outdir)
