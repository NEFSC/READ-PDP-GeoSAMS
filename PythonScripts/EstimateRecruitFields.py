#subprocess.run([ex, DomainName, flin, climfl])

import subprocess
import socket
import os

years = range(1979, 2020)
MC = range(1, 101)


ex = os.path.join('UKsrc', 'UK')
climfl = "KrigClimMA.txt"
DomainName = "MA"
if (os.name == 'nt'):
    move = 'move'
else:
    move = 'mv'


for year in years:

    #flin = "Data/Recruits"+str(year)+"MA.csv"
    flin = os.path.join('Data', 'Recruits'+str(year)+'MA.csv')
    subprocess.run([ex, DomainName, flin])
    print([ex, ' ', DomainName, ' ', flin])
 
    #outdir = "KrigingEstimates/SimMA"+str(year)+"/"
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
        
    #subprocess.run([move, "ClimEstObs.txt", outdir])


climfl = "KrigClimGB.txt"
DomainName = "GB"

for year in years:

    #flin = "Data/Recruits"+str(year)+"GB.csv"
    flin = os.path.join('Data', 'Recruits'+str(year)+'GB.csv')
    subprocess.run([ex, DomainName, flin], shell = True)
    print([ex, ' ', DomainName, ' ', flin])
    
    #outdir = "KrigingEstimates/SimGB"+str(year)+"/"
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
    #subprocess.run([move, "ClimEstObs.txt", outdir])
