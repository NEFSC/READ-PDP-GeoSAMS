import subprocess
import socket
import os

ex="UKsrc/UK"
ex = os.path.join('UKsrc', 'UK')
if (os.name == 'nt'):
    move = 'move'
else:
    move = 'mv'

DomainName="MA"
#flin="Data/RecruitsMALump1NM.csv"
flin = os.path.join('Data', 'RecruitsMALump1NM.csv')
subprocess.run([ex, DomainName, flin], shell=True)
#outdir="KrigingEstimates/SimMAClim/"
outdir = os.path.join('KrigingEstimates', 'SimMAClim', '')

subprocess.run(["mkdir", outdir], shell=True)
subprocess.run([move, "SpatialFunctions.csv", outdir], shell=True)
subprocess.run([move, "KrigingEstimate.txt", outdir], shell=True)
subprocess.run([move, "trendRF.csv", outdir], shell=True)
subprocess.run([move, "noiseRF.csv", outdir], shell=True)
subprocess.run([move, "totalRF.csv", outdir], shell=True)
subprocess.run([move, "NLSFpar.csv", outdir], shell=True)
subprocess.run([move, "SIntV.txt", outdir], shell=True)
subprocess.run([move, "DIntV.txt", outdir], shell=True)
subprocess.run([move, "GammaIntV.txt", outdir], shell=True)
subprocess.run([move, "KRIGpar.txt", outdir], shell=True)
subprocess.run([move, "beta.txt", outdir], shell=True)
subprocess.run([move, "epsilon.txt", outdir], shell=True)
subprocess.run([move, "residuals.csv", outdir], shell=True)
subprocess.run([move, "OLSresidual.txt", outdir], shell=True)
subprocess.run([move, "VSpFn.txt", outdir], shell=True)
subprocess.run([move, "Veps.txt", outdir], shell=True)
subprocess.run([move, "SpatialTrend.txt", outdir], shell=True)
subprocess.run([move, "CovBeta.csv", outdir], shell=True)
 
DomainName="GB"
#flin="Data/RecruitsGBLump1NM.csv"
flin = os.path.join('Data', 'RecruitsGBLump1NM.csv')

subprocess.run([ex, DomainName, flin], shell=True)
#outdir="KrigingEstimates/SimGBClim/"
outdir = os.path.join('KrigingEstimates', 'SimGBClim', '')

subprocess.run(["mkdir", outdir], shell=True)
subprocess.run([move, "SpatialFunctions.csv", outdir], shell=True)
subprocess.run([move, "KrigingEstimate.txt", outdir], shell=True)
subprocess.run([move, "trendRF.csv", outdir], shell=True)
subprocess.run([move, "noiseRF.csv", outdir], shell=True)
subprocess.run([move, "totalRF.csv", outdir], shell=True)
subprocess.run([move, "NLSFpar.csv", outdir], shell=True)
subprocess.run([move, "SIntV.txt", outdir], shell=True)
subprocess.run([move, "DIntV.txt", outdir], shell=True)
subprocess.run([move, "GammaIntV.txt", outdir], shell=True)
subprocess.run([move, "KRIGpar.txt", outdir], shell=True)
subprocess.run([move, "beta.txt", outdir], shell=True)
subprocess.run([move, "epsilon.txt", outdir], shell=True)
subprocess.run([move, "residuals.csv", outdir], shell=True)
subprocess.run([move, "OLSresidual.txt", outdir], shell=True)
subprocess.run([move, "VSpFn.txt", outdir], shell=True)
subprocess.run([move, "Veps.txt", outdir], shell=True)
subprocess.run([move, "SpatialTrend.txt", outdir], shell=True)
subprocess.run([move, "CovBeta.csv", outdir], shell=True)
