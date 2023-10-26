import subprocess
import socket

ex="UKsrc/UK"

DomainName="MA"
flin="Data/RecruitsMALump1NM.csv"
subprocess.call([ex,DomainName,flin])
outdir="Output/ClimMA/"
    
subprocess.call(["mkdir",outdir])
subprocess.call(["mv","SpatialFunctions.csv",outdir])
subprocess.call(["mv","KrigingEstimate.txt",outdir])
subprocess.call(["mv","trendRF.csv",outdir])
subprocess.call(["mv","noiseRF.csv",outdir])
subprocess.call(["mv","totalRF.csv",outdir])
subprocess.call(["mv","NLSFpar.csv",outdir])
subprocess.call(["mv","SIntV.txt",outdir])
subprocess.call(["mv","DIntV.txt",outdir])
subprocess.call(["mv","GammaIntV.txt",outdir])
subprocess.call(["mv","KRIGpar.txt",outdir])
subprocess.call(["mv","beta.txt",outdir])
subprocess.call(["mv","epsilon.txt",outdir])
subprocess.call(["mv","residuals.csv",outdir])
subprocess.call(["mv","OLSresidual.txt",outdir])
subprocess.call(["mv","VSpFn.txt",outdir])
subprocess.call(["mv","Veps.txt",outdir])
subprocess.call(["mv","SpatialTrend.txt",outdir])
subprocess.call(["mv","CovBeta.csv",outdir])
 
DomainName="GB"
flin="Data/RecruitsGBLump1NM.csv"
subprocess.call([ex,DomainName,flin])
outdir="Output/ClimGB/"
    
subprocess.call(["mkdir",outdir])
subprocess.call(["mv","SpatialFunctions.csv",outdir])
subprocess.call(["mv","KrigingEstimate.txt",outdir])
subprocess.call(["mv","trendRF.csv",outdir])
subprocess.call(["mv","noiseRF.csv",outdir])
subprocess.call(["mv","totalRF.csv",outdir])
subprocess.call(["mv","NLSFpar.csv",outdir])
subprocess.call(["mv","SIntV.txt",outdir])
subprocess.call(["mv","DIntV.txt",outdir])
subprocess.call(["mv","GammaIntV.txt",outdir])
subprocess.call(["mv","KRIGpar.txt",outdir])
subprocess.call(["mv","beta.txt",outdir])
subprocess.call(["mv","epsilon.txt",outdir])
subprocess.call(["mv","residuals.csv",outdir])
subprocess.call(["mv","OLSresidual.txt",outdir])
subprocess.call(["mv","VSpFn.txt",outdir])
subprocess.call(["mv","Veps.txt",outdir])
subprocess.call(["mv","SpatialTrend.txt",outdir])
subprocess.call(["mv","CovBeta.csv",outdir])
 
