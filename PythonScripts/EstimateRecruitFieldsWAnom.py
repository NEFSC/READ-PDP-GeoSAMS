#subprocess.call([ex,DomainName,flin,climfl])

import subprocess
import socket

years=range(1979,2019)

MC=range(1,101)


ex="UKsrc/UK"
climfl="Output/ClimMA/KrigingEstimate.txt"
DomainName="MA"

for year in years:

    flin="Data/Recruits"+str(year)+"MA.csv"
    subprocess.call([ex,DomainName,flin,climfl])
 
    outdir="Output/SimMAClim"+str(year)+"/"
    
    
    subprocess.call(["mkdir",outdir])
    subprocess.call(["mv","SpatialFunctions.csv",outdir])
    subprocess.call(["mv","KrigingEstimate.txt",outdir])
    subprocess.call(["mv","NLSFpar.csv",outdir])
    subprocess.call(["mv","SIntV.txt",outdir])
    subprocess.call(["mv","DIntV.txt",outdir])
    subprocess.call(["mv","GammaIntV.txt",outdir])
    subprocess.call(["mv","KRIGpar.txt",outdir])
    subprocess.call(["mv","beta.txt",outdir])
    subprocess.call(["mv","epsilon.txt",outdir])
    subprocess.call(["mv","residuals.csv",outdir])
    subprocess.call(["mv","OLSresidual.txt",outdir])
    subprocess.call(["mv","SpatialTrend.txt",outdir])
    subprocess.call(["mv","KrigSTD.txt",outdir])
    subprocess.call(["mv","CovBeta.csv",outdir])
    #subprocess.call(["mv","RandomField.csv",outdir])
    for j in MC:
        fl="RandomField"+str(j)+".txt"
        subprocess.call(["mv",fl,outdir])
        
   
ex="UKsrc/UK"
climfl="Output/ClimGB/KrigingEstimate.txt"
DomainName="GB"

for year in years:

    flin="Data/Recruits"+str(year)+"GB.csv"
    subprocess.call([ex,DomainName,flin,climfl])
    
    outdir="Output/SimGBClim"+str(year)+"nc/"
    
    
    subprocess.call(["mkdir",outdir])
    subprocess.call(["mv","SpatialFunctions.csv",outdir])
    subprocess.call(["mv","KrigingEstimate.txt",outdir])
    subprocess.call(["mv","NLSFpar.csv",outdir])
    subprocess.call(["mv","SIntV.txt",outdir])
    subprocess.call(["mv","DIntV.txt",outdir])
    subprocess.call(["mv","GammaIntV.txt",outdir])
    subprocess.call(["mv","KRIGpar.txt",outdir])
    subprocess.call(["mv","beta.txt",outdir])
    subprocess.call(["mv","epsilon.txt",outdir])
    subprocess.call(["mv","residuals.csv",outdir])
    subprocess.call(["mv","OLSresidual.txt",outdir])
    subprocess.call(["mv","SpatialTrend.txt",outdir])
    subprocess.call(["mv","KrigSTD.txt",outdir])
    subprocess.call(["mv","CovBeta.csv",outdir])
    #subprocess.call(["mv","RandomField.csv",outdir])
    for j in MC:
        fl="RandomField"+str(j)+".txt"
        subprocess.call(["mv",fl,outdir])
        
    
