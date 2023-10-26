


import subprocess
import socket

years=range(1979,2020)
MC=range(1,101)


ex="UKsrc/UK"
climfl="KrigClimMA.txt"
DomainName="MA"

for year in years:

    flin="Data/Recruits"+str(year)+"MA.csv"
    subprocess.call([ex,DomainName,flin])
    print([ex,' ',DomainName,' ',flin])
 
    outdir="Output/SimMAlin"+str(year)+"/"
    
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
    subprocess.call(["mv","KrigSTD.txt",outdir])
    subprocess.call(["mv","CovBeta.csv",outdir])
    #subprocess.call(["mv","RandomField.csv",outdir])
    for j in MC:
        fl="RandomField"+str(j)+".txt"
        subprocess.call(["mv",fl,outdir])
        
    #subprocess.call(["mv","ClimEstObs.txt",outdir])


ex="UKsrc/UK"
climfl="KrigClimGB.txt"
DomainName="GB"

for year in years:

    flin="Data/Recruits"+str(year)+"GB.csv"
    subprocess.call([ex,DomainName,flin])
    print([ex,' ',DomainName,' ',flin])
    
    outdir="Output/SimGBlin"+str(year)+"/"
    
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
    subprocess.call(["mv","KrigSTD.txt",outdir])
    subprocess.call(["mv","CovBeta.csv",outdir])
    for j in MC:
        fl="RandomField"+str(j)+".txt"
        subprocess.call(["mv",fl,outdir])
    #subprocess.call(["mv","RandomField.csv",outdir])
    #subprocess.call(["mv","ClimEstObs.txt",outdir])
