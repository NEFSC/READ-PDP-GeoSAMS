#subprocess.call([ex,DomainName,flin,climfl])

import subprocess
import socket
import glob
import shutil


ex="ScallopSrc/ScallopPopDy"
DomainName="MA"
StartYr=1990
StopYr=2023

subprocess.call([ex,DomainName,str(StartYr),str(StopYr),"0"])

 
outdir="Output/ScallopMABMS"
subprocess.call(["mkdir",outdir])

years=range(StartYr,StopYr+1)
for year in years:
	subprocess.call(["mv","IndvCaught"+str(year)+".txt",outdir])
	subprocess.call(["mv","F"+str(year)+".txt",outdir])
	subprocess.call(["mv","FishingMortality"+str(year)+".csv",outdir])
	subprocess.call(["mv","NaturalMortality"+str(year)+".csv",outdir])
	subprocess.call(["mv","Fslct"+str(year)+".csv",outdir])
	subprocess.call(["mv","State"+str(year)+".csv",outdir])
	subprocess.call(["mv","Cnts"+str(year)+".csv",outdir])
	subprocess.call(["mv","ExplBMS"+str(year)+".txt",outdir])
	subprocess.call(["mv","BMS"+str(year)+".txt",outdir])
	subprocess.call(["mv","Abundance"+str(year)+".txt",outdir])
	subprocess.call(["mv","Dollars"+str(year)+".txt",outdir])
	subprocess.call(["cp","ScallopSrc/ScallopPopDy.f90",outdir])


subprocess.call(["mv","CASACompTable.txt",outdir])
subprocess.call(["mv","CASACompClosedTable.txt",outdir])
subprocess.call(["mv","CASACompOpenTable.txt",outdir])
subprocess.call(["mv","WeightShellHeight.csv",outdir])

Regions=range(1,13)
for n in Regions:
	subprocess.call(["mv","ManagementRegion"+str(n)+".csv",outdir])

