import sys
import os
import pandas as pd
sys.path.append("PythonScripts/GUI/GeoSAM/PyshpMaster")
import shapefile

from PointInPolygon import *

class GeoShape:
    def __init__(self):
        self.X=[]
        self.Y=[]
        self.lat=[]
        self.lon=[]
        self.Region=''

class Column:
    def __init__(self):
        self.name = ''
        self.inBox = []


inputFile = sys.argv[1]
# determine region of interest from last two chars of file name
l = len(inputFile)
domain = domain = inputFile[l-2:l] 

dataFile = os.path.join('Data', inputFile+'.csv')
outfile = os.path.join('Data', inputFile+'_BUFFER.csv')

M = pd.read_csv(dataFile)

##############################################################################
if domain == 'GB':
    fileName = os.environ['GBShapeBufferFile']
    subDir = 'GB_Buffer'
else:
    fileName = os.environ['MAShapeBufferFile']
    subDir = 'MAB_Buffer'

shapeFile = os.path.join('Shapefiles', subDir, fileName)
sf = shapefile.Reader(shapeFile)
shapes = sf.shapes()
shapeLen = len(sf)

#-------------------------------------------------------#
shape = [ GeoShape() for _ in range(shapeLen)]
for n in range(shapeLen):
    record = sf.record(n)
    as_dict = record.as_dict()
    shape[n].Region = record['Region']
    pointLen = len(shapes[n].points)
    shape[n].X = [0.0 for _ in range(pointLen)]
    shape[n].Y = [0.0 for _ in range(pointLen)]
    for m in range(len(shapes[n].points)):
        shape[n].X[m] = shapes[n].points[m][0]
        shape[n].Y[m] = shapes[n].points[m][1]

columns = [Column() for _ in range(shapeLen)]
for i in range(shapeLen):
    if shape[i].Region == 'VIR': 
        shapeLen -= 1
    else:
        columns[i].name = 'BUFFER_' + shape[i].Region


X_t = M['UTM_X']
Y_t = M['UTM_Y']

rows = len(X_t)

for r in range(rows):
    for rgn in range(shapeLen):
        nodes = len(shape[rgn].X)
        if PointInPolygon(shape[rgn].X, shape[rgn].Y, X_t[r], Y_t[r], nodes):
            columns[rgn].inBox.append(1)
        else:
            columns[rgn].inBox.append(0)

for n in range(shapeLen):
    M[columns[n].name] = columns[n].inBox

M.to_csv(outfile,sep=',',na_rep='NA', index=False)