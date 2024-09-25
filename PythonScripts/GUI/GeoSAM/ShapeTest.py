import sys
sys.path.append("PythonScripts/GUI/GeoSAM/PyshpMaster")
import shapefile
import utm

class GeoShape:
    def __init__(self):
        self.X=[]
        self.Y=[]
        self.lat=[]
        self.lon=[]
        self.SAMS=''
        self.NewSAMS=''
        self.areaKm2 = 0.0

sf = shapefile.Reader("Shapefiles/MAB_Estimation_Areas_2019_UTM18_PDT.shp")

shapes = sf.shapes()
shapeLen = len(sf)
shapeMA = [ GeoShape() for _ in range(shapeLen)]
for n in range(shapeLen):
    record = sf.record(n)
    as_dict = record.as_dict()
    shapeMA[n].SAMS = record['SAMS']
    shapeMA[n].NewSAMS = record['NewSAMS']
    shapeMA[n].areaKm2 = record['AreaKm2']
    pointLen = len(shapes[n].points)
    shapeMA[n].X = [0.0 for _ in range(pointLen)]
    shapeMA[n].Y = [0.0 for _ in range(pointLen)]
    shapeMA[n].lat = [0.0 for _ in range(pointLen)]
    shapeMA[n].lon = [0.0 for _ in range(pointLen)]
    for m in range(len(shapes[n].points)):
        shapeMA[n].X[m] = shapes[n].points[m][0]
        shapeMA[n].Y[m] = shapes[n].points[m][1]
        (shapeMA[n].lat[m], shapeMA[n].lon[m]) = utm.to_latlon(shapeMA[n].X[m], shapeMA[n].Y[m], 18, 'T')
    print( shapeMA[n].SAMS, shapeMA[n].NewSAMS, shapeMA[n].areaKm2)
    print(n, len(shapes[n].points))
    print(n, shapeMA[n].X[5], shapeMA[n].Y[5])
    print(n, shapeMA[n].lat[5], shapeMA[n].lon[5])
    print()

sf = shapefile.Reader("ShapeFiles/GB_Estimation_Areas_2024_UTM19_PDT.shp")
shapes = sf.shapes()
shapeLen = len(sf)
shapeGB = [ GeoShape() for _ in range(shapeLen)]
for n in range(shapeLen):
    print( n, '"',shapeGB[n].NewSAMS,'"')

for n in range(shapeLen):
    record = sf.record(n)
    as_dict = record.as_dict()
    shapeGB[n].SAMS = record['SAMS']
    shapeGB[n].NewSAMS = record['NewSAMS']
    pointLen = len(shapes[n].points)
    shapeGB[n].X = [0.0 for _ in range(pointLen)]
    shapeGB[n].Y = [0.0 for _ in range(pointLen)]
    shapeGB[n].lat = [0.0 for _ in range(pointLen)]
    shapeGB[n].lon = [0.0 for _ in range(pointLen)]
    for m in range(len(shapes[n].points)): 
        shapeGB[n].X[m] = shapes[n].points[m][0]
        shapeGB[n].Y[m] = shapes[n].points[m][1]
        (shapeGB[n].lat[m], shapeGB[n].lon[m]) = utm.to_latlon(shapeGB[n].X[m], shapeGB[n].Y[m], 19, 'T')
    print('GB', shapeGB[n].NewSAMS)
    print('GB', n, len(shapes[n].points))
    print('GB', n, shapeGB[n].X[0], shapeGB[n].Y[0])
    print('GB', n, shapeGB[n].lat[0], shapeGB[n].lon[0])
    print()

for n in range(shapeLen):
    print( n, shapeGB[n].NewSAMS)