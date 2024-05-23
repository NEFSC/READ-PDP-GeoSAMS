from PointInPolygon import *

polyX = [0.999999999, 2.0000, 1.999999998, 0.999999998]
polyY = [0.999999999, 1.0000, 2.000000001, 2.0000]
x = 1.0
y = 1.5

print(PointInPolygon(polyX, polyY, x, y, 4))

polyX = [-74.833333,-74,-74,-74.833333]
polyY = [38.166667,38.166667,37.25,37.25]
x = -74.829700
y = 37.262800

print(PointInPolygon(polyX, polyY, x, y, 4))