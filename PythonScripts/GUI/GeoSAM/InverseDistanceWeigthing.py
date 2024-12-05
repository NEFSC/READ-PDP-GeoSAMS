# https://www.google.com/search?q=IDW+in+python

import numpy as np
import os
import sys
from Globals import *

class InverseDistanceWeighting():
    def __init__(self, inputFname, gridFname):
        self.dataFname = os.path.join(dataDir, inputFname+'_IDW.csv')
        self.predFname = os.path.join(dataDir, inputFname+'_PRED.csv')
        outFile = 'Lat_Lon_Grid_'+inputFname[4:]
        self.interFname = os.path.join(dataDir,outFile+'_IDW.csv')
        self.gridFname = os.path.join(gridDir,gridFname)
        self.finalFname = os.path.join(resultsDir,outFile+'_IDW.csv')

    def idw(self, x, y, z, xi, yi, p=2):
        """
        Inverse Distance Weighting interpolation
        x, y, z: known data points
        xi, yi: interpolation points
        p: power parameter (default: 2)
        """

        zi = np.zeros_like(xi)

        for i in range(len(xi)):
            dist = np.sqrt((x - xi[i])**2 + (y - yi[i])**2)
            weights = 1 / (dist ** p + 1e-10)  # Avoid division by zero
            zi[i] = np.sum(z * weights) / np.sum(weights)

        return zi

    def GetNumberOfRecords(self, filename, hasHeader=True):
        n = 0
        with open(filename, 'r') as f:
            # read header line
            if hasHeader: inputStr = f.readline()

            while True:
                inputStr = f.readline()
                if not inputStr:
                    f.close()
                    break
                n += 1
        return n

    def Interpolate(self):
        #---------------------------------------------------
        # Read in observation data
        #---------------------------------------------------
        # get number of records
        numObs = self.GetNumberOfRecords(self.dataFname)

        x = np.zeros(numObs)
        y = np.zeros(numObs)
        z = np.zeros(numObs)
        with open(self.dataFname, 'r') as f:
            # read header line
            inputStr = f.readline()

            for n in range(numObs):
                inputStr = f.readline()
                inputArr = [s.strip() for s in inputStr.split(',')]
                x[n] = float(inputArr[1])
                y[n] = float(inputArr[2])
                z[n] = float(inputArr[10])

        #---------------------------------------------------
        # Read in grid data
        #---------------------------------------------------
        numGrid = self.GetNumberOfRecords(self.gridFname, hasHeader=False)

        gridX = np.zeros(numGrid)
        gridY = np.zeros(numGrid)
        gridLat = np.zeros(numGrid)
        gridLon = np.zeros(numGrid)

        with open(self.gridFname, 'r') as f:
            for n in range(numGrid):
                inputStr = f.readline()
                inputArr = [s.strip() for s in inputStr.split(',')]
                gridX[n] = float(inputArr[0])
                gridY[n] = float(inputArr[1])
                gridLat[n] = float(inputArr[3])
                gridLon[n] = float(inputArr[4])
            f.close()

        zi = self.idw(x, y, z, gridX, gridY, p=2)

        with open(self.interFname, 'w') as f:
            for n in range(numGrid):
                f.write('{},{},{}\n'.format(gridLat[n], gridLon[n], zi[n]))
            f.close()

        # interpolation complete add results to prediction values
        residFname = open(self.interFname, 'r')
        predFname = open(self.predFname, 'r')
        inputStr = predFname.readline() # read header
        resultFname = open(self.finalFname, 'w')
        # print('READING: ', self.interFname, self.predFname, self.predictIndex)
        # print('WRITING: ', self.finalFname)
        for n in range(numGrid):
            inputStr = residFname.readline()
            inputArr = [s.strip() for s in inputStr.split(',')]
            residual = float(inputArr[2])

            inputStr = predFname.readline()
            inputArr = [s.strip() for s in inputStr.split(',')]
            predict = float(inputArr[10])

            result = residual + predict
            if result < 0.0: result=0.0
            resultFname.write('{},{},{}\n'.format(gridLat[n], gridLon[n], result))
        
        resultFname.close()
        predFname.close()
        residFname.close()
