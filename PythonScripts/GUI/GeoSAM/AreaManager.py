#======================================================================================================
#======================================================================================================
import os

from tkinter import ttk
from tkinter import messagebox

from Widgets import *
from PointInPolygon import *
from Globals import *

#======================================================================================================
##
# This class is used to paint area grouped by
# 
# Area N
#    Comment
#    Number of Nodes         Update Nodes
#    Node 1 .... Node N
#    X data .... X data
#    Y data .... Y data

#======================================================================================================
class AreaManager(ttk.Frame):
    
    def __init__(self, container, parent, maxAreas, maxCorners, elementRow, elementCol, cornerRow, cornerColumn, labelArr,
                 includeYears=False, numYearsMax=0, yearStart=0, yearStop=0):
        super().__init__()

        self.numAreasMax = maxAreas
        self.numCornersMax = maxCorners
        self.numAreas = 1
        self.numCorners = 1

        # row and column where the AreaManger should paint corner information
        self.cornerRow = cornerRow 
        self.areaData = [Corner(self.numCornersMax) for _ in range(self.numAreasMax)]
        self.areaComment = ['' for _ in range(self.numAreasMax)]
        self.areaSubFrame = [AreaMgrSubFrame(self, parent, a, self.numCorners, self.numCornersMax,  
                                         a+elementRow, elementCol, cornerRow, cornerColumn, labelArr,
                                         includeYears, numYearsMax, yearStart, yearStop) for a in range(self.numAreasMax)]

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        
        # --------------------------------------------------------------------------------------------------------
        self.sortAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Sort By Area', style='SAMS.TFrame', width=frameWidth, height=frameHeight)


    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    def NumAreasUpdate(self, numAreas):
        """ Updates the number of areas functions. """
        self.numAreas = numAreas

        for i in range(self.numAreasMax):
            self.areaSubFrame[i].areaFrame.grid_remove()

        # Now update desired funtion definitions
        for i in range(self.numAreasMax):
            self.areaSubFrame[i].areaFrame.grid_remove()
        for i in range(self.numAreas):
            self.areaSubFrame[i].areaFrame.grid()


    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    def ReadAreaCorners(self, fName):
        """Reads an Area file and returns the number of nodes defined"""
        areaIndex = 0
        if os.path.isfile(fName):
            with open(fName, 'r') as f:
                while True:
                    if (areaIndex >= self.numAreasMax):
                        messagebox.showerror("Reading Areas File", f'Max reached {self.numAreasMax}\nStopping at {areaIndex}')
                        break

                    # read longitude values
                    inputStr = f.readline()
                    if not inputStr:
                        f.close()
                        break

                    if inputStr[0] == '#': 
                        # reads in comment. if a mult-line comment only the last line is kept
                        n = len(inputStr.strip()) - 1
                        # remove any trailing commas
                        while inputStr[n] == ',':
                            n -= 1
                        self.areaComment[areaIndex] = inputStr[1:n+1]
                        continue
                    inputArr = [s.strip() for s in inputStr.split(',')]

                    # remove trailing commas
                    inputArr = list(filter(None, inputArr))
                    numCorners = len(inputArr)
                    if (numCorners > self.numCornersMax):
                        messagebox.showerror("Reading Data Sort Areas File", f'Max corners reached. Stoppin at {self.numCornersMax}\n')
                        numCorners = self.numCornersMax
                    
                    for longIndex in range(numCorners):
                        self.areaData[areaIndex].long[longIndex] = float(inputArr[longIndex])
                    longIndex += 1

                    # read latiitude values
                    inputStr = f.readline()
                    if not inputStr:
                        f.close()
                        break
                    if inputStr[0] == '#': continue
                    inputArr = [s.strip() for s in inputStr.split(',')]
                    # remove trailing commas
                    inputArr = list(filter(None, inputArr))
                    numCorners = len(inputArr)
                    if (numCorners > self.numCornersMax):
                        messagebox.showerror("Reading Data Sort Areas File", f'Max corners reached. Stoppin at {self.numCornersMax}\n')
                        numCorners = self.numCornersMax

                    for latIndex in range(numCorners):
                        self.areaData[areaIndex].lat[latIndex] = float(inputArr[latIndex])
                    latIndex += 1

                    if (longIndex != latIndex):
                        messagebox.showerror("Invalid Area Data File", f'Ignoring Corner Data Set @ {areaIndex}')
                    else:
                        self.areaData[areaIndex].numCorners = latIndex
                        areaIndex += 1

                f.close()
            self.numAreas = areaIndex
        else: 
            messagebox.showerror("Data Sort", f'No Data Sort File Has Been Read')
            self.numAreas = 1
        return self.numAreas
    
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    def ReadFields(self, fName):
        """Reads an Area file and returns the number of fields. 
        
        Fields have a Special Area number for the x value with a Mortality setting for the y value."""
        year = []
        definedIndex = 0
        if os.path.isfile(fName):
            with open(fName, 'r') as f:
                while True:
                    if (definedIndex >= self.numAreasMax):
                        messagebox.showerror("Reading Fishing Mort File", f'Max reached {self.numAreasMax}\nStopping at {definedIndex}')
                        break

                    # read defined values
                    inputStr = f.readline()
                    if not inputStr:
                        f.close()
                        break

                    if inputStr[0] == '#':
                        # reads in comment. if a mult-line comment only the last line is kept
                        n = len(inputStr.strip()) - 1
                        # remove any trailing commas
                        while inputStr[n] == ',':
                            n -= 1
                        self.areaComment[definedIndex] = inputStr[1:n+1]
                        continue
                    inputArr = [s.strip() for s in inputStr.split(',')]
                    # remove trailing commas
                    inputArr = list(filter(None, inputArr))

                    year.append(inputArr[0])
                    numFields = int(inputArr[1])
                    # check data size
                    definedLen = len(inputArr)

                    # 2 entries per defined + year + numFieldsVal
                    if definedLen > numFields*2 + 2:
                        messagebox.showerror("Reading Fishing Mort File", f'File row{definedIndex} is ill defined\n')

                    if (numFields > self.numCornersMax):
                        messagebox.showerror("Reading Fishing Mort File", f'Max fields reached. Stoppin at {self.numCornersMax}\n')
                        numFields = self.numCornersMax
                    
                    # get special access area values
                    for i in range(numFields):
                        self.areaData[definedIndex].long[i] = int(inputArr[i+2])
                        self.areaData[definedIndex].lat[i] = float(inputArr[i+numFields+2])

                    self.areaData[definedIndex].numCorners = numFields
                    definedIndex += 1

                f.close()
            self.numAreas = definedIndex
        else: 
            messagebox.showerror("Data Sort", f'No Fields File Has Been Read')
            self.numAreas = 1
        return (self.numAreas, year)

        
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    def UpdateWidgets(self):
        for i in range(self.numAreas):
            self.areaSubFrame[i].commentEntry.myEntry.delete(0,tk.END)
            self.areaSubFrame[i].commentEntry.myEntry.insert(0,self.areaComment[i])
            self.areaSubFrame[i].numCornersEntry.myEntry.delete(0,tk.END)
            self.areaSubFrame[i].numCornersEntry.myEntry.insert(0, str(self.areaData[i].numCorners))
            self.areaSubFrame[i].NumCornersUpdate()
            for j in range(self.areaData[i].numCorners):
                self.areaSubFrame[i].corners[j].longitude.myEntry.delete(0,tk.END)
                self.areaSubFrame[i].corners[j].longitude.myEntry.insert(0, str(self.areaData[i].long[j]))
                self.areaSubFrame[i].corners[j].latitude.myEntry.delete(0,tk.END)
                self.areaSubFrame[i].corners[j].latitude.myEntry.insert(0, str(self.areaData[i].lat[j]))

    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    def SaveSpecialAreaData(self, fname, numAreas):
        with open(fname, 'w') as f:
            for i in range(numAreas):
                # write comment
                f.write('# '+self.areaComment[i]+'\n')

                # write longitude values
                for j in range(int(self.areaSubFrame[i].numCornersEntry.myEntry.get()) - 1):
                    f.write(self.areaSubFrame[i].corners[j].longitude.myEntry.get()+',')
                f.write(self.areaSubFrame[i].corners[j+1].longitude.myEntry.get()+'\n')
            
                # write latitude values
                for j in range(int(self.areaSubFrame[i].numCornersEntry.myEntry.get()) - 1):
                    f.write(self.areaSubFrame[i].corners[j].latitude.myEntry.get()+',')
                f.write(self.areaSubFrame[i].corners[j+1].latitude.myEntry.get()+'\n')
        f.close()


    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    def SaveFishingMortData(self, fname, numAreas, yearEntry):
        with open(fname, 'w') as f:
            for i in range(numAreas):
                # write comment
                f.write('# '+self.areaComment[i]+'\n')

                # save year
                f.write(yearEntry[i].get()+',')

                # save number of entries
                n = int(self.areaSubFrame[i].numCornersEntry.myEntry.get())
                f.write(str(n)+',')

                # write area values
                for j in range(n):
                    f.write(self.areaSubFrame[i].corners[j].longitude.myEntry.get()+',')
            
                # write mort values
                for j in range(n - 1):
                    f.write(self.areaSubFrame[i].corners[j].latitude.myEntry.get()+',')
                f.write(self.areaSubFrame[i].corners[j+1].latitude.myEntry.get()+'\n')
        f.close()

#======================================================================================================
## Defines floating point data for corner defintions
#
# long, lat have become interchangeable with x, y
#======================================================================================================
class Corner:
    def __init__(self, maxCorners):
        self.long = [0.0 for _ in range(maxCorners)]
        self.lat = [0.0 for _ in range(maxCorners)]
        self.numCorners = 0

#======================================================================================================
#======================================================================================================
class AreaMgrSubFrame(tk.Frame):
    def __init__(self, container, parent, areaNum, numCorners, numCornersMax, elementRow, elementCol, cornerRow, cornerCol, labelArr,
                 includeYears=False, numYearsMax=0, yearStart=0, yearStop=0):
        super().__init__()
        self.numCornersMax = numCornersMax
        self.startincCol = 0
        self.yearStart = yearStart
        self.yearStop = yearStop
        self.numYearsMax = numYearsMax

        self.areaFrame = ttk.LabelFrame(parent, text='Area '+str(areaNum+1))
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        if includeYears:
            numYears = yearStop - yearStart + 1
            self.numYrCols = 3
            self.results = [SubFrameElement(self, self.areaFrame, str(yearStart+i), '0', i//self.numYrCols+3,
                                             (i%self.numYrCols)*2, (i%self.numYrCols)*2+1, width=15) for i in range(numYearsMax)]
            # Now hide unused
            for i in range(numYears, numYearsMax):
                self.results[i].myEntry.grid_remove()
                self.results[i].myLabel.grid_remove()
        # --------------------------------------------------------------------------------------------------------
        self.commentEntry = SubFrameElement(self, self.areaFrame, 'Comment',  '',  cornerRow, self.startincCol+cornerCol, self.startincCol+cornerCol+1, width=25)
        # --------------------------------------------------------------------------------------------------------
        self.numCornersEntry = SubFrameElement(self, self.areaFrame, '# corners',  numCorners, 
                                cornerRow+1, self.startincCol+cornerCol, self.startincCol+cornerCol+1,
                                valCmd=numbersCallback, enterCmd=self.EnterKeyClicked)
        # --------------------------------------------------------------------------------------------------------
        areaRow = 2
        self.corners = [SubFrameXY(self, self.areaFrame, str(i+1), areaRow, i+self.startincCol+cornerCol, labelArr)  for i in range(numCornersMax)]
        # now hide unwanted corners
        for i in range(numCorners, numCornersMax):
            self.corners[i].cornerFrame.grid_remove()
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.areaFrame.grid(row=elementRow, column=elementCol, columnspan=10, sticky='ew')

    # ----------------------------------------------------------------------------
    # ----------------------------------------------------------------------------
    def EnterKeyClicked(self, event):
        self.NumCornersUpdate()

    # ----------------------------------------------------------------------------
    # ----------------------------------------------------------------------------
    def NumCornersUpdate(self):
        for i in range(self.numCornersMax):
            self.corners[i].cornerFrame.grid_remove()

        n = int(self.numCornersEntry.myEntry.get())
        if n > self.numCornersMax:
            messagebox.showerror("Number of Corners", f'Max is {self.numCornersMax}\nSetting to max')
            n = self.numCornersMax
            self.numCornersEntry.myEntry.delete(0,tk.END)
            self.numCornersEntry.myEntry.insert(0, str(n))
        self.numCorners = n
        # Now update desired funtion definitions
        for i in range(self.numCornersMax):
            self.corners[i].cornerFrame.grid_remove()
        for i in range(self.numCorners):
            self.corners[i].cornerFrame.grid()

    # ----------------------------------------------------------------------------------------------
    ## This method is used to add results when the original maximum number of years is exceeded
    # ----------------------------------------------------------------------------------------------
    def AppendResults(self, addYears):
        for i in range(addYears):
            # The label will get repainted as the data is displayed, so we use '0' here
            self.results.append(SubFrameElement(self, self.areaFrame, '0', '0', self.numYearsMax+i, 0, 1, width=15))
            self.results[i+self.numYearsMax].myLabel.cget('text')
        self.numYearsMax += addYears
        # Now show all
        for i in range(self.numYearsMax):
            elementRow = i // self.numYrCols + 3
            labelCol = (i % self.numYrCols) * 2
            entryCol = (i % self.numYrCols) * 2 + 1
            self.results[i].myEntry.grid(row=elementRow, column=entryCol, sticky='n', padx=5, pady=5)
            self.results[i].myLabel.grid(row=elementRow, column=labelCol, sticky='n', padx=5, pady=5)
            self.results[i].myEntry.grid()
            self.results[i].myLabel.grid()
