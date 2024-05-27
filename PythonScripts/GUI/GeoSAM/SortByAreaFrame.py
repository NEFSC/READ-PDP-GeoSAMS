#======================================================================================================
## @page page5 Sort By Area Frame
# Assists the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest.
#
# @section p6p1 Number of Areas
#
# @section p6p2 Output Parameters
#
# @section p6p3 Load and Save Data Sort Files
#
# @section p6p4 Run Sort
#
# @section p6p5 Area SubFrames
#
# @subsection p6p5p1 Years Simulated
#
# @subsection p6p5p2 Accumulated Values for Given Output Parameter
#
# @subsection p6p5p3 Corners
#
# @subsubsection p6p5p3p1 Number of Corners
#
# @subsubsection p6p5p3p2 Corner Identifier by Longitude and Latitude
# 
#======================================================================================================
import os

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from PointInPolygon import *

#===============================================================================================================
##
# This class is used to assist the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest
#===============================================================================================================
class SortByArea(ttk.Frame):
    def __init__(self, container, friend, maxAreas, maxCorners, maxYears, paramStr):
        super().__init__()
        
        self.root = os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'DataSort')
        self.friend = friend

        self.numAreasMax = maxAreas
        self.numCornersMax = maxCorners
        self.maxYears = maxYears
        self.numAreas = 1
        self.numCorners = 1
        self.paramStr = paramStr

        self.yearStart = int(self.friend.startYr.myEntry.get())
        self.yearStop = int(self.friend.stopYr.myEntry.get())
        self.numYears = self.yearStop - self.yearStart + 1
        self.areaData = [Corner(self.numCornersMax) for _ in range(self.numAreasMax)]

        self.style = ttk.Style()
        self.style.configure('SortByArea.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('SortByArea.TFrame.Label', font=('courier', 8, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        
        # --------------------------------------------------------------------------------------------------------
        self.sortAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Sort By Area', style='SortByArea.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numAreasLabel = ttk.Label(self.sortAreaFrame, text='# of Areas')
        self.numAreasLabel.grid(row=0, column=0, sticky='w')
        self.numAreasEntry=ttk.Entry(self.sortAreaFrame,validatecommand=numbersCallback, width=5)
        self.numAreasEntry.insert(0, str(self.numAreas))
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numAreasEntry.grid(row=1, column=0, sticky='w')

        self.outputParmLabel = ttk.Label(self.sortAreaFrame, text='Output Parameters')
        self.outputParmLabel.grid(row=0, column=0, sticky='ns')
        self.comboParameter = ttk.Combobox(self.sortAreaFrame, values=paramStr)
        self.comboParameter.grid(row=1, column=0, sticky='ns')

        self.numAreasButton = ttk.Button(self.sortAreaFrame, text='Update # Areas', command=self.NumAreasUpdate)
        self.numAreasButton.grid(row=2, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.style.configure("SortByArea.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("Frame5a.TLabel", padding=6, relief='raised', background="#0FF")
        
        self.openDataSortButton = ttk.Button(self.sortAreaFrame, text='Load Data Sort File', style="SortByArea.TLabel", command=self.GetDataSortFile)
        self.openDataSortButton.grid(row=0, column=1, sticky='w')

        self.saveDataSortButton = ttk.Button(self.sortAreaFrame, text='Save Data Sort File', style="Frame5a.TLabel", command=self.SaveDataSortFile)
        self.saveDataSortButton.grid(row=1, column=1, sticky='w')

        self.saveDataSortButton = ttk.Button(self.sortAreaFrame, text='Run Sort', command=self.RunSort)
        self.saveDataSortButton.grid(row=2, column=1, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.areas = [SubFrameArea(self, self.sortAreaFrame, a, self.numCorners, self.numCornersMax, self.maxYears, 
                                         self.yearStart, self.yearStop, 3+a, 0) for a in range(self.numAreasMax)]

        # now hide
        for a in range(self.numAreas, self.numAreasMax):
            self.areas[a].areaFrame.grid_remove()

        self.sortAreaFrame.grid(row=4, column=0, columnspan=10)
        self.sortAreaFrame.grid_columnconfigure(0,weight=2)
        self.sortAreaFrame.grid_columnconfigure(1,weight=1)
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')

        self.UpdateWidgets()

        self.bind("<Visibility>", self.on_visibility)

    def on_visibility(self, event):
        self.paramStr = []
        parmVal = self.friend.GetSelectedOutputs()
        # ordering of string may not matter. The user ultimately selects the desired vale
        if parmVal & 1: self.paramStr.append('LPUE_')
        if parmVal & 2: self.paramStr.append('EBMS_')
        if parmVal & 4: self.paramStr.append('BMMT_')
        if parmVal & 8: self.paramStr.append('ABUN_')
        if parmVal & 16: self.paramStr.append('LNDW_')
        if parmVal & 32: self.paramStr.append('LAND_')
        if parmVal & 64: self.paramStr.append('FEFF_')
        if parmVal & 128: self.paramStr.append('FMOR_')
        if parmVal & 256: self.paramStr.append('RECR_')
        self.comboParameter.configure(values=self.paramStr)
        self.comboParameter.current(0)

        self.yearStart = int(self.friend.startYr.myEntry.get())
        self.yearStop = int(self.friend.stopYr.myEntry.get())
        self.domainName = self.friend.domainNameCombo.get()
        self.numYears = self.yearStop - self.yearStart + 1
        if self.numYears > self.maxYears:
            self.yearStop = self.maxYears + self.yearStart - 1
            self.numYears = self.maxYears
            messagebox.showerror("Too many years", f'Setting Stop Year to {self.yearStop}')
            self.friend.stopYr.myEntry.delete(0,4)
            self.friend.stopYr.myEntry.insert(0, self.yearStop)
        for i in range(self.numAreas):
            for j in range(self.numYears):
                self.areas[i].results[j].myEntry.grid()
                self.areas[i].results[j].myLabel.grid()
                self.areas[i].results[j].myLabel.config(text = str(self.yearStart+j))

    def RunSort(self):
        paramData = [0.0 for _ in range(self.numYears)] # data read in from file
        rows = self.numAreas
        cols = self.numYears
        accumParamData = [[0.0 for _ in range(cols)] for _ in range(rows)] # accumulated data if in region
        desiredParam = self.comboParameter.get()
        # typical name: Lat_Lon_Grid_EBMS_MA_2015_2017
        paramFName = os.path.join(self.root, 'Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        
        #print(paramFName)

        # The data structure is used with InPolygon algorithm to check 
        # if grid parameter is within area of interest
        self.numAreas = int(self.numAreasEntry.get())
        for i in range(self.numAreas):
            self.areaData[i].numCorners = int(self.areas[i].numCornersEntry.myEntry.get())
            for j in range(self.areaData[i].numCorners):
                self.areaData[i].long[j] = float(self.areas[i].corners[j].longitude.myEntry.get())
                self.areaData[i].lat[j] = float(self.areas[i].corners[j].latitude.myEntry.get())

        of = open('temp.txt', 'w')

        if os.path.isfile(paramFName):
            grid = 1
            with open(paramFName, 'r') as f:
                while True:
                    # Read in a line from paramFName
                    line = f.readline()
                    if not line:
                        f.close()
                        break

                    # parse line
                    dataArray = [s.strip() for s in line.split(',')]
                    lat = float(dataArray[0])
                    lon = float(dataArray[1])
                    for i in range(self.numYears):
                        # column 2 will be initial data and not of interest
                        paramData[i] = float(dataArray[i+3]) 
                    
                    # Now check if the data point (lon, lat) is located in one of the desired areas
                    # self.areas[i] with given coordinates self.areas[i].corners[j]
                    #
                    # if so, add to accumParamData[i][0:n] += paramData[0:n]

                    # For each area
                    for i in range(self.numAreas):
                        # is (lon, lat) in this area
                        nodes = self.areaData[i].numCorners
                        if PointInPolygon(self.areaData[i].long, self.areaData[i].lat, lon, lat, nodes):
                            # if so accumulate parameter data
                            for j in range(self.numYears):
                                accumParamData[i][j] += paramData[j]
                            of.write('Grid #{} found in area{}\n'.format(grid,i+1))
                    grid += 1

        else:
            messagebox.showerror("Reading Parameter File", f'No data for '+desiredParam+'\nHas Simulation been run?\nAre years correct?')

        # display results
        for i in range(self.numAreas):
            for j in range(self.numYears):
                old = self.areas[i].results[j].myEntry.get()
                n = len(old)
                self.areas[i].results[j].myEntry.delete(0, n)
                # round to 4 decimal places
                # round(x,4) can have unpredicable results, use simple math instead
                r = 1e4
                y = int(accumParamData[i][j] * r + 0.5) / r
                self.areas[i].results[j].myEntry.insert(0, str(y))


    def UpdateWidgets(self, filePath=None):
        # Populate from known file
        currentParam = 0
        self.comboParameter.configure(values=self.paramStr)
        self.comboParameter.current(currentParam)
        self.numAreas = self.ReadAreaCorners(filePath)
        self.numAreasEntry.delete(0,3)
        self.numAreasEntry.insert(0, str(self.numAreas))
        self.NumAreasUpdate()
        for i in range(self.numAreas):
            self.areas[i].numCornersEntry.myEntry.delete(0,3)
            self.areas[i].numCornersEntry.myEntry.insert(0, str(self.areaData[i].numCorners))
            self.areas[i].NumCornersUpdate()
            for j in range(self.areaData[i].numCorners):
                self.areas[i].corners[j].longitude.myEntry.delete(0,10)
                self.areas[i].corners[j].longitude.myEntry.insert(0, str(self.areaData[i].long[j]))
                self.areas[i].corners[j].latitude.myEntry.delete(0,10)
                self.areas[i].corners[j].latitude.myEntry.insert(0, str(self.areaData[i].lat[j]))

    def GetDataSortFile(self):
        file_path = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if file_path:
            self.UpdateWidgets(file_path)
    
    def SaveDataSortFile(self):
        fName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if fName:
            with open(fName, 'w') as f:
                for i in range(int(self.numAreasEntry.get())):

                    # write longitude values
                    for j in range(int(self.areas[i].numCornersEntry.myEntry.get()) - 1):
                        f.write(self.areas[i].corners[j].longitude.myEntry.get()+',')
                    f.write(self.areas[i].corners[j+1].longitude.myEntry.get()+'\n')
                
                    # write latitude values
                    for j in range(int(self.areas[i].numCornersEntry.myEntry.get()) - 1):
                        f.write(self.areas[i].corners[j].latitude.myEntry.get()+',')
                    f.write(self.areas[i].corners[j+1].latitude.myEntry.get()+'\n')
            f.close()

    def NumAreasUpdate(self):
        """ Updates the number of areas functions. """

        for i in range(self.numAreasMax):
            self.areas[i].areaFrame.grid_remove()

        n = int(self.numAreasEntry.get())
        if n > self.numAreasMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numAreasMax}\nSetting to max')
            n = self.numAreasMax
            self.numAreasEntry.delete(0,3)
            self.numAreasEntry.insert(0, str(n))
        self.numAreas = n
        # Now update desired funtion definitions
        for i in range(self.numAreasMax):
            self.areas[i].areaFrame.grid_remove()
        for i in range(self.numAreas):
            self.areas[i].areaFrame.grid()

    def ReadAreaCorners(self, fName=None):
        """Reads a DataSort file and returns the number of areas defined"""
        areaIndex = 0
        if fName == None: fName = 'DataSort/AreasOfInterestDataSort.csv'
        if os.path.isfile(fName):
            with open(fName, 'r') as f:
                while True:
                    if (areaIndex > self.numAreasMax):
                        messagebox.showerror("Reading Areas File", f'Max reached {self.numAreasMax}\nStopping at {areaIndex-1}')
                        areaIndex -= 1
                        break

                    inputStr = f.readline()
                    if not inputStr:
                        f.close()
                        break

                    # read longitude values
                    longIndex = 0
                    j = len(inputStr)-1
                    if(inputStr[j]=='\n'): j -= 1
                    # remove trailing commas
                    while (inputStr[j]) == ',':
                        j -= 1
                    subStr = inputStr[0:j+1]

                    while j>0:
                        j=subStr.find(',')
                        if (j>0):
                            self.areaData[areaIndex].long[longIndex] = float(subStr[0:j])
                            subStr = subStr[j+1:len(subStr)]
                            longIndex += 1
                            if (longIndex > self.numCornersMax):
                                messagebox.showerror("Reading Areas File", f'Max corner {self.numCornersMax}\nStopping at {longIndex-1}')
                                longIndex -= 1
                                break
                    #get last value 
                    if (longIndex < self.numCornersMax):
                        self.areaData[areaIndex].long[longIndex] = float(subStr)
                        longIndex += 1

                    # read latiitude values
                    inputStr = f.readline()
                    if not inputStr:
                        f.close()
                        break
                    latIndex = 0
                    j = len(inputStr)-1
                    if(inputStr[j]=='\n'): j -= 1
                    # remove trailing commas
                    while (inputStr[j]) == ',':
                        j -= 1
                    subStr = inputStr[0:j+1]

                    while j>0:
                        j=subStr.find(',')
                        if (j>0):
                            self.areaData[areaIndex].lat[latIndex] = float(subStr[0:j])
                            subStr = subStr[j+1:len(subStr)]
                            latIndex += 1
                            if (latIndex > self.numCornersMax):
                                messagebox.showerror("Reading Areas File", f'Max corner {self.numCornersMax}\nStopping at {latIndex-1}')
                                latIndex -= 1
                                break
                    #get last value 
                    if (latIndex < self.numCornersMax):
                        self.areaData[areaIndex].lat[latIndex] = float(subStr)
                        latIndex += 1

                    if (longIndex != latIndex):
                        messagebox.showerror("Invalud Area Data File")
                        latIndex = 0
                    self.areaData[areaIndex].numCorners = latIndex
                    areaIndex += 1
                f.close()
            return areaIndex
        else: 
            messagebox.showerror("Data Sort", f'No Data Sort File Has Been Saved')
            return 1

class Corner:
    def __init__(self, maxCorners):
        self.long = [0.0 for _ in range(maxCorners)]
        self.lat = [0.0 for _ in range(maxCorners)]
        self.numCorners = 0

