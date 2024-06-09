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
from AreaManager import *

#===============================================================================================================
##
# This class is used to assist the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest
#
# @param friend is used to access Start and Stop year from the Main Frame
# @param maxAreas defined at start up for the maximum allowed areas
# @param maxCorners defined at start up for the maximum allowed corners or nodes
# @param maxYears defined at start up for the maximum allowed year range
# @param paramStr defined at start up for the desired outputs
# 
#===============================================================================================================
class SortByArea(ttk.Frame):
    def __init__(self, container, friend, maxAreas, maxCorners, maxYears, paramStr):
        super().__init__()
        
        labelArr = ['Corner', 'Long', 'Lat ', '0.0', '0.0']
        self.root = os.getcwd() #os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'DataSort')
        self.friend = friend
        self.areaFName = None

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

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        sortAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Sort By Area', style='SAMS.TFrame', width=400, height=200)
        # --------------------------------------------------------------------------------------------------------
        self.numAreasLabel = ttk.Label(sortAreaFrame, text='# of Areas')
        self.numAreasLabel.grid(row=0, column=0, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.numAreasEntry=ttk.Entry(sortAreaFrame,validatecommand=numbersCallback, width=5)
        self.numAreasEntry.insert(0, str(self.numAreas))
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numAreasEntry.grid(row=1, column=0, sticky='w')
        self.numAreasEntry.focus()
        self.numAreasEntry.bind('<Return>', self.EnterKeyClicked)
        # --------------------------------------------------------------------------------------------------------
        self.outputParmLabel = ttk.Label(sortAreaFrame, text='Output Parameters')
        self.outputParmLabel.grid(row=0, column=0, sticky='ns')
        self.comboParameter = ttk.Combobox(sortAreaFrame, values=paramStr)
        self.comboParameter.grid(row=1, column=0, sticky='ns')
        # --------------------------------------------------------------------------------------------------------
        self.dataSortFileEntry = self.myEntry=ttk.Entry(sortAreaFrame, width=25)
        self.dataSortFileEntry.insert(0, 'AreasOfInterestDataSort.csv')
        self.dataSortFileEntry.grid(row=0, column=1, sticky='we', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.openDataSortButton = ttk.Button(sortAreaFrame, text='Load Data Sort File', style="BtnGreen.TLabel", command=self.GetDataSortFile)
        self.openDataSortButton.grid(row=0, column=2, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.saveDataSortButton = ttk.Button(sortAreaFrame, text='Save Data Sort File', style="BtnBluGrn.TLabel", command=self.SaveDataSortFile)
        self.saveDataSortButton.grid(row=1, column=2, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.numAreasButton = ttk.Button(sortAreaFrame, text='Update # Areas', command=self.NumAreasUpdate)
        self.numAreasButton.grid(row=2, column=0, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.saveDataSortButton = ttk.Button(sortAreaFrame, text='Run Sort', command=self.RunSort)
        self.saveDataSortButton.grid(row=2, column=2, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.areas = AreaManager(self, sortAreaFrame, self.numAreasMax, self.numCornersMax,
                                   elementRow=3, elementCol=0, cornerRow=0, cornerColumn=0, labelArr=labelArr,
                                   includeYears=True, numYearsMax=self.maxYears, yearStart=self.yearStart, yearStop=self.yearStop)

        # now hide
        for a in range(self.numAreas, self.numAreasMax):
            self.areas.areaSubFrame[a].areaFrame.grid_remove()

        sortAreaFrame.grid(row=4, column=0, columnspan=10)
        sortAreaFrame.grid_columnconfigure(0,weight=2)
        sortAreaFrame.grid_columnconfigure(1,weight=1)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.scrollFrame.grid(row=1, column=0, sticky='nsew')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Sort By Area Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=0)

        self.bind("<Visibility>", self.on_visibility)

    #---------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
        self.areaFName = os.path.join(self.startDir, self.dataSortFileEntry.get())
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
            self.friend.stopYr.myEntry.delete(0,tk.END)
            self.friend.stopYr.myEntry.insert(0, self.yearStop)
        for i in range(self.numAreas):
            for j in range(self.numYears):
                self.areas.areaSubFrame[i].results[j].myEntry.grid()
                self.areas.areaSubFrame[i].results[j].myLabel.grid()
                self.areas.areaSubFrame[i].results[j].myLabel.config(text = str(self.yearStart+j))
        self.UpdateWidgets()

    #---------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------
    def RunSort(self):
        paramData = [0.0 for _ in range(self.numYears)] # data read in from file
        rows = self.numAreas
        cols = self.numYears
        accumParamData = [[0.0 for _ in range(cols)] for _ in range(rows)] # accumulated data if in region
        desiredParam = self.comboParameter.get()
        # typical name: Lat_Lon_Grid_EBMS_MA_2015_2017
        paramFName = os.path.join(self.root, 'Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        
        # The data structure is used with InPolygon algorithm to check 
        # if grid parameter is within area of interest
        self.numAreas = int(self.numAreasEntry.get())
        for i in range(self.numAreas):
            self.areaData[i].numCorners = int(self.areas.areaSubFrame[i].numCornersEntry.myEntry.get())
            for j in range(self.areaData[i].numCorners):
                self.areaData[i].long[j] = float(self.areas.areaSubFrame[i].corners[j].longitude.myEntry.get())
                self.areaData[i].lat[j] = float(self.areas.areaSubFrame[i].corners[j].latitude.myEntry.get())

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
                    # self.areas.areaSubFrame[i] with given coordinates self.areas.areaSubFrame[i].corners[j]
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
                self.areas.areaSubFrame[i].results[j].myEntry.delete(0,tk.END)
                # round to 4 decimal places
                # round(x,4) can have unpredicable results, use simple math instead
                r = 1e4
                y = int(accumParamData[i][j] * r + 0.5) / r
                self.areas.areaSubFrame[i].results[j].myEntry.insert(0, str(y))

    #---------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------
    def UpdateWidgets(self):
        self.numAreas = self.areas.ReadAreaCorners(self.areaFName)
        self.numAreasEntry.delete(0,tk.END)
        self.numAreasEntry.insert(0, str(self.numAreas))
        self.NumAreasUpdate()
        self.areas.UpdateWidgets()

    #---------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------
    def GetDataSortFile(self):
        file_path = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if file_path:
            self.areaFName = file_path
            self.UpdateWidgets()
            self.dataSortFileEntry.delete(0,tk.END)
            f = file_path.split('/')
            self.dataSortFileEntry.insert(0, f[-1])
    
    #---------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------
    def SaveDataSortFile(self):
        fName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if fName:
            with open(fName, 'w') as f:
                for i in range(int(self.numAreasEntry.get())):

                    # write longitude values
                    for j in range(int(self.areas.areaSubFrame[i].numCornersEntry.myEntry.get()) - 1):
                        f.write(self.areas.areaSubFrame[i].corners[j].longitude.myEntry.get()+',')
                    f.write(self.areas.areaSubFrame[i].corners[j+1].longitude.myEntry.get()+'\n')
                
                    # write latitude values
                    for j in range(int(self.areas.areaSubFrame[i].numCornersEntry.myEntry.get()) - 1):
                        f.write(self.areas.areaSubFrame[i].corners[j].latitude.myEntry.get()+',')
                    f.write(self.areas.areaSubFrame[i].corners[j+1].latitude.myEntry.get()+'\n')
            f.close()
            self.dataSortFileEntry.delete(0,tk.END)
            f = fName.split('/')
            self.dataSortFileEntry.insert(0, f[-1])

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def EnterKeyClicked(self, event):
        self.NumAreasUpdate()

    #---------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------
    def NumAreasUpdate(self):
        """ Updates the number of areas functions. """

        for i in range(self.numAreasMax):
            self.areas.areaSubFrame[i].areaFrame.grid_remove()

        n = int(self.numAreasEntry.get())
        if n > self.numAreasMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numAreasMax}\nSetting to max')
            n = self.numAreasMax
            self.numAreasEntry.delete(0,tk.END)
            self.numAreasEntry.insert(0, str(n))
        self.numAreas = n
        self.areas.NumAreasUpdate(n)

    #-------------------------------------------------------------------------------------
    ## Help Window for Sort By Area
    #-------------------------------------------------------------------------------------
    def pop_up(self):
        about = '''Sort By Area
    (This frame is scrollable, use mouse wheel)
    This frame allows the user to determine the amount of a given parameter
    that is located in a specific area defined by long, lat coordinates of the
    vertices or corners.
    1) The areas must first be defined by specifyin the number of areas, update
    2) For each area
       a) Enter the number of corners

# Defined
    The number of defined areas as determined by the user. This is limited by 
    Max Areas of Interest. See SHOW Args button

    The # Defined is limited by default to 25. See SHOW Args for current values.
    The user can modify this on the command line:
    > python .\PythonScripts\GUI\GeoSAM\GeoSams.py #Areas #Nodes #Years
    Default:
    > python .\PythonScripts\GUI\GeoSAM\GeoSams.py 25 8 5

Update # Defined
    Use Enter Key or click this button after entering a value in # Defined to 
    populate/show the Area N defintions.

Fishing Mort File
    The name of the file used to hold this information. The user can load the 
    default file 'FishingMortality.csv' or save their own configuration.

    If this feature is not desired then enter NONE in the window

    Use Load Fishing Mort File to load a predefined set of data

    Use Save Fishing Mort File to save the currently displayed setting

Year
    The year for which Area N is valid

Area N
    Comment: Optional. Enter a comment to describe the area being specfied.

    # Corners: Specifically, the number of Fields for the year given. 
        This is limited by Max Nodes in Area. See SHOW Args for current values.
        This can be changed on the command line. See above

    Update # Corners
        Use Enter Key or click this button to populate the field entries for 
        the given number

    Field N
        These are the area numbers as determined in Special Access Frame. Enter
        the area number and its Mortality.
'''
        #about = re.sub("\n\s*", "\n", about) # remove leading whitespace from each line
        popup = tk.Toplevel()
        nrows = 35
        ncols = 80
        parentPosn = '+'+str(self.winfo_rootx()+700)+'+'+str(self.winfo_rooty()+50)
        popup.geometry(str(int(ncols*8.5))+"x"+str(nrows*18)+parentPosn)
        T = tk.Text(popup, width=ncols, height=nrows, padx=10)
        T.insert('end', about)
        T.config(state='disabled')
        T.grid()
        btn = tk.Button(popup, text ="Close", command= popup.destroy)
        btn.grid(row =1)
