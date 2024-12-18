## @page Sort Sort By Area Frame
# Assists the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest.
#
# @section Sortp1 Number of Areas
#
# The number of defined areas. This is limited by Max Areas of Interest. See SHOW Args button
# The # of Areas is limited by default to 25. See SHOW Args for current values.
# The user can modify this on the command line:
# > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py Areas Nodes
#
# Default (same as started with no arguments):\n
# > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py 25 8
#
# @section Sortp2 Output Parameters
#
# This is a dropbox of the selected output parameters on the main tab. After 
# a simulation and interpolation have been run, the user would select one of
# these output, click Run Sort, and the amount of that output in each of the
# defined areas is accumulated by year to the left of each area.
#
# @section Sortp3 Load and Save Data Sort Files
# These buttons allow the user to load a predefined set of areas or to save
# the current set to the named file.
#
# @section Sortp4 Run Sort
# This will start the program to check if a region grid value for a given 
# year is within one of the specified area and if so accumulate the year
# sum with that value.
#
# @section Sortp5 Area SubFrames
# @subsection Sortp5p1 YYYY
# For each year, from Start Year to Stop Year as given in the Main tab
# an entry box is provided to store the accumulated parameter for that year.
# These are not populated until after the Run Sort button has been clicked.
#
# @subsection Sortp5p2 Comment
# Optional. Enter a comment to describe the area being specfied.
#
# @subsection Sortp5p3 # Corners
#  Also called nodes or sides. This is limited by Max Nodes in Area. 
# See SHOW Args for current values. This can be changed on the command line. See above
#
# @subsection Sortp5p5 Corner N
# These are the coordinates of the area vertices. Enter the Longitude and Latitude of the 
# vertices for the area. It is up to the user to ensure that a closed shape is defined.
#
import os
import shutil

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from PointInPolygon import *
from AreaManager import *
from Globals import *

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
class SortByArea(ttk.Frame):
    def __init__(self, container, friend, maxAreas, maxCorners, maxYears, paramStr):
        super().__init__()
        
        self.root = os.getcwd() #os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'DataSort')
        self.exportFileName = ''
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
        sortAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Sort By Area', style='SAMS.TFrame', width=frameWidth, height=frameHeight)
        # --------------------------------------------------------------------------------------------------------
        self.numAreasLabel = ttk.Label(sortAreaFrame, text='# of Areas')
        self.numAreasLabel.grid(row=0, column=0, sticky='w', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.numAreasEntry=ttk.Entry(sortAreaFrame,validatecommand=numbersCallback, width=5)
        self.numAreasEntry.insert(0, str(self.numAreas))
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numAreasEntry.grid(row=1, column=0, sticky='w', padx=5)
        self.numAreasEntry.bind('<Return>', self.EnterKeyClicked)
        self.numAreasEntry.bind('<FocusOut>', self.EnterKeyClicked)
        # --------------------------------------------------------------------------------------------------------
        self.outputParmLabel = ttk.Label(sortAreaFrame, text='Output Parameters')
        self.outputParmLabel.grid(row=0, column=0, sticky='e')
        self.comboParameter = ttk.Combobox(sortAreaFrame, values=paramStr, width=8)
        self.comboParameter.grid(row=1, column=0, sticky='e')
        self.comboParameter.bind('<<ComboboxSelected>>', self.CbUpdateUnits)
        # --------------------------------------------------------------------------------------------------------
        self.dataSortFileLabel = ttk.Label(sortAreaFrame, text='Areas of Interest File Name')
        self.dataSortFileLabel.grid(row=0, column=1, sticky='w')
        self.dataSortFileEntry = tk.Entry(sortAreaFrame, width=25)
        self.dataSortFileEntry.insert(0, 'AreasOfInterestDataSort.csv')
        self.dataSortFileEntry.grid(row=1, column=1, sticky='w', padx=5)
        self.dataSortUnitsLabel = ttk.Label(sortAreaFrame, text='Units')
        self.dataSortUnitsLabel.grid(row=2, column=0, sticky='e')
        self.dataSortUnitsEntry = tk.Entry(sortAreaFrame, width=25)
        self.dataSortUnitsEntry.insert(0, '')
        self.dataSortUnitsEntry.grid(row=2, column=1, sticky='w', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.openDataSortButton = ttk.Button(sortAreaFrame, text='Load Data Sort File', style="BtnGreen.TLabel", command=self.GetDataSortFile)
        self.openDataSortButton.grid(row=0, column=2, sticky='e')
        # --------------------------------------------------------------------------------------------------------
        self.saveDataSortButton = ttk.Button(sortAreaFrame, text='Save Data Sort File', style="BtnBluGrn.TLabel", command=self.SaveDataSortFile)
        self.saveDataSortButton.grid(row=1, column=2, sticky='e')
        # --------------------------------------------------------------------------------------------------------
        self.runSortButton = ttk.Button(sortAreaFrame, text='Run Sort', command=self.RunSort)
        self.runSortButton.grid(row=2, column=2, sticky='e')
        # --------------------------------------------------------------------------------------------------------
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        exportFrame = ttk.LabelFrame(sortAreaFrame, text='Export Results', style='SAMS.TFrame')
        # --------------------------------------------------------------------------------------------------------
        self.exportThisSortButton = ttk.Button(exportFrame, text='Export\nThis', style='BtnLime.TLabel', command=self.ExportThis)
        self.exportThisSortButton.grid(row=0, column=0)
        self.exportAllSortButton = ttk.Button(exportFrame, text='Export\nAll', style='BtnLime.TLabel', command=self.ExportAll)
        self.exportAllSortButton.grid(row=0, column=1)
        # --------------------------------------------------------------------------------------------------------
        self.exportFileLabel = ttk.Label(exportFrame, text='Export File Name')
        self.exportFileLabel.grid(row=0, column=2, sticky='n')
        # --------------------------------------------------------------------------------------------------------
        self.exportFileEntry = self.myEntry=ttk.Entry(exportFrame, width=25)
        self.exportFileEntry.insert(0, self.exportFileName)
        self.exportFileEntry.grid(row=0, column=2, sticky='s', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.browseExportButton = ttk.Button(exportFrame, text='Browse For Export File', style="BtnGreen.TLabel", command=self.BrowseExportFile)
        self.browseExportButton.grid(row=0, column=3, sticky='w')
        # --------------------------------------------------------------------------------------------------------

        exportFrame.grid(row=3, column=0, columnspan=3, sticky='we')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # --------------------------------------------------------------------------------------------------------
        self.areas = AreaManager(self, sortAreaFrame, self.numAreasMax, self.numCornersMax,
                                   elementRow=4, elementCol=0, cornerRow=0, cornerColumn=0, labelArr=cornerLabelArr, includeYears=True,
                                   numYearsMax=self.maxYears, yearStart=self.yearStart, yearStop=self.yearStop, includeArea=True)

        # now hide
        for a in range(self.numAreas, self.numAreasMax):
            self.areas.areaSubFrame[a].areaFrame.grid_remove()

        sortAreaFrame.grid(row=4, column=0, columnspan=10, padx=5)
        sortAreaFrame.grid_columnconfigure(0,weight=2)
        sortAreaFrame.grid_columnconfigure(1,weight=1)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.scrollFrame.grid(row=1, column=0, sticky='nsew')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Sort By Area Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=0)

        self.bind("<Visibility>", self.on_visibility)

    ##
    #
    def on_visibility(self, event):
        self.areaFName = os.path.join(self.startDir, self.dataSortFileEntry.get())
        self.paramStr = []
        parmVal = self.friend.GetSelectedOutputs()
        if parmVal & 1: self.paramStr.append(ABUN)
        if parmVal & 2: self.paramStr.append(BIOM)
        if parmVal & 4: self.paramStr.append(EBMS)
        if parmVal & 8: self.paramStr.append(FEFF)
        if parmVal & 16: self.paramStr.append(FMOR)
        if parmVal & 32: self.paramStr.append(LAND)
        if parmVal & 64: self.paramStr.append(LNDW)
        if parmVal & 128: self.paramStr.append(LPUE)
        if parmVal & 256: self.paramStr.append(RECR)
        self.comboParameter.configure(values=self.paramStr)
        self.comboParameter.current(0)
        self.UpdateUnits()
        self.yearStart = int(self.friend.startYr.myEntry.get())
        self.yearStop = int(self.friend.stopYr.myEntry.get())
        self.domainName = self.friend.domainNameCombo.get()
        # year end growth data + initial state
        self.numYears = self.yearStop - self.yearStart + 1
        for i in range(self.numAreasMax):
            for j in range(self.numYears):
                self.areas.areaSubFrame[i].results[j].myEntry.grid()
                self.areas.areaSubFrame[i].results[j].myLabel.grid()
                self.areas.areaSubFrame[i].results[j].myLabel.config(text = str(self.yearStart+j))
            self.areas.areaSubFrame[0].results[0].myLabel.config(text = 'Initial State')
            for j in range(self.numYears, self.maxYears):
                self.areas.areaSubFrame[i].results[j].myEntry.grid_remove()
                self.areas.areaSubFrame[i].results[j].myLabel.grid_remove()
        self.UpdateWidgets()

    ##
    #
    def CbUpdateUnits(self, event):
        self.UpdateUnits()

    def UpdateUnits(self):
        desiredParam = self.comboParameter.get()
        (units, scale) = DetermineUnitsScale(desiredParam)
        UpdateEntry(self.dataSortUnitsEntry, units)

    ##
    #
    def AppendYears(self, addYears):
        for i in range(self.numAreasMax):
            self.areas.areaSubFrame[i].AppendResults(addYears)
        self.maxYears = self.maxYears+addYears

    ##
    #
    def RunSort(self):
        runSortErrors = 0
        paramData = [0.0 for _ in range(self.numYears)] # data read in from file
        rows = self.numAreas
        cols = self.numYears
        accumParamData = [[0.0 for _ in range(cols)] for _ in range(rows)] # accumulated data if in region
        # Used to compute average of data
        countNonZeroData = [[0.0 for _ in range(cols)] for _ in range(rows)]  # data read in from file
        countData = [[0.0 for _ in range(cols)] for _ in range(rows)]  # also serves to count number of grids in area
        desiredParam = self.comboParameter.get()
        # typical name: Lat_Lon_Grid_EBMS_MA_2015_2017
        #               Lat_Lon_Grid_ABUN_AL_2015_2017
        fileName = os.path.join('Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        paramFName = os.path.join(self.root, fileName)
        
        # The data structure is used with InPolygon algorithm to check 
        # if grid parameter is within area of interest
        self.numAreas = int(self.numAreasEntry.get())
        for i in range(self.numAreas):
            self.areaData[i].numCorners = int(self.areas.areaSubFrame[i].numCornersEntry.myEntry.get())
            for j in range(self.areaData[i].numCorners):
                self.areaData[i].long[j] = float(self.areas.areaSubFrame[i].corners[j].longitude.myEntry.get())
                self.areaData[i].lat[j] = float(self.areas.areaSubFrame[i].corners[j].latitude.myEntry.get())

        if os.path.isfile(paramFName):
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
                    # i is index into file array
                    for i in range(self.numYears):
                        paramData[i] = float(dataArray[i+2]) 
                    
                    # Now check if the data point (lon, lat) is located in one of the desired areas
                    # tkinter widgets: self.areas.areaSubFrame[row] with given coordinates self.areas.areaSubFrame[row].corners[j]
                    # or as stored in: self.areaData[row].long[0:j], self.areaData[row].lat[0:j]
                    # 
                    # if so, add to accumParamData[row][0:n] += paramData[0:n]

                    # For each area
                    for row in range(self.numAreas):
                        # is (lon, lat) in this area
                        nodes = self.areaData[row].numCorners
                        if PointInPolygon(self.areaData[row].long, self.areaData[row].lat, lon, lat, nodes):
                            # if so accumulate parameter data
                            for col in range(self.numYears):
                                accumParamData[row][col] += paramData[col]
                                countData[row][col] += 1
                                if paramData[col] > 0:
                                    countNonZeroData[row][col] += 1

            # display results
            (units, scale) = DetermineUnitsScale(desiredParam)

            for row in range(self.numAreas):
                for col in range(self.numYears):
                    areaKm2 = countData[row][col] * grid_area_sqm / 1.0e6
                    self.areas.areaSubFrame[row].myArea = areaKm2
                    UpdateEntry(self.areas.areaSubFrame[row].compAreaEntry.myEntry, f'{areaKm2:.4f}'+' Km^2')
                    if desiredParam == BIOM:
                        # convert to metric tons = 1e6 grams
                        # BIOM is in g/m2
                        # mt = 1e6 g
                        # g/m2 * km2 * (1e6 m2/km2) / 1e6 g
                        # mt = g/m2 * km2
                        accumParamData[row][col] = accumParamData[row][col] * areaKm2 / countData[row][col]
                    
                    if desiredParam == FEFF or desiredParam == FMOR:
                        # compute average
                        if countNonZeroData[row][col] > 0: accumParamData[row][col] = accumParamData[row][col] / countNonZeroData[row][col]
                    
                    if desiredParam == ABUN or desiredParam == RECR:
                        # compute density average
                        accumParamData[row][col] = accumParamData[row][col] / countData[row][col]

                    # round to 4 decimal places
                    UpdateEntry(self.areas.areaSubFrame[row].results[col].myEntry, f'{(accumParamData[row][col] * scale):.4f}')

        else:
            messagebox.showerror("Reading Parameter File", f'No data for '+fileName+'\nHas Simulation been run?\nAre years correct?')
            runSortErrors = 1

        return runSortErrors

    ##
    #
    def UpdateWidgets(self):
        self.numAreas = self.areas.ReadAreaCorners(self.areaFName)
        self.numAreasEntry.delete(0,tk.END)
        self.numAreasEntry.insert(0, str(self.numAreas))

        self.exportFileEntry.delete(0,tk.END)
        f = self.exportFileName.split('/')
        self.exportFileEntry.delete(0,tk.END)
        self.exportFileEntry.insert(0, f[-1])

        self.NumAreasUpdate()
        self.areas.UpdateWidgets()

    ##
    #
    def GetDataSortFile(self):
        file_path = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if file_path:
            self.areaFName = file_path
            self.UpdateWidgets()
            self.dataSortFileEntry.delete(0,tk.END)
            f = file_path.split('/')
            self.dataSortFileEntry.insert(0, f[-1])
    
    ##
    #
    def SaveDataSortFile(self):
        fName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if fName:
            with open(fName, 'w') as f:
                for i in range(int(self.numAreasEntry.get())):
                    # write comment
                    f.write('#'+self.areas.areaSubFrame[i].commentEntry.myEntry.get()+'\n')

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

    ##
    #
    def BrowseExportFile(self):
        file_path = filedialog.asksaveasfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if file_path:
            f = file_path.split('/')
            UpdateEntry(self.exportFileEntry, f[-1])
            self.exportFileName = file_path

    ## This method exports the current page of data, just a single output parameter
    #
    # First row :
    #  AREA     YEAR        PARAMETER (UNITS)
    #   1       StartYear     
    #   1       ...
    #   1       StopYear
    #  ...      ...
    #   N       StartYear     
    #   N       ...
    #   N       StopYear     
    #
    def ExportThis(self, nomsg=False):
        if self.exportFileName != '':
            # Ensure data is up to date
            err = self.RunSort()

            if err == 0:
                # get current parameter
                outStr = self.comboParameter.get()
                units = self.dataSortUnitsEntry.get()
                with open(self.exportFileName, 'w') as f:
                    ##f.write('AREA,YEAR,' + outStr + ' ('+ units + ')\n')
                    # Write Header
                    f.write('Area #,Comment,Area,Units,Init State')
                    for yr in range(1,self.numYears):
                            f.write(',' + str(yr+self.yearStart))
                    f.write('\n')

                    for a in range(self.numAreas):
                        f.write(str(a+1))
                        f.write(',' + self.areas.areaSubFrame[a].commentEntry.myEntry.get())
                        f.write(',' + self.areas.areaSubFrame[a].compAreaEntry.myEntry.get())
                        f.write(',' + outStr + ' ('+ units + ')')
                        for yr in range(self.numYears):
                             f.write( ',' + self.areas.areaSubFrame[a].results[yr].myEntry.get())
                        f.write('\n')

                    f.close()
                if not nomsg: messagebox.showinfo('Export This', f'FILE SAVED: {self.exportFileName}')
        else:
            messagebox.showerror('Export This', 'File Name has not been defined.')

    ## Export all select parameters
    #
    # For each parameter
    # - Verify data file exists, Lat_Lon_Grid_ + ABUN_ + AL + _ 2015_2017
    #
    def ExportAll(self):
        if self.exportFileName != '':
            # Check that data files are present
            filesExist = True
            n = len(self.paramStr)
            for i in range(n):
                dataFileName = 'Lat_Lon_Grid_' + self.paramStr[i] + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv'
                if not os.path.isfile(os.path.join('Results', dataFileName)):
                    filesExist = False
                    break

            if filesExist:
                # Create Initial File
                self.comboParameter.current(0)
                self.RunSort()
                self.ExportThis(True)
                #print('Initial file created', n)

                # now append remaining outputs
                for i in range(1,n):
                    with open(self.exportFileName, 'a') as f:
                        self.comboParameter.current(i)
                        self.UpdateUnits()
                        self.RunSort()
                        
                        outStr = self.comboParameter.get()
                        units = self.dataSortUnitsEntry.get()
                        #print('appending '+outStr)

                        for a in range(self.numAreas):
                            f.write(str(a+1)+ ',' + self.areas.areaSubFrame[a].compAreaEntry.myEntry.get()+ ',' + outStr + ' ('+ units + ')')
                            for yr in range(self.numYears):
                                f.write( ',' + self.areas.areaSubFrame[a].results[yr].myEntry.get())
                            f.write('\n')
                        f.close()

                messagebox.showinfo('Export This', f'FILE SAVED: {self.exportFileName}')

            else: # data files have not yet been generated.
                 messagebox.showerror('Export All', 'Data Files do not exist. START Sim')

        else:
            messagebox.showerror('Export All', 'File Name has not been defined.')

    ##
    #
    def EnterKeyClicked(self, event):
        self.NumAreasUpdate()

    ##
    #
    def NumAreasUpdate(self):
        """ Updates the number of areas functions. """
        for i in range(self.numAreasMax):
            self.areas.areaSubFrame[i].areaFrame.grid_remove()

        if self.numAreasEntry.get() == '':
            n=1
            self.numAreasEntry.insert(0,'1')
        else:
            n = int(self.numAreasEntry.get())
        if n > self.numAreasMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numAreasMax}\nSetting to max')
            n = self.numAreasMax
            self.numAreasEntry.delete(0,tk.END)
            self.numAreasEntry.insert(0, str(n))
        self.numAreas = n
        self.areas.NumAreasUpdate(n)

    ## Help Window for Sort By Area
    #
    def pop_up(self):
        about = '''Sort By Area
    (This frame is scrollable, use mouse wheel)
    This frame allows the user to determine the amount of a given parameter
    that is located in a specific area defined by long, lat coordinates of the
    vertices or corners.
    1) The areas must first be defined by specifyin the number of areas, update
    2) For each area
       a) Enter the number of corners

# of Areas
    The number of areas as determined by the user. This is limited by 
    Max Areas of Interest. See SHOW Args button

    The # of Areas is limited by default to 25. See SHOW Args for current values.
    The user can modify this on the command line:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py #Areas #Nodes
    Default:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py 25 8

Output Parameters
    This is a dropbox of the selected output parameters on the main tab. After 
    a simulation and interpolation have been run, the user would select one of
    these output, click Run Sort, and the amount of that output in each of the
    defined areas is accumulated by year to the left of each area.

Areas of Interest File Name
    The name of the file whose data is currently loaded. There is a default
    file included with the installed files

Load and Save Data Sort Files
    These buttons allow the user to load a predefined set of areas or to 
    save the current set to the named file.

Run Sort
    This will start the program to check if a region grid value for a given 
    year is within one of the specified area and if so accumulate the year
    sum with that value.

Export Results
    These buttons will export the data to a CSV file using the following format

            AREA  YEAR       LPUE_  ... 
            1     StartYear  xxxx
            1     ...
            1     StopYear   xxxx
            ...
            N     StartYear  xxxx
            N     ...
            N     StopYear   xxxx

    Export This
        This button will export the data set to the file name in Export File.
        It will first call the RunSort method to enure the data is up to date.
        There will only be one column of data as given by the current combo box
        setting.

    Export All
        This button will step through each of the Output Parameters given in 
        the combo box, call RunSort for that parameter, and then export the
        parameter data as columns in a CSV file.

    Browse For Export File
        Allows user to select an existing file name or enter their own. The
        file is not saved until one of the Export buttons is clicked.

Area N
    YYYY: For each year, from Start Year to Stop Year as given in the Main tab
    an entry box is provided to store the accumulated parameter for that year.
    These are not populated until after the Run Sort button has been clicked.

    Comment: Optional. Enter a comment to describe the area being specfied.

    # Corners: Also called nodes or sides. 
        This is limited by Max Nodes in Area. See SHOW Args for current values.
        This can be changed on the command line. See above
               
    Corner N
        These are the coordinates of the area vertices. Enter the Longitude and
        Latitude of the vertices for the area. It is up to the user to ensure 
        that a closed shape is defined.

    Year Headings
    Main specifies 2022 as year start and 2025 as year stop
    - Initial state as of June 1, 2022 @ 00:00, i.e. May 31, 2022 @ 24:00
    - 2023: Growth state as of May 31, 2023 @ 24:00, results for 1st year growth
    - 2024: Growth state as of May 31, 2024 @ 24:00, results for 2nd year growth
    - 2025: Growth state as of May 31, 2025 @ 24:00, results for 3rd year growth
    - 2026: Growth state as of May 31, 2026 @ 24:00, results for 4th year growth
'''
        #about = re.sub("\n\s*", "\n", about) # remove leading whitespace from each line
        popup = tk.Toplevel()
        nrows = 35
        ncols = 80
        parentPosn = '+'+str(self.winfo_rootx()+helpXoffset)+'+'+str(self.winfo_rooty()+helpYoffset)
        popup.geometry(str(int(ncols*8.5))+"x"+str(nrows*18)+parentPosn)
        T = tk.Text(popup, width=ncols, height=nrows, padx=10)
        T.insert('end', about)
        T.config(state='disabled')
        T.grid()
        btn = tk.Button(popup, text ="Close", command= popup.destroy)
        btn.grid(row =1)
