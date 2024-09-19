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
import sys
sys.path.append("PythonScripts\GUI\GeoSAM\PyshpMaster")
import shapefile
import utm
import shutil

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from PointInPolygon import *
from AreaManager import *
from Globals import *

##
# This class is used to define the shape of the regional data
#
# This class uses PyShp libraries as defined in
# https://github.com/GeospatialPython/pyshp ==> Code ==> Download ZIP
class GeoShape:
    def __init__(self):
        self.X=[]
        self.Y=[]
        self.lat=[]
        self.lon=[]
        self.SAMS=''
        self.NewSAMS=''
        self.areaKm2 = 0.0

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
class SortByRegion(ttk.Frame):
    def __init__(self, container, friend, maxCorners, maxYears, paramStr):
        super().__init__()
        
        self.root = os.getcwd() #os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'Shapefiles')
        self.exportFileName = ''
        self.friend = friend
        self.areaFName = None

        self.numCornersMax = maxCorners
        self.maxYears = maxYears
        self.numAreas = 1
        self.numCorners = 1
        self.paramStr = paramStr

        self.yearStart = int(self.friend.startYr.myEntry.get())
        self.yearStop = int(self.friend.stopYr.myEntry.get())
        self.numYears = self.yearStop - self.yearStart + 1

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.sortAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Sort By Area', style='SAMS.TFrame', width=frameWidth, height=frameHeight)
        # --------------------------------------------------------------------------------------------------------
        self.outputParmLabel = ttk.Label(self.sortAreaFrame, text='Output Parameters')
        self.outputParmLabel.grid(row=0, column=0)
        self.comboParameter = ttk.Combobox(self.sortAreaFrame, values=paramStr, width=8)
        self.comboParameter.grid(row=1, column=0)
        # --------------------------------------------------------------------------------------------------------
        self.runSortButton = ttk.Button(self.sortAreaFrame, text='Run Sort', command=self.RunSort)
        self.runSortButton.grid(row=0, column=1)
        # --------------------------------------------------------------------------------------------------------
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        exportFrame = ttk.LabelFrame(self.sortAreaFrame, text='Export Results', style='SAMS.TFrame')
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
        self.shape = []
        self.shapeLenMA = 0
        self.shapeLenGB = 0
        self.numAreas = self.GetShapeData() 

        self.areas = AreaManager(self, self.sortAreaFrame, self.numAreas, self.numCornersMax,
                                   elementRow=4, elementCol=0, cornerRow=0, cornerColumn=0, labelArr=cornerLabelArr,
                                   includeYears=True, numYearsMax=self.maxYears, yearStart=self.yearStart, yearStop=self.yearStop)

        for i in range(self.numAreas):
            self.areas.areaSubFrame[i].comment = self.shape[i].NewSAMS

        self.sortAreaFrame.grid(row=4, column=0, columnspan=10, padx=5)
        self.sortAreaFrame.grid_columnconfigure(0,weight=2)
        self.sortAreaFrame.grid_columnconfigure(1,weight=1)
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
        self.paramStr = []
        parmVal = self.friend.GetSelectedOutputs()
        # ordering of string may not matter. The user ultimately selects the desired vale
        if parmVal & 1: self.paramStr.append('LPUE_')
        if parmVal & 2: self.paramStr.append('EBMS_')
        if parmVal & 4: self.paramStr.append('BIOM_')
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
        for i in range(self.numAreas):
            for j in range(self.numYears):
                self.areas.areaSubFrame[i].results[j].myEntry.grid()
                self.areas.areaSubFrame[i].results[j].myLabel.grid()
                self.areas.areaSubFrame[i].results[j].myLabel.config(text = str(self.yearStart+j))
            for j in range(self.numYears, self.maxYears):
                self.areas.areaSubFrame[i].results[j].myEntry.grid_remove()
                self.areas.areaSubFrame[i].results[j].myLabel.grid_remove()
        self.UpdateWidgets()

    ##
    #
    def AppendYears(self, addYears):
        for i in range(self.numAreas):
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
        desiredParam = self.comboParameter.get()
        # typical name: Lat_Lon_Grid_EBMS_MA_2015_2017
        #               Lat_Lon_Grid_ABUN_AL_2015_2017
        paramFName = os.path.join(self.root, 'Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        fileName = os.path.join('Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        
        if os.path.isfile(paramFName):
            ####of = open('temp.txt', 'w')
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
                        nodes = len(self.shape[i].lat)
                        if PointInPolygon(self.shape[i].lon, self.shape[i].lat, lon, lat, nodes):
                            # if so accumulate parameter data
                            for j in range(self.numYears):
                                accumParamData[i][j] += paramData[j]
                            ####of.write('Grid #{} found in area{}\n'.format(grid,i+1))
                    grid += 1
            ####of.close()

            # display results
            for i in range(self.numAreas):
                for j in range(self.numYears):
                    self.areas.areaSubFrame[i].results[j].myEntry.delete(0,tk.END)
                    # round to 4 decimal places
                    # round(x,4) can have unpredicable results, use simple math instead
                    r = 1e4
                    y = int(accumParamData[i][j] * r + 0.5) / r
                    self.areas.areaSubFrame[i].results[j].myEntry.insert(0, str(y))

        else:
            messagebox.showerror("Reading Parameter File", f'No data for '+fileName+'\nHas Simulation been run?\nAre years correct?')
            runSortErrors = 1

        return runSortErrors

    ##
    #
    def UpdateWidgets(self):
        self.exportFileEntry.delete(0,tk.END)
        f = self.exportFileName.split('/')
        self.exportFileEntry.delete(0,tk.END)
        self.exportFileEntry.insert(0, f[-1])

        self.NumAreasUpdate()
        self.areas.UpdateWidgets()

    ##
    #
    def BrowseExportFile(self):
        file_path = filedialog.asksaveasfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if file_path:
            f = file_path.split('/')
            self.exportFileEntry.delete(0,tk.END)
            self.exportFileEntry.insert(0, f[-1])
            self.exportFileName = file_path

    ## This method exports the current page of data, just a single output parameter
    #
    # First row :
    #  AREA     YEAR        PARAMETER
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
                with open(self.exportFileName, 'w') as f:
                    f.write('AREA,YEAR,'+outStr+'\n')

                    for a in range(self.numAreas):
                        for yr in range(self.numYears):
                            f.write(str(a+1) + ',' + str(yr+self.yearStart) + ',' + self.areas.areaSubFrame[a].results[yr].myEntry.get() + '\n')

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
                dataFileName = 'Lat_Lon_Grid_' + self.paramStr[i] + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop ) + '.csv'
                if not os.path.isfile(os.path.join('Results', dataFileName)):
                    filesExist = False
                    break

            if filesExist:
                # Create Initial File
                self.comboParameter.current(0)
                self.RunSort()
                self.ExportThis(True)

                # now append remaining outputs
                scratchFName = os.path.join('Results', 'TEMP.TXT')
                for i in range(1,n):
                    of = open(scratchFName, 'w')
                    with open(self.exportFileName, 'r') as f:
                        self.comboParameter.current(i)
                        self.RunSort()
                        # read header
                        line = f.readline()
                        line = line.strip()+','+self.paramStr[i]+'\n'
                        of.write(line)

                        for a in range(self.numAreas):
                            for yr in range(self.numYears):
                                line = f.readline()
                                line = line.strip()+','+self.areas.areaSubFrame[a].results[yr].myEntry.get()+'\n'
                                of.write(line)
                    of.close()
                    f.close()
                    shutil.move(scratchFName, self.exportFileName)
                
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
        # for i in range(self.numAreas):
        #     self.areas.areaSubFrame[i].areaFrame.grid_remove()

        self.areas.NumAreasUpdate(self.numAreas)

    def GetShapeData(self):
        self.domainName = self.friend.domainNameCombo.get()
        self.shapeLenMA = 0
        self.shapeLenGB = 0

        if (self.domainName == 'AL') or (self.domainName == 'MA'):
            sfName = os.path.join(self.startDir,self.friend.maShapeFileEntry.get())
            sfMA = shapefile.Reader(sfName)
            shapesMA = sfMA.shapes()
            self.shapeLenMA = len(sfMA)
    
        if (self.domainName == 'AL') or (self.domainName == 'GB'):
            sfName = os.path.join(self.startDir,self.friend.gbShapeFileEntry.get())
            sfGB = shapefile.Reader(sfName)
            shapesGB = sfGB.shapes()
            self.shapeLenGB = len(sfGB)

        numRegions = self.shapeLenMA + self.shapeLenGB
        self.shape = [ GeoShape() for _ in range(numRegions)]

        for n in range(self.shapeLenMA):
            record = sfMA.record(n)
            as_dict = record.as_dict()
            self.shape[n].SAMS = record['SAMS']
            self.shape[n].NewSAMS = record['NewSAMS']
            self.shape[n].areaKm2 = record['AreaKm2']
            pointLen = len(shapesMA[n].points)
            self.shape[n].X = [0.0 for _ in range(pointLen)]
            self.shape[n].Y = [0.0 for _ in range(pointLen)]
            self.shape[n].lat = [0.0 for _ in range(pointLen)]
            self.shape[n].lon = [0.0 for _ in range(pointLen)]
            for m in range(len(shapesMA[n].points)):
                self.shape[n].X[m] = shapesMA[n].points[m][0]
                self.shape[n].Y[m] = shapesMA[n].points[m][1]
                (self.shape[n].lat[m], self.shape[n].lon[m]) = utm.to_latlon(self.shape[n].X[m], self.shape[n].Y[m], 18, 'T')

        for n in range(self.shapeLenMA, numRegions):
            offset = n-self.shapeLenMA
            record = sfGB.record(offset)
            as_dict = record.as_dict()
            self.shape[n].SAMS = record['SAMS']
            self.shape[n].NewSAMS = record['NewSAMS']
            self.shape[n].areaKm2 = record['AreaKm2']
            pointLen = len(shapesGB[offset].points)
            self.shape[n].X = [0.0 for _ in range(pointLen)]
            self.shape[n].Y = [0.0 for _ in range(pointLen)]
            self.shape[n].lat = [0.0 for _ in range(pointLen)]
            self.shape[n].lon = [0.0 for _ in range(pointLen)]
            for m in range(len(shapesGB[offset].points)): 
                self.shape[n].X[m] = shapesGB[offset].points[m][0]
                self.shape[n].Y[m] = shapesGB[offset].points[m][1]
                (self.shape[n].lat[m], self.shape[n].lon[m]) = utm.to_latlon(self.shape[n].X[m], self.shape[n].Y[m], 19, 'T')
        return numRegions

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
