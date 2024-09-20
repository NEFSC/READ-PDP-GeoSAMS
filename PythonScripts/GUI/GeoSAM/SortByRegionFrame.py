## @page SortRegion  Sort By Area Frame
# Assists the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest.
#
# @section SortRegion p2 Output Parameters
#
# This is a dropbox of the selected output parameters on the main tab. After 
# a simulation and interpolation have been run, the user would select one of
# these output, click Run Sort, and the amount of that output in each of the
# defined areas is accumulated by year to the left of each area.
#
# @section SortRegion p3 Load and Save Data Sort Files
# These buttons allow the user to load a predefined set of areas or to save
# the current set to the named file.
#
# @section SortRegion p4 Run Sort
# This will start the program to check if a region grid value for a given 
# year is within one of the specified area and if so accumulate the year
# sum with that value.
#
# @section SortRegion p5 Area SubFrames
# @subsection SortRegion p5p1 YYYY
# For each year, from Start Year to Stop Year as given in the Main tab
# an entry box is provided to store the accumulated parameter for that year.
# These are not populated until after the Run Sort button has been clicked.
#
# @subsection SortRegion p5p2 Comment
# Optional. Enter a comment to describe the area being specfied.
#
# @subsection SortRegion p5p3 # Corners
#  Also called nodes or sides. This is limited by Max Nodes in Area. 
# See SHOW Args for current values. This can be changed on the command line. See above
#
# @subsection SortRegion p5p5 Corner N
# These are the coordinates of the area vertices. Enter the Longitude and Latitude of the 
# vertices for the area. It is up to the user to ensure that a closed shape is defined.
#
import os
import sys
sys.path.append("PythonScripts\GUI\GeoSAM\PyshpMaster")
import shapefile
import utm

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
    def __init__(self, container, friend, maxAreas, maxYears, paramStr):
        super().__init__()
        
        self.root = os.getcwd() #os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'Shapefiles')
        self.resultsDir = os.path.join(self.root, 'Results')
        self.exportFileName = ''
        self.friend = friend
        self.areaFName = None

        self.maxAreas = maxAreas
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
        self.runSortButton.grid(row=1, column=1)
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
        self.exportFileEntry = self.myEntry=ttk.Entry(exportFrame, width=15)
        self.exportFileEntry.insert(0, self.exportFileName)
        self.exportFileEntry.grid(row=0, column=2, sticky='s', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.browseExportButton = ttk.Button(exportFrame, text='Browse For Export File', style="BtnGreen.TLabel", command=self.BrowseExportFile)
        self.browseExportButton.grid(row=0, column=3, sticky='w')
        # --------------------------------------------------------------------------------------------------------

        exportFrame.grid(row=3, column=0, columnspan=6, sticky='we')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # --------------------------------------------------------------------------------------------------------
        self.shape = []
        self.shapeLenMA = 0
        self.shapeLenGB = 0
        self.numAreas = self.GetShapeData() 

        # code for creating table
        self.table = [[0 for _ in range(self.maxYears+1)] for _ in range(self.maxAreas+1)]
        for row in range(self.maxAreas+1):
            for col in range(self.maxYears+1):
                self.table[row][col] = ttk.Entry(self.sortAreaFrame, width=15)
                self.table[row][col].grid(row=row+4, column=col)
#                self.e.insert'end', lst[i][j])
        for row in range(self.numAreas):
            self.table[row+1][0].insert(0,self.shape[row].NewSAMS)
        for col in range(self.numYears):
            self.table[0][col+1].insert(0,str(self.yearStart+col))

        self.sortAreaFrame.grid(row=4, column=0, columnspan=10, padx=5)
        self.sortAreaFrame.grid_columnconfigure(0,weight=2)
        self.sortAreaFrame.grid_columnconfigure(1,weight=1)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.scrollFrame.grid(row=1, column=0, sticky='nsew')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Sort By Region Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=0)

        self.bind("<Visibility>", self.on_visibility)

    ##
    #
    def on_visibility(self, event):
        self.paramStr = []
        parmVal = self.friend.GetSelectedOutputs()
        # ordering of string may not matter. The user ultimately selects the desired value
        if parmVal & 1: self.paramStr.append(LPUE)
        if parmVal & 2: self.paramStr.append(EBMS)
        if parmVal & 4: self.paramStr.append(BIOM)
        if parmVal & 8: self.paramStr.append(ABUN)
        if parmVal & 16: self.paramStr.append(LNDW)
        if parmVal & 32: self.paramStr.append(LAND)
        if parmVal & 64: self.paramStr.append(FEFF)
        if parmVal & 128: self.paramStr.append(FMOR)
        if parmVal & 256: self.paramStr.append(RECR)
        self.comboParameter.configure(values=self.paramStr)
        self.comboParameter.current(0)

        self.yearStart = int(self.friend.startYr.myEntry.get())
        self.yearStop = int(self.friend.stopYr.myEntry.get())
        self.domainName = self.friend.domainNameCombo.get()
        self.numYears = self.yearStop - self.yearStart + 1

        self.numAreas = self.GetShapeData() 

        for row in range(self.numAreas):
            for col in range(self.numYears):
                self.table[row+1][col+1].grid()
        for row in range(self.numAreas):
            self.table[row+1][0].delete(0,tk.END)
            self.table[row+1][0].insert(0,self.shape[row].NewSAMS)

        # remove unused columns
        for row in range(self.numAreas):
            for col in range(self.numYears, self.maxYears):
                self.table[row+1][col+1].grid_remove()

        # remove unused rows
        for row in range(self.numAreas, self.maxAreas):
            for col in range(self.maxYears):
                self.table[row+1][col+1].grid_remove()

        # restore row header
        for col in range(self.numYears):
            self.table[0][col+1].grid()
        # clear col header
        for col in range(self.numYears, self.maxYears):
            self.table[0][col+1].grid_remove()
        # restore row header
        for row in range(self.numAreas):
            self.table[row+1][0].grid()
        # clear row header
        for row in range(self.numAreas, self.maxAreas):
            self.table[row+1][0].grid_remove()

        self.UpdateWidgets()

    ##
    #
    def RunSort(self):
        runSortErrors = 0
        paramData = [0.0 for _ in range(self.numYears)] # data read in from file
        rows = self.numAreas
        cols = self.numYears
        accumParamData = [[0.0 for _ in range(cols)] for _ in range(rows)] # accumulated data if in region
        # Used to compute average of data
        countData = [[0.0 for _ in range(cols)] for _ in range(rows)]  # data read in from file
        desiredParam = self.comboParameter.get()
        # typical name: Lat_Lon_Grid_EBMS_MA_2015_2017
        #               Lat_Lon_Grid_ABUN_AL_2015_2017
        paramFName = os.path.join(self.root, 'Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        fileName = os.path.join('Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        
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
                                if paramData[j] > 0:
                                    countData[i][j] += 1

            # display results
            # Display Units
            # Python 3.8 does not have match/case so using if elif
            if desiredParam == ABUN:
                self.UpdateEntry(self.table[0][0], 'K COUNT')
                scale = 1e-3
            if desiredParam == BIOM:
                self.UpdateEntry(self.table[0][0], 'K metric tons')
                scale = 1e-3
            if desiredParam == EBMS:
                self.UpdateEntry(self.table[0][0], 'metric tons')
                scale = 1.0
            if desiredParam == FEFF:
                self.UpdateEntry(self.table[0][0], 'average')
                scale = 1.0
            if desiredParam == FMOR:
                self.UpdateEntry(self.table[0][0], 'average')
                scale = 1.0
            if desiredParam == LAND:
                self.UpdateEntry(self.table[0][0], 'COUNT')
                scale = 1.0
            if desiredParam == LNDW:
                self.UpdateEntry(self.table[0][0], 'grams')
                scale = 1.0
            if desiredParam == LPUE:
                self.UpdateEntry(self.table[0][0], 'land/day')
                scale = 1.0
            if desiredParam == RECR:
                self.UpdateEntry(self.table[0][0], 'g/m2' )
                scale = 1.0

            for row in range(self.numAreas):
                for col in range(self.numYears):
                    self.table[row+1][col+1].delete(0,tk.END)
                    # round to 4 decimal places
                    # round(x,4) can have unpredicable results, use simple math instead
                    r = 1e4
                    if desiredParam == BIOM:
                        # convert to metric tons = 1e6 grams
                        # BIOM is in g/m2
                        # g/m2 * km2 * (1e6 m2/km2) / 1e6
                        # mt = g/m2 * km2
                        areaKm2 = self.shape[row].areaKm2
                        accumParamData[row][col] = accumParamData[row][col] * areaKm2 
                    
                    if desiredParam == FEFF or desiredParam == FMOR:
                        # compute averate
                        y = int((accumParamData[row][col] * r * scale / countData[row][col]) + 0.5) / r
                    else:
                        y = int((accumParamData[row][col] * r * scale) + 0.5) / r
                    self.table[row+1][col+1].insert(0, str(y))

        else:
            messagebox.showerror("Reading Parameter File", f'No data for '+fileName+'\nHas Simulation been run?\nAre years correct?')
            runSortErrors = 1

        return runSortErrors
    
    ##
    #
    def UpdateEntry(self, entry, val):
        entry.delete(0,tk.END)
        entry.insert(0, val)

    ##
    #
    def UpdateWidgets(self):
        f = self.exportFileName.split('/')
        self.UpdateEntry(self.exportFileEntry, f[-1])

    ##
    #
    def BrowseExportFile(self):
        # Note extension is not included so that output parameter name can be appende
        file_path = filedialog.asksaveasfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], initialdir=self.resultsDir)
        if file_path:
            f = file_path.split('/')
            self.UpdateEntry(self.exportFileEntry, f[-1])
            self.exportFileName = file_path

    ## This method exports ouput parameter table to its own file name
    #
    def ExportThis(self, nomsg=False):
        if self.exportFileName != '':
            if not nomsg: messagebox.showinfo('Export This', f'Starting Export')
            # Ensure data is up to date
            err = self.RunSort()

            if err == 0:
                # get current parameter
                outStr = self.comboParameter.get()
                print('Sorting: ', outStr)
                exportFileName = self.exportFileName + '_' + outStr + '.csv'
                with open(exportFileName, 'w') as f:
                    # +1 is to include column and row headers
                    for a in range(self.numAreas+1):
                        for yr in range(self.numYears+1):
                            f.write(self.table[a][yr].get() + ',')
                        f.write('\n')
                    f.close()
                if not nomsg: messagebox.showinfo('Export This', f'FILE SAVED: {exportFileName}')
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
                messagebox.showinfo('Export ALL', f'Starting Export')
                for cb in range(n):
                    self.comboParameter.current(cb)
                    self.ExportThis(True)
                messagebox.showinfo('Export ALL', f'Complete')

            else: # data files have not yet been generated.
                 messagebox.showerror('Export All', 'Data Files do not exist. START Sim')

        else:
            messagebox.showerror('Export All', 'File Name has not been defined.')

    ##
    # @brief Gets shape data and places it into a array of GeoShape
    #
    # MA regions, if selected, are first placed into array followed by GB regions, selected.
    # Logic will always have either MA, GB, or both
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
        about = '''Sort By Region
    (This frame is scrollable, use mouse wheel)
    This frame allows the user to determine the amount of a given parameter
    that is located in a specific area defined by long, lat coordinates 
    provided in the shape files defined on the Main Tab

Output Parameters
    This is a dropbox of the selected output parameters on the main tab. After 
    a simulation and interpolation have been run, the user would select one of
    these output, click Run Sort, and the amount of that output in each of the
    defined areas is accumulated by year to the left of each area.

Run Sort
    This will start the program to check if a region grid value for a given 
    year is within one of the specified area and if so accumulate the year
    sum with that value.

Export Results
    These buttons will export data to a CSV file formated the same as the table

    Export This
        This button will export the data set to the file name in Export File.
        It will first call the RunSort method to enure the data is up to date.
        Just this data with file appended the current combo box setting

    Export All
        This button will step through each of the Output Parameters given in 
        the combo box, call RunSort for that parameter, and then export the
        parameter CSV file.

    Browse For Export File
        Allows user to select an existing file name or enter their own. The
        file is not saved until one of the Export buttons is clicked.

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
