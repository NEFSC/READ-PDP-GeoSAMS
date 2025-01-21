## @page SortRegion  Sort By Area Frame
# Assists the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest.
#
# @section SortRegion p2 Output Parameters
#
# This is a dropbox of the selected output parameters on the main tab. After 
# a simulation and interpolation have been run, the user would select one of
# these output, click Run Sort, and the amount of that output in each of the
# shapefile regions is accumulated by year.
#
# @section SortRegion p4 Run Sort
# This will start the program to check if a region grid value for a given 
# year is within one of the specified area and if so accumulate the year
# sum with that value.
#
import os
import sys
sys.path.append("PythonScripts/GUI/GeoSAM/PyshpMaster")
import shapefile
import utm
import pandas as pd

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from PointInPolygon import *
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
        self.Zone=''
        self.area = 0.0

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

        self.zones = []
        self.maxAreas = maxAreas
        self.maxYears = maxYears
        self.numAreas = 1
        self.numCorners = 1
        self.paramStr = paramStr
        self.areaKm2 = []

        self.yearStart = int(self.friend.startYr.myEntry.get())
        self.yearStop = int(self.friend.stopYr.myEntry.get())
        # year end growth data + initial state
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
        self.numAreas = self.GetZoneData()
        self.areaKm2 = [0.0 for _ in range(self.numAreas)]

        self.tableRows = self.maxAreas+1 # number of areas plus header
        self.tableCols = self.maxYears+2 # one for each year, +1 for header, +1 for area

        # code for creating table
        #                   +1 for row header, +1 for region area     +1 for column header
        self.firstYrCol = 2
        self.table = [[0 for _ in range(self.tableCols)] for _ in range(self.tableRows)]
        for row in range(self.tableRows):
            for col in range(self.tableCols):
                if col==0: self.table[row][col] = ttk.Entry(self.sortAreaFrame, width=15)
                elif row==0: self.table[row][col] = ttk.Entry(self.sortAreaFrame, width=15, justify='center')
                else: self.table[row][col] = ttk.Entry(self.sortAreaFrame, width=15, justify='right')
                self.table[row][col].grid(row=row+4, column=col)
        for row in range(self.numAreas):
            self.table[row+1][0].insert(0,self.zones[row])
            self.table[row+1][1].insert(0,f'{self.areaKm2[row]:.4f}')
        self.table[0][1].insert(0,'Area (Km^2)')
        for col in range(self.firstYrCol+1, self.numYears+self.firstYrCol):
            self.table[0][col].insert(0,str(self.yearStart+col-self.firstYrCol))
        self.table[0][self.firstYrCol].insert(0, 'Initial State')

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

        self.yearStart = int(self.friend.startYr.myEntry.get())
        self.yearStop = int(self.friend.stopYr.myEntry.get())
        self.domainName = self.friend.domainNameCombo.get()
        self.numYears = self.yearStop - self.yearStart + 1

        self.numAreas = self.GetZoneData() 

        for row in range(self.tableRows):
            for col in range(self.tableCols):
                self.table[row][col].grid()
        for row in range(self.numAreas):
            UpdateEntry(self.table[row+1][0], self.zones[row])

        # remove unused columns
        for row in range(self.numAreas):
            for col in range(self.numYears, self.maxYears):
                self.table[row+1][col+self.firstYrCol].grid_remove()

        # remove unused rows
        for row in range(self.numAreas, self.maxAreas):
            for col in range(self.tableCols):
                self.table[row+1][col].grid_remove()

        # restore col header
        for col in range(self.numYears):
            self.table[0][col+self.firstYrCol].grid()
        # clear col header
        for col in range(self.numYears, self.maxYears):
            self.table[0][col+self.firstYrCol].grid_remove()
        # restore row header
        for row in range(self.numAreas):
            self.table[row+1][0].grid()
        # clear row header
        for row in range(self.numAreas, self.maxAreas):
            self.table[row+1][0].grid_remove()

        self.UpdateWidgets()

    ##
    # @brief Max Number of years has increased, need to add additional columns
    def AppendYears(self, numYears):
        addYears = numYears - self.maxYears
        for row in range(self.tableRows):
            for col in range(addYears):
                if row == 0: self.table[row].append(ttk.Entry(self.sortAreaFrame, width=15, justify='center'))
                else: self.table[row].append(ttk.Entry(self.sortAreaFrame, width=15, justify='right'))
                self.table[row][self.maxYears+col+self.firstYrCol].grid(row=row+4, column=self.maxYears+col+self.firstYrCol)
        for col in range(self.numYears, self.maxYears+addYears):
            self.table[0][col+self.firstYrCol].insert(0,str(self.yearStart+col))

        self.maxYears = numYears
        self.numYears = numYears
        self.tableRows = self.maxAreas+1 # number of areas plus header
        self.tableCols = self.maxYears+2 # one for each year, +1 for header, +1 for area
        # ensure all cells are now visible
        for row in range(self.tableRows):
            for col in range(self.tableCols):
                self.table[row][col].grid()

    ##
    #
    def RunSort(self, showMsg=True):
        runSortErrors = 0
        paramData = [0.0 for _ in range(self.numYears)] # data read in from file
        rows = self.numAreas
        cols = self.numYears
        accumParamData = [[0.0 for _ in range(cols)] for _ in range(rows)] # accumulated data if in region
        # Used to compute average of data
        countNonZeroData = [[0.0 for _ in range(cols)] for _ in range(rows)]  # data read in from file
        countData = [[0.0 for _ in range(cols)] for _ in range(rows)]  # data read in from file
        desiredParam = self.comboParameter.get()
        years = range(self.yearStart, self.yearStop + 1) # year_start is initial state
        # typical name: BIOM_2022_KRIGE - BIOM_2026_KRIGE          Lat_Lon_Grid_EBMS_MA_2015_2017

        # build list of files to consider
        fileList = []
        dfAgg = pd.DataFrame()
        for year in years:
            fileName = os.path.join('Results', desiredParam + str(year) + '_KRIGE.csv')
            fileList.append(fileName)
            if os.path.isfile(fileName):
                df = pd.read_csv(fileName)
                dfAgg[str(year)] = df.groupby('ZONE').agg({'FINALPREDICT':'sum'})
            else:
                messagebox.showerror("Reading Parameter File", f'No data for '+fileName+'\nHas Simulation been run?\nAre years correct?')
                runSortErrors = 1

        if runSortErrors == 0:
            # For each area
            for row in range(self.numAreas):
                for col in range(self.numYears):
                    df = pd.read_csv(fileList[col])
                    accumParamData[row][col] = dfAgg[str(col+self.yearStart)][self.zones[row]]
                    countData[row][col] = len(df[df['ZONE']==self.zones[row]])

                    # Determine number of non-zero data by first counting the number of data counts
                    # then subracting the number of zero counts
                    counts =  df[df['ZONE']==self.zones[row]]['FINALPREDICT'].value_counts()
                    try:
                        countNonZeroData[row][col] = countData[row][col] - counts[0]
                    except:
                        countNonZeroData[row][col] = countData[row][col]

            # display results
            (units, scale) = DetermineUnitsScale(desiredParam)
            UpdateEntry(self.table[0][0], units)

            for row in range(self.numAreas):
                ##areaKm2 = self.shape[row].area
                # Shape area not populated in shape file, use computed area
                #gridKm2 = countData[row][0] * grid_area_sqm / 1.0e6
                self.areaKm2[row] = countData[row][0] * grid_area_sqm / 1.0e6
                UpdateEntry(self.table[row+1][1], f'{self.areaKm2[row]:.4f}')
                # print(row, ',', areaKm2, ',', gridKm2)
                for col in range(self.numYears):
                    if desiredParam == BIOM:
                        # convert to metric tons = 1e6 grams
                        # BIOM is in g/m2
                        # mt = 1e6 g
                        # g/m2 * km2 * (1e6 m2/km2) / 1e6 g
                        # mt = g/m2 * km2
                        ### accumParamData[row][col] = accumParamData[row][col] * areaKm2 / countData[row][col]
                        ### OR approximately 
                        ### accumParamData[row][col] = accumParamData[row][col] * gridKm2 / countData[row][col]
                        ### WHICH IS
                        ### accumParamData[row][col] = accumParamData[row][col] * (countData[row][0] * grid_area_sqm / 1.0e6) / countData[row][col]
                        accumParamData[row][col] = accumParamData[row][col] * grid_area_sqm / 1.0e6
                    
                    if desiredParam == FEFF or desiredParam == FMOR:
                        # compute average
                        if countNonZeroData[row][col] > 0: accumParamData[row][col] = accumParamData[row][col] / countNonZeroData[row][col]
                    
                    if desiredParam == ABUN or desiredParam == RECR:
                        # compute density average
                        accumParamData[row][col] = accumParamData[row][col] / countData[row][col]

                    # round to 4 decimal places
                    UpdateEntry(self.table[row+1][col+self.firstYrCol], f'{(accumParamData[row][col] * scale):.4f}')

        else:
            messagebox.showerror("Reading Parameter File", f'No data for '+fileName+'\nHas Simulation been run?\nAre years correct?')
            runSortErrors = 1

        return runSortErrors
    
    ##
    #
    def UpdateWidgets(self):
        f = self.exportFileName.split('/')
        UpdateEntry(self.exportFileEntry, f[-1])

    ##
    #
    def BrowseExportFile(self):
        # Note extension is not included so that output parameter name can be appended
        file_path = filedialog.asksaveasfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], initialdir=self.resultsDir)
        if file_path:
            f = file_path.split('/')
            UpdateEntry(self.exportFileEntry, f[-1])
            self.exportFileName = file_path

    ## This method exports ouput parameter table to its own file name
    #
    def ExportThis(self, exportingAll=False, showMsg=True):
        if self.exportFileName != '':
            # Ensure data is up to date
            err = self.RunSort(showMsg=False)

            if err == 0:
                # get current parameter
                outStr = self.comboParameter.get()
                #print('Sorted: ', outStr)
                if exportingAll: ShowMessage(heading='Export All', message='Sorted: '+outStr)

                exportFileName = self.exportFileName + '_' + outStr + '.csv'
                with open(exportFileName, 'w') as f:
                    # +1 is to include column and row headers
                    for a in range(self.numAreas+1):
                        for yr in range(self.numYears+1):
                            f.write(self.table[a][yr].get() + ',')
                        f.write(self.table[a][self.numYears+1].get())
                        f.write('\n')
                    f.close()
                if showMsg: messagebox.showinfo('Export This', f'FILE SAVED: {exportFileName}')
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
                for cb in range(n):
                    self.comboParameter.current(cb)
                    self.ExportThis(exportingAll=True, showMsg=False)
                messagebox.showinfo('Export ALL', 'Complete')

            else: # data files have not yet been generated.
                 messagebox.showerror('Export All', 'Data Files do not exist. START Sim')

        else:
            messagebox.showerror('Export All', 'File Name has not been defined.')

    ##
    # @brief Gets shape data and places it into a array of GeoShape
    #
    # MA regions, if selected, are first placed into array followed by GB regions, selected.
    # Logic will always have either MA, GB, or both
    def GetZoneData(self):
        df = pd.read_csv(os.path.join('Grids', 'GBRegionGrid.csv'),na_filter=False)
        zonesGB=sorted(df['ZONE'].unique())
        zonesGB = list(filter(lambda x: (x!='NA'),zonesGB))

        df = pd.read_csv(os.path.join('Grids','MARegionGrid.csv'),na_filter=False)
        zonesMA=sorted(df['ZONE'].unique())
        zonesMA = list(filter(lambda x: (x!='NA'),zonesMA))

        self.zones = sorted(zonesGB + zonesMA)
        
        return len(self.zones)
    
    ## 
    # determine Management Area Index
    def DetermineMgmtAreaIndex(self, region):
        mgmtAreaIndx = 21
        if (region == "BI"):
            mgmtAreaIndx = 1
        elif (region == "CL1-Access"):
            mgmtAreaIndx = 2
        elif (region == "CL1-Sliver"):
            mgmtAreaIndx = 3
        elif (region == "CL1-South"):
            mgmtAreaIndx = 4
        elif (region == "CL2-Access"):
            mgmtAreaIndx = 5
        elif (region == "CL2-Ext"):
            mgmtAreaIndx = 6
        elif (region == "CL2-North"):
            mgmtAreaIndx = 7
        elif (region == "DMV"):
            mgmtAreaIndx = 8
        elif (region == "ET"):
            mgmtAreaIndx = 9
        elif (region == "GSC"):
            mgmtAreaIndx = 10
        elif (region == "HCS"):
            mgmtAreaIndx = 11
        elif (region == "LI"):
            mgmtAreaIndx = 12
        elif (region == "MAB-Nearshore"):
            mgmtAreaIndx = 13
        elif (region == "NF"):
            mgmtAreaIndx = 14
        elif (region == "NLS-North"):
            mgmtAreaIndx = 15
        elif (region == "NLS-South"):
            mgmtAreaIndx = 16
        elif (region == "NLS-West"):
            mgmtAreaIndx = 17
        elif (region == "NYB"):
            mgmtAreaIndx = 18
        elif (region == "SF"):
            mgmtAreaIndx = 19
        elif (region == "VIR"):
            mgmtAreaIndx = 20
        else:
            mgmtAreaIndx = 21
        return mgmtAreaIndx

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
        file is not saved until one of the Export buttons is clicked. The 
        parameter is appended to the name chosen by the user and csv extension.
        For example: <UserName>_ABUN_.csv

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
