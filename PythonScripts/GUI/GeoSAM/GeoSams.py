##
# @mainpage GeoSAMS GUI
#
# This is the main program for the GeoSAMS GUI
# 
# The GUI has 7 tabs
# -# <b>Main</b>: Data concerning simulation duration, configutation files in use, and recruitment period
# -# <b>Growth</b>: Define parameters to compute fishing mortality
# -# <b>Special Access</b>: Files used to define special areas for fishing management
# -# <b>Fishing Mort in Special Access</b>: This frame in conjunction with the Special Access Frame is used to define 
#    fishing mortalities within a defined area for a specified year. If a 
#    location falls within the defined area given by the area defintions in 
#    Special Access Frame and assigned the area number.
# -# <b>Sort By Area</b>: Parameters that are used to sort output data and associate with areas of interest
# -# <b>UKInterpolation</b>: Parameters that are used to interpolate results from survey grid to regional grid
# -# <b>Math Setup</b>: This frame allows the user to modify the Matlab/Octave startup files.
#
# The program is started by entering the following command in the root directory of the workspace
# $ python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py [10 8]
#
# Where the last three number are optional and used to set limits on:
# - The maximum number of areas of interest that can be defined.
# - The maximum number of nodes used to specify each area of interest.
# - The maximum number of years that the simulation can cover, i.e. Stop Year - Start Year + 1
#
# When commanded without these values the GUI defaults to 10, 8, and 5. These values can be viewed by 
# clicking the <b>SHOW Args</b> Button
#
# @section p1p1 SHOW Args
# As already mention this button is used to show the setup parameters that the GUI is using for
# maximum number of areas, nodes, and years
#
# @section p1p2 START Sim
# This button will start both the GeoSAMS sim and if succsessful continue with the UK interpolation.
# It does so by first saving the data contained in the other tabs of the GUI to configuraton files specified 
# on this page. It will overwrite the files named if they already exist. 
#
# NOTE: The file names listed are part of the package installed when downloaded from GitHub.
# The user may change these names to preserve the original files. Or reinstall from GitHub to 
# restore the original data.
#
# @section p1p3 SAVE ALL Configs
# This is the first step in <i><b>START Sim</b></i>. This button will save all of the configuration files using
# the names given. 
#
# @section p1p4 Year in file names
# GUI specifies 2022 to 2025
# - X_Y_BIOM_2022_DN  Initial state as of June 1, 2022 @ 00:00, i.e. May 31, 2022 @ 24:00
# - X_Y_BIOM_2023_DN  Growth state as of May 31, 2023 @ 24:00, results for 1st year growth
# - X_Y_BIOM_2024_DN  Growth state as of May 31, 2024 @ 24:00, results for 2nd year growth
# - X_Y_BIOM_2025_DN  Growth state as of May 31, 2025 @ 24:00, results for 3rd year growth
# - X_Y_BIOM_2026_DN  Growth state as of May 31, 2026 @ 24:00, results for 4th year growth

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
import pandas as pd

import subprocess
import sys
import os
import glob
import sys
import platform
import csv
import multiprocessing
import time
from collections import defaultdict
from multiprocessing import Process
from datetime import datetime

from MainInputFrame import *
from GrowthFrame import *
from InterpolationFrame import *
from SortByAreaFrame import *
from SortByRegionFrame import *
from SpecialAreaFrame import *
from FishMortBySpecAcc import *
from EditMathSetupFrame import *
from Globals import *

# [            ex,              obsFile,                            gridFile
# Rscript .\GAM\R_GAM_GeoSAMS.r X_Y_<param>_AL<yyyy>_<MA|GB>_BUFFER <MA|GB>RegionGrid.csv 
#  arg#        1     2                         3                    4
def ComputeResiduals(obsFile, gridFile, procID, retDict):
    ex = os.path.join('GAM', 'R_GAM_GeoSAMS.r')
    outputFile = 'proc_' + str(procID).zfill(3) + '_' + obsFile + '.txt'
    outputFile = os.path.join(analDir, outputFile)
    cmd = ['Rscript', ex, obsFile, gridFile ]
    with open(outputFile, "w") as outfile:
        result = subprocess.run(cmd, stdout=outfile, stderr=outfile)
    retDict[procID] = result.returncode

##
# This class is the parent class for the GUI
#
class MainApplication(tk.Tk):
    def __init__(self, title, maxAreas, maxCorners, maxYears):
        super().__init__()

        # member variables
        self.monDict = {'JAN':0, 'FEB':1, 'MAR':2, 'APR':3, 'MAY':4, 'JUN':5, 'JUL':6, 'AUG':7, 'SEP':8, 'OCT':9, 'NOV':10, 'DEC':11} 
        self.daysInYear = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]

        self.maxAreas = maxAreas
        self.maxCorners = maxCorners
        self.maxYears = maxYears
        self.addFrameClicked = False
        self.tsPerYear = 0
        self.paramVal = 0
        self.paramStr = []
        self.useHabCamData = False

        # Sim settings
        self.domainName = ''
        self.yearStart=0
        self.yearStop=0
        self.simConfigFile=''

        # setup
        self.title(title)
        self.geometry(geometryStr)
        self.style = ttk.Style()
        # Inheritable frame, label defintions
        self.style.configure('SAMS.TFrame', borderwidth=1, relief='solid', labelmargins=20)
        self.style.configure('SAMS.TFrame.Label', font=('courier', 10, 'bold'))
        self.style.configure("BtnGreen.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("BtnLime.TLabel", padding=6, relief='raised', background="#d3eb00")
        self.style.configure("BtnBluGrn.TLabel", padding=6, relief='raised', background="#0FF")
        self.style.configure("BtnMaize.TLabel", padding=6, relief='raised', background="#cf34eb")
        self.style.configure("Help.TLabel", padding=6, relief='raised', foreground='blue', background="#42e6f5")


        # vscode will not set this variable, must be done via control panel
        # in command terminal on Windows, assuming user is in the install directory
        # Now using cwd assuming python is started from there.
        #   > set ROOT=%CD%
        self.root = os.getcwd() #os.environ['ROOT']

        # Read in Sim Configuration File to set default output selection
        self.simConfigFile  = os.path.join(self.root,configDir, simCfgDir,'Scallop.cfg')
        self.ReadSimConfigFile()
        self.notebook = ttk.Notebook(self)

        self.frame1 = MainInput(self.notebook, self, self.tsPerYear, self.paramVal, self.maxYears)
        # NOTE: These will still be default values as the user would not as yet entered anything!!
        self.simConfigFile  = os.path.join(self.root,configDir, simCfgDir, self.frame1.simCfgFile.myEntry.get())
        self.recrConfigFile = os.path.join(self.root,configDir, simCfgDir, self.frame1.recrCfgFile.myEntry.get())

        self.frame2 = EditMathSetup(self.notebook)
        self.frame3 = Growth(self.notebook, self)
        self.frame5 = SortByArea(self.notebook, self.frame1, self.maxAreas, self.maxCorners, self.maxYears, self.paramStr)
        self.frame6 = SpecialArea(self.notebook, self, self.maxAreas, self.maxCorners)
        self.frame7 = FishMortBySpecAcc(self.notebook, self.maxAreas, self.maxCorners)
        self.frame8 = SortByRegion(self.notebook, self.frame1, self.maxAreas, self.maxYears, self.paramStr)

        self.gmConfigFile = os.path.join(self.root,configDir, simCfgDir, self.frame6.gmCfgFile.myEntry.get())

        # Update strings based on given configuration files
        self.frame7.fishMortFile.myEntry.insert(0, self.frame3.fmortFileName)
        self.ReadGridMgrConfigFile()
        self.frame6.specAccFile.myEntry.insert(0, self.specAccFileStr)

        self.notebook.add(self.frame2, text='Math Setup')
        self.notebook.add(self.frame1, text='Main')
        self.notebook.add(self.frame3, text='Growth')
        self.notebook.add(self.frame6, text='Special Access')
        self.notebook.add(self.frame7, text='Fishing Mort in\n Special Access')
        self.notebook.add(self.frame5, text='SortByArea')
        self.notebook.add(self.frame8, text='SortByRegion')
        self.notebook.pack()

        self.isSkip = False
        self.skipStatusMsgs = tk.BooleanVar(self, False)
        ttk.Button(self, text='SHOW Args',    style="BtnGreen.TLabel", command=self.ShowArgs).place(relx=0, rely=1, anchor='sw')
        self.skipStatusMsgsRB = ttk.Radiobutton(self, text='Skip Status Msgs', value=True, variable=self.skipStatusMsgs, command=self.ToggleSkipStatusMsgs).place(relx=0.35, rely=1, anchor='s')
        ttk.Button(self, text='START Sim',    style="BtnGreen.TLabel", command=self.Run_Sim).place(relx=.25, rely=1, anchor='s')
        ttk.Button(self, text='SAVE ALL Configs', style="BtnGreen.TLabel", command=self.SaveConfigFiles).place(relx=.5, rely=1, anchor='s')
        ttk.Button(self, text= "Help", style="Help.TLabel", command = self.pop_up).place(relx=.75, rely=1, anchor='s')

        self.ReadRecruitConfigFile() # update current recruitment settings
    ##
    #
    def ToggleSkipStatusMsgs(self):
        self.isSkip = not self.isSkip
        self.skipStatusMsgs.set(self.isSkip)

    ##
    # Display setup limits here
    # Messagebox blocks entry widgets if attempted to open before the main window completes.
    #
    def ShowArgs(self):
        messagebox.showinfo("GeoSAMS",f'Using these parameters\n\
        Max Areas of Interest: {self.maxAreas}\n\
        Max Nodes in Areas: {self.maxCorners}')

    ## 
    # Starts the GeoSAMS simulatation <b>ScallopPopDensity</b>. 
    #
    # 1a) TrawlData5mmbin(Year, 'DN')   this will Delete bin5mm2022AL.csv
    #     INPUT: OriginalData\dredgetowbysize7917.csv
    #     OUTPUT: Data\bin5mm<yyyy><dn>.csv
        
    # 1b) HabCamData5mmbin(Year, 'DN')
    #     INPUT: OriginalData\Habcam_BySegment_2000_2011-2023_v2.csv
    #     OUTPUT: appends to Data\bin5mm<yyyy><dn>.csv
        
    # 2) \SRC\ScallopPopDensity.exe Scallop.cfg 2022 2025 AL
    #     INPUT: Data\bin5mm2022AL.csv
    #     OUTPUT: Data\X_Y_<param>_AL<yyyy>_<ma|gb>.csv
        
    # 3) For all YEARS, all PARAMS, MA|GB:
    #     python .\PythonScripts\GUI\GeoSAM\SortIntoColumns.py X_Y_<param>_AL<yyy>_[MA|GB]
    #     INPUT: Data\X_Y_<param>_AL<yyyy>_<ma|gb>.csv
    #     OUTPUT: Data\X_Y_<param>_AL<yyyy>_<ma|gb>_BUFFER.csv

    # 4) For all YEARS, all PARAMS, MA|GB: 
    #     Rscript .\GAM\R_GAM_GeoSAMS.r X_Y_<param>_AL<yyyy>_<MA|GB>_BUFFER <MA|GB>RegionGrid.csv 
    #     INPUT: Data\X_Y_<param>_AL<yyyy>_<ma|gb>_BUFFER.csv
    #     INPUT: Grids\<ma|gb>RegionGrid.csv
    #     OUTPUT:
    #         Residuals: Data\X_Y_<param>_AL<yyyy>_<ma|gb>_BUFFER_RESID.csv
    #         Predicts:  Data\X_Y_<param>_AL<yyyy>_<ma|gb>_BUFFER_PRED.csv
            
    # 5) For all YEARS, all PARAMS, MA|GB: 
    #     python python .\PythonScripts\OK\pykrige_geosams.py X_Y_<yyyy>_AL<yyyy>_<ma|gb>
    #     INPUT: X_Y_<yyyy>_AL<yyyy>_<ma|gb>
    #     ADJUSTED TO:
    #         Survey Residuals: Data\X_Y_<param>_AL<yyyy>_<ma|gb>_BUFFER_RESID.csv  X_Y_BIOM_AL2022_GB_BUFFER_GAM.csv'
    #         Grid Predicts:    Data\X_Y_<param>_AL<yyyy>_<ma|gb>_BUFFER_PRED.csv   GBxyzLatLonRgn_REGION_GAM.csv
    #     OUTPUT:   Results\Lat_Lon_Grid_<param>_AL<yyyy>_<ma|gb>_REGION_KRIGE.csv  GBxyzLatLonRgn_REGION_GAM_KRIGE_SPH.csv


    def Run_Sim(self):
        # Set Environment Variables
        self.frame1.SetDredgeFileEnvVar()
        self.frame1.SetHabCamFileEnvVar()
        self.frame1.SetMaShapeFileEnvVar()
        self.frame1.SetMaShapeBufferFileEnvVar()
        self.frame1.SetGBShapeFileEnvVar()
        self.frame1.SetGBShapeBufferFileEnvVar()
        # No check for variables changed, therefore update all configuration files with current values in GUI
        # OR
        # Create new files based on names given by user, or same if not changed
        self.SaveConfigFiles()
        # exec to be called, ScallopPopDensity, prepends directory structure, ReadSimConfigFile does not
        simConfigFile = self.frame1.simCfgFile.myEntry.get()
        self.simConfigFile = os.path.join(configDir, simCfgDir, simConfigFile)

        # exec to be called, UK, prepends directory structure

        startYear = self.frame1.startYr.myEntry.get()
        stopYear = self.frame1.stopYr.myEntry.get()
        self.domainName = self.frame1.domainNameCombo.get()
        recruitYrStrt = self.frame1.recrYrStrt.myEntry.get()
        recruitYrStop = self.frame1.recrYrStop.myEntry.get()
        # check range
        self.yearStart = int(startYear)
        self.yearStop = int(stopYear)
        self.recruitYrStrt = recruitYrStrt
        self.recruitYrStop = recruitYrStop
        
        # Typical data file name: Data/bin5mm2015AL.csv
        # Delete any existing files, want a clean slate
        for yr in range(self.yearStart, self.yearStop):
            dataFName = os.path.join(self.root, 'Data', 'bin5mm'+str(yr)+self.domainName+'.csv')
            if os.path.isfile(dataFName): 
                os.remove(dataFName)
            dataFName = os.path.join(self.root, 'Data', 'Recruits'+str(yr)+self.domainName+'.csv')
            if os.path.isfile(dataFName): 
                os.remove(dataFName)

        filesExist = False
        if self.frame2.usingMatlab.get():
            mathArg = 'M'
        else:
            mathArg = 'O'

        ######################################################################################################################################################################            
        # Unpack includes:
        # 1a) TrawlData5mmbin(Year, 'DN')
        # 1b) HabCamData5mmbin(Year, 'DN')
        if platform.system() == 'Windows':
            cmd = [os.path.join(self.root, 'Unpack.bat'), str(self.yearStart), self.recruitYrStrt, self.recruitYrStop, self.domainName, mathArg]
        else:
            cmd = [os.path.join(self.root, 'Unpack.sh'), str(self.yearStart), self.recruitYrStrt, self.recruitYrStop, self.domainName, mathArg]
            subprocess.run(['chmod','744','Unpack.sh']) # make file executable
        if not self.skipStatusMsgs.get(): messagebox.showinfo("Unpack", f'Starting Unpack.\nThis could take several minutes, longer if using Octave.\nPlease be patient.')
        result = subprocess.run(cmd)
        if result.returncode == 0:
            if not self.skipStatusMsgs.get(): messagebox.showinfo("Unpack", f'Completed Successfully\n{result.args}')
            filesExist = True
        else:
            if result.returncode == 1:
                messagebox.showerror("TrawlData5mmbin", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
            elif result.returncode == 3:
                messagebox.showerror("PullOutProcessRecruitData", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
            elif result.returncode == 4:
                messagebox.showerror("NearestNeighborRecInterp", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
            else:
                messagebox.showerror("Unpack", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
            
        ######################################################################################################################################################################            
        # 2) \SRC\ScallopPopDensity.exe Scallop.cfg 2022 2025 AL
        if filesExist:
            # Continue with starting the GeoSAMS simulation ----------------------------------------------------------------------
            # 
            # typical command line:
            # > ./SRC/ScallopPopDensity.exe Scallop.cfg StartYear StopYear Domain
            ex = os.path.join('SRC', 'ScallopPopDensity')
            # ScallopPopDensity 
            #  - prepends directory structure to simConfigFile
            #  - still interprets year as start of year, so deduct 1 from stop year
            cmd = [ex, simConfigFile, str(self.yearStart), str(self.yearStop-1), self.domainName]
            print(cmd)
            if not self.skipStatusMsgs.get(): messagebox.showinfo("GeoSAMS Sim", f"Program Started: {cmd}")
            result = subprocess.run(cmd)

            if result.returncode == 0:
                if not self.skipStatusMsgs.get(): messagebox.showinfo("GeoSAM Sim", 
                    f'Completed Successfully\n{result.args}\nStarting Interp&Plot\nIf all output selected this will run over an hour.\nPlease be patient.')

                # Then Continue with Interpolation and Plotting Results -----------------------------------------------------------
                (retDict, procID) = self.InterpAndPlotResults()
                if sum(retDict.values()) == 0:
                    messagebox.showinfo("GeoSAMS/Interp/Plotting", f'ALL DONE\nRan {procID} processes')
                else:
                    messagebox.showerror("InterpAndPlotResults", 'Failed')
            else:
                messagebox.showerror("GeoSAM Sim", f'Failed\n{result.args}\nReturn Code = {result.returncode}')
    
    ##
    def InterpAndPlotResults(self):
        rets = 0
        # year_start is initial state, yearStop represents the last year of simulation output
        years = range(self.yearStart, self.yearStop+1)  

        # Used while concatenating files
        # number of colums in csv file, starting at 0
        # lat, lon, initial data
        ncols = self.yearStop - self.yearStart + 2
        # number of years plus initial state
        nyears = self.yearStop - self.yearStart + 1

        self.ReadSimConfigFile()

        # Determine which if any file suffixes are needed
        if self.domainName=='GB':
            # region = ['_SW', '_N', '_S', '_W']
            region = ['_GB']
        elif self.domainName=='MA':
            region = ['_MA']
        else:
            # This would be AL
            #region = ['_SW', '_N', '_S', '_W', '_MA']
            region = ['_GB', '_MA']

        # setting up for muli-core processing
        numCores = multiprocessing.cpu_count()
        if numCores > 3:
            numCores -= 2      # number of available cores, ran out of memory when using all available
        procNum  = 0           # number of concurrent processes, i.e. 1 to numCores
        procID   = 0           # unique number to identify each independent process
        procs    = []          # holds Proccess Class instantiation
        manager = multiprocessing.Manager()
        retDict = manager.dict()
        # clean up leftover files from prior run
        for f in glob.glob(os.path.join(analDir, '*.txt')):
            os.remove(f)
        start = time.time()

        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        #  COMPUTE RESIDUALS, INTERPOLATE AND CONCATENATE REGIONS INTO SINGLE FILE
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Compute Residuals
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        for pStr in self.paramStr:
            for r in region:    # typically  region = ['_GB', '_MA']
                if self.domainName == 'AL':
                    # ALxyzLatLon_MA uses the same grid file as MA, 
                    # and did not want to configuration manage multiple identical files
                    #     'MA'+'xyzLatLon' + '' + '.csv'
                    # ALxyzLatLon_SW to ALxyzLatLon_W use the same grid files as GB
                    # and did not want to configuration manage multiple identical files
                    # 'GBxyzLatLon' + r + '.csv'
                    #
                    # These data files also need to use separate spatial functions
                    # Override command line argument
                    if r == '_MA':
                        gridFile = 'MARegionGrid.csv'
                    else:
                        #gridFile = 'GBxyzLatLon' + r + '.csv'
                        gridFile = 'GBRegionGrid.csv'
                else:
                    gridFile = self.domainName+'RegionGrid.csv'

                for year in years:
                    # 3) For all YEARS, all PARAMS, MA|GB:
                    # First add columns to Data\X_Y_<param>_AL<yyyy>_<ma|gb>.csv
                    ex = os.path.join('PythonScripts', 'GUI', 'GeoSAM', 'SortIntoColumns.py')
                    inputFile = 'X_Y_'+ pStr + self.domainName + str(year) + r
                    if platform.system() == 'Windows':
                        cmd = ['python', ex, inputFile ]
                    else: # TODO depends on how python gets installed.
                        cmd = ['python3', ex, inputFile ]
                    result = subprocess.run(cmd)

                    # 4) For all YEARS, all PARAMS, MA|GB: 
                    #    Rscript .\GAM\R_GAM_GeoSAMS.r X_Y_<param>_AL<yyyy>_<MA|GB>_BUFFER <MA|GB>RegionGrid.csv 
                    obsFile = 'X_Y_' + pStr + self.domainName + str(year) + r + '_BUFFER'
                    procNum += 1
                    procID  += 1
                    proc = Process(target=ComputeResiduals, args=(obsFile, gridFile, procID, retDict))
                    procs.append(proc)
                    proc.start()

                    # calls the join() method on each process, which tells Python to wait for the process to terminate. 
                    if procNum == numCores:
                        for proc in procs:
                            proc.join()
                        
                        # We are not trying to identify which process failed, just that something failed
                        # the user can look into the proc*.txt files to see what failed and why.
                        rets = sum(retDict.values())
                        if rets > 0:
                            return (retDict, 0)
                        
                        procs = []
                        procNum = 0
                        print('Finished set through {}\n'.format(procID))
                # end for year in years
            # end for r in region
        # end for pStr in paramStr

        # finish up any remaining procs
        for proc in procs:
            proc.join()

        # We are not trying to identify which process failed, just that something failed
        # the user can look into the proc*.txt files to see what failed and why.
        rets = sum(retDict.values())
        if rets > 0:
            return (retDict, procID)
        
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Intepolate using OK
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # all survey data residuals computed now to interpolate. 
        # At this point all parallel processing is complete, restart procID counter
        procID = 0
        for pStr in self.paramStr:
            for r in region:
                for year in years:
                    # 5) For all YEARS, all PARAMS, MA|GB: 
                    #    python .\PythonScripts\OK\pykrige_geosams.py X_Y_BIOM_AL2022_GB
                    ex = os.path.join('PythonScripts', 'OK', 'pykrige_geosams.py')
                    inputFile = 'X_Y_'+ pStr + self.domainName + str(year) + r
                    stdOutFile = os.path.join('Analysis', inputFile+'.txt')
                    if platform.system() == 'Windows':
                        cmd = ['python', ex, inputFile ]
                    else: # TODO depends on how python is installed.
                        cmd = ['python3', ex, inputFile ]
                    with open(stdOutFile, 'w') as fid:
                        result = subprocess.run(cmd, stdout=fid)

                    retDict[procID] = result.returncode

                    # could wait until all scripts have run. If failure most likely with python itself
                    # so fail at first occurance.
                    if sum(retDict.values()) != 0:
                        messagebox.showerror("GeoSAM OK", f'Failed\n{result.args}\nReturn Code = {result.returncode}')
                        return (retDict, procID)
            procID += 1

        end = time.time()

        print( 'Interpolation exec time: {}'.format(end-start))

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # PLOTTING
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # We have the needed output paramters so lets plot data and save to pdf files
        for pStr in self.paramStr:
            if self.frame2.usingMatlab.get():
                matlabStr = 'PlotLatLonGrid(' + "'" + pStr + "', " + str(self.yearStart) + "," + str(self.yearStop) +  ", 0);exit"
                cmd = ['matlab.exe', '-batch', matlabStr]
            else:
                cmd = ['octave', 'mfiles/PlotLatLonGrid.m', pStr, str(self.yearStart), str(self.yearStop), '0']

            result = subprocess.run(cmd)
            if (result.returncode != 0):
                errorStr = '[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m'
                print(errorStr)
                procID += 1
                retDict[procID] = result.returncode

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Combine results by year
        #  modeling region could change from one year to another so it is difficult to concatenate all 
        #  files as X-Y positions may differ. 
        # Thus to aid in post processing combine the MA|GB files into a single file for each year
        # Lat_Lon_Grid_ABUN_ALyyyy_GB_REGION_KRIGE \  
        # Lat_Lon_Grid_ABUN_ALyyyy_GB_REGION_KRIGE /  BIOM_yyyy_KRIGE.csv
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        pfix = os.path.join('Results', 'Lat_Lon_Grid_')
        for pStr in self.paramStr:
            for year in years:
                flout = os.path.join('Results', pStr + str(year) + '_KRIGE.csv')
                for r in region:
                    flin = pfix + pStr + self.domainName + str(year) + r + '_REGION_KRIGE.csv'

                    df = pd.read_csv(flin, usecols=['UTM_X', 'UTM_Y', 'LAT','LON', 'DEPTH', 'STRATUM', 'ZONE', 'FINALPREDICT'])

                    if r == region[0]: 
                        df.to_csv(flout, index=False)
                    else:
                        df.to_csv(flout, mode='a', index=False,header=False)

        return (retDict, procID)

    ##
    # Save all of the defined configuration files
    #
    def SaveConfigFiles(self):
        self.WriteScallopConfig()
        self.WriteRecruitmentConfig()
        self.WriteGrowthConfig()
        self.WriteGridMgrConfig()

        # cfgFile  = os.path.join(self.root,configDir, interCfgDir, self.frame4.spatCfgFile.myEntry.get())
        # self.WriteSpatialFncsConfig(cfgFile)
        if not self.skipStatusMsgs.get(): messagebox.showinfo("Save Files", "Configuration Files Saved")

    ##
    # Saves simulation configuration file. It does so by writeing the parameters for the 
    # to the name file as well as keeping helpfule comments.
    #
    def WriteScallopConfig(self):
        simCfgFile  = os.path.join(self.root,configDir, simCfgDir, self.frame1.simCfgFile.myEntry.get())
        with open(simCfgFile, 'w') as f:
            f.write('# input file for Scallops \n')
            f.write('Time steps per Year = ' + str(self.frame1.tsPerYear.myEntry.get())+'\n')
            f.write('# Configuration files are expected to be in the Configuration directory\n')
            f.write('Mortality Config File = '   + self.frame3.growthCfgFile.myEntry.get() + '\n')
            f.write('Recruit Config File = '     + self.frame1.recrCfgFile.myEntry.get() + '\n')
            f.write('Grid Manager Config File = '+ self.frame6.gmCfgFile.myEntry.get()   + '\n')
            f.write('# The following items determine the parameters for output and plotting\n')
            f.write('# One need only select the desired parameters, the default is to not show them\n')
            f.write('# Anything after = is ignored.\n')
            if(self.frame1.abunVar.get()):     f.write('Select Abundance          =# ABUN abundance scallops per square meter\n')
            if(not self.frame1.abunVar.get()): f.write('# Select Abundance          =# ABUN abundance scallops per square meter\n')
            if(self.frame1.bmsVar.get()):      f.write('Select BMS                =# BIOM biomass in gpsm\n')
            if(not self.frame1.bmsVar.get()):  f.write('# Select BMS                =# BIOM biomass in gpsm\n')
            if(self.frame1.ebmsVar.get()):     f.write('Select Expl BMS           =# EBMS exploitable biomass in metric tons\n')
            if(not self.frame1.ebmsVar.get()): f.write('# Select Expl BMS           =# EBMS exploitable biomass in metric tons\n')
            if(self.frame1.feffVar.get()):     f.write('Select Fishing Effort     =# FEFF Fishing Effort\n')
            if(not self.frame1.feffVar.get()): f.write('# Select Fishing Effort     =# FEFF Fishing Effort\n')
            if(self.frame1.fmortVar.get()):    f.write('Select Fishing Mortality  =# FMOR Fishing Mortality\n')
            if(not self.frame1.fmortVar.get()):f.write('# Select Fishing Mortality  =# FMOR Fishing Mortality\n')
            if(self.frame1.landVar.get()):     f.write('Select Landings by Number =# LAND Landings by number of scallops\n')
            if(not self.frame1.landVar.get()): f.write('# Select Landings by Number =# LAND Landings by number of scallops\n')
            if(self.frame1.lndwVar.get()):     f.write('Select Landings by Weight =# LNDW Landings by weight in grams\n')
            if(not self.frame1.lndwVar.get()): f.write('# Select Landings by Weight =# LNDW Landings by weight in grams\n')
            if(self.frame1.lpueVar.get()):     f.write('Select LPUE               =# LPUE Landing Per Unit Effort, (per day)\n')
            if(not self.frame1.lpueVar.get()): f.write('# Select LPUE               =# LPUE Landing Per Unit Effort, (per day)\n')
            if(self.frame1.recrVar.get()):     f.write('Select RECR               =# Recruitment\n')
            if(not self.frame1.recrVar.get()): f.write('# Select RECR               =# Recruitment\n')
            f.close()

    ##
    # This method is used to converty the recruitment start and stop dates from a string month 
    # numerical day into days in a year. Changed entry to combo box to guarantee format
    # @param monthDayStr string that holds month and day in alpha format.
    # That is 'JAN 01'
    #
    # The Growth year starts on June 1st, actually May 31 at 2400
    # Jun  1st @ 0600 is day 0.25 which is = 0.25   /365.2425 = 0.00068 years
    # June 1st @ 1200 is day 0.50 which is = 0.50   /365.2425 = 0.00137      
    # June 1st @ 1800 is day 0.75 which is = 0.75   /365.2425 = 0.00205      
    # June 1st @ 2359 is day 0.99 which is = 0.99931/365.2425 = 0.002736     
    # Jun2 2nd @ 0000 is day   1  which is = 1.00000/365.2425 = 0.00274      
    # Jun2 2nd @ 2400 is day   2  which is = 2.00000/365.2425 = 0.00548      
    # Dec 31st @ 2400 is day 214  which is = 214.   /365.2425 = 0.58591      
    # Jan  1st @ 2400 is day 215  which is = 215.   /365.2425 = 0.58865      
    #         = 1 + DayOfYear(12,31) - DayOfYear(5,31)
    # Apr 10   @ 2400 is day 314 which is  = 314.   /365.2425 = 0.85970      
    #     if leap year       315 which is  = 315.   /365.2425 = 0.86244
    #     However, leap year is handled in the main loop 
    #     in which it is considered only for the current year
    def ConvertMonthDayToDayOfYr(self, monthDayStr):
        parseArr = [s.strip() for s in monthDayStr.split(' ')]
        month = parseArr[0]
        day = int(parseArr[1])

        dayOfYear = self.daysInYear[self.monDict[month]] + day
        # Apply offset
        dayOfYear = (dayOfYear - self.daysInYear[self.monDict['JUN']]) % 365
        return dayOfYear
    
    def ConvertDayOfYrToMonthDay(self, dayOfYear):
        # Remove offset
        dayOfYear = (dayOfYear + self.daysInYear[self.monDict['JUN']]) % 365
        dayOfYear_str = str(dayOfYear)
        # adjusting day num
        dayOfYear_str.rjust(3 + len(dayOfYear_str), '0')
         # Initialize non-leap year
        year = "1900"
        # converting to date
        res = datetime.strptime(year + "-" + dayOfYear_str, "%Y-%j").strftime("%m-%d-%Y")
        f = res.split('-')
        return(int(f[0]), int(f[1]))
    
    ## 
    # Read in the (tag, value) parameters from the recruitment to update parameters.
    #
    def ReadRecruitConfigFile(self):
        tags = self.ReadConfigFile(self.recrConfigFile)

        for (tag, value) in tags:
            if (tag == 'Stop Period'): 
                (month,day) = self.ConvertDayOfYrToMonthDay(int(value))
                self.frame1.stopDayComboMonth.current(month-1)
                self.frame1.stopDayComboDay.current(day-1)
            if (tag == 'Start Period'): 
                (month,day) = self.ConvertDayOfYrToMonthDay(int(value))
                self.frame1.startDayComboMonth.current(month-1)
                self.frame1.startDayComboDay.current(day-1)
            if (tag == 'Recruit Year Strt'): UpdateEntry(self.frame1.recrYrStrt.myEntry, value)
            if (tag == 'Recruit Year Stop'): UpdateEntry(self.frame1.recrYrStop.myEntry, value)
            if (tag == 'Avg Recr Over Years'): UpdateEntry(self.frame1.numYrsAvg.myEntry, value)

    ##
    # Saves recruitment parameters to a configuration file.
    #
    def WriteRecruitmentConfig(self):
        cfgFile  = os.path.join(self.root,configDir, simCfgDir, self.frame1.recrCfgFile.myEntry.get())

        periodMonthStr = self.frame1.startDayComboMonth.get()
        periodDayStr = self.frame1.startDayComboDay.get()
        periodStr = periodMonthStr+' '+periodDayStr
        startPeriod = self.ConvertMonthDayToDayOfYr(periodStr)

        periodMonthStr = self.frame1.stopDayComboMonth.get()
        periodDayStr = self.frame1.stopDayComboDay.get()
        periodStr = periodMonthStr+' '+periodDayStr
        stopPeriod = self.ConvertMonthDayToDayOfYr(periodStr)

        with open(cfgFile, 'w') as f:
            f.write('# configuration file for Recruitment\n')
            f.write('###############################\n')
            f.write('# Start of Growth Year is June 1 at 00:00\n')
            f.write('# Thus this represents the number of days from May 31\n')
            f.write('# Start/Stop Period should already have offset computed\n')
            f.write('Start Period = {}\n'.format(startPeriod))
            f.write('Stop Period = {}\n'.format(stopPeriod))
            f.write('###############################\n')
            f.write('Recruit Year Strt = {}\n'.format(self.frame1.recrYrStrt.myEntry.get()))
            f.write('Recruit Year Stop = {}\n'.format(self.frame1.recrYrStop.myEntry.get()))
            f.write('###############################\n')
            f.write('Avg Recr Over Years = {}\n'.format(self.frame1.numYrsAvg.myEntry.get()))
            f.close()
            
    ##
    # Saves mortality parameters to a configuration file.
    #
    def WriteGrowthConfig(self):
        configFile  = os.path.join(self.root,configDir, simCfgDir, self.frame3.growthCfgFile.myEntry.get())
        with open(configFile, 'w') as f:
            f.write('# Was configuration file for mortality\n')
            f.write('# Actually contains parameters that define both Growth and Mortality\n')
            f.write('#\n')
            f.write('Fishing Mortality = ' + self.frame3.fishMort.myEntry.get()   + '\n')
            f.write('# Fishing Mortality proportional to LPUE^alpha\n')
            f.write('Alpha Mortality = '   + self.frame3.alphaMort.myEntry.get()  + '\n')
            f.write('MA Cull size = '     + self.frame3.maCullSize.myEntry.get() + '\n')
            f.write('MA Discard = '       + self.frame3.maDiscard.myEntry.get() + '\n')
            f.write('GB Cull size = '     + self.frame3.gbCullSize.myEntry.get() + '\n')
            f.write('GB Discard = '       + self.frame3.gbDiscard.myEntry.get() + '\n')
            f.write('# increasing logistic function\n')
            f.write('MA FSelectA = '       + self.frame3.maFSelectA.myEntry.get() + '\n')
            f.write('MA FSelectB = '       + self.frame3.maFSelectB.myEntry.get() + '\n')
            f.write('GB Closed FSelectA = '+ self.frame3.gbClosedFSelectA.myEntry.get() + '\n')
            f.write('GB Closed FSelectB = '+ self.frame3.gbClosedFSelectB.myEntry.get() + '\n')
            f.write('GB Open FSelectA = '  + self.frame3.gbOpenFSelectA.myEntry.get() + '\n')
            f.write('GB Open FSelectB = '  + self.frame3.gbOpenFSelectB.myEntry.get() + '\n')
            f.write('# Natural adult mortality\n')
            f.write('MA Adult Mortality = '+ self.frame3.maAdultMort.myEntry.get() + '\n')
            f.write('GB Adult Mortality = '+ self.frame3.gbAdultMort.myEntry.get() + '\n')
            f.write('MA Incidental = '     + self.frame3.maIncident.myEntry.get() + '\n')
            f.write('GB Incidental = '     + self.frame3.gbIncident.myEntry.get() + '\n')
            f.write('MA Length_0 = '       + self.frame3.maLength0.myEntry.get() + '\n')
            f.write('GB Length_0 = '       + self.frame3.gbLength0.myEntry.get() + '\n')
            f.write('# special area fishing mortalities\n# if not used set to NONE\n')
            f.write('Fishing Mortality File = '+ self.frame7.fishMortFile.myEntry.get() + '\n')
            f.write('# Used to compute LPUE\n')
            f.write('LPUE Slope = '        + self.frame3.lpueSlope.myEntry.get() +   '\n')
            f.write('LPUE Slope2 = '       + self.frame3.lpueSlope2.myEntry.get() +  '\n')
            f.write('LPUE Intercept = '    + self.frame3.lpueIntcept.myEntry.get() + ' # slope and intercept of regression\n')
            f.write('Max Per Day = '       + self.frame3.maxPerDay.myEntry.get() +   '  # max scallops shucked per day\n')
            f.write('Max Time = '          + self.frame3.maxTime.myEntry.get() +     '  # Max hours dredging per day  \n')
            f.write('Dredge Width = '      + self.frame3.dredgeWth.myEntry.get() +   '    # average total dredge width meters\n')
            f.write('Towing Speed = '      + self.frame3.towSpeed.myEntry.get() +    '  # knots, mean towing speed\n')
            f.close()

    ##
    # Saves grid manager parameters to a configuration file.
    #
    def WriteGridMgrConfig(self):
        cfgFile  = os.path.join(self.root,configDir, simCfgDir, self.frame6.gmCfgFile.myEntry.get())
        with open(cfgFile, 'w') as f:
            f.write('# configuration file for GridManager\n')
            f.write('# The following is the file name with corner coordinates associated with Special Access Areas.\n')
            f.write('# If not used then set to NONE or comment out line\n')
            f.write('# NOTE: Setting to NONE will also cause Mortality to not read in\n')
            f.write('# its special access fishing mortalities and default Fishing Mortalities are used.\n')
            f.write('Special Access Config File = '+self.frame6.specAccFile.myEntry.get()+'\n')
            f.close()

    ##
    # Saves spatial function parameters to a configuration file.
    #
    def WriteSpatialFncsConfig(self, cfgFile, functions, numFncsEntry):
        with open(cfgFile, 'w') as f:
            f.write('# Define non linear spatial functions(NLSF) and paramater search range.\n')
            f.write('#\n')
            f.write('# - "Function 1, dim=z, shape=Logistic, precon=0 "\n')
            f.write('# - "Function 2, dim=z, shape=Gaussian, precon=0 "\n')
            f.write('# - "Function 3, dim=x, shape=Logistic, precon=1 "\n')
            f.write('#\n')
            f.write('# These define spatial functions for setting the spatial trend in the universal kriging algorithm. \n')
            f.write('#\n')
            f.write('# The precon=0 term means that the function is not multiplied by another function. For example,\n')
            f.write('#    "Function 3, dim=x, shape=Logistic, precon=1 " \n')
            f.write('# indicates that the third function is multiplied by the first function.\n')
            f.write('# This is true for fitting the nonlinear parameters of function 3 hence \n')
            f.write('# the parameters of function 1 must be fit before the parameters of function 3.\n')
            for i in range(int(numFncsEntry.get())):
               f.write('Function, dim=' + functions[i].dimVal.get())
               f.write(', shape=' + functions[i].shapeVal.get())
               f.write(', precon=' + functions[i].preconEntry.get()+'\n')
            f.close()

    ##
    # Reads a typical configuration file to recover the tags and values. The parameters in these files all
    # have the following format:
    # - \# indicates that the line is a comment. Otherwise
    # - 'tag' = 'value
    #
    # @param fName: The name of the file to read.
    # @returns An array of tuples showing (tag, value) found in the file
    #
    def ReadConfigFile(self, fName):
        parms=[]
        with open(fName, 'r') as f:
            while True:
                line = f.readline()
                if not line:
                    f.close()
                    break
                if (line[0] != '#'):
                    j = line.find('=')
                    tag = line[0:j].strip()
                    k = line.find('#')
                    if (k == 0):
                        k = len(line)
                    value = line[j+1:k].strip()
                    parms.append((tag,value))
        return parms

    ## 
    # Read in the (tag, value) parameters from the simulation configuration file.
    #
    def ReadSimConfigFile(self):
        # need to read Configuration/Simulation/Scallop.cfg to determine which parameters are output
        self.paramStr = []
        self.paramVal = 0
        tags = self.ReadConfigFile(self.simConfigFile)

        for (tag, value) in tags:
            # Python 3.8 does not have match/case so using if elif
            if (tag == 'Select Abundance'):
                self.paramStr.append(ABUN)
                self.paramVal += 8
            elif (tag == 'Select BMS'):
                self.paramStr.append(BIOM)
                self.paramVal += 4
            elif (tag == 'Select Expl BMS'):
                self.paramStr.append(EBMS)
                self.paramVal += 2
            elif (tag == 'Select Fishing Effort'):
                self.paramStr.append(FEFF)
                self.paramVal += 64
            elif (tag == 'Select Fishing Mortality'):
                self.paramStr.append(FMOR)
                self.paramVal += 128
            elif (tag == 'Select Landings by Number'):
                self.paramStr.append(LAND)
                self.paramVal += 32
            elif (tag == 'Select Landings by Weight'):
                self.paramStr.append(LNDW)
                self.paramVal += 16
            elif (tag == 'Select LPUE'):
                self.paramStr.append(LPUE)
                self.paramVal += 1
            elif (tag == 'Select RECR'):
                self.paramStr.append(RECR)
                self.paramVal += 256
            elif (tag == 'Time steps per Year'):
                self.tsPerYear = int(value)

    ## 
    # Read in the (tag, value) parameters from the grid manager configuration file.
    #
    def ReadGridMgrConfigFile(self):
        tags = self.ReadConfigFile(self.gmConfigFile)

        for (tag, value) in tags:
            if (tag == 'Special Access Config File'): self.specAccFileStr = value

    ## 
    #
    def pop_up(self):
        about = '''SHOW Args
    Shows the limit Number of Areas, Nodes in each Area
	To Change, restart with 
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py Areas Nodes
	
START Sim
    Will run the GeoSAMS simulation base on the parameters given by the GUI. 
    First saves config files to use latest data provided in the GUI using the
    names provided
       - Sim Config File
       - Recruitment File
       - Growth Config File
       - Grid Mgr Config File
    If simulation completed successfully, will the interpolate the results from
	the survey grid to the region grid.
	
	NOTE: 
    1) This does not save the Special Access File or the Fishing Mort File. 
       See Special Acces Tab and the FishingMort in Special Access Tab

Skip Status Msgs
    Select to have scripts run straight through without reporting status.
    Except for last ALL DONE message.

SAVE ALL Configs
    Same as the first step in START Sim
'''
        popup = tk.Toplevel()
        nrows = 31
        ncols = 80
        parentPosn = '+'+str(self.winfo_rootx()+helpXoffset)+'+'+str(self.winfo_rooty()+helpYoffset)
        popup.geometry(str(int(ncols*8.5))+"x"+str(nrows*18)+parentPosn)
        T = tk.Text(popup, width=ncols, height=nrows, padx=10)
        T.insert('end', about)
        T.config(state='disabled')
        T.grid()
        btn = tk.Button(popup, text ="Close", command= popup.destroy)
        btn.grid(row =1)

##
#
def main():
    nargs = len(sys.argv)
    maxYears = 5 # this can change as user enters a greater range, start year to stop year
    if (nargs != 3):
        maxAreas = 50
        maxCorners = 8
        print ("Missing command line arguments. Expecting: ")
        print ("  $ GeoSams.py MaxNumAreas MaxNumCorners")
        print ("  Proceeding with default values:")
        print ("  Maximum areas of interest: {}".format(maxAreas))
        print ("  Maximum nodes for area of interest: {}".format(maxCorners))
        print()
    else:
        maxAreas = int(sys.argv[1])
        maxCorners = int(sys.argv[2])

    title = 'GeoSAMS'
    if platform.system()=='Windows': os.system('color') # Enable ANSI sequences for color monitor
    r = MainApplication(title, maxAreas, maxCorners, maxYears)
    r.mainloop()

##
if __name__ == "__main__":
    main()
