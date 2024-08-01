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
import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

import subprocess
import sys
import os
import sys
import platform
import csv
from collections import defaultdict

from MainInputFrame import *
from GrowthFrame import *
from InterpolationFrame import *
from SortByAreaFrame import *
from SpecialAreaFrame import *
from FishMortBySpecAcc import *
from EditMathSetupFrame import *
from Globals import *

##
# This class is the parent class for the GUI
#
class MainApplication(tk.Tk):
    def __init__(self, title, maxAreas, maxCorners, maxYears):
        super().__init__()

        # member variables
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
        self.ukCfgFile=''

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
        self.simConfigFile  = os.path.join(self.root,configDir, simCfgDir,'Scallop.cfg')
        self.ReadSimConfigFile()
        self.notebook = ttk.Notebook(self)

        self.frame1 = MainInput(self.notebook, self, self.tsPerYear, self.paramVal, self.maxYears)
        # NOTE: These will still be default values as the user would not as yet entered anything!!
        self.simConfigFile  = os.path.join(self.root,configDir, simCfgDir, self.frame1.simCfgFile.myEntry.get())
        self.mortConfigFile = os.path.join(self.root,configDir, simCfgDir, self.frame1.mortCfgFile.myEntry.get())
        self.recrConfigFile = os.path.join(self.root,configDir, simCfgDir, self.frame1.recrCfgFile.myEntry.get())
        self.gmConfigFile   = os.path.join(self.root,configDir, simCfgDir, self.frame1.gmCfgFile.myEntry.get())
        self.ukConfigFile   = os.path.join(self.root,configDir, interCfgDir, self.frame1.ukCfgFile.myEntry.get())

        # Read in configuration parameters
        self.ReadSimConfigFile()
        #
        # NOTE: MA does not use stratum and forces it to false
        # 
        self.frame3 = Growth(self.notebook, self.mortConfigFile)
        self.frame4 = UKInterpolation(self.notebook, self, self.frame1)
        self.frame5 = SortByArea(self.notebook, self.frame1, self.maxAreas, self.maxCorners, self.maxYears, self.paramStr)
        self.frame6 = SpecialArea(self.notebook, self.maxAreas, self.maxCorners)
        self.frame7 = FishMortBySpecAcc(self.notebook, self.maxAreas, self.maxCorners)
        self.frame2 = EditMathSetup(self.notebook)
        self.ReadUKConfigFile()

        # Update strings based on given configuration files
        self.frame7.fishMortFile.myEntry.insert(0, self.frame3.fmorFileStr)
        self.ReadGridMgrConfigFile()
        self.frame6.specAccFile.myEntry.insert(0, self.specAccFileStr)

        self.notebook.add(self.frame1, text='Main')
        self.notebook.add(self.frame3, text='Growth')
        self.notebook.add(self.frame6, text='Special Access')
        self.notebook.add(self.frame7, text='Fishing Mort in\n Special Access')
        self.notebook.add(self.frame5, text='SortByArea')
        self.notebook.add(self.frame4, text='UKInterpolation')
        self.notebook.add(self.frame2, text='Math Setup')
        self.notebook.pack()

        ttk.Button(self, text='SHOW Args',    style="BtnGreen.TLabel", command=self.ShowArgs).place(relx=0, rely=1, anchor='sw')
        ttk.Button(self, text='START Sim',    style="BtnGreen.TLabel", command=self.Run_Sim).place(relx=.25, rely=1, anchor='s')
        ttk.Button(self, text='SAVE ALL Configs', style="BtnGreen.TLabel", command=self.SaveConfigFiles).place(relx=.5, rely=1, anchor='s')
        ttk.Button(self, text= "Help", style="Help.TLabel", command = self.pop_up).place(relx=.75, rely=1, anchor='s')

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
    # If it runs successfully then UK interpolation is started
    #
    def Run_Sim(self):
        # No check for variables changed, therefore update all configuration files with current values in GUI
        # OR
        # Create new files based on names given by user, or same if not changed
        self.SaveConfigFiles()
        # exec to be called, ScallopPopDensity, prepends directory structure, ReadSimConfigFile does not
        simConfigFile = self.frame1.simCfgFile.myEntry.get()
        self.simConfigFile = os.path.join(configDir, simCfgDir, simConfigFile)

        # exec to be called, UK, prepends directory structure
        self.ukCfgFile = self.frame1.ukCfgFile.myEntry.get()

        startYear = self.frame1.startYr.myEntry.get()
        stopYear = self.frame1.stopYr.myEntry.get()
        self.domainName = self.frame1.domainNameCombo.get()
        # check range
        self.yearStart = int(startYear)
        self.yearStop = int(stopYear)
        
        # Ensure data is available, by first checking if data files have been created.
        # Typical data file name: Data/bin5mm2015AL.csv
        filesExist = True
        for yr in range(self.yearStart, self.yearStop+1):
            dataFName = os.path.join(self.root, 'Data', 'bin5mm'+str(yr)+self.domainName+'.csv')
            if not os.path.isfile(dataFName): 
                filesExist = False
                break
        
        if not filesExist: 
            # Create them
            if self.frame2.usingMatlab.get():
                mathArg = 'M'
            else:
                mathArg = 'O'

            if platform.system() == 'Windows':
                cmd = [os.path.join(self.root, 'Unpack.bat'), startYear, stopYear, '0', self.domainName, mathArg]
            else:
                cmd = [os.path.join(self.root, 'Unpack.sh'), startYear, stopYear, '0', self.domainName, mathArg]
                subprocess.run(['chmod','744','Unpack.sh']) # make file executable
            messagebox.showinfo("Unpack", f'Starting Unpack.\nThis could take several minutes, longer if using Octave.\nPlease be patient.')
            result = subprocess.run(cmd)
            if result.returncode == 0:
                messagebox.showinfo("Unpack", f'Completed Successfully\n{result.args}')
                filesExist = True
            else:
                if result.returncode == 1:
                    messagebox.showerror("TrawlData5mmbin", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
                elif result.returncode == 2:
                    messagebox.showerror("PullOutRecruitData", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
                elif result.returncode == 3:
                    messagebox.showerror("ProcessRecruitData", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
                elif result.returncode == 4:
                    messagebox.showerror("NearestNeighborRecInterp", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
                else:
                    messagebox.showerror("Unpack", f'Failed\n{result.args}\nReturn Code = {result.returncode}\nSee Monitor for Reason')
            
        if filesExist:
            # Continue with starting the GeoSAMS simulation ----------------------------------------------------------------------
            # 
            # typical command line:
            # > ./SRC/ScallopPopDensity.exe Scallop.cfg StartYear StopYear Domain
            ex = os.path.join(self.root, 'SRC', 'ScallopPopDensity')
            # ScallopPopDensity prepends directory structure to simConfigFile
            cmd = [ex, simConfigFile, startYear, stopYear, self.domainName]
            print(cmd)
            messagebox.showinfo("GeoSAMS Sim", "Program Started")
            result = subprocess.run(cmd)

            if result.returncode == 0:
                messagebox.showinfo("GeoSAM Sim", 
                    f'Completed Successfully\n{result.args}\nStarting UK Interp\nIf all output selected this will run over an hour.\nPlease be patient.')

                # Then Continue with Interpolation and Plotting Results -----------------------------------------------------------
                (returnCode, args) = self.InterpAndPlotResults()
                if returnCode == 0:
                    messagebox.showinfo("GeoSAMS/UK/Plotting", f'ALL DONE\n{args}')
                else:
                    if returnCode == 99: 
                        messagebox.showerror("UK", f'Failed\n{args}\nReturn Code = {returnCode}\nMath Failure. Try changing Spatial Functions')
                    else:
                        messagebox.showerror("UK", f'Failed\n{args}\nReturn Code = {returnCode}')
            else:
                messagebox.showerror("GeoSAM Sim", f'Failed\n{result.args}\nReturn Code = {result.returncode}')

    ##
    # Interpolates the survey data onto the regional grids and saves results to CSV files. 
    # Concatenates CSV files into a single file. Then uses this file to plot the results
    #
    # Uses the following member variables
    #    self.domainName
    #    self.yearStart
    #    self.yearStop
    #    self.simConfigFile ( in call to ReadSimConfigFile)
    #    self.ukCfgFile
    #    self.paramStr
    # 
    # prefix for the concatenated files,  Output file name is in the form:
    #      Lat_Lon_Grid_ABUN_AL_2015_2017
    # Matlab/Octave will also place the it results in a similar file name
    #      Lat_Lon_Grid_RECR_AL_2017_100_MA_North
    #                    ^ Output parameter
    #                        ^ Domain name
    #                            ^ Year, yearStart to yearStop, e.g. 2014 initial data, 1 less than yearStart
    #                                                                2015 growth in yearStart
    #                                                                ...
    #                                                                2017 growth in yearStop
    #                                  ^ Multiplier to normalize data
    #                                     ^ rgn
    #                                        ^ MA is divided into North and South to better display the data
    #
    def InterpAndPlotResults(self):
        dataDir = 'Data'
        years = range(self.yearStart-1, self.yearStop+1) # year_start-1 is initial state

        # set configuration file name for UK.exe
        ukCfgFile = self.ukCfgFile  # This may be overwritten depending on domain

        # Used while concatenating files
        # number of colums in csv file, starting at 0
        # lat, lon, initial data
        ncols = self.yearStop - self.yearStart + 3
        # number of years plus initial state
        nyears = self.yearStop - self.yearStart + 2

        self.ReadSimConfigFile()

        # Determine which if any file suffixes are needed
        if self.domainName=='GB':
            # rgn = ['_SW', '_N', '_S', '_W']
            rgn = ['_GB']
        elif self.domainName=='MA':
            rgn = ['_MA']
        else:
            # This would be AL
            #rgn = ['_SW', '_N', '_S', '_W', '_MA']
            rgn = ['_GB', '_MA']

        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        #  INTERPOLATE AND CONCATENATE REGIONS INTO SINGLE FILE
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
        prefix = ['Results/Lat_Lon_Grid_']

        for pStr in self.paramStr:

            if (pStr == 'RECR_'):
                zArg = '70.0'
            else:
                zArg = '0.0'

            ex = os.path.join('UKsrc', 'UK')
            # Process multiple GB region files
            #                                                 | --- optional but need both -----|
            #        ConfigFile Domain  ObservFile                GridFile            ZF0Max
            # [ex,   ukCfgFile,   dn,     obsFile,                  gridFile,           value
            # .\UKsrc\UK UK.cfg GB      X_Y_EBMS_GB2005_0_SW.csv  GBxyzLatLon_SW.csv   0.0 | 70.0
            #  arg#        1     2              3                  4                   5
            for r in rgn:
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
                        gridFile = 'MAxyzLatLon.csv'
                        ukCfgFile = 'UK_MA.cfg'
                    else:
                        #gridFile = 'GBxyzLatLon' + r + '.csv'
                        gridFile = 'GBxyzLatLon.csv'
                        ukCfgFile = 'UK_GB.cfg'
                else:
                    # DEPRECATE: if we no longer need to separate GB into sub regions
                    #gridFile = self.domainName+'xyzLatLon' + r + '.csv'
                    gridFile = self.domainName+'xyzLatLon.csv'

                for year in years:
                    obsFile = 'X_Y_' + pStr + self.domainName + str(year) + r + '.csv'
                    cmd = [ex, ukCfgFile, self.domainName, obsFile, gridFile, zArg]
                    result = subprocess.run(cmd)
                    if (result.returncode != 0):
                        errorStr = '[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m'
                        print(errorStr)
                        return (result.returncode, result.args)
                    print( 'Just Finished: ', cmd)
                    # Cleanup dataDir
                    os.remove(os.path.join(dataDir, obsFile))

                for pfix in prefix:
                    ###########################################################################################
                    # subprocess.run takes in Data/X_Y_* and creates
                    #    Results/Lat_Lon_Grid* 
                    # Concatenate individual year files into a single file
                    ###########################################################################################
                    col = [defaultdict(list) for _ in range(nyears)]
                    k = 0
                    
                    # append remaining years as additional columns to first data set
                    for year in years:
                        flin = pfix + pStr + self.domainName + str(year) + r + '.csv'
                        with open(flin) as f:
                            reader = csv.reader(f)
                            for row in reader:
                                for (i,v) in enumerate(row):
                                    col[k][i].append(v)
                            f.close()
                        os.remove(flin)

                        for i in range (len(col[0][0])):    
                            col[0][k + 2].append(col[k][2][i])
                        k  += 1

                    # brute force write out results
                    flout = open(pfix + pStr + self.domainName + r + '.csv', 'w')
                    for row in range(len(col[0][0])):
                        for c in range(ncols):
                            flout.write(col[0][c][row])
                            flout.write(',')
                        flout.write(col[0][ncols][row])
                        flout.write('\n')
                    flout.close()
                # end for pfix
            # end for rgn

            # now combine all region files into one file
            for pfix in prefix:
                flout = pfix + pStr + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv'
                wrFile = open(flout, 'w')
                for r in rgn:
                    flin = pfix + pStr + self.domainName + r + '.csv'
                    rdFile = open(flin, 'r')
                    lines = rdFile.readlines()
                    wrFile.writelines(lines)
                    rdFile.close()
                    os.remove(flin)
                wrFile.close()
                print('Files concatenated to: ',flout)
            # end for pfix
        # end for pStr

        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
        #  PLOTTING
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
        # We have the needed output paramters so lets plot data and save to pdf files
        for pStr in self.paramStr:
            str1 = 'Results/Lat_Lon_Surv_' + pStr + self.domainName
            str2 = 'Results/Lat_Lon_Grid_' + pStr + self.domainName+'_'+str(self.yearStart) + '_' + str(self.yearStop)

            if self.frame2.usingMatlab.get():
                matlabStr = 'PlotLatLonGridSurvey(' + "'" + str1 + "','" + str2 + "', " + str(self.yearStart) + ',' +str(self.tsPerYear) + ", '" + self.domainName + "');exit"
                cmd = ['matlab.exe', '-batch', matlabStr]
            else:
                cmd = ['octave', 'mfiles/PlotLatLonGridSurvey.m', str1, str2, str(self.yearStart), str(self.tsPerYear), self.domainName]

            result = subprocess.run(cmd)
            if (result.returncode != 0):
                errorStr = '[31m' + ''.join(str(e)+' ' for e in cmd) + ' error: ' + hex(result.returncode) + '[0m'
                print(errorStr)
                return (result.returncode, result.args)

        return (0, '')

    ##
    # Save all of the defined configuration files
    #
    def SaveConfigFiles(self):
        self.WriteScallopConfig()
        self.WriteRecruitmentConfig()
        self.WriteGrowthConfig()
        self.WriteGridMgrConfig()
        self.WriteUKConfig()
        # cfgFile  = os.path.join(self.root,configDir, interCfgDir, self.frame4.spatCfgFile.myEntry.get())
        # self.WriteSpatialFncsConfig(cfgFile)
        messagebox.showinfo("Save Files", "Configuration Files Saved")

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
            f.write('Mortality Config File = '   + self.frame1.mortCfgFile.myEntry.get() + '\n')
            f.write('Recruit Config File = '     + self.frame1.recrCfgFile.myEntry.get() + '\n')
            f.write('Grid Manager Config File = '+ self.frame1.gmCfgFile.myEntry.get()   + '\n')
            f.write('# The following items determine the parameters for output and plotting\n')
            f.write('# One need only select the desired parameters, the default is to not show them\n')
            f.write('# Anything after = is ignored.\n')
            if(self.frame1.abunVar.get()):     f.write('Select Abundance          =# ABUN abundance scallops per square meter\n')
            if(not self.frame1.abunVar.get()): f.write('# Select Abundance          =# ABUN abundance scallops per square meter\n')
            if(self.frame1.bmsVar.get()):      f.write('Select BMS                =# BMMT biomass in metric tons\n')
            if(not self.frame1.bmsVar.get()):  f.write('# Select BMS                =# BMMT biomass in metric tons\n')
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
    # @param monthDayStr string that holds month and day as either alpha format.
    # That is 'JAN 01', or '01/01'
    #
    def ConvertMonthDayToDayOfYr(self, monthDayStr):
        monDict = {'JAN':0, 'FEB':1, 'MAR':2, 'APR':3, 'MAY':4, 'JUN':5, 'JUL':6, 'AUG':7, 'SEP':8, 'OCT':9, 'NOV':10, 'DEC':11} 
        daysInYear = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
        parseArr = [s.strip() for s in monthDayStr.split(' ')]
        month = parseArr[0]
        day = int(parseArr[1])
        return daysInYear[monDict[month]] + day - 1

    ##
    # Saves recruitment parameters to a configuration file.
    #
    def WriteRecruitmentConfig(self):
        cfgFile  = os.path.join(self.root,configDir, simCfgDir, self.frame1.recrCfgFile.myEntry.get())

        periodMonthStr = self.frame1.startDayComboMonth.get()
        periodDayStr = self.frame1.startDayComboDay.get()
        periodStr = periodMonthStr+' '+periodDayStr
        startPeriod = self.ConvertMonthDayToDayOfYr(periodStr)
        startMonIndx = self.frame1.monthsArr.index(periodMonthStr)

        periodMonthStr = self.frame1.stopDayComboMonth.get()
        periodDayStr = self.frame1.stopDayComboDay.get()
        periodStr = periodMonthStr+' '+periodDayStr
        stopPeriod = self.ConvertMonthDayToDayOfYr(periodStr)
        stopMonIndx = self.frame1.monthsArr.index(periodMonthStr)

        if startMonIndx > stopMonIndx:
            messagebox.showerror("Recruitment", f'Start Month is > Stop Month\nPlease Fix\nFile NOT saved')
        else:
            with open(cfgFile, 'w') as f:
                f.write('# configuration file for Recruitment\n')
                f.write('Start Period = '+str(startPeriod)+'  # Jan 1 is 0\n')
                f.write('Stop Period = '+str(stopPeriod)  +' # converted to fraction of year, i.e. /365\n')
                f.close()
            
    ##
    # Saves mortality parameters to a configuration file.
    #
    def WriteGrowthConfig(self):
        simCfgFile  = os.path.join(self.root,configDir, simCfgDir, self.frame1.mortCfgFile.myEntry.get())
        with open(simCfgFile, 'w') as f:
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
        cfgFile  = os.path.join(self.root,configDir, simCfgDir, self.frame1.gmCfgFile.myEntry.get())
        with open(cfgFile, 'w') as f:
            f.write('# configuration file for GridManager\n')
            f.write('# The following is the file name with corner coordinates associated with Special Access Areas.\n')
            f.write('# If not used then set to NONE or comment out line\n')
            f.write('# NOTE: Setting to NONE will also cause Mortality to not read in\n')
            f.write('# its special access fishing mortalities and default Fishing Mortalities are used.\n')
            f.write('Special Access Config File = '+self.frame6.specAccFile.myEntry.get()+'\n')
            f.close()

    ##
    # Saves Universal Kriging parameters to a configuration file.
    #
    def WriteUKConfig(self):
        cfgFile  = os.path.join(self.root,configDir, interCfgDir, self.frame1.ukCfgFile.myEntry.get())
        self.CloseUKConfig(cfgFile, 
                           self.frame4.formCombo.get(),
                           self.frame4.useSaturateCombo.get(),
                           self.frame4.saturateThresh.myEntry.get(),
                           self.frame4.spatCfgFile.myEntry.get())
    
    def CloseUKConfig(self, cfgFile, combo, useSaturate, threshold, fName):
        with open(cfgFile, 'w') as f:
            f.write('# Set inputs for universal kriging\n')
            f.write('Kriging variogram form = ' + combo + '\n')
            f.write('#\n')
            f.write('# Configuration files are expected to be in the Configuration directory\n')
            f.write('#\n')
            f.write('NLS Spatial Fcn File Name = ' + fName + '\n')
            f.write('#\n')
            f.write('# Save interim data by writing out supporting data files\n')
            f.write('#\n')
            f.write('Save Data = F\n')
            f.write('#\n')
            f.write('# Interpolation can sometimes create excessively large values.\n')
            f.write('# To bypass, Use Saturate can be T or F, but set a larger threshold, i.e. 1E309 (Infinity).\n')
            f.write('# The user can choose to saturate to the threshold, (T), or \n')
            f.write('# reset the value to 0.0 when exceeded, (F).\n')
            f.write('#\n')
            f.write('Use Saturate = ' + useSaturate + '\n')
            f.write('#\n')
            f.write('# Threshold value to use\n')
            f.write('# Use Saturate = T, if field > Threshold then field = Threshold\n')
            f.write('# Use Saturate = F, if field > Threshold then field = 0.0\n')
            f.write('#\n')
            f.write('Overflow Threshold = ' + threshold + '\n')
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
                self.paramStr.append('ABUN_')
                self.paramVal += 8
            elif (tag == 'Select BMS'):
                self.paramStr.append('BMMT_')
                self.paramVal += 4
            elif (tag == 'Select Expl BMS'):
                self.paramStr.append('EBMS_')
                self.paramVal += 2
            elif (tag == 'Select Fishing Effort'):
                self.paramStr.append('FEFF_')
                self.paramVal += 64
            elif (tag == 'Select Fishing Mortality'):
                self.paramStr.append('FMOR_')
                self.paramVal += 128
            elif (tag == 'Select Landings by Number'):
                self.paramStr.append('LAND_')
                self.paramVal += 32
            elif (tag == 'Select Landings by Weight'):
                self.paramStr.append('LNDW_')
                self.paramVal += 16
            elif (tag == 'Select LPUE'):
                self.paramStr.append('LPUE_')
                self.paramVal += 1
            elif (tag == 'Select RECR'):
                self.paramStr.append('RECR_')
                self.paramVal += 256
            elif (tag == 'Time steps per Year'):
                self.tsPerYear = int(value)

    ## 
    # Read in the (tag, value) parameters from the UK configuration file.
    #
    def ReadUKConfigFile(self):
        tags = self.ReadConfigFile(self.ukConfigFile)
        self.frame4.UpdateUKParameters(tags)

        # MA and GB UK Config Files Names are hardcoded
        cfgFile  = os.path.join(self.root,configDir, interCfgDir, 'UK_MA.cfg')
        tags = self.ReadConfigFile(cfgFile)
        self.frame4.UpdateMAParameters(tags)

        cfgFile  = os.path.join(self.root,configDir, interCfgDir, 'UK_GB.cfg')
        tags = self.ReadConfigFile(cfgFile)
        self.frame4.UpdateGBParameters(tags)

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
       - UK Config File
       - Spatial Fcn Config File (in UKIterpolation tab)
    If simulation completed successfully, will the interpolate the results from
	the survey grid to the region grid.
	
	NOTE: 
    1) Before running with Domain Name 'AL', Go to UKIterpolation tab and make 
    sure the Spatial Fcn Config File 'SpatialFcnsMA.cfg' and 
    'SpatialFcnsGB.cfg' have the function definitions desired. These files are
    used to interpolate data in their respective regions

    2) This does not save the Special Access File or the Fishing Mort File. 
       See Special Acces Tab and the FishingMort in Special Access Tab

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
    if (nargs != 4):
        maxAreas = 25
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
