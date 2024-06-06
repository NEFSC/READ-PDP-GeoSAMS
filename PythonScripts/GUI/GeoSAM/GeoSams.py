#-------------------------------------------------------------------------------------------
##
# @mainpage GeoSAMS GUI
#
# This is the main program for the GeoSAMS GUI
# 
# The GUI has 5 tabs
# -# Main: Data concerning simulation duration, configutation files in use, and recruitment period
# -# Special Access: Files used to define special areas for fishing management
# -# Mortality: Parameters that are used to define Fishing Mortality
# -# UKInterpolation: Parameters that are used to interpolate results from survey grid to regional grid
# -# Sort By Area: Parameters that are used to sort output data and associate with areas of interest
#
# The program is started by entering the following command in the root directory of the workspace
# $ python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py [10 8 10]
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
# This button will save all of the configuration files using the names given. This is the same as the 
# first step in <b>START Sim</b>

#-------------------------------------------------------------------------------------------
import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

import subprocess
import sys
import os
import sys
import platform
import re

from MainInputFrame import *
from GrowthFrame import *
from InterpolationFrame import *
from SortByAreaFrame import *
from SpecialAreaFrame import *
from FishMortBySpecAcc import *

#======================================================================================================
##
# This class is the parent class for the GUI
#
#======================================================================================================
class MainApplication(tk.Tk):
    #-------------------------------------------------------------------------------------
    ##
    # Constructor
    #
    #-------------------------------------------------------------------------------------
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
        self.savedByStratum = False

        # setup
        self.title(title)
        self.geometry('1020x725+10+10')
        self.style = ttk.Style()

        # vscode will not set this variable, must be done via control panel
        # in command terminal on Windows, assuming user is in the install directory
        #   > set ROOT=%CD%
        self.root = os.environ['ROOT']
        self.simConfigFile  = os.path.join(self.root,'Configuration', 'Scallop.cfg')
        (self.paramStr, self.paramVal) = self.ReadSimConfigFile()
        self.notebook = ttk.Notebook(self)

        self.frame1 = MainInput(self.notebook, self, self.tsPerYear, self.paramVal)
        # NOTE: These will still be default values as the user would not as yet entered anything!!
        self.simConfigFile  = os.path.join(self.root,'Configuration', self.frame1.simCfgFile.myEntry.get())
        self.mortConfigFile = os.path.join(self.root,'Configuration', self.frame1.mortCfgFile.myEntry.get())
        self.recrConfigFile = os.path.join(self.root,'Configuration', self.frame1.recrCfgFile.myEntry.get())
        self.gmConfigFile   = os.path.join(self.root,'Configuration', self.frame1.gmCfgFile.myEntry.get())

        # Read in configuration parameters
        (self.paramStr, self.paramVal) = self.ReadSimConfigFile()
        #
        # NOTE: MA does not use stratum and forces it to false
        # 
        #self.frame2 = SpecialAccess(self.notebook)
        self.frame3 = Growth(self.notebook, self.mortConfigFile)
        self.frame4 = UKInterpolation(self.notebook, self, self.frame1)
        self.frame5 = SortByArea(self.notebook, self.frame1, self.maxAreas, self.maxCorners, self.maxYears, self.paramStr)
        self.frame6 = SpecialArea(self.notebook, self.maxAreas, self.maxCorners)
        self.frame7 = FishMortBySpecAcc(self.notebook, self.maxAreas, self.maxCorners)

        # Update strings based on given configuration files
        # Frame 3 reads in Mortality config file
        #self.frame2.fishMortFile.myEntry.insert(0, self.frame3.fmorFileStr)
        self.frame7.fishMortFile.myEntry.insert(0, self.frame3.fmorFileStr)
        self.ReadGridMgrConfigFile()
        #self.frame2.specAccFile.myEntry.insert(0, self.specAccFileStr)
        self.frame6.specAccFile.myEntry.insert(0, self.specAccFileStr)

        self.notebook.add(self.frame1, text='Main')
        self.notebook.add(self.frame3, text='Growth')
        self.notebook.add(self.frame6, text='Special Access')
        self.notebook.add(self.frame7, text='Fishing Mort in\n Special Access')
        self.notebook.add(self.frame5, text='SortByArea')
        self.notebook.add(self.frame4, text='UKInterpolation')
        self.notebook.pack()

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#0F0")
        ttk.Button(self, text='SHOW Args',    style="Custom.TLabel", command=self.ShowArgs).place(relx=0, rely=1, anchor='sw')
        ttk.Button(self, text='START Sim',    style="Custom.TLabel", command=self.Run_Sim).place(relx=.25, rely=1, anchor='s')
        ttk.Button(self, text='SAVE ALL Configs', style="Custom.TLabel", command=self.SaveConfigFiles).place(relx=.5, rely=1, anchor='s')
        self.style.configure("Help.TLabel", padding=6, relief="flat", foreground='white', background="#5783db")
        ttk.Button(self, text= "Help", style="Help.TLabel", command = self.pop_up).place(relx=.75, rely=1, anchor='s')

    #-------------------------------------------------------------------------------------
    ##
    # Display setup limits here
    # Messagebox blocks entry widgets if attempted to open before the main window completes.
    #
    #-------------------------------------------------------------------------------------
    def ShowArgs(self):
        messagebox.showinfo("GeoSAMS",f'Using these parameters\n\
        Max Areas of Interest: {self.maxAreas}\n\
        Max Nodes in Areas: {self.maxCorners}\n\
        Max Year Range: {self.maxYears}')

    #-------------------------------------------------------------------------------------
    ## 
    # Starts the GeoSAMS simulatation <b>ScallopPopDensity</b>. 
    # If it runs successfully then UK interpolation is started
    #
    #-------------------------------------------------------------------------------------
    def Run_Sim(self):
        # No check for variables changed, therefore update all configuration files with current values in GUI
        # OR
        # Create new files based on names given by user, or same if not changed
        self.SaveConfigFiles()
        # 
        # typical command line:
        # > ./SRC/ScallopPopDensity.exe Scallop.cfg StartYear StopYear Domain
        ex = os.path.join(self.root, 'SRC', 'ScallopPopDensity')
        simCfgFile = self.frame1.simCfgFile.myEntry.get()
        ukCfgFile = self.frame1.ukCfgFile.myEntry.get()
        startYear = self.frame1.startYr.myEntry.get()
        stopYear = self.frame1.stopYr.myEntry.get()
        dn = self.frame1.domainNameCombo.get()
        # check range
        self.yearStart = int(startYear)
        self.yearStop = int(stopYear)
        numYears = self.yearStop - self.yearStart + 1
        if numYears > self.maxYears:
            self.yearStop = self.maxYears + self.yearStart - 1
            stopYear = str(self.yearStop)
            numYears = self.maxYears
            messagebox.showerror("Too many years", f'Setting Stop Year to {stopYear}')
            self.frame1.stopYr.myEntry.delete(0,4)
            self.frame1.stopYr.myEntry.insert(0, stopYear)
        
        # First ensure data is available
        if platform.system() == 'Windows':
            cmd = [os.path.join(self.root, 'Unpack.bat'), startYear, stopYear, '0', dn]
        else:
            cmd = [os.path.join(self.root, 'Unpack.sh'), startYear, stopYear, '0', dn]

        result = subprocess.run(cmd)
        if result.returncode == 0:
            messagebox.showinfo("Unpack", f'Completed Successfully\n{result.args}')

            cmd = [ex, simCfgFile, startYear, stopYear, dn]
            print(cmd)
            messagebox.showinfo("GeoSAMS Sim", "Program Started")
            result = subprocess.run(cmd)

            if result.returncode == 0:
                messagebox.showinfo("GeoSAM Sim", f'Completed Successfully\n{result.args}')
                # python .\PythonScripts\ProcessResults.py GB 2015 2017 Scallop.cfg UK.cfg
                ex = 'python'
                script = os.path.join(self.root, 'PythonScripts', 'ProcessResults.py')
                cmd = [ex, script, dn, startYear, stopYear, simCfgFile, ukCfgFile] 
                print(cmd)
                result = subprocess.run(cmd)
                if result.returncode == 0:
                    messagebox.showinfo("UK", f'Completed Successfully\n{result.args}')
                else:
                    messagebox.showerror("UK", f'Failed\n{result.args}\nReturn Code = {result.returncode}')
            else:
                messagebox.showerror("GeoSAM Sim", f'Failed\n{result.args}\nReturn Code = {result.returncode}')
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

    #-------------------------------------------------------------------------------------
    ##
    # Save all of the defined configuration files
    #
    #-------------------------------------------------------------------------------------
    def SaveConfigFiles(self):
        self.WriteScallopConfig()
        self.WriteRecruitmentConfig()
        self.WriteGrowthConfig()
        self.WriteGridMgrConfig()
        self.WriteUKConfig()
        cfgFile  = os.path.join(self.root,'Configuration', self.frame4.spatCfgFile.myEntry.get())
        self.WriteSpatialFncsConfig(cfgFile)
        messagebox.showinfo("Save Files", "Configuration Files Saved")

    #-------------------------------------------------------------------------------------
    ##
    # Saves simulation configuration file. It does so by writeing the parameters for the 
    # to the name file as well as keeping helpfule comments.
    #
    #-------------------------------------------------------------------------------------
    def WriteScallopConfig(self):
        simCfgFile  = os.path.join(self.root,'Configuration', self.frame1.simCfgFile.myEntry.get())
        with open(simCfgFile, 'w') as f:
            f.write('# input file for Scallops \n')
            f.write('Time steps per Year = ' + str(self.frame1.tsPerYear.myEntry.get())+'\n')
            f.write('Save By Stratum = '     + str(self.frame1.useStratumCombo.get())+'\n')
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
            if(self.frame1.lpueVar.get()):     f.write('Select LPUE               =# LPUE Landing Per Unit Effor, (per day)\n')
            if(not self.frame1.lpueVar.get()): f.write('# Select LPUE               =# LPUE Landing Per Unit Effor, (per day)\n')
            if(self.frame1.recrVar.get()):     f.write('Select RECR               =# Recruitment\n')
            if(not self.frame1.recrVar.get()): f.write('# Select RECR               =# Recruitment\n')
            f.close()

    #-------------------------------------------------------------------------------------
    ##
    # This method is used to converty the recruitment start and stop dates from a string month 
    # numerical day into days in a year. Changed entry to combo box to guarantee format
    # @param monthDayStr string that holds month and day as either alpha format.
    # That is 'JAN 01', or '01/01'
    #
    #-------------------------------------------------------------------------------------
    def ConvertMonthDayToDayOfYr(self, monthDayStr):
        monDict = {'JAN':0, 'FEB':1, 'MAR':2, 'APR':3, 'MAY':4, 'JUN':5, 'JUL':6, 'AUG':7, 'SEP':8, 'OCT':9, 'NOV':10, 'DEC':11} 
        daysInYear = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
        parseArr = [s.strip() for s in monthDayStr.split(' ')]
        month = parseArr[0]
        day = int(parseArr[1])
        return daysInYear[monDict[month]] + day - 1
           
    #-------------------------------------------------------------------------------------
    ##
    # Saves recruitment parameters to a configuration file.
    #
    #-------------------------------------------------------------------------------------
    def WriteRecruitmentConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration', self.frame1.recrCfgFile.myEntry.get())

        periodMonthStr = self.frame1.startDayComboMonth.get()
        periodDayStr = self.frame1.startDayComboDay.get()
        if periodMonthStr in ['SEP', 'APR', 'JUN', 'NOV'] and int(periodDayStr) > 30:
            periodDayStr = '30'
            self.frame1.startDayComboDay.current(29)
        if periodMonthStr == 'FEB' and int(periodDayStr) > 28:
            periodDayStr = '28'
            self.frame1.startDayComboDay.current(27)
        periodStr = periodMonthStr+' '+periodDayStr
        startPeriod = self.ConvertMonthDayToDayOfYr(periodStr)
        startMonIndx = self.frame1.monthsArr.index(periodMonthStr)

        periodMonthStr = self.frame1.stopDayComboMonth.get()
        periodDayStr = self.frame1.stopDayComboDay.get()
        if periodMonthStr in ['SEP', 'APR', 'JUN', 'NOV'] and int(periodDayStr) > 30:
            periodDayStr = '30'
            self.frame1.stopDayComboDay.current(29)
        if periodMonthStr == 'FEB' and int(periodDayStr) > 28:
            periodDayStr = '28'
            self.frame1.startDayComboDay.current(27)
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
            
    #-------------------------------------------------------------------------------------
    ##
    # Saves mortality parameters to a configuration file.
    #
    #-------------------------------------------------------------------------------------
    def WriteGrowthConfig(self):
        simCfgFile  = os.path.join(self.root,'Configuration', self.frame1.mortCfgFile.myEntry.get())
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

    #-------------------------------------------------------------------------------------
    ##
    # Saves grid manager parameters to a configuration file.
    #
    #-------------------------------------------------------------------------------------
    def WriteGridMgrConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration', self.frame1.gmCfgFile.myEntry.get())
        with open(cfgFile, 'w') as f:
            f.write('# configuration file for GridManager\n')
            f.write('# The following is the file name with corner coordinates associated with Special Access Areas.\n')
            f.write('# If not used then set to NONE or comment out line\n')
            f.write('# NOTE: Setting to NONE will also cause Mortality to not read in\n')
            f.write('# its special access fishing mortalities and default Fishing Mortalities are used.\n')
            f.write('Special Access Config File = '+self.frame6.specAccFile.myEntry.get()+'\n')
            f.close()

    #-------------------------------------------------------------------------------------
    ##
    # Saves Universal Kriging parameters to a configuration file.
    #
    #-------------------------------------------------------------------------------------
    def WriteUKConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration', self.frame1.ukCfgFile.myEntry.get())
        with open(cfgFile, 'w') as f:
            f.write('# Set inputs for universal kriging\n')
            #DEPRECATE#f.write('# Observation files are expecting in the Data subdirectory\n')
            #DEPRECATE#f.write('#\n')
            #DEPRECATE#f.write('#(max interp field < hlf*max(obs))\n')
            #DEPRECATE#f.write('High Limit Factor = '+self.frame4.highLimit.myEntry.get()+'\n')
            f.write('Kriging variogram form = '+self.frame4.formCombo.get()+'\n')
            f.write('#\n')
            #DEPRECATE#f.write('# Keep this line before "Power Transform Parameter"\n')
            #DEPRECATE#f.write('#\n')
            #DEPRECATE#f.write('Log Transform = '+self.frame4.useLogTransCombo.get()+'\n')
            #DEPRECATE#f.write('#\n')
            #DEPRECATE#f.write('# Power transform interpolates f(x)^alpha \n')
            #DEPRECATE#f.write('# generally 0< alpha < 1 but this has not been tested \n')
            #DEPRECATE#f.write('# not used if "Log Transform = T"\n')
            #DEPRECATE#f.write('#\n')
            #DEPRECATE#f.write('Power Transform Parameter = '+self.frame4.powerTrans.myEntry.get()+'\n')
            #DEPRECATE#f.write('#\n')
            f.write('# Configuration files are expected to be in the Configuration directory\n')
            f.write('#\n')
            f.write('NLS Spatial Fcn File Name = '+self.frame4.spatCfgFile.myEntry.get()+'\n')
            f.write('#\n')
            f.write('# Save interim data by writing out supporting data files\n')
            f.write('#\n')
            f.write('Save Data = F\n')
            f.close()

    #-------------------------------------------------------------------------------------
    ##
    # Saves spatial function parameters to a configuration file.
    #
    #-------------------------------------------------------------------------------------
    def WriteSpatialFncsConfig(self, cfgFile):
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
            for i in range(int(self.frame4.numFcnsEntry.get())):
               f.write('Function, dim='+self.frame4.functions[i].dimVal.get())
               f.write(', shape='+self.frame4.functions[i].shapeVal.get())
               f.write(', precon='+self.frame4.functions[i].preconEntry.get()+'\n')
            f.close()

    #-------------------------------------------------------------------------------------
    ##
    # Reads a typical configuration file to recover the tags and values. The parameters in these files all
    # have the following format:
    # - \# indicates that the line is a comment. Otherwise
    # - 'tag' = 'value
    #
    # @parameter fName: The name of the file to read.
    # @returns An array of tuples showing (tag, value) found in the file
    #
    #-------------------------------------------------------------------------------------
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

    #-------------------------------------------------------------------------------------
    ## 
    # Read in the (tag, value) parameters from the simulation configuration file.
    #
    #-------------------------------------------------------------------------------------
    def ReadSimConfigFile(self):
        # need to read Configuration/Scallop.cfg to determine which parameters are output
        paramStr = []
        paramVal = 0
        tags = self.ReadConfigFile(self.simConfigFile)

        for (tag, value) in tags:
            # Python 3.8 does not have match/case so using if elif
            if (tag == 'Select Abundance'):
                paramStr.append('ABUN_')
                paramVal += 8
            elif (tag == 'Select BMS'):
                paramStr.append('BMMT_')
                paramVal += 4
            elif (tag == 'Select Expl BMS'):
                paramStr.append('EBMS_')
                paramVal += 2
            elif (tag == 'Select Fishing Effort'):
                paramStr.append('FEFF_')
                paramVal += 64
            elif (tag == 'Select Fishing Mortality'):
                paramStr.append('FMOR_')
                paramVal += 128
            elif (tag == 'Select Landings by Number'):
                paramStr.append('LAND_')
                paramVal += 32
            elif (tag == 'Select Landings by Weight'):
                paramStr.append('LNDW_')
                paramVal += 16
            elif (tag == 'Select LPUE'):
                paramStr.append('LPUE_')
                paramVal += 1
            elif (tag == 'Select RECR'):
                paramStr.append('RECR_')
                paramVal += 256
            elif (tag == 'Time steps per Year'):
                self.tsPerYear = int(value)
            elif (tag == 'Save By Stratum'):
                self.savedByStratum = value[0] == 'T'
        return (paramStr, paramVal)

    #-------------------------------------------------------------------------------------
    ## 
    # Read in the (tag, value) parameters from the grid manager configuration file.
    #
    #-------------------------------------------------------------------------------------
    def ReadGridMgrConfigFile(self):
        tags = self.ReadConfigFile(self.gmConfigFile)

        for (tag, value) in tags:
            if (tag == 'Special Access Config File'): self.specAccFileStr = value

    #-------------------------------------------------------------------------------------
    ## 
    #-------------------------------------------------------------------------------------
    def pop_up(self):
        about = '''SHOW Args
    Shows the limits for Number of Areas, Nodes in each Area, Year range
	To Change, restart with 
    > python .\PythonScripts\GUI\GeoSAM\GeoSams.py Areas Nodes Years
	
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
        #about = re.sub("\n\s*", "\n", about) # remove leading whitespace from each line
        popup = tk.Toplevel()
        nrows = 31
        ncols = 80
        parentPosn = '+'+str(self.winfo_rootx()+700)+'+'+str(self.winfo_rooty()+50)
        popup.geometry(str(int(ncols*8.5))+"x"+str(nrows*18)+parentPosn)
        T = tk.Text(popup, width=ncols, height=nrows, padx=10)
        T.insert('end', about)
        T.config(state='disabled')
        T.grid()
        btn = tk.Button(popup, text ="Close", command= popup.destroy)
        btn.grid(row =1)

#======================================================================================================
#======================================================================================================
def main():
    nargs = len(sys.argv)
    if (nargs != 4):
        maxAreas = 25
        maxCorners = 8
        maxYears = 5
        print ("Missing command line arguments. Expecting: ")
        print ("  $ GeoSams.py MaxNumAreas MaxNumCorners MaxNumYears")
        print ("  Proceeding with default values:")
        print ("  Maximum areas of interest: {}".format(maxAreas))
        print ("  Maximum nodes for area of interest: {}".format(maxCorners))
        print ("  Maximum years range for simulation: {}".format(maxYears))
        print()
    else:
        maxAreas = int(sys.argv[1])
        maxCorners = int(sys.argv[2])
        maxYears = int(sys.argv[3])

    title = 'GeoSAMS'
    r = MainApplication(title, maxAreas, maxCorners, maxYears)
    r.mainloop()

#======================================================================================================
if __name__ == "__main__":
    main()
