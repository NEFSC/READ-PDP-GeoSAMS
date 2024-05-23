import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

import subprocess
import os
import sys
import platform

from Frames import *


class MainApplication(tk.Tk):
    def __init__(self, title):
        super().__init__()

        # member variables
        self.addFrameClicked = False
        self.tsInYear = 0
        self.paramVal = 0
        self.paramStr = []
        self.savedByStratum = False

        # setup
        self.title(title)
        #self.geometry('800x800')
        self.style = ttk.Style()

        # vscode will not set this variable, must be done via control panel
        # in command terminal on Windows, assuming user is in the install directory
        #   > set ROOT=%CD%
        self.root = os.environ['ROOT']

        self.notebook = ttk.Notebook(self)

        self.frame1 = Frame1(self.notebook)
        # NOTE: These will still be default values as the user would not as yet entered anything!!
        self.simConfigFile  = os.path.join(self.root,'Configuration/'+self.frame1.simCfgFile.myEntry.get())
        self.mortConfigFile = os.path.join(self.root,'Configuration/'+self.frame1.mortCfgFile.myEntry.get())
        self.recrConfigFile = os.path.join(self.root,'Configuration/'+self.frame1.recrCfgFile.myEntry.get())
        self.gmConfigFile   = os.path.join(self.root,'Configuration/'+self.frame1.gmCfgFile.myEntry.get())

        # Read in configuration parameters
        (self.paramStr, self.paramVal) = self.ReadSimConfigFile()
        #
        # NOTE: MA does not use stratum and forces it to false
        # 
        self.frame1a = Frame1a(self.notebook)
        self.frame2 = Frame2(self.notebook, self.tsInYear, self.paramVal, self.savedByStratum)
        self.frame3 = Frame3(self.notebook, self.mortConfigFile)
        self.frame4 = Frame4(self.notebook, self.frame1.domainName.myEntry.get)
        self.frame5 = None # Frame5(self.notebook, self.paramStr)

        # Update strings based on given configuration files
        # Frame 3 reads in Mortality config file
        self.frame1a.fishMortFile.myEntry.insert(0, self.frame3.fmorFileStr)
        self.ReadGridMgrConfigFile()
        self.frame1a.specAccFile.myEntry.insert(0, self.specAccFileStr)

        self.notebook.add(self.frame1, text='Main')
        self.notebook.add(self.frame1a, text='Special Access')
        self.notebook.add(self.frame2, text='Outputs')
        self.notebook.add(self.frame3, text='Mortality')
        self.notebook.add(self.frame4, text='Interpolation')
        #self.notebook.add(self.frame5, text='SortByArea')
        self.notebook.pack()

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#0F0")
        ttk.Button(self, text='START Sim',    style="Custom.TLabel", command=self.Run_Sim).place(relx=.25, rely=1, anchor='s')
        ttk.Button(self, text='SAVE Configs', style="Custom.TLabel", command=self.SaveConfigFiles).place(relx=.5, rely=1, anchor='s')
        ttk.Button(self, text='LOAD Sort',    style="Custom.TLabel", command=self.AddSortFrame).place(relx=.75, rely=1, anchor='s')

    def Run_Sim(self):
        # No check for variables changed, therefore update all configuration files with current values in GUI
        # OR
        # Create new files based on names given by user, or same if not changed
        self.SaveConfigFiles
        # 
        # typical command line:
        # > .\SRC\ScallopPopDensity.exe Scallop.cfg StartYear StopYear Domain

        ex = self.root+'/SRC/ScallopPopDensity'
        simCfgFile = self.frame1.simCfgFile.myEntry.get()
        ukCfgFile = self.frame1.ukCfgFile.myEntry.get()
        startYear = self.frame1.startYr.myEntry.get()
        stopYear = self.frame1.stopYr.myEntry.get()
        dn = self.frame1.domainName.myEntry.get()
        cmd = [ex, simCfgFile, str(startYear), str(stopYear), dn]
        print(cmd)
        result = subprocess.run(cmd)
        if result.returncode == 0:
            messagebox.showinfo("GeoSAM Sim", f'Completed Successfully\n{result.args}')
        else:
            messagebox.showerror("GeoSAM Sim", f'Failed\n{result.args}\nReturn Code = {result.returncode}')

        # python .\PythonScripts\ProcessResults.py GB 2015 2017 Scallop.cfg UK.cfg
        ex = 'python'
        script = self.root+'/PythonScripts/ProcessResults.py'
        cmd = [ex, script, dn, str(startYear), str(stopYear), simCfgFile, ukCfgFile] 
        print(cmd)
        result = subprocess.run(cmd)
        if result.returncode == 0:
            messagebox.showinfo("UK", f'Completed Successfully\n{result.args}')
        else:
            messagebox.showerror("UK", f'Failed\n{result.args}\nReturn Code = {result.returncode}')


    def SaveConfigFiles(self):
        self.WriteScallopConfig()
        self.WriteRecruitmentConfig()
        self.WriteMortalityConfig()
        self.WriteGridMgrConfig()
        self.WriteUKConfig()
        self.WriteSpatialFncsConfig()

    def AddSortFrame(self):
        if not self.addFrameClicked: 
            self.addFrameClicked = True

            # Get start and stop years
            startYear = int(self.frame1.startYr.myEntry.get())
            stopYear =  int(self.frame1.stopYr.myEntry.get())
            # Read in configuration parameters
            (self.paramStr, self.paramVal) = self.ReadSimConfigFile()
            self.frame5 = Frame5(self.notebook, self.paramStr, 
                                 self.frame1.startYr.myEntry.get, 
                                 self.frame1.stopYr.myEntry.get,
                                 self.frame1.domainName.myEntry.get)
            self.notebook.add(self.frame5, text='SortByArea')
            self.notebook.pack()


    def WriteScallopConfig(self):
        simCfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.simCfgFile.myEntry.get())
        with open(simCfgFile, 'w') as f:
            f.write('# input file for Scallops \n')
            f.write('Time steps per Year = ' + str(self.frame2.tsPerYear.myEntry.get())+'\n')
            f.write('Save By Stratum = '     + str(self.frame2.useStratum.myEntry.get())+'\n')
            f.write('# Configuration files are expected to be in the Configuration directory\n')
            f.write('Mortality Config File = '   + self.frame1.mortCfgFile.myEntry.get() + '\n')
            f.write('Recruit Config File = '     + self.frame1.recrCfgFile.myEntry.get() + '\n')
            f.write('Grid Manager Config File = '+ self.frame1.gmCfgFile.myEntry.get()   + '\n')
            f.write('# The following items determine the parameters for output and plotting\n')
            f.write('# One need only select the desired parameters, the default is to not show them\n')
            f.write('# Anything after = is ignored.\n')
            if(self.frame2.abunVar.get()):     f.write('Select Abundance          =# ABUN abundance scallops per square meter\n')
            if(not self.frame2.abunVar.get()): f.write('# Select Abundance          =# ABUN abundance scallops per square meter\n')
            if(self.frame2.bmsVar.get()):      f.write('Select BMS                =# BMMT biomass in metric tons\n')
            if(not self.frame2.bmsVar.get()):  f.write('# Select BMS                =# BMMT biomass in metric tons\n')
            if(self.frame2.ebmsVar.get()):     f.write('Select Expl BMS           =# EBMS exploitable biomass in metric tons\n')
            if(not self.frame2.ebmsVar.get()): f.write('# Select Expl BMS           =# EBMS exploitable biomass in metric tons\n')
            if(self.frame2.feffVar.get()):     f.write('Select Fishing Effort     =# FEFF Fishing Effort\n')
            if(not self.frame2.feffVar.get()): f.write('# Select Fishing Effort     =# FEFF Fishing Effort\n')
            if(self.frame2.fmortVar.get()):    f.write('Select Fishing Mortality  =# FMOR Fishing Mortality\n')
            if(not self.frame2.fmortVar.get()):f.write('# Select Fishing Mortality  =# FMOR Fishing Mortality\n')
            if(self.frame2.landVar.get()):     f.write('Select Landings by Number =# LAND Landings by number of scallops\n')
            if(not self.frame2.landVar.get()): f.write('# Select Landings by Number =# LAND Landings by number of scallops\n')
            if(self.frame2.lndwVar.get()):     f.write('Select Landings by Weight =# LNDW Landings by weight in grams\n')
            if(not self.frame2.lndwVar.get()): f.write('# Select Landings by Weight =# LNDW Landings by weight in grams\n')
            if(self.frame2.lpueVar.get()):     f.write('Select LPUE               =# LPUE Landing Per Unit Effor, (per day)\n')
            if(not self.frame2.lpueVar.get()): f.write('# Select LPUE               =# LPUE Landing Per Unit Effor, (per day)\n')
            if(self.frame2.recrVar.get()):     f.write('Select RECR               =# Recruitment\n')
            if(not self.frame2.recrVar.get()): f.write('# Select RECR               =# Recruitment\n')
            f.close()

    def ConvertMonthDayToDay(self,month,day):
        daysInYear = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
        monDict = {'Jan':0, 'Feb':1, 'Mar':2, 'Apr':3, 'May':4, 'Jun':5, 'Jul':6, 'Aug':7, 'Sep':8, 'Oct':9, 'Nov':10, 'Dec':11} 
        return daysInYear[monDict[month]] + day - 1
            
    def WriteRecruitmentConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.recrCfgFile.myEntry.get())

        # TODO Assumes MMM D[D]
        periodStr = self.frame1.startDay.myEntry.get()
        m = len(periodStr)
        month = periodStr[0:3]
        day = int(periodStr[3:m])
        startPeriod = self.ConvertMonthDayToDay(month,day)

        periodStr = self.frame1.stopDay.myEntry.get()
        m = len(periodStr)
        month = periodStr[0:3]
        day = int(periodStr[3:m])
        stopPeriod = self.ConvertMonthDayToDay(month,day)

        with open(cfgFile, 'w') as f:
            f.write('# configuration file for Recruitment\n')
            f.write('Start Period = '+str(startPeriod)+'  # Jan 1 is 0\n')
            f.write('Stop Period = '+str(stopPeriod)  +' # converted to fraction of year, i.e. /365\n')
            f.close()
            
    def WriteMortalityConfig(self):
        simCfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.mortCfgFile.myEntry.get())
        with open(simCfgFile, 'w') as f:
            f.write('# configuration file for mortality\n')
            f.write('Fishing Mortality = ' + self.frame3.fishMort.myEntry.get()   + '\n')
            f.write('# Fishing can be USD, BMS, or, CAS\n')
            f.write('Fishing = '          + self.frame3.fishSelect.myEntry.get() + '\n')
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
            f.write('# special area fishing mortalities\n# to use default value set to NONE\n')
            f.write('Fishing Mortality File = '+ self.frame1a.fishMortFile.myEntry.get() + '\n')
            f.write('# Used to compute LPUE\n')
            f.write('LPUE Slope = '        + self.frame3.lpueSlope.myEntry.get() +   '\n')
            f.write('LPUE Slope2 = '       + self.frame3.lpueSlope2.myEntry.get() +  '\n')
            f.write('LPUE Intercept = '    + self.frame3.lpueIntcept.myEntry.get() + ' # slope and intercept of regression\n')
            f.write('Max Per Day = '       + self.frame3.maxPerDay.myEntry.get() +   '  # max scallops shucked per day\n')
            f.write('Max Time = '          + self.frame3.maxTime.myEntry.get() +     '  # Max hours dredging per day  \n')
            f.write('Dredge Width = '      + self.frame3.dredgeWth.myEntry.get() +   '    # average total dredge width meters\n')
            f.write('Towing Speed = '      + self.frame3.towSpeed.myEntry.get() +    '  # knots, mean towing speed\n')
            f.close()

    def WriteGridMgrConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.gmCfgFile.myEntry.get())
        with open(cfgFile, 'w') as f:
            f.write('# configuration file for GridManager\n')
            f.write('# The following is the file name with corner coordinates associated with Special Access Areas.\n')
            f.write('# If not used then set to NONE or comment out line\n')
            f.write('# NOTE: Setting to NONE will also cause Mortality to not read in\n')
            f.write('# its special access fishing mortalities and default Fishing Mortalities are used.\n')
            f.write('Special Access Config File = '+self.frame1a.specAccFile.myEntry.get()+'\n')
            f.close()

    def WriteUKConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.ukCfgFile.myEntry.get())
        with open(cfgFile, 'w') as f:
            f.write('# Set inputs for universal kriging\n')
            f.write('# Observation files are expecting in the Data subdirectory\n')
            f.write('#\n')
            f.write('#(max interp field < hlf*max(obs))\n')
            f.write('High Limit Factor = '+self.frame4.highLimit.myEntry.get()+'\n')
            f.write('Kriging variogram form = '+self.frame4.form.myEntry.get()+'\n')
            f.write('#\n')
            f.write('# Keep this line before "Power Transform Parameter"\n')
            f.write('#\n')
            f.write('Log Transform = '+self.frame4.useLogTrans.myEntry.get()+'\n')
            f.write('#\n')
            f.write('# Power transform interpolates f(x)^alpha \n')
            f.write('# generally 0< alpha < 1 but this has not been tested \n')
            f.write('# not used if "Log Transform = T"\n')
            f.write('#\n')
            f.write('Power Transform Parameter = '+self.frame4.powerTrans.myEntry.get()+'\n')
            f.write('#\n')
            f.write('# Configuration files are expected to be in the Configuration directory\n')
            f.write('#\n')
            f.write('NLS Spatial Fcn File Name = '+self.frame4.spatCfgFile.myEntry.get()+'\n')
            f.write('#\n')
            f.write('# Save interim data by writing out supporting data files\n')
            f.write('#\n')
            f.write('Save Data = F\n')
            f.close()

    def WriteSpatialFncsConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration/'+self.frame4.spatCfgFile.myEntry.get())
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
               f.write(', precon='+self.frame4.functions[i].myEntry.get()+'\n')
            f.close()

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
                self.tsInYear = int(value)
            elif (tag == 'Save By Stratum'):
                self.savedByStratum = value[0] == 'T'
        return (paramStr, paramVal)

    def ReadGridMgrConfigFile(self):
        tags = self.ReadConfigFile(self.gmConfigFile)

        for (tag, value) in tags:
            if (tag == 'Special Access Config File'): self.specAccFileStr = value


def main():
    r = MainApplication('GeoSAMS')
    r.mainloop()

if __name__ == "__main__":
    main()
