import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

import subprocess
import os
import sys
import csv
import platform


class MainApplication(tk.Tk):
    def __init__(self, title):
        super().__init__()

        # member variables
        self.tsInYear = 0
        self.paramVal = 0
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
        self.simConfigFile = os.path.join(self.root,'Configuration/'+self.frame1.simCfgFile.myEntry.get())
        self.mortConfigFile = os.path.join(self.root,'Configuration/'+self.frame1.mortCfgFile.myEntry.get())
        self.recrConfigFile = os.path.join(self.root,'Configuration/'+self.frame1.recrCfgFile.myEntry.get())
        self.gmConfigFile = os.path.join(self.root,'Configuration/'+self.frame1.gmCfgFile.myEntry.get())

        # Read in configuration parameters
        self.ReadSimConfigFile()
        #
        # NOTE: MA does not use stratum and forces it to false
        # 
        self.frame2 = Frame2(self.notebook, self.tsInYear, self.paramVal, self.savedByStratum)
        self.frame3 = Frame3(self.notebook, self.mortConfigFile)
        self.frame4 = Frame4(self.notebook, self.frame1.domainName.myEntry.get)

        # Update strings based on given configuration files
        # Frame 3 reads in Mortality config file
        self.frame1.fishMortFile.myEntry.insert(0, self.frame3.fmorFileStr)
        self.ReadGridMgrConfigFile()
        self.frame1.specAccFile.myEntry.insert(0, self.specAccFileStr)

        self.notebook.add(self.frame1, text='Main')
        self.notebook.add(self.frame2, text='Outputs')
        self.notebook.add(self.frame3, text='Mortality')
        self.notebook.add(self.frame4, text='Interpolation')
        self.notebook.pack()

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#0F0")
        ttk.Button(self, text='START', style="Custom.TLabel", command=self.Run_Sim).pack()

    def Run_Sim(self):
        # No check for variables changed, therefore update all configuration files with current values in GUI
        # OR
        # Create new files based on names given by user, or same if not changed
        # 
        # typical command line:
        # > .\SRC\ScallopPopDensity.exe Scallop.cfg StartYear StopYear Domain
        self.WriteScallopConfig()
        self.WriteRecruitmentConfig()
        self.WriteMortalityConfig()
        self.WriteGridMgrConfig()
        self.WriteUKConfig()
        self.WriteSpatialFncsConfig()

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
            f.write('Fishing Mortality File = '+ self.frame1.fishMortFile.myEntry.get() + '\n')
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
            f.write('Special Access Config File = '+self.frame1.specAccFile.myEntry.get()+'\n')
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
            for i in range(int(self.frame4.numFcns.get())):
               f.write('Function, dim='+self.frame4.function[i].dimVal.get())
               f.write(', shape='+self.frame4.function[i].shapeVal.get())
               f.write(', precon='+self.frame4.function[i].myEntry.get()+'\n')
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
        tags = self.ReadConfigFile(self.simConfigFile)

        for (tag, value) in tags:
            # Python 3.8 does not have match/case so using if elif
            if (tag == 'Select Abundance'):
                paramStr.append('ABUN_')
                self.paramVal += 8
            elif (tag == 'Select BMS'):
                paramStr.append('BMMT_')
                self.paramVal += 4
            elif (tag == 'Select Expl BMS'):
                paramStr.append('EBMS_')
                self.paramVal += 2
            elif (tag == 'Select Fishing Effort'):
                paramStr.append('FEFF_')
                self.paramVal += 64
            elif (tag == 'Select Fishing Mortality'):
                paramStr.append('FMOR_')
                self.paramVal += 128
            elif (tag == 'Select Landings by Number'):
                paramStr.append('LAND_')
                self.paramVal += 32
            elif (tag == 'Select Landings by Weight'):
                paramStr.append('LNDW_')
                self.paramVal += 16
            elif (tag == 'Select LPUE'):
                paramStr.append('LPUE_')
                self.paramVal += 1
            elif (tag == 'Select RECR'):
                paramStr.append('RECR_')
                self.paramVal += 256
            elif (tag == 'Time steps per Year'):
                self.tsInYear = int(value)
            elif (tag == 'Save By Stratum'):
                self.savedByStratum = value[0] == 'T'
        return paramStr

    def ReadGridMgrConfigFile(self):
        tags = self.ReadConfigFile(self.gmConfigFile)

        for (tag, value) in tags:
            if (tag == 'Special Access Config File'): self.specAccFileStr = value


class Frame1(ttk.Frame):
    def __init__(self, container):
        super().__init__()

        self.style = ttk.Style()
        self.style.configure('Frame1.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame1.TFrame.Label', font=('courier', 8, 'bold'))

        growthFrame = ttk.LabelFrame(self, text='Growth', style='Frame1.TFrame')
        self.startYr    = SubFrameElement(self, growthFrame, 'Start Year', '2015',        0, 0, 1)
        self.stopYr     = SubFrameElement(self, growthFrame, 'Stop Year ', '2017',        1, 0, 1)
        self.domainName = SubFrameElement(self, growthFrame, 'Domain Name\nMA or GB', 'MA',         2, 0, 1)
        growthFrame.grid(row=0, column=0)

        recruitFrame = ttk.LabelFrame(self, text='Recruitment', style='Frame1.TFrame')
        self.startDay    = SubFrameElement(self, recruitFrame, 'Start Day\nMmm DD', 'Jan 1',  0, 0, 1)
        self.stopDay     = SubFrameElement(self, recruitFrame, 'Stop Day\nMmm DD ', 'Apr 11', 1, 0, 1)
        recruitFrame.grid(row=0, column=1)

        configFrame = ttk.LabelFrame(self, text='Configuration Files', style='Frame1.TFrame')
        self.mortCfgFile = SubFrameElement(self, configFrame, 'Mortality Config File', 'Mortality.cfg',  0, 0, 1)
        self.recrCfgFile = SubFrameElement(self, configFrame, 'Recruitment File',      'Recruitment.cfg',1, 0, 1)
        self.gmCfgFile   = SubFrameElement(self, configFrame, 'Grid Manager File',     'GridManager.cfg',2, 0, 1)
        self.simCfgFile  = SubFrameElement(self, configFrame, 'Sim Config File',       'Scallop.cfg',    3, 0, 1)
        self.ukCfgFile   = SubFrameElement(self, configFrame, 'UK Config File',        'UK.cfg',         4, 0, 1)
        configFrame.grid(row=1, column=0)

        specialAccFrame = ttk.LabelFrame(self, text='Special Access', style='Frame1.TFrame')
        self.specAccFile  = SubFrameElement(self, specialAccFrame, 'Special Access Points', '', 0, 0, 1)
        self.fishMortFile = SubFrameElement(self, specialAccFrame, 'Fishing Mort File', '', 1, 0, 1)

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#080")
        self.openFishCSVButton = ttk.Button(specialAccFrame, text='View', style="Custom.TLabel", command=self.OpenFishingCSV)
        self.openFishCSVButton.grid(row=0, column=2)

        self.tree = ttk.Treeview(specialAccFrame, show="headings")
        self.tree.grid(row=2, column=0, columnspan=15, padx=10)

        specialAccFrame.grid(row=1, column=1)

    def OpenFishingCSV(self):
        file_path = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")])
        if file_path:
            self.DisplayCSVData(file_path)

    def DisplayCSVData(self, file_path):
        try:
            with open(file_path, 'r', newline='') as file:
                csv_reader = csv.reader(file)
                header = next(csv_reader)  # Read the header row
                self.tree.delete(*self.tree.get_children())  # Clear the current data

                self.tree["columns"] = header
                for col in header:
                    self.tree.column(col, width=50, stretch=True)
                    self.tree.heading(col, text=col)
                    #print(self.tree.column(col, 'width'))

                for row in csv_reader:
                    self.tree.insert("", "end", values=row)

        except Exception as e:
            messagebox.showerror("GeoSAM Sim", f"Error: {str(e)}")


class Frame2(ttk.Frame):
    def __init__(self, container, tsPerYear, selectedOutputs, useStratum):
        super().__init__()

        self.style = ttk.Style()
        self.style.configure('Frame2.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame2.TFrame.Label', font=('courier', 8, 'bold'))

        outputsFrame = ttk.LabelFrame(self, text='Outputs', style='Frame1.TFrame')
        self.tsPerYear  = SubFrameElement(self, outputsFrame, 'tsPerYear', str(tsPerYear),  0, 0, 1)
        self.useStratum = SubFrameElement(self, outputsFrame, 'Use Stratum\n(Not Used by MA)', str(useStratum), 1, 0, 1)
        outputsFrame.grid(row=0, column=0, columnspan=3)

        # Check Buttons
        self.lpueVar = tk.IntVar(value=(selectedOutputs   )&1)
        self.ebmsVar = tk.IntVar(value=(selectedOutputs>>1)&1)
        self.bmsVar  = tk.IntVar(value=(selectedOutputs>>2)&1)
        self.abunVar = tk.IntVar(value=(selectedOutputs>>3)&1)
        self.lndwVar = tk.IntVar(value=(selectedOutputs>>4)&1)
        self.landVar = tk.IntVar(value=(selectedOutputs>>5)&1)
        self.feffVar = tk.IntVar(value=(selectedOutputs>>6)&1)
        self.fmortVar= tk.IntVar(value=(selectedOutputs>>7)&1)
        self.recrVar = tk.IntVar(value=(selectedOutputs>>8)&1)
        ttk.Checkbutton(self, text='Abundance',     variable=self.abunVar, command=self.CBSelected).grid(row=2,  column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='Biomass',       variable=self.bmsVar,  command=self.CBSelected).grid(row=2,  column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='ExplBiomass',   variable=self.ebmsVar, command=self.CBSelected).grid(row=2,  column=2, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='LPUE',          variable=self.lpueVar, command=self.CBSelected).grid(row=2,  column=3, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='Fish Mort',    variable=self.fmortVar, command=self.CBSelected).grid(row=3,  column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='Fish Effort',  variable=self.feffVar , command=self.CBSelected).grid(row=3,  column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='Lands By Num',  variable=self.landVar , command=self.CBSelected).grid(row=3, column=2, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='Lands By Wght', variable=self.lndwVar , command=self.CBSelected).grid(row=3, column=3, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(self, text='Recruitment',   variable=self.recrVar , command=self.CBSelected).grid(row=4, column=2, sticky='sw', padx=10, pady=5)
    
    def CBSelected(self):
        self.desiredOutput = self.abunVar.get()*8 + self.bmsVar.get()*4 + self.ebmsVar.get()*2 + self.lpueVar.get()
        self.desiredOutput+= self.fmortVar.get()*128 + self.feffVar.get()*64 + self.landVar.get()*32 + self.lndwVar.get()*16
        self.desiredOutput+= self.recrVar.get()*256
        # print(f'0X{self.desiredOutput:X}')

class Frame3(ttk.Frame, MainApplication):
    def __init__(self, container, fName):
        super().__init__()

        self.UpdateValues(fName)

        self.style = ttk.Style()
        self.style.configure('Frame3.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame3.TFrame.Label', font=('courier', 8, 'bold'))

        
        fishingFrame= ttk.LabelFrame(self, text='Mortality', style='Frame3.TFrame')
        self.fishMort   = SubFrameElement(self, fishingFrame, 'Fishing Mort', self.fmortStr,            0, 0, 1)
        self.fishSelect = SubFrameElement(self, fishingFrame, 'Fishing Effort\n(USD, BMS, or CAS)', self.fishStr, 1, 0, 1)
        self.alphaMort  = SubFrameElement(self, fishingFrame, 'Alpha Mortality',self.alphStr,           2, 0, 1)
        self.maAdultMort= SubFrameElement(self, fishingFrame, 'MA Adult Mortality',self.maAdultMortStr, 3, 0, 1)
        self.gbAdultMort= SubFrameElement(self, fishingFrame, 'GB Adult Mortality',self.gbAdultMortStr, 4, 0, 1)
        self.maLength0  = SubFrameElement(self, fishingFrame, 'MA Length_0',  self.maLen0Str,        5, 0, 1)
        self.gbLength0  = SubFrameElement(self, fishingFrame, 'GB Length_0',  self.gbLen0Str,        6, 0, 1)
        fishingFrame.grid(row=0, column=0, padx=10)

        ilfFrame = ttk.LabelFrame(self, text='Selectivity', style='Frame3.TFrame')
        self.maFSelectA       = SubFrameElement(self, ilfFrame, 'MA FSelectA', self.maFSelAStr,          0, 0, 1)
        self.maFSelectB       = SubFrameElement(self, ilfFrame, 'MA FSelectB', self.maFSelBStr,          1, 0, 1)
        self.gbClosedFSelectA = SubFrameElement(self, ilfFrame, 'GB Closed FSelectA', self.gbClFSelAStr, 2, 0, 1)
        self.gbClosedFSelectB = SubFrameElement(self, ilfFrame, 'GB Closed FSelectB', self.gbClFSelBStr, 3, 0, 1)
        self.gbOpenFSelectA   = SubFrameElement(self, ilfFrame, 'GB Open FSelectA', self.gbOpFSelAStr,   4, 0, 1)
        self.gbOpenFSelectB   = SubFrameElement(self, ilfFrame, 'GB Open FSelectB', self.gbOpFSelBStr,   5, 0, 1)
        ilfFrame.grid(row=0, column=1, padx=10)
        
        lpueFrame   = ttk.LabelFrame(self, text='LPUE', style='Frame3.TFrame')
        self.lpueSlope   = SubFrameElement(self, lpueFrame, 'LPUE Slope', self.lpueSlStr,       0, 0, 1)
        self.lpueSlope2  = SubFrameElement(self, lpueFrame, 'LPUE Slope2', self.lpueSl2Str,     1, 0, 1)
        self.lpueIntcept = SubFrameElement(self, lpueFrame, 'LPUE Intercept', self.lpueIntcStr, 2, 0, 1)
        self.maxPerDay   = SubFrameElement(self, lpueFrame, 'Max Per Day', self.maxPerDayStr,   3, 0, 1)
        self.maxTime     = SubFrameElement(self, lpueFrame, 'Max Time', self.maxTimeStr,        4, 0, 1)
        self.dredgeWth   = SubFrameElement(self, lpueFrame, 'Dredge Width', self.dredgeWdStr,   5, 0, 1)
        self.towSpeed    = SubFrameElement(self, lpueFrame, 'Towing Speed', self.towSpdStr,     6, 0, 1)
        lpueFrame.grid(row=0, column=2, padx=10)

        incidentalFrame = ttk.LabelFrame(self, text='Incidental', style='Frame3.TFrame')
        self.maIncident = SubFrameElement(self, incidentalFrame, 'MA Incidental', self.maIncidStr, 0, 0, 1)
        self.gbIncident = SubFrameElement(self, incidentalFrame, 'GB Incidental', self.gbIncidStr, 1, 0, 1)
        incidentalFrame.grid(row=1, column=0, padx=10)

        discardFrame   = ttk.LabelFrame(self, text='Discard', style='Frame3.TFrame')
        self.maCullSize = SubFrameElement(self, discardFrame, 'MA Cull Size', self.maCullStr, 0, 0, 1)
        self.maDiscard  = SubFrameElement(self, discardFrame, 'MA Discard',   self.maDiscStr, 1, 0, 1)
        self.gbCullSize = SubFrameElement(self, discardFrame, 'GB Cull Size', self.gbCullStr, 0, 2, 3)
        self.gbDiscard  = SubFrameElement(self, discardFrame, 'GB Discard',   self.gbDiscStr, 1, 2, 3)
        discardFrame.grid(row=1, column=1, columnspan=2, padx=10)

    def UpdateValues(self, fName):
        # read current settings
        tags = self.ReadConfigFile(fName)
        for (tag, value) in tags:
            # Python 3.8 does not have match/case so using if elif
            if   tag == 'Fishing Mortality': self.fmortStr = value
            elif tag == 'Fishing' :          self.fishStr = value
            elif tag == 'Alpha Mortality':   self.alphStr = value
            elif tag == 'MA Cull size':      self.maCullStr = value
            elif tag == 'MA Discard':        self.maDiscStr = value
            elif tag == 'GB Cull size':      self.gbCullStr = value
            elif tag == 'GB Discard':        self.gbDiscStr = value
            elif tag == 'MA FSelectA':       self.maFSelAStr = value
            elif tag == 'MA FSelectB':       self.maFSelBStr = value
            elif tag == 'GB Closed FSelectA': self.gbClFSelAStr = value
            elif tag == 'GB Closed FSelectB': self.gbClFSelBStr = value
            elif tag == 'GB Open FSelectA':   self.gbOpFSelAStr = value
            elif tag == 'GB Open FSelectB':   self.gbOpFSelBStr = value
            elif tag == 'MA Adult Mortality': self.maAdultMortStr = value
            elif tag == 'GB Adult Mortality': self.gbAdultMortStr = value
            elif tag == 'MA Incidental':      self.maIncidStr = value
            elif tag == 'GB Incidental':      self.gbIncidStr = value
            elif tag == 'MA Length_0':        self.maLen0Str = value
            elif tag == 'GB Length_0':        self.gbLen0Str = value
            elif tag == 'Fishing Mortality File': self.fmorFileStr = value
            elif tag == 'LPUE Slope':         self.lpueSlStr = value
            elif tag == 'LPUE Slope2':        self.lpueSl2Str = value
            elif tag == 'LPUE Intercept':     self.lpueIntcStr = value
            elif tag == 'Max Per Day':        self.maxPerDayStr = value
            elif tag == 'Max Time':           self.maxTimeStr = value
            elif tag == 'Dredge Width':       self.dredgeWdStr = value
            elif tag == 'Towing Speed':       self.towSpdStr = value

class Frame4(ttk.Frame):
    def __init__(self, container, get):
        super().__init__()

        self.myget = get
        self.domainName = self.myget()
        self.style = ttk.Style()
        self.style.configure('Frame4.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame4.TFrame.Label', font=('courier', 8, 'bold'))
        
        self.funcFrame = ttk.LabelFrame(self, text='Spatial Functions', style='Frame4.TFrame')

        self.numFncsLabel = ttk.Label(self.funcFrame, text='Number')
        self.numFncsLabel.grid(row=0, column=0, sticky='w')
        self.numFcns=ttk.Entry(self.funcFrame)
        self.numFcns.grid(row=0, column=0, sticky='e')
        if self.domainName == 'MA':
            self.numFcns.insert(0, '9')
        else:
            self.numFcns.insert(0, '5')

        self.function = []
        # paint first row to show x, y, and z
        # remaining rows are just x, y
        desNumCol = 2
        for i in range(int(self.numFcns.get())):
            if i < 3:
                row = 1
                precon = 0
                self.function.append(SubFrameInterpFunction(self, self.funcFrame, str(i+1), 'z', 'Logistic', precon, row, i%3))
            else:
                row = ((i-3) // desNumCol) + 2
                col = (i-3) % desNumCol
                precon = row - 1
                self.function.append(SubFrameInterpFunction(self, self.funcFrame, str(i+1), chr(120+col), 'Logistic', precon, row, col))
        self.funcFrame.grid(row=0, column=1)
        self.function[1].dimVal.set('x')
        self.function[2].dimVal.set('y')

        paramFrame= ttk.LabelFrame(self, text='Parameters', style='Frame4.TFrame')
        self.highLimit   = SubFrameElement(self, paramFrame, 'High Limit Factor ', '1.5',  0, 0, 1)
        self.form = SubFrameElement(self, paramFrame, 'variogram form', 'spherical', 1, 0, 1)
        self.useLogTrans  = SubFrameElement(self, paramFrame, 'Use Log Transfrom', 'True', 2, 0, 1)
        self.powerTrans  = SubFrameElement(self, paramFrame, 'Power Tranform\n(Not used if Log = True)', '1.0', 3, 0, 1)
        self.spatCfgFile  = SubFrameElement(self, paramFrame, 'Spatial Fcn Config File', 'SpatialFcns.cfg', 4, 0, 1)
        paramFrame.grid(row=0, column=4)

        self.bind("<Visibility>", self.on_visibility)

    def on_visibility(self, event):
            self.domainName = self.myget()
            n = int(self.numFcns.get())
            for i in range(n):
                self.function[i].funcFrame.grid_forget()

            self.numFcns.delete(0,2)
            if self.domainName == 'MA':
                self.numFcns.insert(0, '9')
            else:
                self.numFcns.insert(0, '5')

            self.function = []
            # paint first row to show x, y, and z
            # remaining rows are just x, y
            desNumCol = 2
            for i in range(int(self.numFcns.get())):
                if i < 3:
                    self.function.append(SubFrameInterpFunction(self, self.funcFrame, str(i+1), 'z', 'Logistic', 0, 1, i%3))
                else:
                    row = ((i-3) // desNumCol) + 2
                    col = (i-3) % desNumCol
                    precon = row - 1
                    self.function.append(SubFrameInterpFunction(self, self.funcFrame, str(i+1), chr(120+col), 'Logistic', precon, row, col))
            self.function[1].dimVal.set('x')
            self.function[2].dimVal.set('y')

class SubFrameElement(tk.Frame):
    def __init__(self, container, parent, label, value, elementRow, labelCol, entryCol):
        super().__init__()
        self.myEntry=ttk.Entry(parent)
        self.myEntry.grid(row=elementRow, column=entryCol, rowspan=1, sticky='nsew', pady=10)
        self.myEntry.insert(0, value)
        self.myLabel = ttk.Label(parent, text=label, wraplength=200, anchor='nw', justify='right')
        self.myLabel.grid(row=elementRow, column=labelCol, rowspan=1, sticky='nsew', pady=10)


class SubFrameInterpFunction(tk.Frame):
    def __init__(self, container, parent, funcNum, dim, shape, preconNum, elementRow, elementCol):
        super().__init__()

        self.funcFrame = ttk.LabelFrame(parent, text='Function '+str(funcNum))
        dimFrame = ttk.LabelFrame(self.funcFrame, text='dim')
        self.dimVal = tk.StringVar(dimFrame, dim)
        self.myDimRBx = ttk.Radiobutton(dimFrame, text='x', value='x', variable=self.dimVal, command=self.GetDimRB).pack()
        self.myDimRBy = ttk.Radiobutton(dimFrame, text='y', value='y', variable=self.dimVal, command=self.GetDimRB).pack()
        self.myDimRBz = ttk.Radiobutton(dimFrame, text='z', value='z', variable=self.dimVal, command=self.GetDimRB).pack()
        dimFrame.grid(row=elementRow, column=0, rowspan=3, sticky='nsew', pady=10)

        shapeFrame = ttk.LabelFrame(self.funcFrame, text='shape')
        self.shapeVal = tk.StringVar(shapeFrame, shape)
        self.myShapeG = ttk.Radiobutton(shapeFrame, text='Gaussian', value='Gaussian', variable=self.shapeVal, command=self.GetShapeRB).pack()
        self.myShapeL = ttk.Radiobutton(shapeFrame, text='Logistic', value='Logistic', variable=self.shapeVal, command=self.GetShapeRB).pack()
        self.myShapeS = ttk.Radiobutton(shapeFrame, text='SinExp',   value='SinExp',   variable=self.shapeVal, command=self.GetShapeRB).pack()
        self.myShapeC = ttk.Radiobutton(shapeFrame, text='CosExp',   value='CosExp',   variable=self.shapeVal, command=self.GetShapeRB).pack()
        shapeFrame.grid(row=elementRow, column=1, rowspan=3, sticky='nsew', pady=10)

        self.precon = ttk.Label(self.funcFrame, text='precon')
        self.precon.grid (row=elementRow, column=2, columnspan=1, sticky='n', pady=10)
        self.myEntry=ttk.Entry(self.funcFrame)
        self.myEntry.grid(row=elementRow, column=2, columnspan=1, sticky='s', pady=10)
        self.myEntry.insert(0, preconNum)

        self.funcFrame.grid(row=elementRow, column=elementCol)

    def GetDimRB(self):
        print(self.dimVal.get())

    def GetShapeRB(self):
        print(self.shapeVal.get())

def main():
    r = MainApplication('GeoSAMS')
    r.mainloop()

if __name__ == "__main__":
    main()
