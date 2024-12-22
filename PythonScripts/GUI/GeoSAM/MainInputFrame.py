## @page page1 Main
# @section p2p1 Growth subframe that identifies
# These are the parameters used to control how long the scallop growth is simulated as well as the
# granularity of the growth computations
#
#   - Start Year of the simulation
#   - Stop Year of the simulation
#   - Time Steps per year
#   - Domain Name or region of interest, Mid-Atlantic, MA, or Georges Bannk, GB
#   - Sort By Statum: Used when processing Georges-Bank to break the region into quadrants due to its unique shape
#  
# @section p2p2 Recuitment
# Recruitment is only applied at a certain time of the year. These values determine this period. Combo boxes
# are used to format the formatting of the month and day. 
#
#   - Start Day, calendar day of the year when recruitment influence begins. 
#   - Stop Day, calendar day of the year when reruitment influence ends
#
# @section p2p3 Configuration Files
# Files used by the sim to set up parameters. The GUI can use the default values or change the names before
# starting the sim. The initial names are the default names of the files when first downloaded from GitHub.
# The names can be changed and the GUI sets up the simulation to use the new names.
#
# @section p2p4  Output Selection
# Checkboxes to allow the user to select the desired parameters of interest. This is used to save processing 
# time rather than processing everything. Especially true during interpolation as it would take over and hour 
# to do the interpolation. For example for MA with 11631 grid locations.
#   - Approx 2 minutes per interpolation
#   - Given 3 years worth of data, plus initial conditions
#   - 9 listed outputs
#
# Thus 9 x 4 x 2 or 72 minutes. GB is proportionately shorter with only 6802 grid locations.
#
import os
import subprocess
import webbrowser
import glob
import tkinter as tk
from tkinter import messagebox
from tkinter import ttk
from tkinter import filedialog
from Widgets import *
from Globals import *

##
# This class displays information about GeoSAMS simulation. This same information is used
# on the command line when starting SRC\\ScallopPopDensity
#
class MainInput(ttk.Frame):

    def __init__(self, container, friend, tsPerYear, selectedOutputs, maxYears):
        super().__init__()
        self.root = os.getcwd() #os.environ['ROOT']
        self.simStartDir = os.path.join(self.root, configDir, simCfgDir)
        self.interpStartDir = os.path.join(self.root, configDir, interCfgDir)
        self.surveyStartDir = os.path.join(self.root, surveyDataDir)
        self.shapeFileDir = os.path.join(self.root, shapeFileDir)
        self.friend = friend
        self.maxYears = maxYears

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        simFrame = ttk.LabelFrame(self, text='Simulation Configuration Files', style='SAMS.TFrame')
        self.recrCfgFile = SubFrameElement(self, simFrame, 'Recruit Config File  ', 'Recruitment.cfg',1, 0, 1, width=20)
        self.simCfgFile  = SubFrameElement(self, simFrame, 'Sim Config File      ', 'Scallop.cfg',    2, 0, 1, width=20)
        #-------------------------------------------------------------------------------------------
        self.openRecrConfigButton = ttk.Button(simFrame, text='Change/Save Recr File', style="BtnBluGrn.TLabel", command=self.GetRecrConfigFName)
        self.openRecrConfigButton.grid(row=1, column=3)
        #-------------------------------------------------------------------------------------------
        self.openSimConfigButton = ttk.Button(simFrame, text='Change/Save Sim File', style="BtnBluGrn.TLabel", command=self.GetSimConfigFName)
        self.openSimConfigButton.grid(row=2, column=3)
        #-------------------------------------------------------------------------------------------
        simFrame.grid(row=1, column=0, padx=5, pady=0, sticky='n')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        showResultsFrame = ttk.LabelFrame(self, text='View Plots', style='SAMS.TFrame')
        # --------------------------------------------------------------------------------------------------------
        self.openPDFButton = ttk.Button(showResultsFrame, text='Open PDF File', style="BtnGreen.TLabel", command=self.OpenPDF)
        self.openPDFButton.grid(row=0, column=0, padx=10, sticky='we')
        #-------------------------------------------------------------------------------------------
        showResultsFrame.grid(row=2, column=1, padx=5, pady=0, sticky='ne')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        survDataFrame = ttk.LabelFrame(self, text='Survey Data Files, Environment Variables', style='SAMS.TFrame')
        self.dredgeDataFile = SubFrameElement(self, survDataFrame, 'Dredge Survey Data File', 'dredgetowbysize7917',  0, 0, 1, width=40)
        self.habCamDataFile = SubFrameElement(self, survDataFrame, 'HabCam Survey Data File', 'Habcam_BySegment_2000_2011-2023_v2',1, 0, 1, width=40)
        #-------------------------------------------------------------------------------------------
        self.setDredgeDataButton = ttk.Button(survDataFrame, text='Set DredgeData', style="BtnBluGrn.TLabel", command=self.SetDredgeFileName)
        self.setDredgeDataButton.grid(row=0, column=3)
        #-------------------------------------------------------------------------------------------
        self.setHabCamDataButton = ttk.Button(survDataFrame, text='Set HabCamData', style="BtnBluGrn.TLabel", command=self.SetHabCamFileName)
        self.setHabCamDataButton.grid(row=1, column=3)
        #-------------------------------------------------------------------------------------------
        survDataFrame.grid(row=2, column=0, padx=5, pady=0)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        shapeFileFrame = ttk.LabelFrame(self, text='Shape Files', style='SAMS.TFrame')
        # --------------------------------------------------------------------------------------------------------
        self.maShapeFileEntry = ttk.Entry(shapeFileFrame, width=60)
        self.maShapeFileEntry.insert(0, 'MAB_Est_Areas_2024_UTM18_Habcam_GeoSAMS.shp')
        self.maShapeFileEntry.grid(row=0, column=1, sticky='w', padx=5)
        self.gbShapeFileEntry = ttk.Entry(shapeFileFrame, width=60)
        self.gbShapeFileEntry.insert(0, 'GB_Est_Areas_2024_UTM19_Habcam_GeoSAMS.shp')
        self.gbShapeFileEntry.grid(row=1, column=1, sticky='w', padx=5)

        self.maShapeBufferFileEntry = ttk.Entry(shapeFileFrame, width=60)
        self.maShapeBufferFileEntry.insert(0, 'MAB_Est_Areas_2024_UTM18_Habcam_Buffer_15k_GeoSAMS.shp')
        self.maShapeBufferFileEntry.grid(row=2, column=1, sticky='w', padx=5)
        self.gbShapeBufferFileEntry = ttk.Entry(shapeFileFrame, width=60)
        self.gbShapeBufferFileEntry.insert(0, 'GB_Est_Areas_2024_UTM19_Habcam_Buffer_5k_GeoSAMS.shp')
        self.gbShapeBufferFileEntry.grid(row=3, column=1, sticky='w', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.setMaShapeFileButton = ttk.Button(shapeFileFrame, text='Set MA Shape File', style="BtnBluGrn.TLabel", command=self.SetMaShapeFile)
        self.setMaShapeFileButton.grid(row=0, column=2, sticky='e')
        # --------------------------------------------------------------------------------------------------------
        self.setGbShapeFileButton = ttk.Button(shapeFileFrame, text='Set GB Shape File', style="BtnBluGrn.TLabel", command=self.SetGbShapeFile)
        self.setGbShapeFileButton.grid(row=1, column=2, sticky='e')
        # --------------------------------------------------------------------------------------------------------
        self.setMaShapeBufferFileButton = ttk.Button(shapeFileFrame, text='Set MA Shape Buffer File', style="BtnBluGrn.TLabel", command=self.SetMaShapeBufferFile)
        self.setMaShapeBufferFileButton.grid(row=2, column=2, sticky='e')
        # --------------------------------------------------------------------------------------------------------
        self.setGbShapeBufferFileButton = ttk.Button(shapeFileFrame, text='Set GB Shape Buffer File', style="BtnBluGrn.TLabel", command=self.SetGbShapeBufferFile)
        self.setGbShapeBufferFileButton.grid(row=3, column=2, sticky='e')
        #-------------------------------------------------------------------------------------------
        shapeFileFrame.grid(row=3, column=0, padx=5, pady=0, sticky='sw')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        recruitFrame = ttk.LabelFrame(self, text='Recruitment', style='SAMS.TFrame')
        self.monthsArr = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
        self.startDayLabel = ttk.Label(recruitFrame, text='Start Day')
        self.startDayLabel.grid(row=0, column=0)
        #-------------------------------------------------------------------------------------------
        self.startDayComboMonth = ttk.Combobox(recruitFrame, width=4, values=self.monthsArr)
        self.startDayComboMonth.current(self.monthsArr.index('JUN'))
        self.startDayComboMonth.bind('<<ComboboxSelected>>', self.CheckStartDay)
        self.startDayComboMonth.grid(row=0, column=1, padx=5, sticky='w')
        #-------------------------------------------------------------------------------------------
        self.startDayComboDay = ttk.Combobox(recruitFrame, width=2, values=[str(i) for i in range(1,32)])
        self.startDayComboDay.current(0)
        self.startDayComboDay.bind('<<ComboboxSelected>>', self.CheckStartDay)
        self.startDayComboDay.grid(row=0, column=2, padx=5, sticky='e')
        #-------------------------------------------------------------------------------------------
        self.stopDayLabel = ttk.Label(recruitFrame, text='Stop Day')
        self.stopDayLabel.grid(row=1, column=0)
        #-------------------------------------------------------------------------------------------
        self.stopDayComboMonth = ttk.Combobox(recruitFrame, width=4, values=self.monthsArr)
        self.stopDayComboMonth.current(self.monthsArr.index('SEP'))
        self.stopDayComboMonth.bind('<<ComboboxSelected>>', self.CheckStopDay)
        self.stopDayComboMonth.grid(row=1, column=1, padx=5, sticky='w')
        #-------------------------------------------------------------------------------------------
        self.stopDayComboDay = ttk.Combobox(recruitFrame, width=2, values=[str(i) for i in range(1,32)])
        self.stopDayComboDay.current(9)
        self.stopDayComboDay.bind('<<ComboboxSelected>>', self.CheckStopDay)
        self.stopDayComboDay.grid(row=1, column=2, padx=5, sticky='e')
        #-------------------------------------------------------------------------------------------
        self.recrYrStrt = SubFrameElement(self, recruitFrame, 'Recruit Yr Start', '2012', 0, 3, 4,
                                          enterCmd=self.EnterKeyClicked, valCmd=numbersCallback)
        #-------------------------------------------------------------------------------------------
        self.recrYrStop = SubFrameElement(self, recruitFrame, 'Recruit Yr Stop ', '2023', 1, 3, 4,
                                          enterCmd=self.EnterKeyClicked, valCmd=numbersCallback)
        # --------------------------------------------------------------------------------------------------------
        self.numYrsAvg = SubFrameElement(self, recruitFrame, 'Number of Years to Avg', '3', 2, 3, 4,
                                          enterCmd=self.EnterKeyClicked, valCmd=numbersCallback)
        #-------------------------------------------------------------------------------------------
        recruitFrame.grid(row=3, column=1, padx=5, pady=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        durationFrame = ttk.LabelFrame(self, text='Duration:', style='SAMS.TFrame')
        #------------------------------------------------------------------------------------------
        self.startYr    = SubFrameElement(self, durationFrame, 'Start, @24:00 on May 31, ', '2022', 0, 0, 1,
                                          enterCmd=self.EnterKeyClicked, valCmd=numbersCallback)
        #-------------------------------------------------------------------------------------------
        self.stopYr     = SubFrameElement(self, durationFrame, 'End, @ 24:00 on May 31,', '2026', 1, 0, 1,
                                          enterCmd=self.EnterKeyClicked, valCmd=numbersCallback)
        #-------------------------------------------------------------------------------------------
        self.tsPerYear  = SubFrameElement(self, durationFrame, 'Time Steps / Year', str(tsPerYear), 2, 0, 1)
        #-------------------------------------------------------------------------------------------
        self.domainNameLabel = ttk.Label(durationFrame, text='Domain Name')
        self.domainNameLabel.grid(row=3, column=0, pady=5)
        #-------------------------------------------------------------------------------------------
        self.domainNameCombo = ttk.Combobox(durationFrame, width=3, values=['MA', 'GB', 'AL'])
        self.domainNameCombo.current(2)
        self.domainNameCombo.grid(row=3, column=1, sticky='w', pady=5)
        #-------------------------------------------------------------------------------------------
        durationFrame.grid(row=2, column=1, padx=5, pady=0, sticky='nw')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        outputSelFrame = ttk.LabelFrame(self, text='Output Selection', style='SAMS.TFrame')
        #-------------------------------------------------------------------------------------------
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
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='Abundance',     variable=self.abunVar,  command=self.CBSelectedOutput).grid(row=0, column=0, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='Biomass',       variable=self.bmsVar,   command=self.CBSelectedOutput).grid(row=0, column=1, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='ExplBiomass',   variable=self.ebmsVar,  command=self.CBSelectedOutput).grid(row=0, column=2, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='Fish Mort',    variable=self.fmortVar,  command=self.CBSelectedOutput).grid(row=1, column=0, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='Fish Effort',  variable=self.feffVar ,  command=self.CBSelectedOutput).grid(row=1, column=1, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='Lands By Num',  variable=self.landVar,  command=self.CBSelectedOutput).grid(row=1, column=2, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='Lands By Wght', variable=self.lndwVar , command=self.CBSelectedOutput).grid(row=2, column=0, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='LPUE',          variable=self.lpueVar,  command=self.CBSelectedOutput).grid(row=2, column=1, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        ttk.Checkbutton(outputSelFrame, text='Recruitment',   variable=self.recrVar , command=self.CBSelectedOutput).grid(row=2, column=2, sticky='sw', padx=10, pady=5)
        #-------------------------------------------------------------------------------------------
        outputSelFrame.grid(row=1, column=1, padx=5, pady=0, sticky='n')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Main Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=1)

    ## This method is called on both Enter Key clicked and goes out of focus
    #
    def EnterKeyClicked(self, event):
        # Check for unintended null values
        if self.startYr.myEntry.get() == '':
            self.startYr.myEntry.insert(0,'2000')
        if self.stopYr.myEntry.get() == '':
            self.stopYr.myEntry.insert(0,'2000')
        startYear = int(self.startYr.myEntry.get())
        stopYear = int(self.stopYr.myEntry.get())
        if stopYear <= startYear:
            stopYear = startYear + 1
            self.stopYr.myEntry.delete(0,tk.END)
            self.stopYr.myEntry.insert(0,str(stopYear))
            messagebox.showerror('YEAR RANGE','Stop Year is same or before Start Year\nSetting them 1 year offset')
        # We will have growth for each year plus the initial state
        numYears = stopYear - startYear + 1
        if numYears > self.maxYears:
            addYears = numYears - self.maxYears
            self.friend.frame5.AppendYears(addYears)
            self.friend.frame8.AppendYears(numYears)
            self.maxYears = numYears

    ## 
    #  @brief Checks start day to validate date is appropriate for month. Does not consider if leap year
    #
    def CheckStartDay(self, event):
        periodMonthStr = self.startDayComboMonth.get()
        periodDayStr = self.startDayComboDay.get()

        if periodMonthStr in ['SEP', 'APR', 'JUN', 'NOV'] and int(periodDayStr) > 30:
            self.startDayComboDay.current(29)
        if periodMonthStr == 'FEB' and int(periodDayStr) > 28:
            self.startDayComboDay.current(27)

    ## 
    #  @brief Checks stop day to validate date is appropriate for month. Does not consider if leap year
    #
    def CheckStopDay(self, event):
        periodMonthStr = self.stopDayComboMonth.get()
        periodDayStr = self.stopDayComboDay.get()

        if periodMonthStr in ['SEP', 'APR', 'JUN', 'NOV'] and int(periodDayStr) > 30:
            self.stopDayComboDay.current(29)
        if periodMonthStr == 'FEB' and int(periodDayStr) > 28:
            self.stopDayComboDay.current(27)

    ## 
    #  @brief Determines the value for which outputs are selected as they are checked.
    #
    def CBSelectedOutput(self):
        self.desiredOutput = self.ComputeSelectOuputValue()

    ## 
    #  @brief Updates the final value from which outputs are selected.
    #
    def GetSelectedOutputs(self):
        return self.ComputeSelectOuputValue()
    
    ## 
    #  @brief Bit shifts (multiplies) checkbuttons and computes bit position value.
    #
    def ComputeSelectOuputValue(self):
        value = (self.abunVar.get())    + (self.bmsVar.get()<<1)  + (self.ebmsVar.get()<<2) + (self.feffVar.get()<<3)\
              + (self.fmortVar.get()<<4) + (self.landVar.get()<<5) + (self.lndwVar.get()<<6) + (self.lpueVar.get()<<7)\
              + (self.recrVar.get()<<8)
        return value

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Recruitment Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetRecrConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Recruit Config File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.simStartDir)
        f = file_path.split('/')
        if file_path:
            self.recrCfgFile.myEntry.delete(0,tk.END)
            self.recrCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteRecruitmentConfig()

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Simulation Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetSimConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Simulation Config File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.simStartDir)
        f = file_path.split('/')
        if file_path:
            self.simCfgFile.myEntry.delete(0,tk.END)
            self.simCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteScallopConfig()

    ##
    # 
    def SetDredgeFileName(self):
        file_path = filedialog.askopenfilename(title="Set Dredge Survey Data File", filetypes=[("ZIP files", "*.zip")],
                    initialdir=self.surveyStartDir)
        f = file_path.split('/')
        fName = f[-1]
        end = len(fName) - 4
        if file_path:
            self.dredgeDataFile.myEntry.delete(0,tk.END)
            self.dredgeDataFile.myEntry.insert(0,fName[0:end])
        self.SetDredgeFileEnvVar()

    ##
    # 
    def SetDredgeFileEnvVar(self):
        os.environ["DredgeFile"] = self.dredgeDataFile.myEntry.get()
        # fname = os.path.join('OriginalData', os.environ["DredgeFile"]+'.csv')
        # print(fname)

    ##
    # 
    def SetHabCamFileName(self):
        file_path = filedialog.askopenfilename(title="Set HabCam Survey Data File", filetypes=[("ZIP files", "*.zip")],
                    initialdir=self.surveyStartDir)
        f = file_path.split('/')
        fName = f[-1]
        end = len(fName) - 4
        if file_path:
            self.habCamDataFile.myEntry.delete(0,tk.END)
            self.habCamDataFile.myEntry.insert(0,fName[0:end])
        self.SetHabCamFileEnvVar()

    ##
    # 
    def SetHabCamFileEnvVar(self):
        os.environ["HabCamFile"] = self.habCamDataFile.myEntry.get()
        # fname = os.path.join('OriginalData', os.environ["HabCamFile"]+'.csv')
        # print(fname)

    ##
    # 
    def OpenPDF(self):
        # Checking to see if processing has been run
        fName = os.path.join('Results', 'Lat_Lon_Grid_*.pdf')
        fileList = glob.glob(fName)
        if fileList:
            fName = filedialog.askopenfilename(title="Open PDF File", filetypes=[("PDF files", "*.pdf")],
                    defaultextension='pdf', initialdir='Results')
            if fName:
                if platform.system() == 'Windows':
                    # Opens file in default pdf view, i.e. Acrobat
                    webbrowser.open_new(fName)
                elif platform.system() == 'Darwin':
                    # Opens file in default pdf view, i.e. Preview
                    c = webbrowser.get('macosx')
                    c.open_new_tab('file://'+fName)
                else:
                    # Opens file in the default browser
                    c = webbrowser.get('chrome')
                    c.open_new_tab('file://'+fName)
        else:
            messagebox.showerror("Open PDF File", 'There are no PDF files to open. START Sim to create files.')

    ##
    #
    def SetMaShapeFile(self):
        file_path = filedialog.askopenfilename(title="Open SHP File", filetypes=[("SHP files", "*.shp")], defaultextension='csv', initialdir=self.shapeFileDir)
        if file_path:
            self.areaFName = file_path
            self.maShapeFileEntry.delete(0,tk.END)
            f = file_path.split('/')
            self.maShapeFileEntry.insert(0, f[-1])
        self.SetMaShapeFileEnvVar()
    
    ##
    # 
    def SetMaShapeFileEnvVar(self):
        os.environ["MAShapeFile"] = self.maShapeFileEntry.get()

    ##
    #
    def SetGbShapeFile(self):
        file_path = filedialog.askopenfilename(title="Open SHP File", filetypes=[("SHP files", "*.shp")], defaultextension='csv', initialdir=self.shapeFileDir)
        if file_path:
            self.areaFName = file_path
            self.gbShapeFileEntry.delete(0,tk.END)
            f = file_path.split('/')
            self.gbShapeFileEntry.insert(0, f[-1])
        self.SetGBShapeFileEnvVar()
    
    ##
    # 
    def SetGBShapeFileEnvVar(self):
        os.environ["GBShapeFile"] = self.gbShapeFileEntry.get()

    def SetMaShapeBufferFile(self):
        file_path = filedialog.askopenfilename(title="Open SHP File", filetypes=[("SHP files", "*.shp")], defaultextension='csv', initialdir=self.shapeFileDir)
        if file_path:
            self.areaFName = file_path
            self.maShapeBufferFileEntry.delete(0,tk.END)
            f = file_path.split('/')
            self.maShapeBufferFileEntry.insert(0, f[-1])
        self.SetMaShapeBufferFileEnvVar()
    
    ##
    # 
    def SetMaShapeBufferFileEnvVar(self):
        os.environ["MAShapeBufferFile"] = self.maShapeBufferFileEntry.get()

    ##
    #
    def SetGbShapeBufferFile(self):
        file_path = filedialog.askopenfilename(title="Open SHP File", filetypes=[("SHP files", "*.shp")], defaultextension='csv', initialdir=self.shapeFileDir)
        if file_path:
            self.areaFName = file_path
            self.gbShapeBufferFileEntry.delete(0,tk.END)
            f = file_path.split('/')
            self.gbShapeBufferFileEntry.insert(0, f[-1])
        self.SetGBShapeBufferFileEnvVar()
    
    ##
    # 
    def SetGBShapeBufferFileEnvVar(self):
        os.environ["GBShapeBufferFile"] = self.gbShapeBufferFileEntry.get()

    ## 
    #
    def pop_up(self):
        about = '''(This frame is scrollable, use mouse wheel)
Simulation Configuration Files
    These are the names for the configuration files. The user can change the 
    names in order to prevent overwriting the installed configuration files.

    Use the Change/Save buttons to use a different name. This also saves the 
    data under that name.

    Growth Config File: This file holds the data from the Growth Tab.
    Recruit Config File: This file holds the data for recruitment, this Tab.
    Grid Mgr Config File: This file holds the data from Special Access Tab.
    Sim Config File: This file holds the names of the previous files, the time
        steps per year, and the selected outputs

Interpolation Configuration Files
    UK Config File: This file holds the data from UKInterpolation Tab.
        namely the form selected. The remaining data is kept in the 
        Spatial Fcn Config File

Output Selection
    Selects the desired outputs to be analyzed.
    Checkboxes allow the user to select the desired parameters of interest.
    This is used to save processing time rather than processing everything. 
    Especially true during interpolation as it would take over an hour to do 
    the interpolation. For example for MA with 11631 grid locations.
    •	Approx 2 minutes per interpolation
    •	Given 3 years worth of data, plus initial conditions
    •	9 listed outputs
    Thus 9 x 4 x 2 or 72 minutes. 
    GB is proportionately shorter with only 6802 grid locations.

    •	ABUN: abundance in scallops per square meter
    •	BIOM: biomass in gpsm
    •	EBMS: exploitable biomass in metric tons
    •	FEFF: Fishing Effort
    •	FMOR: Fishing Mortality
    •	LAND: Landings by number of scallops
    •	LNDW: Landings by weight in grams
    •	LPUE: Landing Per Unit Effort, (per day)
    •	RECR: Recruitment in scallops per square meter
    
Duration
    This defines the start and stop years over which the simulation will 
    forecast growth at time intervals specified by time steps per year.
    For example, for the default value of 13
    •	1/13 = 0.077 years
    •	0.077 * 365 = 28.077 days or roughly every 4 weeks

    The domain name is where the region where the growth takes place, Georges 
    Bank or Mid-Atlantic, GB or MA, respectively. AL covers both regions. 

Recruitment
    Defines the period in which recruitment is used in the growth calculations.

    Recruit Yr Start/Stop specifies which years to pull from the survey data
    files to model recruitment. These years are randomly selected to serve
    as a basis for recruitment for each year of the forecast growth.

View PDF Plots
    Once START Sim has been executed one can use this button to open and view
    PDF documents showing plots of the Output selections by lat/lon location.

Survey Data Files
    The GUI sets environement variables such that all scripts, M-files, Python,
    and shell, access the same data files without having to pass yet another 
    argument or two. These survey data files are stored in the "OriginalData"
    subdirectory as zip files. They are too large to be managed in the git
    repositiory as a CSV file. The scripts then unzip the files as necessary.
    Extensions are not specified.

    Use NONE to skip that survey data
'''
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
