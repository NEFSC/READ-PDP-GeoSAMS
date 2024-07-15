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
        self.friend = friend
        self.maxYears = maxYears

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        simFrame = ttk.LabelFrame(self, text='Simulation Configuration Files', style='SAMS.TFrame')
        self.mortCfgFile = SubFrameElement(self, simFrame, 'Growth Config File   ', 'Growth.cfg',  0, 0, 1, width=20)
        self.recrCfgFile = SubFrameElement(self, simFrame, 'Recruit Config File  ',      'Recruitment.cfg',1, 0, 1, width=20)
        self.gmCfgFile   = SubFrameElement(self, simFrame, 'Grid Mgr Config File ',     'GridManager.cfg',2, 0, 1, width=20)
        self.simCfgFile  = SubFrameElement(self, simFrame, 'Sim Config File      ',       'Scallop.cfg',    3, 0, 1, width=20)

        self.openMortConfigtButton = ttk.Button(simFrame, text='Change/Save Growth File', style="BtnBluGrn.TLabel", command=self.GetMortConfigFName)
        self.openMortConfigtButton.grid(row=0, column=3)
        #-------------------------------------------------------------------------------------------
        self.openRecrConfigtButton = ttk.Button(simFrame, text='Change/Save Recr File', style="BtnBluGrn.TLabel", command=self.GetRecrConfigFName)
        self.openRecrConfigtButton.grid(row=1, column=3)
        #-------------------------------------------------------------------------------------------
        self.openGmgrConfigtButton = ttk.Button(simFrame, text='Change/Save GMgr File', style="BtnBluGrn.TLabel", command=self.GetGMgrConfigFName)
        self.openGmgrConfigtButton.grid(row=2, column=3)
        #-------------------------------------------------------------------------------------------
        self.openSimConfigtButton = ttk.Button(simFrame, text='Change/Save Sim File', style="BtnBluGrn.TLabel", command=self.GetSimConfigFName)
        self.openSimConfigtButton.grid(row=3, column=3)
        #-------------------------------------------------------------------------------------------
        simFrame.grid(row=1, column=0, padx=5, sticky='n')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        interpFrame = ttk.LabelFrame(self, text='Interpolation Configuration Files', style='SAMS.TFrame')
        self.ukCfgFile = SubFrameElement(self, interpFrame, 'UK Config File', 'UK.cfg', 0, 0, 1, width=20)
        self.openUKConfigButton = ttk.Button(interpFrame, text='Change/Save UK File', style="BtnBluGrn.TLabel", command=self.GetUKConfigFName)
        self.openUKConfigButton.grid(row=0, column=3)
        #-------------------------------------------------------------------------------------------
        interpFrame.grid(row=2, column=0, padx=5, sticky='nw')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        showResultsFrame = ttk.LabelFrame(self, text='View PDF Plots', style='SAMS.TFrame')
        # --------------------------------------------------------------------------------------------------------
        self.openPDFButton = ttk.Button(showResultsFrame, text='Open PDF File', style="BtnGreen.TLabel", command=self.OpenPDF)
        self.openPDFButton.grid(row=0, column=0, sticky='we')
        #-------------------------------------------------------------------------------------------
        showResultsFrame.grid(row=2, column=0, padx=5, sticky='sw')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        recruitFrame = ttk.LabelFrame(self, text='Recruitment', style='SAMS.TFrame')

        self.monthsArr = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
        self.startDayLabel = ttk.Label(recruitFrame, text='Start Day')
        self.startDayLabel.grid(row=0, column=0)
        #-------------------------------------------------------------------------------------------
        self.startDayComboMonth = ttk.Combobox(recruitFrame, width=4, values=self.monthsArr)
        self.startDayComboMonth.current(0)
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
        self.stopDayComboMonth.current(3)
        self.stopDayComboMonth.bind('<<ComboboxSelected>>', self.CheckStopDay)
        self.stopDayComboMonth.grid(row=1, column=1, padx=5, sticky='w')
        #-------------------------------------------------------------------------------------------
        self.stopDayComboDay = ttk.Combobox(recruitFrame, width=2, values=[str(i) for i in range(1,32)])
        self.stopDayComboDay.current(10)
        self.stopDayComboDay.bind('<<ComboboxSelected>>', self.CheckStopDay)
        self.stopDayComboDay.grid(row=1, column=2, padx=5, sticky='e')
        #-------------------------------------------------------------------------------------------
        recruitFrame.grid(row=2, column=1, padx=5, sticky='e')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        durationFrame = ttk.LabelFrame(self, text='Duration', style='SAMS.TFrame')
        self.startYr    = SubFrameElement(self, durationFrame, 'Start Year',       '2015',            0, 0, 1,
                                          enterCmd=self.EnterKeyClicked, valCmd=numbersCallback)
        #-------------------------------------------------------------------------------------------
        self.stopYr     = SubFrameElement(self, durationFrame, 'Stop Year ',       '2017',            1, 0, 1,
                                          enterCmd=self.EnterKeyClicked, valCmd=numbersCallback)
        #-------------------------------------------------------------------------------------------
        self.tsPerYear  = SubFrameElement(self, durationFrame, 'Time Steps / Year', str(tsPerYear),   2, 0, 1)
        #-------------------------------------------------------------------------------------------
        self.domainNameLabel = ttk.Label(durationFrame, text='Domain Name')
        self.domainNameLabel.grid(row=3, column=0, pady=5)
        #-------------------------------------------------------------------------------------------
        self.domainNameCombo = ttk.Combobox(durationFrame, width=3, values=['MA', 'GB', 'AL'])
        self.domainNameCombo.current(2)
        self.domainNameCombo.grid(row=3, column=1, sticky='w', pady=5)
        #-------------------------------------------------------------------------------------------
        self.usingHabCamLabel = ttk.Label(durationFrame, text='Domain Name')
        self.usingHabCam = tk.BooleanVar(durationFrame, self.friend.useHabCamData)
        self.usingHabCamLabel = ttk.Label(durationFrame, text='Data Source')
        self.usingHabCamLabel.grid(row=4, column=0)
        self.useHabCamRB = ttk.Radiobutton(durationFrame, text='Habcam', value=True, variable=self.usingHabCam, command=None)
        self.useHabCamRB.grid(row=4, column=1,sticky='w')
        self.useDredgeRB = ttk.Radiobutton(durationFrame, text='Dredge', value=False, variable=self.usingHabCam, command=None)
        self.useDredgeRB.grid(row=5, column=1,sticky='w')
        #-------------------------------------------------------------------------------------------
        self.useStratumLabel = ttk.Label(durationFrame, text='Use Stratum\n(Not Used by MA)')
        self.useStratumLabel.grid(row=6, column=0)
        #-------------------------------------------------------------------------------------------
        self.useStratumCombo = ttk.Combobox(durationFrame, width=3, values=comboTFStr)
        if self.friend.savedByStratum:
            self.useStratumCombo.current(comboTFStr.index('T'))
        else:
            self.useStratumCombo.current(comboTFStr.index('F'))
        self.useStratumCombo.grid(row=6, column=1, sticky='w')
        # Not user configurable at this time        
        self.useStratumLabel.grid_remove()
        self.useStratumCombo.grid_remove()
        #-------------------------------------------------------------------------------------------
        durationFrame.grid(row=2, column=1, padx=5, sticky='w')
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
        outputSelFrame.grid(row=1, column=1, padx=5, sticky='n')
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
        if stopYear < startYear:
            stopYear = startYear
            self.stopYr.myEntry.delete(0,tk.END)
            self.stopYr.myEntry.insert(0,str(stopYear))
            messagebox.showerror('YEAR RANGE','Stop Year is before Start Year\nSetting them equal')
        numYears = stopYear - startYear + 1
        if numYears > self.maxYears:
            addYears = numYears - self.maxYears
            self.friend.frame5.AppendYears(addYears)
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
        value = (self.abunVar.get()<<3) + (self.bmsVar.get()<<2) + (self.ebmsVar.get()<<1) + self.lpueVar.get()\
              + (self.fmortVar.get()<<7) + (self.feffVar.get()<<6) + (self.landVar.get()<<5) + (self.lndwVar.get()<<4)\
              + (self.recrVar.get()<<8)
        return value

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Mortality Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetMortConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.simStartDir)
        f = file_path.split('/')
        if file_path:
            self.mortCfgFile.myEntry.delete(0,tk.END)
            self.mortCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteGrowthConfig()

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Recruitment Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetRecrConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.simStartDir)
        f = file_path.split('/')
        if file_path:
            self.recrCfgFile.myEntry.delete(0,tk.END)
            self.recrCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteRecruitmentConfig()

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Grid Manager Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetGMgrConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.simStartDir)
        f = file_path.split('/')
        if file_path:
            self.gmCfgFile.myEntry.delete(0,tk.END)
            self.gmCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteGridMgrConfig()

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Simulation Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetSimConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.simStartDir)
        f = file_path.split('/')
        if file_path:
            self.simCfgFile.myEntry.delete(0,tk.END)
            self.simCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteScallopConfig()

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Universal Kriging
    # Configuration file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetUKConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.interpStartDir)
        f = file_path.split('/')
        if file_path:
            self.ukCfgFile.myEntry.delete(0,tk.END)
            self.ukCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteUKConfig()

    def OpenPDF(self):
        # Checking to see if processing has been run
        fName = os.path.join('Results', 'Lat_Lon_Grid_*.pdf')
        fileList = glob.glob(fName)
        if fileList:
            fName = filedialog.askopenfilename(title="Open PDF File", filetypes=[("PDF files", "*.pdf")], defaultextension='pdf', initialdir='Results')
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

    Saturate
        Interpolation can sometimes create excessively large values.
        To bypass, Use Saturate can be T or F, but set a larger threshold,
        i.e. 1E309 (Infinity).
        The user can choose to saturate to the threshold, (T), or 
        reset the value to 0.0 when exceeded, (F).

    Saturate Threshold
        Threshold value to use
        Use Saturate = T, if field > Threshold then field = Threshold
        Use Saturate = F, if field > Threshold then field = 0.0

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
    •	BMMT: biomass in metric tons
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

    The year range is limited by default to 5 years, e.g. 2015 to 2019.
    See SHOW Args. The user can modify this on the command line:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py #Areas #Nodes
    Default:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py 25 8

    The domain name shows the region where the growth takes place, Georges Bank
    or Mid-Atlantic, GB or MA, respectively. AL covers both regions. 

Recruitment
    Defines the period in which recruitment is used in the growth calculations.

View PDF Plots
    Once START Sim has been executed one can use this button to open and view
    PDF documents showing plots of the Output selections by lat/lon location.
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
