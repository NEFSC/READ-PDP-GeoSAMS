#--------------------------------------------------------------------------------------------------
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
# Recruitment is only applied at a certain time of the year. These values determine this period.
#
#   - Start Day, calendar day of the year when recruitment influence begins. 
#     Formatted as any of '01/1', '1/01', 'Jan 1', 'JAN 1', 'January 1', or 'JANUARY 1'
#   - Stop Day, calendar day of the year when reruitment influence ends
#     Same formatting accepted as for Start Day
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
#--------------------------------------------------------------------------------------------------
import os
import tkinter as tk
from tkinter import ttk
from tkinter import filedialog
from Widgets import *

#======================================================================================================
##
# This class displays information about GeoSAMS simulation. This same information is used
# on the command line when starting SRC\ScallopPopDensity
#
#======================================================================================================
class MainInput(ttk.Frame):

    def __init__(self, container, friend, tsPerYear, selectedOutputs):
        super().__init__()
        self.root = os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'Configuration')
        self.friend = friend

        self.style = ttk.Style()
        self.style.configure('MainInput.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('MainInput.TFrame.Label', font=('courier', 10, 'bold'))
        #-------------------------------------------------------------------------------------------
        growthFrame = ttk.LabelFrame(self, text='Growth', style='MainInput.TFrame')
        self.startYr    = SubFrameElement(self, growthFrame, 'Start Year', '2015',          0, 0, 1)
        self.stopYr     = SubFrameElement(self, growthFrame, 'Stop Year ', '2017',          1, 0, 1)
        self.tsPerYear  = SubFrameElement(self, growthFrame, 'tsPerYear', str(tsPerYear),   2, 0, 1)
        self.domainNameLabel = ttk.Label(growthFrame, text='Domain Name')
        self.domainNameLabel.grid(row=3, column=0)
        self.domainNameCombo = ttk.Combobox(growthFrame, width=3, values=['MA', 'GB', 'AL'])
        self.domainNameCombo.current(2)
        self.domainNameCombo.grid(row=3, column=1, sticky='w')

        self.useStratumLabel = ttk.Label(growthFrame, text='Use Stratum\n(Not Used by MA)')
        self.useStratumLabel.grid(row=4, column=0)
        self.useStratumCombo = ttk.Combobox(growthFrame, width=3, values=['T', 'F'])
        self.useStratumCombo.current(0)
        self.useStratumCombo.grid(row=4, column=1, sticky='w')

        growthFrame.grid(row=0, column=1, padx=5, sticky='n')
        #-------------------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------------------
        recruitFrame = ttk.LabelFrame(self, text='Recruitment', style='MainInput.TFrame')

        self.monthsArr = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
        self.startDayLabel = ttk.Label(recruitFrame, text='Start Day')
        self.startDayLabel.grid(row=0, column=0)
        self.startDayComboMonth = ttk.Combobox(recruitFrame, width=4, values=self.monthsArr)
        self.startDayComboMonth.current(0)
        self.startDayComboMonth.grid(row=0, column=1, padx=5, sticky='w')
        self.startDayComboDay = ttk.Combobox(recruitFrame, width=2, values=[str(i) for i in range(1,32)])
        self.startDayComboDay.current(0)
        self.startDayComboDay.grid(row=0, column=2, padx=5, sticky='e')

        self.stopDayLabel = ttk.Label(recruitFrame, text='Stop Day')
        self.stopDayLabel.grid(row=1, column=0)
        self.stopDayComboMonth = ttk.Combobox(recruitFrame, width=4, values=self.monthsArr)
        self.stopDayComboMonth.current(3)
        self.stopDayComboMonth.grid(row=1, column=1, padx=5, sticky='w')
        self.stopDayComboDay = ttk.Combobox(recruitFrame, width=2, values=[str(i) for i in range(1,32)])
        self.stopDayComboDay.current(10)
        self.stopDayComboDay.grid(row=1, column=2, padx=5, sticky='e')

        recruitFrame.grid(row=1, column=1, padx=5, sticky='n')
        #-------------------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------------------
        configFrame = ttk.LabelFrame(self, text='Configuration Files', style='MainInput.TFrame')
        self.mortCfgFile = SubFrameElement(self, configFrame, 'Mortality Config File', 'Mortality.cfg',  0, 0, 1, width=20)
        self.recrCfgFile = SubFrameElement(self, configFrame, 'Recruitment File',      'Recruitment.cfg',1, 0, 1, width=20)
        self.gmCfgFile   = SubFrameElement(self, configFrame, 'Grid Manager File',     'GridManager.cfg',2, 0, 1, width=20)
        self.simCfgFile  = SubFrameElement(self, configFrame, 'Sim Config File',       'Scallop.cfg',    3, 0, 1, width=20)
        self.ukCfgFile   = SubFrameElement(self, configFrame, 'UK Config File',        'UK.cfg',         4, 0, 1, width=20)

        self.style.configure("Frame1.TLabel", padding=6, relief='raised', background="#0FF")
        self.openMortConfigtButton = ttk.Button(configFrame, text='Change/Save Mort File', style="Frame1.TLabel", command=self.GetMortConfigFName)
        self.openMortConfigtButton.grid(row=0, column=3)
        self.openRecrConfigtButton = ttk.Button(configFrame, text='Change/Save Recr File', style="Frame1.TLabel", command=self.GetRecrConfigFName)
        self.openRecrConfigtButton.grid(row=1, column=3)
        self.openGmgrConfigtButton = ttk.Button(configFrame, text='Change/Save GMgr File', style="Frame1.TLabel", command=self.GetGMgrConfigFName)
        self.openGmgrConfigtButton.grid(row=2, column=3)
        self.openSimConfigtButton = ttk.Button(configFrame, text='Change/Save Sim File', style="Frame1.TLabel", command=self.GetSimConfigFName)
        self.openSimConfigtButton.grid(row=3, column=3)
        self.openUKConfigtButton = ttk.Button(configFrame, text='Change/Save UK File', style="Frame1.TLabel", command=self.GetUKConfigFName)
        self.openUKConfigtButton.grid(row=4, column=3)

        configFrame.grid(row=0, column=0, padx=5, sticky='n')
        #-------------------------------------------------------------------------------------------
        outputSelFrame = ttk.LabelFrame(self, text='Output Selection', style='MainInput.TFrame')
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
        ttk.Checkbutton(outputSelFrame, text='Abundance',     variable=self.abunVar,  command=self.CBSelectedOutput).grid(row=0, column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Biomass',       variable=self.bmsVar,   command=self.CBSelectedOutput).grid(row=0, column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='ExplBiomass',   variable=self.ebmsVar,  command=self.CBSelectedOutput).grid(row=0, column=2, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Fish Mort',    variable=self.fmortVar,  command=self.CBSelectedOutput).grid(row=1, column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Fish Effort',  variable=self.feffVar ,  command=self.CBSelectedOutput).grid(row=1, column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Lands By Num',  variable=self.landVar,  command=self.CBSelectedOutput).grid(row=1, column=2, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Lands By Wght', variable=self.lndwVar , command=self.CBSelectedOutput).grid(row=2, column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='LPUE',          variable=self.lpueVar,  command=self.CBSelectedOutput).grid(row=2, column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Recruitment',   variable=self.recrVar , command=self.CBSelectedOutput).grid(row=2, column=2, sticky='sw', padx=10, pady=5)
        outputSelFrame.grid(row=1, column=0, padx=5, sticky='n')
        #-------------------------------------------------------------------------------------------

    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Determines the value for which outputs are selected as they are checked.
    #
    #--------------------------------------------------------------------------------------------------
    def CBSelectedOutput(self):
        self.desiredOutput = self.ComputeSelectOuputValue()

    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Updates the final value from which outputs are selected.
    #
    #--------------------------------------------------------------------------------------------------
    def GetSelectedOutputs(self):
        return self.ComputeSelectOuputValue()
    
    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Bit shifts (multiplies) checkbuttons and computes bit position value.
    #
    #--------------------------------------------------------------------------------------------------
    def ComputeSelectOuputValue(self):
        value = (self.abunVar.get()<<3) + (self.bmsVar.get()<<2) + (self.ebmsVar.get()<<1) + self.lpueVar.get()\
              + (self.fmortVar.get()<<7) + (self.feffVar.get()<<6) + (self.landVar.get()<<5) + (self.lndwVar.get()<<4)\
              + (self.recrVar.get()<<8)
        return value

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Mortality Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def GetMortConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.mortCfgFile.myEntry.get())
            self.mortCfgFile.myEntry.delete(0,n)
            self.mortCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteMortalityConfig()

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Recruitment Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def GetRecrConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.recrCfgFile.myEntry.get())
            self.recrCfgFile.myEntry.delete(0,n)
            self.recrCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteRecruitmentConfig()

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Grid Manager Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def GetGMgrConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.gmCfgFile.myEntry.get())
            self.gmCfgFile.myEntry.delete(0,n)
            self.gmCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteGridMgrConfig()

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Simulation Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def GetSimConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.simCfgFile.myEntry.get())
            self.simCfgFile.myEntry.delete(0,n)
            self.simCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteScallopConfig()

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Universal Kriging
    # Configuration file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def GetUKConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.ukCfgFile.myEntry.get())
            self.ukCfgFile.myEntry.delete(0,n)
            self.ukCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteUKConfig()
            