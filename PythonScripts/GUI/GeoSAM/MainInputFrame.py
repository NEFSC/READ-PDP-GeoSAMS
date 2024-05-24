#--------------------------------------------------------------------------------------------------
## @page page1 Main
#  - Growth subframe that identifies
#    - Start Year of the simulation
#    - Stop Year of the simulation
#    - Time Steps per year
#    - Domain Name or region of interest, Mid-Atlantic, MA, or Georges Bannk, GB
#    - Sort By Statum: Used when processing Georges-Bank to break the region into quadrants due to its unique shape
#  
#  - Recuitment
#    - Start Day, calendar day of the year when recruitment influence begins
#    - Stop Day, calendar day of the year when reruitment influence ends
#
#  - Configuration Files\n
#  Files used by the sim to set up parameters. The GUI can use the default values or change the names before
#  starting the sim.
#
#  - Output Selection\n
#  Checkboxes to allow the user to select the desired parameters of interest. This is used to save processing 
#  time rather than processing everything. Especially true during interpolation as it would take over and hour 
#  to do the interpolation. For example for MA with 11631 grid locations.
#    - Approx 2 minutes per interpolation
#    - Given 3 years worth of data, plus initial conditions
#    - 9 listed outputs\n
#   Thus 9 x 4 x 2 or 72 minutes. GB is proportionately smaller with only 6802 grid locations.

#--------------------------------------------------------------------------------------------------
import tkinter as tk
from tkinter import ttk
from Widgets import *

#======================================================================================================
##
# This class displays information about GeoSAMS simulation. This same information is used
# on the command line when starting SRC\ScallopPopDensity
#
#======================================================================================================
class MainInput(ttk.Frame):

    def __init__(self, container, tsPerYear, selectedOutputs, useStratum):
        super().__init__()

        self.style = ttk.Style()
        self.style.configure('MainInput.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('MainInput.TFrame.Label', font=('courier', 8, 'bold'))
        #-------------------------------------------------------------------------------------------
        growthFrame = ttk.LabelFrame(self, text='Growth', style='MainInput.TFrame')
        self.startYr    = SubFrameElement(self, growthFrame, 'Start Year', '2015',          0, 0, 1)
        self.stopYr     = SubFrameElement(self, growthFrame, 'Stop Year ', '2017',          1, 0, 1)
        self.tsPerYear  = SubFrameElement(self, growthFrame, 'tsPerYear', str(tsPerYear),   2, 0, 1)
        self.domainName = SubFrameElement(self, growthFrame, 'Domain Name\nMA or GB', 'MA', 3, 0, 1, self.valDN)
        reg=self.domainName.myEntry.register(self.valDN)
        self.domainName.myEntry.configure(validate='key', validatecommand=(reg, '%P'))

        self.useStratum = SubFrameElement(self, growthFrame, 'Use Stratum\n(Not Used by MA)', str(useStratum), 4, 0, 1)
        growthFrame.grid(row=0, column=0)
        #-------------------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------------------
        recruitFrame = ttk.LabelFrame(self, text='Recruitment', style='MainInput.TFrame')
        self.startDay    = SubFrameElement(self, recruitFrame, 'Start Day\nMmm DD', 'Jan 1',  0, 0, 1)
        self.stopDay     = SubFrameElement(self, recruitFrame, 'Stop Day\nMmm DD ', 'Apr 11', 1, 0, 1)
        recruitFrame.grid(row=0, column=1, sticky='n')
        #-------------------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------------------
        configFrame = ttk.LabelFrame(self, text='Configuration Files', style='MainInput.TFrame')
        self.mortCfgFile = SubFrameElement(self, configFrame, 'Mortality Config File', 'Mortality.cfg',  0, 0, 1)
        self.recrCfgFile = SubFrameElement(self, configFrame, 'Recruitment File',      'Recruitment.cfg',1, 0, 1)
        self.gmCfgFile   = SubFrameElement(self, configFrame, 'Grid Manager File',     'GridManager.cfg',2, 0, 1)
        self.simCfgFile  = SubFrameElement(self, configFrame, 'Sim Config File',       'Scallop.cfg',    3, 0, 1)
        self.ukCfgFile   = SubFrameElement(self, configFrame, 'UK Config File',        'UK.cfg',         4, 0, 1)
        configFrame.grid(row=0, column=2, sticky='n')
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
        outputSelFrame.grid(row=1, column=0)
        #-------------------------------------------------------------------------------------------

    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Used to limit the value for Domain \n
    #  Valid inputs become
    #  - M
    #  - MA
    #  - G
    #  - GB
    #  @ returns false if invalid input
    #
    #--------------------------------------------------------------------------------------------------
    def valDN(self, input):
        """Only allows alpha"""
        if input == 'M': return True
        elif input == 'MA': return True  
        elif input == 'G': return True
        elif input == 'GB': return True  
        elif input == "": return True
        else: return False

    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Determines the value for which outputs are selected as they are checked
    #
    #--------------------------------------------------------------------------------------------------
    def CBSelectedOutput(self):
        self.desiredOutput = self.ComputeSelectOuputValue()

    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Updates the final value from which outputs are selected
    #
    #--------------------------------------------------------------------------------------------------
    def GetSelectedOutputs(self):
        return self.ComputeSelectOuputValue()
    
    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Scales checkbuttons and computes bit position value
    #
    #--------------------------------------------------------------------------------------------------
    def ComputeSelectOuputValue(self):
        value = self.abunVar.get()*8 + self.bmsVar.get()*4 + self.ebmsVar.get()*2 + self.lpueVar.get()\
              + self.fmortVar.get()*128 + self.feffVar.get()*64 + self.landVar.get()*32 + self.lndwVar.get()*16\
              + self.recrVar.get()*256
        return value

