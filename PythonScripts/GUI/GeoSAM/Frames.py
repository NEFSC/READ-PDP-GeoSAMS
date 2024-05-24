import tkinter as tk
import csv
import platform
import os

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from PointInPolygon import *
from GeoSams import MainApplication

#--------------------------------------------------------------------------------------------------
## @page page1 Main
#  - Growth subframe that identifies
#    - Start Year of the simulation
#    - Stop Year of the simulation
#    - Domain Name or region of interest, Mid-Atlantic, MA, or Georges Bannk, GB
#  - Recuitment
#    - Start Day, calendar day of the year when recruitment influence begins
#    - Stop Day, calendar day of the year when reruitment influence ends

#--------------------------------------------------------------------------------------------------
class MainInput(ttk.Frame):
    """ This class displays information about GeoSAMS simulation. This same information is used
    on the command line when starting SRC\ScallopPopDensity"""

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
        outputSelFrame = ttk.LabelFrame(self, text='OutputSelection', style='MainInput.TFrame')
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
        ttk.Checkbutton(outputSelFrame, text='Abundance',     variable=self.abunVar,  command=self.CBSelected).grid(row=0, column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Biomass',       variable=self.bmsVar,   command=self.CBSelected).grid(row=0, column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='ExplBiomass',   variable=self.ebmsVar,  command=self.CBSelected).grid(row=0, column=2, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Fish Mort',    variable=self.fmortVar,  command=self.CBSelected).grid(row=1, column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Fish Effort',  variable=self.feffVar ,  command=self.CBSelected).grid(row=1, column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Lands By Num',  variable=self.landVar,  command=self.CBSelected).grid(row=1, column=2, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Lands By Wght', variable=self.lndwVar , command=self.CBSelected).grid(row=2, column=0, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='LPUE',          variable=self.lpueVar,  command=self.CBSelected).grid(row=2, column=1, sticky='sw', padx=10, pady=5)
        ttk.Checkbutton(outputSelFrame, text='Recruitment',   variable=self.recrVar , command=self.CBSelected).grid(row=2, column=2, sticky='sw', padx=10, pady=5)
        outputSelFrame.grid(row=1, column=0)
        #-------------------------------------------------------------------------------------------

    def valDN(self, input):
        """Only allows alpha"""
        if input == 'M': return True
        elif input == 'MA': return True  
        elif input == 'G': return True
        elif input == 'GB': return True  
        elif input == "": return True
        else: return False

    def CBSelected(self):
        self.desiredOutput = self.abunVar.get()*8 + self.bmsVar.get()*4 + self.ebmsVar.get()*2 + self.lpueVar.get()
        self.desiredOutput+= self.fmortVar.get()*128 + self.feffVar.get()*64 + self.landVar.get()*32 + self.lndwVar.get()*16
        self.desiredOutput+= self.recrVar.get()*256

    def GetCheckBoxValue(self):
        self.desiredOutput = self.abunVar.get()*8 + self.bmsVar.get()*4 + self.ebmsVar.get()*2 + self.lpueVar.get()
        self.desiredOutput+= self.fmortVar.get()*128 + self.feffVar.get()*64 + self.landVar.get()*32 + self.lndwVar.get()*16
        self.desiredOutput+= self.recrVar.get()*256
        return self.desiredOutput


class SpecialAccess(ttk.Frame):
    """ This class displays information about GeoSAMS simulation. This same information is used
    on the command line when starting SRC\ScallopPopDensity"""

    def __init__(self, container):
        super().__init__()
        self.style = ttk.Style()
        self.style.configure('MainInput.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('MainInput.TFrame.Label', font=('courier', 8, 'bold'))
        #-------------------------------------------------------------------------------------------
        specialAccFrame = ttk.LabelFrame(self, text='Special Access', style='MainInput.TFrame')
        self.specAccFile  = SubFrameElement(self, specialAccFrame, 'Special Access Points', '', 0, 0, 1)
        self.fishMortFile = SubFrameElement(self, specialAccFrame, 'Fishing Mort File', '', 1, 0, 1)

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#080")
        self.openFishCSVButton = ttk.Button(specialAccFrame, text='View', style="Custom.TLabel", command=self.OpenSpecAccCSV)
        self.openFishCSVButton.grid(row=0, column=2)

        self.tree = ttk.Treeview(specialAccFrame, show="headings", height=20)
        self.tree.grid(row=2, column=0, columnspan=5, padx=10)
        specialAccFrame.grid(row=0, column=0, columnspan=4, sticky='w')
        #-------------------------------------------------------------------------------------------

    def OpenSpecAccCSV(self):
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
                    self.tree.column(col)
                    self.tree.heading(col, text=col)

                for row in csv_reader:
                    self.tree.insert("", "end", values=row)

        except Exception as e:
            messagebox.showerror("GeoSAM Sim", f"Error: {str(e)}")

class Mortality(ttk.Frame, MainApplication):
    def __init__(self, container, fName):
        super().__init__()

        self.UpdateValues(fName)

        self.style = ttk.Style()
        self.style.configure('Mortality.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Mortality.TFrame.Label', font=('courier', 8, 'bold'))
        
        fishingFrame= ttk.LabelFrame(self, text='Mortality', style='Mortality.TFrame')
        self.fishMort   = SubFrameElement(self, fishingFrame, 'Fishing Mort', self.fmortStr,            0, 0, 1)
        self.fishSelect = SubFrameElement(self, fishingFrame, 'Fishing Effort\n(USD, BMS, or CAS)', self.fishStr, 1, 0, 1)
        self.alphaMort  = SubFrameElement(self, fishingFrame, 'Alpha Mortality',self.alphStr,           2, 0, 1)
        self.maAdultMort= SubFrameElement(self, fishingFrame, 'MA Adult Mortality',self.maAdultMortStr, 3, 0, 1)
        self.gbAdultMort= SubFrameElement(self, fishingFrame, 'GB Adult Mortality',self.gbAdultMortStr, 4, 0, 1)
        self.maLength0  = SubFrameElement(self, fishingFrame, 'MA Length_0',  self.maLen0Str,        5, 0, 1)
        self.gbLength0  = SubFrameElement(self, fishingFrame, 'GB Length_0',  self.gbLen0Str,        6, 0, 1)
        fishingFrame.grid(row=0, column=0, padx=10)

        ilfFrame = ttk.LabelFrame(self, text='Selectivity', style='Mortality.TFrame')
        self.maFSelectA       = SubFrameElement(self, ilfFrame, 'MA FSelectA', self.maFSelAStr,          0, 0, 1)
        self.maFSelectB       = SubFrameElement(self, ilfFrame, 'MA FSelectB', self.maFSelBStr,          1, 0, 1)
        self.gbClosedFSelectA = SubFrameElement(self, ilfFrame, 'GB Closed FSelectA', self.gbClFSelAStr, 2, 0, 1)
        self.gbClosedFSelectB = SubFrameElement(self, ilfFrame, 'GB Closed FSelectB', self.gbClFSelBStr, 3, 0, 1)
        self.gbOpenFSelectA   = SubFrameElement(self, ilfFrame, 'GB Open FSelectA', self.gbOpFSelAStr,   4, 0, 1)
        self.gbOpenFSelectB   = SubFrameElement(self, ilfFrame, 'GB Open FSelectB', self.gbOpFSelBStr,   5, 0, 1)
        ilfFrame.grid(row=0, column=1, padx=10)
        
        lpueFrame   = ttk.LabelFrame(self, text='LPUE', style='Mortality.TFrame')
        self.lpueSlope   = SubFrameElement(self, lpueFrame, 'LPUE Slope', self.lpueSlStr,       0, 0, 1)
        self.lpueSlope2  = SubFrameElement(self, lpueFrame, 'LPUE Slope2', self.lpueSl2Str,     1, 0, 1)
        self.lpueIntcept = SubFrameElement(self, lpueFrame, 'LPUE Intercept', self.lpueIntcStr, 2, 0, 1)
        self.maxPerDay   = SubFrameElement(self, lpueFrame, 'Max Per Day', self.maxPerDayStr,   3, 0, 1)
        self.maxTime     = SubFrameElement(self, lpueFrame, 'Max Time', self.maxTimeStr,        4, 0, 1)
        self.dredgeWth   = SubFrameElement(self, lpueFrame, 'Dredge Width', self.dredgeWdStr,   5, 0, 1)
        self.towSpeed    = SubFrameElement(self, lpueFrame, 'Towing Speed', self.towSpdStr,     6, 0, 1)
        lpueFrame.grid(row=0, column=2, padx=10)

        incidentalFrame = ttk.LabelFrame(self, text='Incidental', style='Mortality.TFrame')
        self.maIncident = SubFrameElement(self, incidentalFrame, 'MA Incidental', self.maIncidStr, 0, 0, 1)
        self.gbIncident = SubFrameElement(self, incidentalFrame, 'GB Incidental', self.gbIncidStr, 1, 0, 1)
        incidentalFrame.grid(row=1, column=0, padx=10)

        discardFrame   = ttk.LabelFrame(self, text='Discard', style='Mortality.TFrame')
        self.maCullSize = SubFrameElement(self, discardFrame, 'MA Cull Size', self.maCullStr, 0, 0, 1)
        self.maDiscard  = SubFrameElement(self, discardFrame, 'MA Discard',   self.maDiscStr, 1, 0, 1)
        self.gbCullSize = SubFrameElement(self, discardFrame, 'GB Cull Size', self.gbCullStr, 0, 2, 3)
        self.gbDiscard  = SubFrameElement(self, discardFrame, 'GB Discard',   self.gbDiscStr, 1, 2, 3)
        discardFrame.grid(row=1, column=1, columnspan=2, padx=10)

    def UpdateValues(self, fName):
        # default values should file get corrupted
        self.fmortStr = None
        self.fishStr = None
        self.alphStr = None
        self.maCullStr = None
        self.maDiscStr = None
        self.gbCullStr = None
        self.gbDiscStr = None
        self.maFSelAStr = None
        self.maFSelBStr = None
        self.gbClFSelAStr = None
        self.gbClFSelBStr = None
        self.gbOpFSelAStr = None
        self.gbOpFSelBStr = None
        self.maAdultMortStr = None
        self.gbAdultMortStr = None
        self.maIncidStr = None
        self.gbIncidStr = None
        self.maLen0Str = None
        self.gbLen0Str = None
        self.fmorFileStr = None
        self.lpueSlStr = None
        self.lpueSl2Str = None
        self.lpueIntcStr = None
        self.maxPerDayStr = None
        self.maxTimeStr = None
        self.dredgeWdStr = None
        self.towSpdStr = None
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

class Interpolation(ttk.Frame):
    def __init__(self, container, get):
        super().__init__()

        self.okToRepaintFunctions = True
        self.nsfMax = 20
        self.functions = [None for _ in range(self.nsfMax)]
        self.myget = get # pointer function to domain name entry
        self.domainName = self.myget()
        if self.domainName == 'MA':
            self.nsf = 9
        else:
            self.nsf = 5
        self.style = ttk.Style()
        self.style.configure('Interpolation.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Interpolation.TFrame.Label', font=('courier', 8, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.

        # --------------------------------------------------------------------------------------------------------
        self.funcFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Spatial Functions', style='Interpolation.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFncsLabel = ttk.Label(self.funcFrame, text='# of Functions')
        self.numFncsLabel.grid(row=0, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFcnsEntry=ttk.Entry(self.funcFrame, validatecommand=numbersCallback)
        self.numFcnsEntry.insert(0, str(self.nsf))
        self.numFcnsEntry.grid(row=0, column=0, sticky='e')
        reg=self.numFcnsEntry.register(numbersCallback)
        self.numFcnsEntry.configure(validate='key', validatecommand=(reg, '%P'))
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFncsButton = ttk.Button(self.funcFrame, text='Update', command=self.NumFuncsUpdate)
        self.numFncsButton.grid(row=0, column=1)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # The grid manager keep all widgets once defined. We are just going to decide which are shown
        # paint first row to show x, y, and z
        # remaining rows are just x, y
        desNumCol = 2
        for i in range(self.nsfMax):
            if i < 3:
                row = 1
                precon = 0
                self.functions[i] = SubFrameInterpFunction(self, self.funcFrame, str(i+1), 'z', 'Logistic', precon, row, i%3)
            else:
                row = ((i-3) // desNumCol) + 2
                col = (i-3) % desNumCol
                precon = row - 1
                self.functions[i] = SubFrameInterpFunction(self, self.funcFrame, str(i+1), chr(120+col), 'Logistic', precon, row, col)
        self.functions[1].dimVal.set('x')
        self.functions[2].dimVal.set('y')
        # Now hide undesired funtion definitions
        for i in range(self.nsf, self.nsfMax):
            self.functions[i].funcFrame.grid_remove()

        self.funcFrame.grid(row=0, column=0, sticky='n')
        # --------------------------------------------------------------------------------------------------------
        # --------------------------------------------------------------------------------------------------------
        paramFrame= ttk.LabelFrame(self.scrollFrame.viewPort, text='Parameters', style='Interpolation.TFrame')
        self.highLimit   = SubFrameElement(self, paramFrame, 'High Limit Factor ', '1.5',  0, 0, 1)
        self.form = SubFrameElement(self, paramFrame, 'variogram form', 'spherical', 1, 0, 1)
        self.useLogTrans  = SubFrameElement(self, paramFrame, 'Use Log Transfrom', 'True', 2, 0, 1)
        self.powerTrans  = SubFrameElement(self, paramFrame, 'Power Tranform\n(Not used if Log = True)', '1.0', 3, 0, 1)
        self.spatCfgFile  = SubFrameElement(self, paramFrame, 'Spatial Fcn Config File', 'SpatialFcns.cfg', 4, 0, 1)
        paramFrame.grid(row=0, column=4, sticky='n')
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')

        self.bind("<Visibility>", self.on_visibility)

    def on_visibility(self, event):
            if self.okToRepaintFunctions:
                self.domainName = self.myget()
                self.numFcnsEntry.delete(0,2)
                if self.domainName == 'MA':
                    self.nsf = 9
                else:
                    self.nsf = 5
                self.numFcnsEntry.insert(0, str(self.nsf))

                # Now update desired funtion definitions
                for i in range(self.nsfMax):
                    self.functions[i].funcFrame.grid_remove()
                for i in range(self.nsf):
                    self.functions[i].funcFrame.grid()


    def NumFuncsUpdate(self):
        """ Updates the number of spatial functions. Overrides default value for MA and GB"""

        # Once a new value is manually enterred, prevent changing tabs from repainting spatial fucntions
        self.okToRepaintFunctions = False            
        for i in range(self.nsfMax):
            self.functions[i].funcFrame.grid_remove()

        n = int(self.numFcnsEntry.get())
        if n > self.nsfMax:
            messagebox.showerror("Number of Spatial functions", f'Max is {self.nsfMax}\nSetting to max')
            n = self.nsfMax
            self.numFcnsEntry.delete(0,3)
            self.numFcnsEntry.insert(0, str(n))
        self.nsf = n
        # Now update desired funtion definitions
        for i in range(self.nsfMax):
            self.functions[i].funcFrame.grid_remove()
        for i in range(self.nsf):
            self.functions[i].funcFrame.grid()

class SortByArea(ttk.Frame):
    def __init__(self, container, maxAreas, maxCorners, maxYears, paramStr, 
                 getYearStart, getYearStop, setYearStop, delYearStop, getDomainName, getCB):
        super().__init__()
        
        self.root = os.environ['ROOT']
        self.numAreasMax = maxAreas
        self.numCornersMax = maxCorners
        self.maxYears = maxYears
        self.numAreas = 1
        self.numCorners = 1
        self.paramStr = paramStr

        self.getYearStart = getYearStart
        self.getYearStop = getYearStop
        self.setYearStop = setYearStop
        self.delYearStop = delYearStop
        self.getDomainName = getDomainName
        self.getCheckBox = getCB
        self.domainName = self.getDomainName()
        self.yearStart = int(self.getYearStart())
        self.yearStop = int(self.getYearStop())
        self.numYears = self.yearStop - self.yearStart + 1
        self.areaData = [Corner(self.numCornersMax) for _ in range(self.numAreasMax)]

        self.style = ttk.Style()
        self.style.configure('SortByArea.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('SortByArea.TFrame.Label', font=('courier', 8, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        
        # --------------------------------------------------------------------------------------------------------
        self.sortAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Sort By Area', style='SortByArea.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numAreasLabel = ttk.Label(self.sortAreaFrame, text='# of Areas')
        self.numAreasLabel.grid(row=0, column=0, sticky='w')

        self.numAreasLabel = ttk.Label(self.sortAreaFrame, text='Output Parameters')
        self.numAreasLabel.grid(row=0, column=0, sticky='ns')

        self.comboParameter = ttk.Combobox(self.sortAreaFrame, values=paramStr)
        self.comboParameter.grid(row=1, column=0, sticky='ns')

        self.numAreasEntry=ttk.Entry(self.sortAreaFrame,validatecommand=numbersCallback)
        self.numAreasEntry.insert(0, str(self.numAreas))
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numAreasEntry.grid(row=1, column=0, sticky='w')

        self.numAreasButton = ttk.Button(self.sortAreaFrame, text='Update # Areas', command=self.NumAreasUpdate)
        self.numAreasButton.grid(row=2, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.style.configure("SortByArea.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("Frame5a.TLabel", padding=6, relief='raised', background="#0FF")
        
        self.openDataSortButton = ttk.Button(self.sortAreaFrame, text='Load Data Sort File', style="SortByArea.TLabel", command=self.GetDataSortFile)
        self.openDataSortButton.grid(row=0, column=1, sticky='w')

        self.saveDataSortButton = ttk.Button(self.sortAreaFrame, text='Save Data Sort File', style="Frame5a.TLabel", command=self.SaveDataSortFile)
        self.saveDataSortButton.grid(row=1, column=1, sticky='w')

        self.saveDataSortButton = ttk.Button(self.sortAreaFrame, text='Run Sort', command=self.RunSort)
        self.saveDataSortButton.grid(row=2, column=1, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.areas = [SubFrameArea(self, self.sortAreaFrame, a, self.numCorners, self.numCornersMax, self.maxYears, 
                                         self.yearStart, self.yearStop, 3+a, 0) for a in range(self.numAreasMax)]

        # now hide
        for a in range(self.numAreas, self.numAreasMax):
            self.areas[a].areaFrame.grid_remove()

        self.sortAreaFrame.grid(row=4, column=0, columnspan=10)
        self.sortAreaFrame.grid_columnconfigure(0,weight=2)
        self.sortAreaFrame.grid_columnconfigure(1,weight=1)
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')

        self.UpdateWidgets()

        self.bind("<Visibility>", self.on_visibility)

    def on_visibility(self, event):
        self.paramStr = []
        parmVal = self.getCheckBox()
        # ordering of string may not matter. The user ultimately selects the desired vale
        if parmVal & 1: self.paramStr.append('LPUE_')
        if parmVal & 2: self.paramStr.append('EBMS_')
        if parmVal & 4: self.paramStr.append('BMMT_')
        if parmVal & 8: self.paramStr.append('ABUN_')
        if parmVal & 16: self.paramStr.append('LNDW_')
        if parmVal & 32: self.paramStr.append('LAND_')
        if parmVal & 64: self.paramStr.append('FEFF_')
        if parmVal & 128: self.paramStr.append('FMOR_')
        if parmVal & 256: self.paramStr.append('RECR_')
        self.comboParameter.configure(values=self.paramStr)
        self.comboParameter.current(0)

        self.yearStart = int(self.getYearStart())
        self.yearStop = int(self.getYearStop())
        self.domainName = self.getDomainName()
        self.numYears = self.yearStop - self.yearStart + 1
        if self.numYears > self.maxYears:
            self.yearStop = self.maxYears + self.yearStart - 1
            self.numYears = self.maxYears
            messagebox.showerror("Too many years", f'Setting Stop Year to {self.yearStop}')
            self.delYearStop(0,4)
            self.setYearStop(0, self.yearStop)
        for i in range(self.numAreas):
            for j in range(self.numYears):
                self.areas[i].results[j].myEntry.grid()
                self.areas[i].results[j].myLabel.grid()
                self.areas[i].results[j].myLabel.config(text = str(self.yearStart+j))

    def RunSort(self):
        paramData = [0.0 for _ in range(self.numYears)] # data read in from file
        rows = self.numAreas
        cols = self.numYears
        accumParamData = [[0.0 for _ in range(cols)] for _ in range(rows)] # accumulated data if in region
        desiredParam = self.comboParameter.get()
        # typical name: Lat_Lon_Grid_EBMS_MA_2015_2017
        paramFName = os.path.join(self.root, 'Results', 'Lat_Lon_Grid_' + 
             desiredParam + self.domainName + '_' + str(self.yearStart) + '_' + str(self.yearStop) + '.csv')
        
        #print(paramFName)

        # The data structure is used with InPolygon algorithm to check 
        # if grid parameter is within area of interest
        # TODO can this be done as data is entered?
        self.numAreas = int(self.numAreasEntry.get())
        for i in range(self.numAreas):
            self.areaData[i].numCorners = int(self.areas[i].numCornersEntry.myEntry.get())
            for j in range(self.areaData[i].numCorners):
                self.areaData[i].long[j] = float(self.areas[i].corners[j].longitude.myEntry.get())
                self.areaData[i].lat[j] = float(self.areas[i].corners[j].latitude.myEntry.get())

        of = open('temp.txt', 'w')

        if os.path.isfile(paramFName):
            grid = 1
            with open(paramFName, 'r') as f:
                while True:
                    # Read in a line from paramFName
                    line = f.readline()
                    if not line:
                        f.close()
                        break

                    # parse line
                    dataArray = [s.strip() for s in line.split(',')]
                    lat = float(dataArray[0])
                    lon = float(dataArray[1])
                    for i in range(self.numYears):
                        # column 2 will be initial data and not of interest
                        paramData[i] = float(dataArray[i+3]) 
                    
                    # Now check if the data point (lon, lat) is located in one of the desired areas
                    # self.areas[i] with given coordinates self.areas[i].corners[j]
                    #
                    # if so, add to accumParamData[i][0:n] += paramData[0:n]

                    # For each area
                    for i in range(self.numAreas):
                        # is (lon, lat) in this area
                        nodes = self.areaData[i].numCorners
                        if PointInPolygon(self.areaData[i].long, self.areaData[i].lat, lon, lat, nodes):
                            # if so accumulate parameter data
                            for j in range(self.numYears):
                                accumParamData[i][j] += paramData[j]
                            of.write('Grid #{} found in area{}\n'.format(grid,i+1))
                    grid += 1

        else:
            messagebox.showerror("Reading Parameter File", f'Has Simulation been run?\nAre years correct?')

        # display results
        for i in range(self.numAreas):
            for j in range(self.numYears):
                self.areas[i].results[j].myEntry.insert(0, str(accumParamData[i][j]))


    def UpdateWidgets(self, filePath=None):
        # Populate from known file
        currentParam = 0
        self.comboParameter.configure(values=self.paramStr)
        self.comboParameter.current(currentParam)
        self.numAreas = self.ReadAreaCorners(filePath)
        self.numAreasEntry.delete(0,3)
        self.numAreasEntry.insert(0, str(self.numAreas))
        self.NumAreasUpdate()
        for i in range(self.numAreas):
            self.areas[i].numCornersEntry.myEntry.delete(0,3)
            self.areas[i].numCornersEntry.myEntry.insert(0, str(self.areaData[i].numCorners))
            self.areas[i].NumCornersUpdate()
            for j in range(self.areaData[i].numCorners):
                self.areas[i].corners[j].longitude.myEntry.delete(0,10)
                self.areas[i].corners[j].longitude.myEntry.insert(0, str(self.areaData[i].long[j]))
                self.areas[i].corners[j].latitude.myEntry.delete(0,10)
                self.areas[i].corners[j].latitude.myEntry.insert(0, str(self.areaData[i].lat[j]))

    def GetDataSortFile(self):
        file_path = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")])
        if file_path:
            self.UpdateWidgets(file_path)
    

    def SaveDataSortFile(self):
        fName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv')
        if fName:
            with open(fName, 'w') as f:
                for i in range(int(self.numAreasEntry.get())):

                    # write longitude values
                    for j in range(int(self.areas[i].numCornersEntry.myEntry.get()) - 1):
                        f.write(self.areas[i].corners[j].longitude.myEntry.get()+',')
                    f.write(self.areas[i].corners[j+1].longitude.myEntry.get()+'\n')
                
                    # write latitude values
                    for j in range(int(self.areas[i].numCornersEntry.myEntry.get()) - 1):
                        f.write(self.areas[i].corners[j].latitude.myEntry.get()+',')
                    f.write(self.areas[i].corners[j+1].latitude.myEntry.get()+'\n')
            f.close()

    def NumAreasUpdate(self):
        """ Updates the number of areas functions. """

        for i in range(self.numAreasMax):
            self.areas[i].areaFrame.grid_remove()

        n = int(self.numAreasEntry.get())
        if n > self.numAreasMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numAreasMax}\nSetting to max')
            n = self.numAreasMax
            self.numAreasEntry.delete(0,3)
            self.numAreasEntry.insert(0, str(n))
        self.numAreas = n
        # Now update desired funtion definitions
        for i in range(self.numAreasMax):
            self.areas[i].areaFrame.grid_remove()
        for i in range(self.numAreas):
            self.areas[i].areaFrame.grid()

    def ReadAreaCorners(self, fName=None):
        """Reads a DataSort file and returns the number of areas defined"""
        areaIndex = 0
        if fName == None: fName = 'DataSort/AreasOfInterestDataSort.csv'
        if os.path.isfile(fName):
            with open(fName, 'r') as f:
                while True:
                    if (areaIndex > self.numAreasMax):
                        messagebox.showerror("Reading Areas File", f'Max reached {self.numAreasMax}\nStopping at {areaIndex-1}')
                        areaIndex -= 1
                        break

                    inputStr = f.readline()
                    if not inputStr:
                        f.close()
                        break

                    # read longitude values
                    longIndex = 0
                    j = len(inputStr)-1
                    if(inputStr[j]=='\n'): j -= 1
                    # remove trailing commas
                    while (inputStr[j]) == ',':
                        j -= 1
                    subStr = inputStr[0:j+1]

                    while j>0:
                        j=subStr.find(',')
                        if (j>0):
                            self.areaData[areaIndex].long[longIndex] = float(subStr[0:j])
                            subStr = subStr[j+1:len(subStr)]
                            longIndex += 1
                            if (longIndex > self.numCornersMax):
                                messagebox.showerror("Reading Areas File", f'Max corner {self.numCornersMax}\nStopping at {longIndex-1}')
                                longIndex -= 1
                                break
                    #get last value 
                    if (longIndex < self.numCornersMax):
                        self.areaData[areaIndex].long[longIndex] = float(subStr)
                        longIndex += 1

                    # read latiitude values
                    inputStr = f.readline()
                    if not inputStr:
                        f.close()
                        break
                    latIndex = 0
                    j = len(inputStr)-1
                    if(inputStr[j]=='\n'): j -= 1
                    # remove trailing commas
                    while (inputStr[j]) == ',':
                        j -= 1
                    subStr = inputStr[0:j+1]

                    while j>0:
                        j=subStr.find(',')
                        if (j>0):
                            self.areaData[areaIndex].lat[latIndex] = float(subStr[0:j])
                            subStr = subStr[j+1:len(subStr)]
                            latIndex += 1
                            if (latIndex > self.numCornersMax):
                                messagebox.showerror("Reading Areas File", f'Max corner {self.numCornersMax}\nStopping at {latIndex-1}')
                                latIndex -= 1
                                break
                    #get last value 
                    if (latIndex < self.numCornersMax):
                        self.areaData[areaIndex].lat[latIndex] = float(subStr)
                        latIndex += 1

                    if (longIndex != latIndex):
                        messagebox.showerror("Invalud Area Data File")
                        latIndex = 0
                    self.areaData[areaIndex].numCorners = latIndex
                    areaIndex += 1
                f.close()
            return areaIndex
        else: 
            messagebox.showerror("Data Sort", f'No Data Sort File Has Been Saved')
            return 1

class Corner:
    def __init__(self, maxCorners):
        self.long = [0.0 for _ in range(maxCorners)]
        self.lat = [0.0 for _ in range(maxCorners)]
        self.numCorners = 0

