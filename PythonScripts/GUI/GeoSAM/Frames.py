import tkinter as tk
import csv
import platform

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from GeoSams import MainApplication


class Frame1(ttk.Frame):
    """ This class displays information about GeoSAMS simulation. This same information is used
    on the command line when starting SRC\ScallopPopDensity"""

    def __init__(self, container):
        super().__init__()

        self.style = ttk.Style()
        self.style.configure('Frame1.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame1.TFrame.Label', font=('courier', 8, 'bold'))
        #-------------------------------------------------------------------------------------------
        growthFrame = ttk.LabelFrame(self, text='Growth', style='Frame1.TFrame')
        self.startYr    = SubFrameElement(self, growthFrame, 'Start Year', '2015',          0, 0, 1)
        self.stopYr     = SubFrameElement(self, growthFrame, 'Stop Year ', '2017',          1, 0, 1)
        self.domainName = SubFrameElement(self, growthFrame, 'Domain Name\nMA or GB', 'MA', 2, 0, 1, self.valDN)
        reg=self.domainName.myEntry.register(self.valDN)
        self.domainName.myEntry.configure(validate='key', validatecommand=(reg, '%P'))

        growthFrame.grid(row=0, column=0)
        #-------------------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------------------
        recruitFrame = ttk.LabelFrame(self, text='Recruitment', style='Frame1.TFrame')
        self.startDay    = SubFrameElement(self, recruitFrame, 'Start Day\nMmm DD', 'Jan 1',  0, 0, 1)
        self.stopDay     = SubFrameElement(self, recruitFrame, 'Stop Day\nMmm DD ', 'Apr 11', 1, 0, 1)
        recruitFrame.grid(row=0, column=1)
        #-------------------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------------------
        configFrame = ttk.LabelFrame(self, text='Configuration Files', style='Frame1.TFrame')
        self.mortCfgFile = SubFrameElement(self, configFrame, 'Mortality Config File', 'Mortality.cfg',  0, 0, 1)
        self.recrCfgFile = SubFrameElement(self, configFrame, 'Recruitment File',      'Recruitment.cfg',1, 0, 1)
        self.gmCfgFile   = SubFrameElement(self, configFrame, 'Grid Manager File',     'GridManager.cfg',2, 0, 1)
        self.simCfgFile  = SubFrameElement(self, configFrame, 'Sim Config File',       'Scallop.cfg',    3, 0, 1)
        self.ukCfgFile   = SubFrameElement(self, configFrame, 'UK Config File',        'UK.cfg',         4, 0, 1)
        configFrame.grid(row=0, column=2)
        #-------------------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------------------
        specialAccFrame = ttk.LabelFrame(self, text='Special Access', style='Frame1.TFrame')
        self.specAccFile  = SubFrameElement(self, specialAccFrame, 'Special Access Points', '', 0, 0, 1)
        self.fishMortFile = SubFrameElement(self, specialAccFrame, 'Fishing Mort File', '', 1, 0, 1)

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#080")
        self.openFishCSVButton = ttk.Button(specialAccFrame, text='View', style="Custom.TLabel", command=self.OpenSpecAccCSV)
        self.openFishCSVButton.grid(row=0, column=2)

        self.tree = ttk.Treeview(specialAccFrame, show="headings")
        self.tree.grid(row=2, column=0, columnspan=5, padx=10)
        specialAccFrame.grid(row=1, column=0, columnspan=3, sticky='w')
        #-------------------------------------------------------------------------------------------

    def valDN(self, input):
        """Only allows alpha"""
        if input == 'M': return True
        elif input == 'MA': return True  
        elif input == 'G': return True
        elif input == 'GB': return True  
        elif input == "": return True
        else: return False


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

        self.okToRepaintFunctions = True
        self.nsfMax = 20
        self.functions = [None for i in range(self.nsfMax)]
        self.myget = get # pointer function to domain name entry
        self.domainName = self.myget()
        if self.domainName == 'MA':
            self.nsf = 9
        else:
            self.nsf = 5
        self.style = ttk.Style()
        self.style.configure('Frame4.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame4.TFrame.Label', font=('courier', 8, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.

        # --------------------------------------------------------------------------------------------------------
        self.funcFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Spatial Functions', style='Frame4.TFrame', width=400, height=200)

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
        paramFrame= ttk.LabelFrame(self.scrollFrame.viewPort, text='Parameters', style='Frame4.TFrame')
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

class Frame5(ttk.Frame):
    def __init__(self, container):
        super().__init__()

        self.numAreasMax = 25
        self.numCornersMax = 8
        self.numAreas = 1
        self.numCorners = 1

        self.areas = [None for i in range(self.numAreasMax)]

        self.style = ttk.Style()
        self.style.configure('Frame5.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame5.TFrame.Label', font=('courier', 8, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        
        # --------------------------------------------------------------------------------------------------------
        self.sortAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Sort By Area', style='Frame5.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numAreasLabel = ttk.Label(self.sortAreaFrame, text='# of Areas')
        self.numAreasLabel.grid(row=0, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numAreasEntry=ttk.Entry(self.sortAreaFrame,validatecommand=numbersCallback)
        self.numAreasEntry.insert(0, str(self.numAreas))
        self.numAreasEntry.grid(row=1, column=0, sticky='w')
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numAreasButton = ttk.Button(self.sortAreaFrame, text='Update', command=self.NumAreasUpdate)
        self.numAreasButton.grid(row=2, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        
        # # The grid manager keep all widgets once defined. We are just going to decide which are shown
        # # paint first row to show x, y, and z
        # # remaining rows are just x, y
        # desNumCol = 5
        # for i in range(self.numCornersMax):  
        #     row = (i // desNumCol) + 2
        #     col = i % desNumCol
        #     self.corners[i] = SubFrameLongLat(self, self.sortAreaFrame, str(i+1), '-70', '45', row, col)

        # # Now remove undesired funtion definitions
        # for i in range(self.numAreas, self.numCornersMax):
        #     self.corners[i].cornerFrame.grid_remove()
        
        for a in range(self.numAreasMax):
            row = 3 + a
            col = 0
            self.areas[a] = SubFrameArea(self, self.sortAreaFrame, a, self.numCorners, self.numCornersMax, row, col)

        # now hide
        for a in range(self.numAreas, self.numAreasMax):
            self.areas[a].areaFrame.grid_remove()

        self.sortAreaFrame.grid(row=4, column=0, columnspan=10)
        self.sortAreaFrame.grid_columnconfigure(0,weight=2)
        self.sortAreaFrame.grid_columnconfigure(1,weight=1)
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')


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

