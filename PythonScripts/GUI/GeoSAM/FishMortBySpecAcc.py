#======================================================================================================
## @page FishMort Determine Fishing Mortality in Special Access Area
# Assists the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest.
#
# @section pFMSAp1 Number of Areas
#
# @section pFMSAp2 Output Parameters
#
# @section pFMSAp3 Load and Save Data Sort Files
#
# @section pFMSAp4 Run Sort
#
# @section pFMSAp5 Area SubFrames
#
# @subsection pFMSAp5p1 Years Simulated
#
# @subsection pFMSAp5p2 Accumulated Values for Given Output Parameter
#
# @subsection pFMSAp5p3 Corners
#
# @subsubsection pFMSAp5p3p1 Number of Corners
#
# @subsubsection pFMSAp5p3p2 Corner Identifier by Longitude and Latitude
# 
#======================================================================================================
import os

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from AreaManager import *

#===============================================================================================================
##
# This class is used to assist the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest
#===============================================================================================================
class FishMortBySpecAcc(ttk.Frame):
    def __init__(self, container, maxAreas, maxCorners):
        super().__init__()
        
        labelArr = ['Field', 'SpecArea', 'Mortality', '0', '0.4']
        self.root = os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'Configuration')
        self.fmFName = None

        self.numDefinedMax = maxAreas
        self.numFieldsMax = maxCorners
        self.numDefined = 1
        self.numFieldss = 1

        self.style = ttk.Style()
        self.style.configure('FishMort.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('FishMort.TFrame.Label', font=('courier', 8, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        
        # --------------------------------------------------------------------------------------------------------
        self.fishMortFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Fishing Mort in Special Access', style='FishMort.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numDefinedLabel = ttk.Label(self.fishMortFrame, text='# Defined')
        self.numDefinedLabel.grid(row=0, column=0, sticky='w')
        self.numDefinedEntry=ttk.Entry(self.fishMortFrame,validatecommand=numbersCallback, width=5)
        self.numDefinedEntry.insert(0, str(self.numDefined))
        reg=self.numDefinedEntry.register(numbersCallback)
        self.numDefinedEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numDefinedEntry.grid(row=0, column=1, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.fishMortFile  = SubFrameElement(self, self.fishMortFrame, 'Fishing Mort File\nSet to NONE if not used', '', 0, 2, 3, width=20)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numDefinedButton = ttk.Button(self.fishMortFrame, text='Update # Defined', command=self.NumDefinedUpdate)
        self.numDefinedButton.grid(row=1, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.style.configure("FishMort.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("FishMortA.TLabel", padding=6, relief='raised', background="#0FF")
        
        self.openFMFileButton = ttk.Button(self.fishMortFrame, text='Load Fishing Mort File', style="FishMort.TLabel", command=self.GetFMFile)
        self.openFMFileButton.grid(row=0, column=4, sticky='w')

        self.saveFMFileButton = ttk.Button(self.fishMortFrame, text='Save Fishing Mort File', style="FishMortA.TLabel", command=self.SaveFMFile)
        self.saveFMFileButton.grid(row=1, column=4, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        self.yearEntry = [ttk.Entry(self.fishMortFrame, width=5) for _ in range(self.numDefinedMax)]
        for i in range(self.numDefinedMax):
            self.yearEntry[i].grid(row=2+i, column=0, sticky='n', padx=5)
        
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.areaMgr = AreaManager(self, self.fishMortFrame, self.numDefinedMax, self.numFieldsMax, 
                                   elementRow=2, elementCol=1, cornerRow=0, cornerColumn=0, labelArr=labelArr)

        # now hide
        for a in range(self.numDefined, self.numDefinedMax):
            self.areaMgr.areaSubFrame[a].areaFrame.grid_remove()
            self.yearEntry[a].grid_remove()

        self.fishMortFrame.grid(row=4, column=0, columnspan=10)
        self.fishMortFrame.grid_columnconfigure(0,weight=2)
        self.fishMortFrame.grid_columnconfigure(1,weight=1)
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')

        self.bind("<Visibility>", self.on_visibility)

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
        self.fmFName = os.path.join(self.startDir, self.fishMortFile.myEntry.get())
        self.UpdateWidgets()


    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def NumDefinedUpdate(self):
        n = int(self.numDefinedEntry.get())
        if n > self.numDefinedMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numDefinedMax}\nSetting to max')
            n = self.numDefinedMax
            self.numDefinedEntry.delete(0,3)
            self.numDefinedEntry.insert(0, str(n))
        self.numDefined = n
        self.areaMgr.NumAreasUpdate(self.numDefined)

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def UpdateWidgets(self):
        # Populate from known file
        (self.numDefined, year) = self.areaMgr.ReadFields(self.fmFName)
        self.numDefinedEntry.delete(0,3)
        self.numDefinedEntry.insert(0, str(self.numDefined))
        self.NumDefinedUpdate()
        self.areaMgr.UpdateWidgets()

        for i in range(len(year)):
            self.yearEntry[i].grid()
            self.yearEntry[i].delete(0,4)
            self.yearEntry[i].insert(0, year[i])


    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def GetFMFile(self):
        self.fmFName = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.fmFName:
            self.UpdateWidgets()
            n = len(self.fishMortFile.myEntry.get())
            self.fishMortFile.myEntry.delete(0,n)
            f = self.fmFName.split('/')
            self.fishMortFile.myEntry.insert(0, f[-1])
    
    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def SaveFMFile(self):
        self.fmFName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.fmFName:
            self.areaMgr.SaveFishingMortData(self.fmFName, int(self.numDefinedEntry.get()), self.yearEntry)
            n = len(self.fishMortFile.myEntry.get())
            self.fishMortFile.myEntry.delete(0,n)
            f = self.fmFName.split('/')
            self.fishMortFile.myEntry.insert(0, f[-1])

