#======================================================================================================
## @page SpecialArea Special Access Area
# Assists the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest.
#
# @section pSAp1 Number of Areas
#
# @section pSAp2 Output Parameters
#
# @section pSAp3 Load and Save Data Sort Files
#
# @section pSAp4 Run Sort
#
# @section pSAp5 Area SubFrames
#
# @subsection pSAp5p1 Years Simulated
#
# @subsection pSAp5p2 Accumulated Values for Given Output Parameter
#
# @subsection pSAp5p3 Corners
#
# @subsubsection pSAp5p3p1 Number of Corners
#
# @subsubsection pSAp5p3p2 Corner Identifier by Longitude and Latitude
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
class SpecialArea(ttk.Frame):
    def __init__(self, container, maxAreas, maxCorners):
        super().__init__()
        
        labelArr = ['Corner', 'Long', 'Lat ', '0.0', '0.0']
        self.root = os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'Configuration')
        self.areaFName = None

        self.numAreasMax = maxAreas
        self.numCornersMax = maxCorners
        self.numAreas = 1
        self.numCorners = 1

        self.style = ttk.Style()
        self.style.configure('SpecialArea.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('SpecialArea.TFrame.Label', font=('courier', 10, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        
        # --------------------------------------------------------------------------------------------------------
        self.specialAreaFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Special Area', style='SpecialArea.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numAreasLabel = ttk.Label(self.specialAreaFrame, text='# of Areas')
        self.numAreasLabel.grid(row=0, column=0, sticky='w')
        self.numAreasEntry=ttk.Entry(self.specialAreaFrame,validatecommand=numbersCallback, width=5)
        self.numAreasEntry.insert(0, str(self.numAreas))
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numAreasEntry.grid(row=0, column=1, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.specAccFile  = SubFrameElement(self, self.specialAreaFrame, 'Special Access File\nSet to NONE if not used\nAlso blocks Fish Mort File', '', 0, 2, 3, width=20)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numAreasButton = ttk.Button(self.specialAreaFrame, text='Update # Areas', command=self.NumAreasUpdate)
        self.numAreasButton.grid(row=1, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.style.configure("SpecialArea.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("SpecialAreaA.TLabel", padding=6, relief='raised', background="#0FF")
        
        self.openAreaFileButton = ttk.Button(self.specialAreaFrame, text='Load Special Area File', style="SpecialArea.TLabel", command=self.GetAreaFile)
        self.openAreaFileButton.grid(row=0, column=4, sticky='w')

        self.saveAreaFileButton = ttk.Button(self.specialAreaFrame, text='Save Special Area File', style="SpecialAreaA.TLabel", command=self.SaveAreaFile)
        self.saveAreaFileButton.grid(row=1, column=4, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.areaMgr = AreaManager(self, self.specialAreaFrame, self.numAreasMax, self.numCornersMax,
                                   elementRow=2, elementCol=0, cornerRow=0, cornerColumn=0, labelArr=labelArr)

        # now hide
        for a in range(self.numAreas, self.numAreasMax):
            self.areaMgr.areaSubFrame[a].areaFrame.grid_remove()

        self.specialAreaFrame.grid(row=4, column=0, columnspan=10)
        self.specialAreaFrame.grid_columnconfigure(0,weight=2)
        self.specialAreaFrame.grid_columnconfigure(1,weight=1)
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')

        self.bind("<Visibility>", self.on_visibility)

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
        self.areaFName = os.path.join(self.startDir, self.specAccFile.myEntry.get())
        self.UpdateWidgets()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def NumAreasUpdate(self):
        n = int(self.numAreasEntry.get())
        if n > self.numAreasMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numAreasMax}\nSetting to max')
            n = self.numAreasMax
            self.numAreasEntry.delete(0,3)
            self.numAreasEntry.insert(0, str(n))
        self.numAreas = n
        self.areaMgr.NumAreasUpdate(self.numAreas)

    def UpdateWidgets(self):
        # Populate from known file
        self.numAreas = self.areaMgr.ReadAreaCorners(self.areaFName)
        self.numAreasEntry.delete(0,3)
        self.numAreasEntry.insert(0, str(self.numAreas))
        self.NumAreasUpdate()
        self.areaMgr.UpdateWidgets()

    def GetAreaFile(self):
        self.areaFName = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.areaFName:
            self.UpdateWidgets()
            n = len(self.specAccFile.myEntry.get())
            self.specAccFile.myEntry.delete(0,n)
            f = self.areaFName.split('/')
            self.specAccFile.myEntry.insert(0, f[-1])
    
    def SaveAreaFile(self):
        self.areaFName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.areaFName:
            self.areaMgr.SaveSpecialAreaData(self.areaFName, int(self.numAreasEntry.get()))
            n = len(self.specAccFile.myEntry.get())
            self.specAccFile.myEntry.delete(0,n)
            f = self.areaFName.split('/')
            self.specAccFile.myEntry.insert(0, f[-1])

