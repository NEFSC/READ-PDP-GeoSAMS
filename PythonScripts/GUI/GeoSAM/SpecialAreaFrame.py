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
from Globals import *

#===============================================================================================================
##
# This class is used to assist the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest
#===============================================================================================================
class SpecialArea(ttk.Frame):
    def __init__(self, container, maxAreas, maxCorners):
        super().__init__()
        
        self.root = os.getcwd() #os.environ['ROOT']
        self.startDir = os.path.join(self.root, configDir, specAccCfgDir)
        self.areaFName = None

        self.numAreasMax = maxAreas
        self.numCornersMax = maxCorners
        self.numAreas = 1
        self.numCorners = 1

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        specialAreaFrame = ttk.LabelFrame(scrollFrame.viewPort, text='Special Area', style='SAMS.TFrame', width=frameWidth, height=frameHeight)
        # --------------------------------------------------------------------------------------------------------
        self.numAreasLabel = ttk.Label(specialAreaFrame, text='# of Areas')
        self.numAreasLabel.grid(row=0, column=0, sticky='w', padx=5)
        self.numAreasEntry=ttk.Entry(specialAreaFrame,validatecommand=numbersCallback, width=5)
        self.numAreasEntry.insert(0, str(self.numAreas))
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numAreasEntry.grid(row=0, column=1, sticky='w', padx=5)
        self.numAreasEntry.focus()
        self.numAreasEntry.bind('<Return>', self.EnterKeyClicked)
        # --------------------------------------------------------------------------------------------------------
        self.specAccFile  = SubFrameElement(self, specialAreaFrame, 'Special Access File\nSet to NONE if not used\nAlso blocks Fish Mort File', '', 0, 2, 3, width=20)
        # --------------------------------------------------------------------------------------------------------
        self.numAreasButton = ttk.Button(specialAreaFrame, text='Update # Areas', command=self.NumAreasUpdate)
        self.numAreasButton.grid(row=1, column=0, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.openAreaFileButton = ttk.Button(specialAreaFrame, text='Load Special Area File', style="BtnGreen.TLabel", command=self.GetAreaFile)
        self.openAreaFileButton.grid(row=0, column=4, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.saveAreaFileButton = ttk.Button(specialAreaFrame, text='Save Special Area File', style="BtnBluGrn.TLabel", command=self.SaveAreaFile)
        self.saveAreaFileButton.grid(row=1, column=4, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.areaMgr = AreaManager(self, specialAreaFrame, self.numAreasMax, self.numCornersMax,
                                   elementRow=2, elementCol=0, cornerRow=0, cornerColumn=0, labelArr=labelArr)

        # now hide
        for a in range(self.numAreas, self.numAreasMax):
            self.areaMgr.areaSubFrame[a].areaFrame.grid_remove()
        # --------------------------------------------------------------------------------------------------------
        specialAreaFrame.grid(row=4, column=0, columnspan=10, padx=5)
        specialAreaFrame.grid_columnconfigure(0,weight=2)
        specialAreaFrame.grid_columnconfigure(1,weight=1)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        scrollFrame.grid(row=1, column=0, sticky='nsew')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Special Access Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=0)

        self.bind("<Visibility>", self.on_visibility)

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
        self.areaFName = os.path.join(self.startDir, self.specAccFile.myEntry.get())
        self.UpdateWidgets()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def EnterKeyClicked(self, event):
        self.NumAreasUpdate()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def NumAreasUpdate(self):
        n = int(self.numAreasEntry.get())
        if n > self.numAreasMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numAreasMax}\nSetting to max')
            n = self.numAreasMax
            self.numAreasEntry.delete(0,tk.END)
            self.numAreasEntry.insert(0, str(n))
        self.numAreas = n
        self.areaMgr.NumAreasUpdate(self.numAreas)

    def UpdateWidgets(self):
        # Populate from known file
        if self.areaFName == os.path.join(self.startDir, 'NONE'):
            self.numAreas = 0
        else:
            self.numAreas = self.areaMgr.ReadAreaCorners(self.areaFName)
        self.numAreasEntry.delete(0,tk.END)
        self.numAreasEntry.insert(0, str(self.numAreas))
        self.NumAreasUpdate()
        self.areaMgr.UpdateWidgets()

    def GetAreaFile(self):
        self.areaFName = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.areaFName:
            self.UpdateWidgets()
            self.specAccFile.myEntry.delete(0,tk.END)
            f = self.areaFName.split('/')
            self.specAccFile.myEntry.insert(0, f[-1])
    
    def SaveAreaFile(self):
        self.areaFName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.areaFName:
            self.areaMgr.SaveSpecialAreaData(self.areaFName, int(self.numAreasEntry.get()))
            self.specAccFile.myEntry.delete(0,tk.END)
            f = self.areaFName.split('/')
            self.specAccFile.myEntry.insert(0, f[-1])

    #-------------------------------------------------------------------------------------
    ## Help Window for Special Access Area
    #-------------------------------------------------------------------------------------
    def pop_up(self):
        about = '''Special Area
    (This frame is scrollable, use mouse wheel)
    This frame in conjunction with the FishingMort in Special Access frame is 
    used to define fishing mortalities within a defined area for a specified 
    year. If a sim data point falls within a defined area given in this frame 
    by the assigned area number. Then if the current year is the same as the 
    year given in the FishingMort in Special Access frame and the area number 
    is listed then the fishing mortality is specified by the Mortality value. 
    Otherwise it is the default value which is defined in the Growth Frame as 
    Fishing mortality

# of Areas
    The number of areas the user wishes to define. This is limited by Max Areas
    of Interest. See SHOW Args button

Update # Areas
    Use Enter Key or click this button after entering a value in # of Areas to 
    populate/show the Area N defintions

    The # of Areas is limited by default to 25. See SHOW Args. 
    The user can modify this on the command line:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py #Areas #Nodes
    Default:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py 25 8


Special Access File
    The name of the file used to hold this information. The user can load the 
    default file 'SpecialAreas.csv' or define and save their own configuration.

    If this feature is not desired then enter NONE in the window

    Use Load Special Area File to load a predefined set of data

    Use Save Special Area File to save the currently displayed setting

Area N
    Comment: Optional. Enter a comment to describe the area being specfied.

    # Corners: Also called nodes or sides. 
        This is limited by Max Nodes in Area. See SHOW Args for current values.
        This can be changed on the command line. See above
               
    Update # Corners
        Use Enter Key or click this button to populate the corner entries for 
        the given number

    Corner N
        These are the coordinates of the area vertices. Enter the Longitude and
        Latitude of the vertices for the area. It is up to the user to ensure 
        that a closed shape is defined.
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
