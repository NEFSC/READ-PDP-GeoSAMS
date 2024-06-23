#======================================================================================================
## @page FishMort Determine Fishing Mortality in Special Access Area
# Assists the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest.
#
# @section pFMSAp1 Number of Areas
#
# @section pFMSAp3 Load and Save Fishing Mortality Files
#
# @section pFMSAp5 Area SubFrames
#
# @subsection pFMSAp5p1 Year Definitions
#
# @subsection pFMSAp5p2 Accumulated Values for Given Output Parameter
#
# @subsection pFMSAp5p3 Corners, or Defined Mortalities by Area
#
# @subsubsection pFMSAp5p3p1 Number of Corners
#
# @subsubsection pFMSAp5p3p2 Corner Identifier by Special Area and Mortality
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
class FishMortBySpecAcc(ttk.Frame):
    def __init__(self, container, maxAreas, maxCorners):
        super().__init__()
        
        labelArr = ['Field', 'SpecArea', 'Mortality', '0', '0.4']
        self.root = os.getcwd() #os.environ['ROOT']
        self.startDir = os.path.join(self.root, configDir, specAccCfgDir)
        self.fmFName = None

        self.numDefinedMax = maxAreas
        self.numFieldsMax = maxCorners
        self.numDefined = 1
        self.numFieldss = 1

        scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        fishMortFrame = ttk.LabelFrame(scrollFrame.viewPort, text='Fishing Mort in Special Access', style='SAMS.TFrame', width=frameWidth, height=frameHeight)
        # --------------------------------------------------------------------------------------------------------
        self.numDefinedLabel = ttk.Label(fishMortFrame, text='# Defined')
        self.numDefinedLabel.grid(row=0, column=0, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.numDefinedEntry=ttk.Entry(fishMortFrame,validatecommand=numbersCallback, width=5)
        self.numDefinedEntry.insert(0, str(self.numDefined))
        reg=self.numDefinedEntry.register(numbersCallback)
        self.numDefinedEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numDefinedEntry.grid(row=0, column=1, sticky='w')
        self.numDefinedEntry.focus()
        self.numDefinedEntry.bind('<Return>', self.EnterKeyClicked)
        # --------------------------------------------------------------------------------------------------------
        self.fishMortFile  = SubFrameElement(self, fishMortFrame, 'Fishing Mort File\nSet to NONE if not used', '', 0, 2, 3, width=20)
        # --------------------------------------------------------------------------------------------------------
        self.numDefinedButton = ttk.Button(fishMortFrame, text='Update # Defined', command=self.NumDefinedUpdate)
        self.numDefinedButton.grid(row=1, column=0, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.openFMFileButton = ttk.Button(fishMortFrame, text='Load Fishing Mort File', style="BtnGreen.TLabel", command=self.GetFMFile)
        self.openFMFileButton.grid(row=0, column=4, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.saveFMFileButton = ttk.Button(fishMortFrame, text='Save Fishing Mort File', style="BtnBluGrn.TLabel", command=self.SaveFMFile)
        self.saveFMFileButton.grid(row=1, column=4, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.yearEntry = [ttk.Entry(fishMortFrame, width=5) for _ in range(self.numDefinedMax)]
        self.yearLabel = [ttk.Label(fishMortFrame, text='Year', width=4, justify='right') for _ in range(self.numDefinedMax)]
        for i in range(self.numDefinedMax):
            self.yearEntry[i].grid(row=2+i, column=0, sticky='ne')
            self.yearLabel[i].grid(row=2+i, column=0, sticky='nw', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.areaMgr = AreaManager(self, fishMortFrame, self.numDefinedMax, self.numFieldsMax, 
                                   elementRow=2, elementCol=1, cornerRow=0, cornerColumn=0, labelArr=labelArr)

        # now hide
        for a in range(self.numDefined, self.numDefinedMax):
            self.areaMgr.areaSubFrame[a].areaFrame.grid_remove()
            self.yearEntry[a].grid_remove()
            self.yearLabel[a].grid_remove()
        # --------------------------------------------------------------------------------------------------------
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        fishMortFrame.grid(row=4, column=0, columnspan=10)
        fishMortFrame.grid_columnconfigure(0,weight=2)
        fishMortFrame.grid_columnconfigure(1,weight=1)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        scrollFrame.grid(row=1, column=0, sticky='nsew')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Fishing Mort Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=0)

        self.bind("<Visibility>", self.on_visibility)

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
        self.fmFName = os.path.join(self.startDir, self.fishMortFile.myEntry.get())
        self.UpdateWidgets()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def EnterKeyClicked(self, event):
        self.NumDefinedUpdate()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def NumDefinedUpdate(self):
        n = int(self.numDefinedEntry.get())
        if n > self.numDefinedMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numDefinedMax}\nSetting to max')
            n = self.numDefinedMax
            self.numDefinedEntry.delete(0,tk.end)
            self.numDefinedEntry.insert(0, str(n))
        self.numDefined = n
        self.areaMgr.NumAreasUpdate(self.numDefined)

        # First hide everything
        for n in range(self.numDefinedMax):
            self.yearEntry[n].grid_remove()
            self.yearLabel[n].grid_remove()
        # then show desired number
        for n in range(self.numDefined):
            self.yearEntry[n].grid()
            self.yearLabel[n].grid()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def UpdateWidgets(self):
        # Populate from known file
        if self.fmFName == os.path.join(self.startDir, 'NONE'):
            self.numDefined = 0
            year = []
        else:
            (self.numDefined, year) = self.areaMgr.ReadFields(self.fmFName)
        self.numDefinedEntry.delete(0,tk.END)
        self.numDefinedEntry.insert(0, str(self.numDefined))
        self.NumDefinedUpdate()
        self.areaMgr.UpdateWidgets()

        # remove all
        for i in range(self.numDefinedMax):
            self.yearEntry[i].grid_remove()
            self.yearLabel[i].grid_remove()

        for i in range(len(year)):
            self.yearEntry[i].grid()
            self.yearLabel[i].grid()
            self.yearEntry[i].delete(0,tk.END)
            self.yearEntry[i].insert(0, year[i])

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def GetFMFile(self):
        self.fmFName = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.fmFName:
            self.UpdateWidgets()
            self.fishMortFile.myEntry.delete(0,tk.END)
            f = self.fmFName.split('/')
            self.fishMortFile.myEntry.insert(0, f[-1])
    
    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def SaveFMFile(self):
        self.fmFName = filedialog.asksaveasfilename(title="Save CSV File", filetypes=[("CSV files", "*.csv")], defaultextension='csv', initialdir=self.startDir)
        if self.fmFName:
            self.areaMgr.SaveFishingMortData(self.fmFName, int(self.numDefinedEntry.get()), self.yearEntry)
            self.fishMortFile.myEntry.delete(0,tk.END)
            f = self.fmFName.split('/')
            self.fishMortFile.myEntry.insert(0, f[-1])

    #-------------------------------------------------------------------------------------
    ## Help Window for Fishing Mortatlity in Special Access Area
    #-------------------------------------------------------------------------------------
    def pop_up(self):
        about = '''Fishing Mortatlity in Special Access Area
    (This frame is scrollable, use mouse wheel)
    This frame in conjunction with the Special Access Frame is used to define 
    fishing mortalities within a defined area for a specified year. If a 
    location falls within the defined area given by the area defintions in 
    Special Access Frame and assigned the area number. Then if the current year
    is the same as the year identified in this frame and the area number is 
    listed then the fishing mortality is specified by the Mortality value. 
    Otherwise it is the default value which is defined in the Growth Frame as 
    Fishing mortality

# Defined
    The number of defined areas as determined by the user. This is limited by 
    Max Areas of Interest. See SHOW Args button for current values.

    The # Defined is limited by default to 25. See SHOW Args for current values.
    The user can modify this on the command line:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py #Areas #Nodes #Years
    Default:
    > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py 25 8 5

Update # Defined
    Use Enter Key or click this button after entering a value in # Defined to
    populate/show the Area N defintions.

Fishing Mort File
    The name of the file used to hold this information. The user can load the 
    default file 'FishingMortality.csv' or save their own configuration.

    If this feature is not desired then enter NONE in the window

    Use Load Fishing Mort File to load a predefined set of data

    Use Save Fishing Mort File to save the currently displayed setting

Year
    The year for which Area N is valid

Area N
    Comment: Optional. Enter a comment to describe the area being specfied.

    # Corners: Specifically, the number of Fields for the year given. 
        This is limited by Max Nodes in Area. See SHOW Args for current values.
        This can be changed on the command line. See above

    Update # Corners
        Use Enter Key or click this button to populate the field entries for 
        the given number

    Field N
        These are the area numbers as determined in Special Access Frame. Enter
        the area number and its Mortality.
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
