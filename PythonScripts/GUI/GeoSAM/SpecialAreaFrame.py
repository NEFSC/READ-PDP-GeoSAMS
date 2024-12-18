## @page SpecialArea Special Access Area
# This frame in conjunction with the FishingMort in Special Access frame is 
# used to define fishing mortalities within a defined area for a specified 
# year. If a sim data point falls within a defined area given in this frame 
# by the assigned area number. Then if the current year is the same as the 
# year given in the FishingMort in Special Access frame and the area number 
# is listed then the fishing mortality is specified by the Mortality value. 
# Otherwise it is the default value which is defined in the Growth Frame as 
# Fishing mortality
# #
# @section pSAp1 Number of Areas
# The number of areas the user wishes to define. This is limited by Max Areas
# of Interest. See SHOW Args button
#
# The # of Areas is limited by default to 25. See SHOW Args. 
# The user can modify this on the command line:
# > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py #Areas #Nodes
# Default:
# > python .\\PythonScripts\\GUI\\GeoSAM\\GeoSams.py 25 8
#
# @section pSAp2 Special Access File
# The name of the file used to hold this information. The user can load the 
# default file 'SpecialAreas.csv' or define and save their own configuration.
#
# If this feature is not desired then enter NONE in the window
#
# Use Load Special Area File to load a predefined set of data
#
# Use Save Special Area File to save the currently displayed setting
#
# @section pSAp3 Area Definitions
# @subsection pSAp3p1 Area N
#   - Comment: Optional. Enter a comment to describe the area being specfied.
#
# @subsection pSAp3p2 Corners: 
# Also called nodes or sides. 
# This is limited by Max Nodes in Area. See SHOW Args for current values.
# This can be changed on the command line. See above
#               
# @subsection pSAp3p3  Corner N
# These are the coordinates of the area vertices. Enter the Longitude and
# Latitude of the vertices for the area. It is up to the user to ensure 
# that a closed shape is defined.
#
import os

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *
from AreaManager import *
from Globals import *

##
# This class is used to assist the user in defining areas of interest to assess accumulated parameters located
# in these areas of interest
#
class SpecialArea(ttk.Frame):
    def __init__(self, container, friend, maxAreas, maxCorners):
        super().__init__()

        self.friend = friend
        self.root = os.getcwd() #os.environ['ROOT']
        self.simStartDir = os.path.join(self.root, configDir, simCfgDir)
        
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
        self.numAreasLabel.grid(row=0, column=0, sticky='s', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.numAreasEntry=ttk.Entry(specialAreaFrame,validatecommand=numbersCallback, width=5)
        self.numAreasEntry.insert(0, str(self.numAreas))
        reg=self.numAreasEntry.register(numbersCallback)
        self.numAreasEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numAreasEntry.bind('<Return>', self.EnterKeyClicked)
        self.numAreasEntry.bind('<FocusOut>', self.EnterKeyClicked)
        self.numAreasEntry.grid(row=1, column=0, sticky='n', padx=5)
        # --------------------------------------------------------------------------------------------------------
        self.gmCfgFile   = SubFrameElement(self, specialAreaFrame, 'Grid Mgr Config File ', 'GridManager.cfg',0, 1, 2, width=20)
        #-------------------------------------------------------------------------------------------
        self.openGmgrConfigButton = ttk.Button(specialAreaFrame, text='Change/Save GMgr File', style="BtnBluGrn.TLabel", command=self.GetGMgrConfigFName)
        self.openGmgrConfigButton.grid(row=0, column=3)
        # --------------------------------------------------------------------------------------------------------
        self.openAreaFileButton = ttk.Button(specialAreaFrame, text='Load Special Area File', style="BtnGreen.TLabel", command=self.GetAreaFile)
        self.openAreaFileButton.grid(row=1, column=3)
        # --------------------------------------------------------------------------------------------------------
        self.specAccFile  = SubFrameElement(self, specialAreaFrame, 'Special Access File', '', 1, 1, 2, width=20)
        self.specAccFileLabel  = ttk.Label(specialAreaFrame, text='Set to NONE if not used\nAlso blocks Fish Mort File')
        self.specAccFileLabel.grid(row=2, column=2)
        # --------------------------------------------------------------------------------------------------------
        self.saveAreaFileButton = ttk.Button(specialAreaFrame, text='Save Special Area File', style="BtnBluGrn.TLabel", command=self.SaveAreaFile)
        self.saveAreaFileButton.grid(row=2, column=3)
        # --------------------------------------------------------------------------------------------------------
        self.areaMgr = AreaManager(self, specialAreaFrame, self.numAreasMax, self.numCornersMax,
                                   elementRow=3, elementCol=0, cornerRow=0, cornerColumn=0, labelArr=cornerLabelArr)

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

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Grid Manager Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetGMgrConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Grid Manager Config File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.simStartDir)
        f = file_path.split('/')
        if file_path:
            self.gmCfgFile.myEntry.delete(0,tk.END)
            self.gmCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteGridMgrConfig()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def NumAreasUpdate(self):
        if self.numAreasEntry.get() == '':
            n=1
            self.numAreasEntry.insert(0,'1')
        else:
            n = int(self.numAreasEntry.get())
        if n > self.numAreasMax:
            messagebox.showerror("Number of Areas ", f'Max is {self.numAreasMax}\nSetting to max')
            n = self.numAreasMax
            self.numAreasEntry.delete(0,tk.END)
            self.numAreasEntry.insert(0, str(n))
        self.numAreas = n
        self.areaMgr.NumAreasUpdate(self.numAreas)

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def UpdateWidgets(self):
        # Populate from known file
        if self.areaFName == os.path.join(self.startDir, 'NONE'):
            self.numAreas = 0
        else:
            self.numAreas = self.areaMgr.ReadAreaCorners(self.areaFName)
        self.numAreasEntry.delete(0,tk.END)
        self.numAreasEntry.insert(0, str(self.numAreas))
        self.NumAreasUpdate()
        self.areaMgr.UpdateWidgets(showCompArea=False)

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def GetAreaFile(self):
        self.areaFName = filedialog.askopenfilename(title="Open Special Area CSV File", filetypes=[("CSV files", "*.csv")],
                                                    defaultextension='csv', initialdir=self.startDir)
        if self.areaFName:
            self.UpdateWidgets()
            self.specAccFile.myEntry.delete(0,tk.END)
            f = self.areaFName.split('/')
            self.specAccFile.myEntry.insert(0, f[-1])
    
    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def SaveAreaFile(self):
        self.areaFName = filedialog.asksaveasfilename(title="Save Special Area CSV File", filetypes=[("CSV files", "*.csv")],
                                                      defaultextension='csv', initialdir=self.startDir)
        if self.areaFName:
            self.SaveSpecialAreaData(self.areaFName, int(self.numAreasEntry.get()))
            self.specAccFile.myEntry.delete(0,tk.END)
            f = self.areaFName.split('/')
            self.specAccFile.myEntry.insert(0, f[-1])

    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    def SaveSpecialAreaData(self, fname, numAreas):
        with open(fname, 'w') as f:
            f.write('# This file contains the vertices for special area polygons which are entered\n')
            f.write('# as a vector of longitude coordinates followed by a vector of latitude coordinates.\n')
            f.write('# The length of each vector must be the same that is same number of comma separated values not characters\n')
            f.write('# Lines starting with # are used as comments.\n')
            f.write('# For multiple lines (such as this note) - only the last line is retained\n')
            for i in range(numAreas):
                # write comment
                f.write('#'+self.areaMgr.areaSubFrame[i].comment+'\n')

                # write longitude values
                for j in range(int(self.areaMgr.areaSubFrame[i].numCornersEntry.myEntry.get()) - 1):
                    f.write(self.areaMgr.areaSubFrame[i].corners[j].longitude.myEntry.get()+',')
                f.write(self.areaMgr.areaSubFrame[i].corners[j+1].longitude.myEntry.get()+'\n')
            
                # write latitude values
                for j in range(int(self.areaMgr.areaSubFrame[i].numCornersEntry.myEntry.get()) - 1):
                    f.write(self.areaMgr.areaSubFrame[i].corners[j].latitude.myEntry.get()+',')
                f.write(self.areaMgr.areaSubFrame[i].corners[j+1].latitude.myEntry.get()+'\n')
        f.close()



    ## Help Window for Special Access Area
    #
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
