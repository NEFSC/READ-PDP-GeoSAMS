#======================================================================================================
## @page page4 UKInterpolation Frame
# This frame assists the user in defining spatial functions used to perform Universal Kriging, UK interpolation.
# 
# @section p5p1 Parameters
# These are parameters that define limits and type of interpolation. For more details on Universal Kriging 
# refer to Noel A. C. Cressie, "Statistics for Spatial Data", published by John Wiley & Sons, Inc., 1973
# pages 151 to 154
#
# @subsection p5p1p1 High Limit Factor
#
# @subsection p5p1p2 Variogram Form
# This defines the shape of the variogram models. The kriging refernces identify a typical variogram shape.
# It is a positive sloped function with a y intercept defined as nugget. The assymptote is defined as the sill.
# The inflection point where the function reaches the sill is defined as the ranges. Four shapes are implemented
# for UK interpolation
# - spherical
# - exponential
# - gaussian
# - matern
#
# @subsubsection p5p1p2p1 spherical
#
# @f[ \gamma(h) = \begin{cases}  nugget + sill *\frac{3h}{2*range} - \frac{1}{2}(\frac{h^3}{range^3}), & 0 < h \leq  range\\  nugget + sill, & h > range \\ 0, & h = 0  \end{cases} @f]
#
# @subsubsection p5p1p2p2 exponential
#
# @f[ \gamma(h) = \begin{cases}  nugget + sill *(1 - exp(\frac{-h}{range})), & h > 0     \\  0, & h = 0  \end{cases} @f]
#
# @subsubsection p5p1p2p3 gaussian
#
# @f[ \gamma(h) = \begin{cases}  nugget + sill *(1 - exp(\frac{-h^2}{range^2})), & h > 0 \\  0, & h = 0  \end{cases} @f]
#
# @subsubsection p5p1p2p4 matern
#
# @f[  \gamma(h) = \begin{cases} nugget + sill *\left(1 - \frac{\sqrt(2)}{\Gamma(0.5)}*J_n(2,\frac{h}{range})*\sqrt(\frac{h}{range})\right), & h > 0 \\   0, & h = 0  \end{cases} @f]
# @f$ \text{ where }J_n\text{ is the Bessel function of the first kind} @f$
#
# @subsection p5p1p3 Use Log Transform
# If this is set to True, then a log of the observed data is used before starting the interpolation.
# @f$ \vec{obs} = log( c + \vec{obs} / \mu) @f$
#
# @subsection p5p1p4 Power Transform
#
# Power transform interpolates f(x)^alpha. Generally 0< alpha < 1 but this has not been tested. Not used if "Log Transform = T"
#
#
# @subsection p5p1p5 Spatial Fcn Configuration File
# The name of the file to hold the formating of the spactial functions read in by UK.exe during interpolation
#
# @section p5p2 Spatial Functions
# Define non linear spatial functions(NLSF) and paramater search range.
#
# - "Function 1, dim=z, shape=Logistic, precon=0 "
# - "Function 2, dim=z, shape=Gaussian, precon=0 "
# - "Function 3, dim=x, shape=Logistic, precon=1 "
#
# These define spatial functions for setting the spatial trend in the universal kriging algorithm. 
#
# The precon=0 term means that the function is not multiplied by another function. For example,\n
#    "Function 3, dim=x, shape=Logistic, precon=1 " \n
# indicates that the third function is multiplied by the first function.\n\n
# This is true for fitting the nonlinear parameters of function 3 hence the parameters of function 1 must be fit before the parameters of function 3.
#
# @subsection p5p2p1 Number (#) of Functions
# 
# @subsection p5p2p2 Function Definitions
# 
#======================================================================================================
import tkinter as tk
import csv
import platform
import os

from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

from Widgets import *

#======================================================================================================
##
# This class is used to present the parameters to the user to customize how the interpolation is performed.\n
# Testing has shown that 
# - MA works best with 9 spatial functions
# - GB works best with 5 spatial functions

#======================================================================================================
class UKInterpolation(ttk.Frame):
    def __init__(self, container, parent, friend):
        super().__init__()
        self.parent = parent
        self.root = os.getcwd() #os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'Configuration', 'Interpolation')
        self.friend = friend
        self.okToRepaintFunctions = True
        self.nsfMax = 20
        self.functions = [None for _ in range(self.nsfMax)]
        self.domainName = self.friend.domainNameCombo.get()
        if self.domainName == 'AL':
            self.nsf = 11
        elif self.domainName == 'MA':
            self.nsf = 9
        else:
            self.nsf = 5
        self.style = ttk.Style()
        self.style.configure('UKInterpolation.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('UKInterpolation.TFrame.Label', font=('courier', 10, 'bold'))

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        scrollFrame = ScrollFrame(self) # add a new scrollable frame.
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        funcFrame = ttk.LabelFrame(scrollFrame.viewPort, text='Spatial Functions', style='UKInterpolation.TFrame', width=400, height=200)
        # --------------------------------------------------------------------------------------------------------
        self.numFncsLabel = ttk.Label(funcFrame, text='# of Functions')
        self.numFncsLabel.grid(row=0, column=0, sticky='w')
        # --------------------------------------------------------------------------------------------------------
        self.numFcnsEntry=ttk.Entry(funcFrame, validatecommand=numbersCallback, width=5)
        self.numFcnsEntry.insert(0, str(self.nsf))
        self.numFcnsEntry.grid(row=0, column=0, sticky='e')
        reg=self.numFcnsEntry.register(numbersCallback)
        self.numFcnsEntry.configure(validate='key', validatecommand=(reg, '%P'))
        self.numFcnsEntry.focus()
        self.numFcnsEntry.bind('<Return>', self.EnterKeyClicked)
        # --------------------------------------------------------------------------------------------------------
        self.numFncsButton = ttk.Button(funcFrame, text='Update', command=self.NumFuncsUpdate)
        self.numFncsButton.grid(row=0, column=1)
        # --------------------------------------------------------------------------------------------------------
        # The tkinter grid manager keep all widgets once defined. We are just going to decide which are shown
        # paint first row to show x, y, and z
        # remaining rows are just x, y
        desNumCol = 2
        for i in range(self.nsfMax):
            if i < 3:
                row = 1
                precon = 0
                initShape = ['Logistic', 'Gaussian', 'SinExp']
                self.functions[i] = SubFrameInterpFunction(self, funcFrame, str(i+1), 'z', initShape[i], precon, row, i%3)
            else:
                row = ((i-3) // desNumCol) + 2
                col = (i-3) % desNumCol
                precon = row - 2
                self.functions[i] = SubFrameInterpFunction(self, funcFrame, str(i+1), chr(120+col), 'Logistic', precon, row, col)
        # Now hide undesired funtion definitions
        for i in range(self.nsf, self.nsfMax):
            self.functions[i].funcFrame.grid_remove()

        funcFrame.grid(row=0, column=1, rowspan=3, sticky='n')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        paramFrame= ttk.LabelFrame(scrollFrame.viewPort, text='Parameters', style='UKInterpolation.TFrame')
        #DEPRECATE#self.highLimit   = SubFrameElement(self, paramFrame, 'High Limit Factor ', '1.5',  0, 0, 1)

        formComboList = ['spherical', 'exponential', 'gaussian', 'matern']
        self.formLabel = ttk.Label(paramFrame, text='Variogram Form')
        self.formLabel.grid(row=0, column=0)
        # --------------------------------------------------------------------------------------------------------
        self.formCombo = ttk.Combobox(paramFrame, values=formComboList, width=14)
        self.formCombo.current(0)
        self.formCombo.grid(row=0, column=1, pady=5)
        # --------------------------------------------------------------------------------------------------------
        #DEPRECATE#self.useLogTransLabel = ttk.Label(paramFrame, text='Use Log Transfrom)')
        #DEPRECATE#self.useLogTransLabel.grid(row=2, column=0)
        # --------------------------------------------------------------------------------------------------------
        #DEPRECATE#self.useLogTransCombo = ttk.Combobox(paramFrame, width=3, values=['T', 'F'])
        #DEPRECATE#self.useLogTransCombo.current(0)
        #DEPRECATE#self.useLogTransCombo.grid(row=2, column=1, pady=5)
        # --------------------------------------------------------------------------------------------------------
        #DEPRECATE#self.powerTrans  = SubFrameElement(self, paramFrame, 'Power Tranform\n(Not used if Log = T)', '1.0', 3, 0, 1)
        # --------------------------------------------------------------------------------------------------------
        self.spatCfgFile  = SubFrameElement(self, paramFrame, 'Spatial Fcn Config File', 'SpatialFcns.cfg', 1, 0, 1, width=20)
        # --------------------------------------------------------------------------------------------------------
        self.style.configure("Frame4.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("Frame4A.TLabel", padding=6, relief='raised', background="#0FF")
        self.openSpatFncConfigButton = ttk.Button(paramFrame, text='Load Spat Fcn File', style="Frame4.TLabel", command=self.GetSpatialFcnConfigFName)
        self.openSpatFncConfigButton.grid(row=2, column=0)
        # --------------------------------------------------------------------------------------------------------
        self.saveSpatFncConfigButton = ttk.Button(paramFrame, text='Save Spat Fcn File', style="Frame4A.TLabel", command=self.SaveSpatialFcnConfigFName)
        self.saveSpatFncConfigButton.grid(row=2, column=1)
        # --------------------------------------------------------------------------------------------------------
        paramFrame.grid(row=0, column=0, sticky='n')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.paramMAFrame= ttk.LabelFrame(scrollFrame.viewPort, text='MA Parameters', style='UKInterpolation.TFrame')
        # --------------------------------------------------------------------------------------------------------
        self.formMALabel = ttk.Label(self.paramMAFrame, text='MA Variogram Form')
        self.formMALabel.grid(row=0, column=0)
        # --------------------------------------------------------------------------------------------------------
        self.formMACombo = ttk.Combobox(self.paramMAFrame, values=formComboList, width=14)
        self.formMACombo.current(0)
        self.formMACombo.grid(row=0, column=1, pady=5)
        # --------------------------------------------------------------------------------------------------------
        self.spatMACfgFile  = SubFrameElement(self, self.paramMAFrame, 'MA Spatial Fcn File', 'SpatialFcnsMA.cfg', 1, 0, 1, width=20)
        # --------------------------------------------------------------------------------------------------------
        self.style.configure("Frame4.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("Frame4A.TLabel", padding=6, relief='raised', background="#0FF")
        self.openMASpatFncConfigButton = ttk.Button(self.paramMAFrame, text='Load MA Fcn File', style="Frame4.TLabel", command=self.GetMASpatialFcnConfigFName)
        self.openMASpatFncConfigButton.grid(row=2, column=0)
        # --------------------------------------------------------------------------------------------------------
        self.saveMASpatFncConfigButton = ttk.Button(self.paramMAFrame, text='Save MA Fcn File', style="Frame4A.TLabel", command=self.SaveMASpatialFcnConfigFName)
        self.saveMASpatFncConfigButton.grid(row=2, column=1)
        # --------------------------------------------------------------------------------------------------------
        self.paramMAFrame.grid(row=1, column=0, sticky='n')
        # This window is only needed with domain AL
        if not self.parent.frame1.domainNameCombo.get() == 'AL':
            self.paramMAFrame.grid_remove()
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.paramGBFrame= ttk.LabelFrame(scrollFrame.viewPort, text='GB Parameters', style='UKInterpolation.TFrame')
        # --------------------------------------------------------------------------------------------------------
        self.formGBLabel = ttk.Label(self.paramGBFrame, text='GB Variogram Form')
        self.formGBLabel.grid(row=0, column=0)
        # --------------------------------------------------------------------------------------------------------
        self.formGBCombo = ttk.Combobox(self.paramGBFrame, values=formComboList, width=14)
        self.formGBCombo.current(0)
        self.formGBCombo.grid(row=0, column=1, pady=5)
        # --------------------------------------------------------------------------------------------------------
        self.spatGBCfgFile  = SubFrameElement(self, self.paramGBFrame, 'GB Spatial Fcn File', 'SpatialFcnsGB.cfg', 1, 0, 1, width=20)
        # --------------------------------------------------------------------------------------------------------
        self.style.configure("Frame4.TLabel", padding=6, relief='raised', background="#0F0")
        self.style.configure("Frame4A.TLabel", padding=6, relief='raised', background="#0FF")
        self.openGBSpatFncConfigButton = ttk.Button(self.paramGBFrame, text='Load GB Fcn File', style="Frame4.TLabel", command=self.GetGBSpatialFcnConfigFName)
        self.openGBSpatFncConfigButton.grid(row=2, column=0)
        # --------------------------------------------------------------------------------------------------------
        self.saveGBSpatFncConfigButton = ttk.Button(self.paramGBFrame, text='Save GB Fcn File', style="Frame4A.TLabel", command=self.SaveGBSpatialFcnConfigFName)
        self.saveGBSpatFncConfigButton.grid(row=2, column=1)
        # --------------------------------------------------------------------------------------------------------
        self.paramGBFrame.grid(row=2, column=0, sticky='n')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # This window is only needed with domain AL
        if not self.parent.frame1.domainNameCombo.get() == 'AL':
            self.paramGBFrame.grid_remove()
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        scrollFrame.grid(row=2, column=0, sticky='nsew')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.style.configure("Help.TLabel", padding=6, relief="flat", foreground='white', background="#5783db")
        helpButton = ttk.Button(self, text= "UK Interpolation Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=0)

        self.bind("<Visibility>", self.on_visibility)

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Spatial Function Cfg File
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def SaveSpatialFcnConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.spatCfgFile.myEntry.get())
            self.spatCfgFile.myEntry.delete(0,n)
            self.spatCfgFile.myEntry.insert(0,f[-1])
            self.parent.WriteSpatialFncsConfig()

    def SaveMASpatialFcnConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.spatMACfgFile.myEntry.get())
            self.spatMACfgFile.myEntry.delete(0,n)
            self.spatMACfgFile.myEntry.insert(0,f[-1])
            self.parent.WriteSpatialFncsConfig(file_path)
        
        # Save MA Unique UK Config file
        cfgFile  = os.path.join(self.root,'Configuration', 'Interpolation', 'UK_MA.cfg')
        with open(cfgFile, 'w') as f:
            f.write('# Set inputs for universal kriging\n')
            f.write('Kriging variogram form = '+self.formMACombo.get()+'\n')
            f.write('#\n')
            f.write('# Configuration files are expected to be in the Configuration directory\n')
            f.write('#\n')
            f.write('NLS Spatial Fcn File Name = '+self.spatMACfgFile.myEntry.get()+'\n')
            f.write('#\n')
            f.write('# Save interim data by writing out supporting data files\n')
            f.write('#\n')
            f.write('Save Data = F\n')
            f.close()


    def SaveGBSpatialFcnConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.spatGBCfgFile.myEntry.get())
            self.spatGBCfgFile.myEntry.delete(0,n)
            self.spatGBCfgFile.myEntry.insert(0,f[-1])
            self.parent.WriteSpatialFncsConfig(file_path)
        
        # Save GB Unique UK Config file
        cfgFile  = os.path.join(self.root,'Configuration', 'Interpolation', 'UK_GB.cfg')
        with open(cfgFile, 'w') as f:
            f.write('# Set inputs for universal kriging\n')
            f.write('Kriging variogram form = '+self.formGBCombo.get()+'\n')
            f.write('#\n')
            f.write('# Configuration files are expected to be in the Configuration directory\n')
            f.write('#\n')
            f.write('NLS Spatial Fcn File Name = '+self.spatGBCfgFile.myEntry.get()+'\n')
            f.write('#\n')
            f.write('# Save interim data by writing out supporting data files\n')
            f.write('#\n')
            f.write('Save Data = F\n')
            f.close()

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method askopenfilename to name a file to be used for the Spatial Function Cfg File
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def GetSpatialFcnConfigFName(self):
        fName = filedialog.askopenfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = fName.split('/')
        if fName:
            n = len(self.spatCfgFile.myEntry.get())
            self.spatCfgFile.myEntry.delete(0,n)
            self.spatCfgFile.myEntry.insert(0,f[-1])
            self.ReadSpactialFunctionFile(fName)

    def GetMASpatialFcnConfigFName(self):
        fName = filedialog.askopenfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = fName.split('/')
        if fName:
            n = len(self.spatMACfgFile.myEntry.get())
            self.spatMACfgFile.myEntry.delete(0,n)
            self.spatMACfgFile.myEntry.insert(0,f[-1])
            self.ReadSpactialFunctionFile(fName)

    def GetGBSpatialFcnConfigFName(self):
        fName = filedialog.askopenfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = fName.split('/')
        if fName:
            n = len(self.spatGBCfgFile.myEntry.get())
            self.spatGBCfgFile.myEntry.delete(0,n)
            self.spatGBCfgFile.myEntry.insert(0,f[-1])
            self.ReadSpactialFunctionFile(fName)
    #--------------------------------------------------------------------------------------------------
    ## 
    # Read in the Spactial Function File. 
    # @param fName Name of the file to read, assumes it is valid.
    # It then writes parameters to the widgets.
    #
    #--------------------------------------------------------------------------------------------------
    def ReadSpactialFunctionFile(self, fName):
        correctDimStr =['x', 'y', 'z']
        correctShapeStr = ['Gaussian', 'Logistic', 'SinExp', 'CosExp']
        nsf = 0

        with open(fName, 'r') as f:
            while True:
                if (nsf >= self.nsfMax):
                    messagebox.showerror("Reading Spatial Function File", f'Max reached {self.nsfMax}\nStopping at {nsf}')
                    break

                # read longitude values
                inputStr = f.readline()
                if not inputStr:
                    f.close()
                    break

                if inputStr[0] == '#':
                    continue

                inputArr = [s.strip() for s in inputStr.split(',')]
                if (inputArr[0][0]) == 'F':

                    # set dim
                    token =  [s.strip() for s in inputArr[1].split('=')]
                    tag = token[0].strip()
                    val = token[1].strip()
                    if tag == 'dim':
                        if val not in correctDimStr:
                            messagebox.showerror("Reading Spatial Function File", 
                                                    f'Unknown string for dim value {val}\nSkipping Function Defintion')
                            continue
                        else:
                            self.functions[nsf].dimVal.set(val)
                    else:
                        messagebox.showerror("Reading Spatial Function File", 
                                                f'Unexpected string for dim tag {tag}\nSkipping Function Defintion')
                        continue


                    # set shape
                    token =  [s.strip() for s in inputArr[2].split('=')]
                    tag = token[0].strip()
                    val = token[1].strip()
                    if tag == 'shape':
                        if val not in correctShapeStr:
                            messagebox.showerror("Reading Spatial Function File", 
                                                    f'Unknown string for shape value {val}\nSkipping Function Defintion')
                            continue
                        else:
                            self.functions[nsf].shapeVal.set(val)
                    else:
                        messagebox.showerror("Reading Spatial Function File", 
                                                f'Unexpected string for shape tag {tag}\nSkipping Function Defintion')
                        continue

                    # set precon
                    token =  [s.strip() for s in inputArr[3].split('=')]
                    tag = token[0].strip()
                    val = token[1].strip()
                    # first remove any inline comment
                    k = val.find('#')
                    if k>0:
                        val = val[0:k].strip()
                    if tag == 'precon':
                        if not val.isdigit():
                            messagebox.showerror("Reading Spatial Function File", 
                                                    f'Unknown string for precon value {val}\nSkipping Function Defintion')
                            continue
                        else:
                            self.functions[nsf].preconEntry.delete(0,3)
                            self.functions[nsf].preconEntry.insert(0, val)
                    else:
                        messagebox.showerror("Reading Spatial Function File", 
                                                f'Unexpected string for precon tag {tag}\nSkipping Function Defintion')
                        continue

                    nsf += 1
            f.close()

        self.nsf = nsf
        self.numFcnsEntry.delete(0,2)
        self.numFcnsEntry.insert(0, str(nsf))
        self.NumFuncsUpdate()


    #--------------------------------------------------------------------------------------------------
    ## 
    # This method is used to update widgets each time the user switches to this tab
    #
    #--------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
        if self.okToRepaintFunctions:
            self.domainName = self.friend.domainNameCombo.get()
            self.numFcnsEntry.delete(0,2)
            if self.domainName == 'AL':
                self.nsf = 11
            elif self.domainName == 'MA':
                self.nsf = 9
            else:
                self.nsf = 5
            self.numFcnsEntry.insert(0, str(self.nsf))

            # Now update desired funtion definitions
            for i in range(self.nsfMax):
                self.functions[i].funcFrame.grid_remove()
            for i in range(self.nsf):
                self.functions[i].funcFrame.grid()

        # This window is only needed with domain AL
        if not self.parent.frame1.domainNameCombo.get() == 'AL':
            self.paramMAFrame.grid_remove()
            self.paramGBFrame.grid_remove()
        else:
            self.paramMAFrame.grid()
            self.paramGBFrame.grid()

    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    def EnterKeyClicked(self, event):
        self.NumFuncsUpdate()

    #--------------------------------------------------------------------------------------------------
    ##
    # This method updates the number of spatial functions. Overrides default value for MA and GB
    #
    #--------------------------------------------------------------------------------------------------
    def NumFuncsUpdate(self):
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

    #-------------------------------------------------------------------------------------
    ## Help Window for Sort By Area
    #-------------------------------------------------------------------------------------
    def pop_up(self):
        about = '''Universal Krigin Interpolation
    (This frame is scrollable, use mouse wheel)
    This frame allows the user to customize how the interpolation is performed.

Parameters

Variogram Form: 
    This defines the shape of the variogram models. The kriging refernces 
    identify a typical variogram shape. It is a positive sloped function with a
    y intercept defined as nugget. The assymptote is defined as the sill. The 
    inflection point at the sill is defined as the range. Four shapes are 
    implemented for UK interpolation
    •	spherical
    •	exponential
    •	gaussian
    •	matern

    spherical:   
    γ(h) = nugget + sill * (3h/2 * range) - 0.5 * (h/range)^3   0 < h <= range
         = nugget + sill                                        h > range
         = 0                                                    h = 0

    exponential:
    γ(h) = nugget + sill * (1 - exp(-h/range))                  h > 0
         = 0                                                    , & h = 0

    gaussian
    γ(h) = nugget + sill * (1 - exp(-(h/range)^2))              h > 0
         = 0                                                    h = 0 
    
    matern
    γ(h) = nugget + sill 
         * (1 - {sqrt(2)/Γ(0.5)} * Jn(2, h/range) * sqrt(h/range))  h > 0
         = 0                                                        h = 0
    where Jn is the Bessel function of the first kind

Spatial Fcn Config File:
    This is the name of the file used to hold the spatial function definitions.
    It is saved in the UK Configuration file.

    SPECIAL NOTE: When processing AL domain, two additional boxes will appear

    MA Parameters:
        MA Spatial Fcn: 'SpatialFcnsMA.cfg' 
        Is used to interpolate data points within the MA region.
        The user can load this file, change the setting, and then save to a
        different file to preserve installed file.

    GB Parameters:
        GB Spatial Fcn: 'SpatialFcnsGB.cfg' 
        Is used to interpolate data points within the GB region.
        The user can load this file, change the setting, and then save to a
        different file to preserve installed file.

    UK_MA.cfg, UK_GB.cfg:
        These file names are not configurable. The GUI will save the config
        parameters for MA and GB to these files, respectively. 
        The interpolation scripts will then use these files when processing AL.

Load/Save Spat Fcn File:
    Used to load predefined spatial functions and save user defined spatial
    functions. respectively.

    Note: If MA is selected in the Main Tab, then the Spatial Functions are 
    populated with MA defaults. If GB is selected then similarly loaded with
    GB defaults. Once # of Functions is changed then this no longer occurs.

Spatial Functions
    Define non linear spatial functions and paramater search range.

    - Function 1, dim=z, shape=Logistic, precon=0 
    - Function 2, dim=z, shape=Gaussian, precon=0 
    - Function 3, dim=x, shape=Logistic, precon=1 

    These define spatial functions for setting the spatial trend in the 
    universal kriging algorithm. 

    The precon=0 term means that the function is not multiplied by another 
    function. For example,
       Function 3, dim=x, shape=Logistic, precon=1
    indicates that the third function is multiplied by the first function.  
    This is true for fitting the nonlinear parameters of function 3 hence 
    the parameters of function 1 must be fit before the parameters of 
    function 3.

    # of Functions: This is limited to 20.

    Update: Use Enter Key or click this button to populate the function entries.

Function N
    These are the Function definitions as explained above

    Dim
       - x: computations along longitude paramater
       - y: computations along latitude paramater
       - z: computations along depth paramater

    Shape
       Let vector{A} = Vector{dim} - f0
       Gaussian:  exp( -( Vector{A} / λ )^2 )
       Logistic: 1 / ( 1 + exp( - Vector{A}/λ) )
       SinExp: sin(Vector{A}/λ) * exp(-(Vector{A}/λ )^2)
       CosExp: cos(Vector{A}/λ) * exp(-(Vector{A}/(2λ) )^2)

       f0 is used for a linear interpolation and is approximately
       λ_max = max(Vector{A}) - min(Vector{A})
       λ_min = 5000 for x and y, 5 for z
       λ = λ_min + (λ_max - λ_min) * Linear Function
       f0_min = min(Vector{A})
       f0_max = max(Vector{A})
       f0 = f0_min + (f0_max - f0_min) * Linear Function

    Precon
       Precon value determines order of computation. See above.
'''
        #about = re.sub("\n\s*", "\n", about) # remove leading whitespace from each line
        popup = tk.Toplevel()
        nrows = 35
        ncols = 80
        parentPosn = '+'+str(self.winfo_rootx()+700)+'+'+str(self.winfo_rooty()+50)
        popup.geometry(str(int(ncols*8.5))+"x"+str(nrows*18)+parentPosn)
        T = tk.Text(popup, width=ncols, height=nrows, padx=10)
        T.insert('end', about)
        T.config(state='disabled')
        T.grid()
        btn = tk.Button(popup, text ="Close", command= popup.destroy)
        btn.grid(row =1)
