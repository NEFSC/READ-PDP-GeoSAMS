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
        self.root = os.environ['ROOT']
        self.startDir = os.path.join(self.root, 'Configuration')
        self.friend = friend
        self.okToRepaintFunctions = True
        self.nsfMax = 20
        self.functions = [None for _ in range(self.nsfMax)]
        self.domainName = self.friend.domainNameCombo.get()
        if self.domainName == 'MA':
            self.nsf = 9
        else:
            self.nsf = 5
        self.style = ttk.Style()
        self.style.configure('UKInterpolation.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('UKInterpolation.TFrame.Label', font=('courier', 10, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.

        # --------------------------------------------------------------------------------------------------------
        self.funcFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Spatial Functions', style='UKInterpolation.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFncsLabel = ttk.Label(self.funcFrame, text='# of Functions')
        self.numFncsLabel.grid(row=0, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFcnsEntry=ttk.Entry(self.funcFrame, validatecommand=numbersCallback, width=5)
        self.numFcnsEntry.insert(0, str(self.nsf))
        self.numFcnsEntry.grid(row=0, column=0, sticky='e')
        reg=self.numFcnsEntry.register(numbersCallback)
        self.numFcnsEntry.configure(validate='key', validatecommand=(reg, '%P'))
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
        paramFrame= ttk.LabelFrame(self.scrollFrame.viewPort, text='Parameters', style='UKInterpolation.TFrame')
        self.highLimit   = SubFrameElement(self, paramFrame, 'High Limit Factor ', '1.5',  0, 0, 1)

        formComboList = ['spherical', 'exponential', 'gaussian', 'matern']
        self.formLabel = ttk.Label(paramFrame, text='Variogram Form')
        self.formLabel.grid(row=1, column=0)
        self.formCombo = ttk.Combobox(paramFrame, values=formComboList, width=10)
        self.formCombo.current(0)
        self.formCombo.grid(row=1, column=1, pady=5)

        self.useLogTransLabel = ttk.Label(paramFrame, text='Use Log Transfrom)')
        self.useLogTransLabel.grid(row=2, column=0)
        self.useLogTransCombo = ttk.Combobox(paramFrame, width=3, values=['T', 'F'])
        self.useLogTransCombo.current(0)
        self.useLogTransCombo.grid(row=2, column=1, pady=5)

        self.powerTrans  = SubFrameElement(self, paramFrame, 'Power Tranform\n(Not used if Log = T)', '1.0', 3, 0, 1)
        self.spatCfgFile  = SubFrameElement(self, paramFrame, 'Spatial Fcn Config File', 'SpatialFcns.cfg', 4, 0, 1, width=20)

        self.style.configure("Frame4.TLabel", padding=6, relief='raised', background="#0FF")
        self.openMortConfigButton = ttk.Button(paramFrame, text='Change/Save Spat Fcn File', style="Frame4.TLabel", command=self.GetSpatialFcnConfigFName)
        self.openMortConfigButton.grid(row=5, column=0)

        paramFrame.grid(row=0, column=4, sticky='n')
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')

        self.bind("<Visibility>", self.on_visibility)

    #--------------------------------------------------------------------------------------------------
    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Spatial Function Cfg File
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    #--------------------------------------------------------------------------------------------------
    def GetSpatialFcnConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Configuration File", filetypes=[("CFG files", "*.cfg")], defaultextension='cfg', initialdir=self.startDir)
        f = file_path.split('/')
        if file_path:
            n = len(self.spatCfgFile.myEntry.get())
            self.spatCfgFile.myEntry.delete(0,n)
            self.spatCfgFile.myEntry.insert(0,f[-1])
            self.parent.WriteSpatialFncsConfig()
    #--------------------------------------------------------------------------------------------------
    ## 
    # This method is used to update widgets each time the user switches to this tab
    #
    #--------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
            if self.okToRepaintFunctions:
                self.domainName = self.friend.domainNameCombo.get()
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
