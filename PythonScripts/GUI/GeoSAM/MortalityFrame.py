#======================================================================================================
## @page page3 Mortality Frame
# Allows the user to modify parameters that are used to define mortality computations
#
# @section p4p1 Mortality
# @subsection p4p1p1 Fishing Mortality
# This is the default fishing mortality lacking any other definition
#
# @subsection p4p1p2 Alpha Mortality
#  So for open areas, an overall fishing mortality F_avg would be specified and then F at each location
#  would be computed so that:\n
#   -# The weighted (by exploitable numbers) average F over all locations is equal to F_avg and 
#   -# F at each location is proportional to LPUE^alpha_mort.\n
#  This would also apply to special access areas, but each one would have their own specified F, 
#  and the average would only be for those points within that access area.
# f_avg = dot_product(expl_num, F_mort_raw) / sum(expl_num)
#
# @subsection p4p1p3 Adult Mortality
# There are two values here, one associated with the Mid-Atlantic and the other associated with Georges-Bank
# @subsubsection p4p1p3p1 Adult mortality for Mid-Atlantic
# @subsubsection p4p1p3p2 Adult Mortality for Georges Bank
#
# @subsection p4p1p4 Length 0
# Length_0 is used in computing Natural Mortality
# @f[
# \vec{alpha} = 1 - \frac{1}{1 + exp\left(-(\vec{length_{shell}} - length_0)/10 \right))}
# @f]
#
# Then natural mortality is computed from juvenile natural mortality and adult natural mortality as
# @f[
# \vec{mort_{nat}} = \vec{alpha} * mort_{nat_{juv}} + \left(1 - \vec{alpha} \right) * mort_{nat_{adult}}
# @f]
# 
# @subsubsection p4p1p4p1 Length_0 for Mid-Atlantic
# @subsubsection p4p1p4p2 Length_0 for Georges Bank
#
# @section       p4p2 Selectivity
# @subsection    p4p2p1 FSelectA
# @subsubsection p4p2p1p1 MA
# @subsubsection p4p2p1p2 GB Closed
# @subsubsection p4p2p1p3 GB Open
# @subsection    p4p2p2 FSelectB
# @subsubsection p4p2p2p1 MA
# @subsubsection p4p2p2p2 GB Closed
# @subsubsection p4p2p2p3 GB 
#
# @section    p4p3 Computing Landing Per Unit Effor, LPUE
# @subsection p4p3p1 LPUE Slope
# @subsection p4p3p2 LPUE Slope2
# @subsection p4p3p3 LPUE Intercept
# @subsection p4p3p4 Maximum Number of Scallops Shucked Per Day
# @subsection p4p3p5 Maximum Number of Hours Dredging Per Day
# @subsection p4p3p6 Dredge Width in meters
# @subsection p4p3p7 Towing Speed in knots
#
# @section    p4p4 Incidental
# @subsection p4p4p1 MA
# @subsection p4p4p2 GB
#
# @section    p4p5 Discard
# @subsection p4p5p1 Cull Size
# @subsubsection p4p5p1p1 MA
# @subsubsection p4p5p1p2 GB
# @subsection p4p5p2 Discard
# @subsubsection p4p5p2p1 MA
# @subsubsection p4p5p2p2 GB
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
from GeoSams import MainApplication

#===============================================================================================================
##
# This class allows the user to adjust parameters used in computing fishing mortality
#
#===============================================================================================================
class Mortality(ttk.Frame, MainApplication):
    ##
    # Constructor for Mortality Class
    #
    def __init__(self, container, fName):
        super().__init__()
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

        self.UpdateValues(fName)

        self.style = ttk.Style()
        self.style.configure('Mortality.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Mortality.TFrame.Label', font=('courier', 8, 'bold'))
        fishingFrame= ttk.LabelFrame(self, text='Mortality', style='Mortality.TFrame')
        row = 0
        self.fishMort   = SubFrameElement(self, fishingFrame, 'Fishing Mortality', self.fmortStr,            row, 0, 1)
        row += 1
        #self.fishSelect = SubFrameElement(self, fishingFrame, 'Fishing Effort\n(USD, BMS, or CAS)', self.fishStr, row, 0, 1)
        #row += 1
        self.alphaMort  = SubFrameElement(self, fishingFrame, 'Alpha Mortality',self.alphStr,           row, 0, 1)
        row += 1
        self.maAdultMort= SubFrameElement(self, fishingFrame, 'MA Adult Mortality',self.maAdultMortStr, row, 0, 1)
        row += 1
        self.gbAdultMort= SubFrameElement(self, fishingFrame, 'GB Adult Mortality',self.gbAdultMortStr, row, 0, 1)
        row += 1
        self.maLength0  = SubFrameElement(self, fishingFrame, 'MA Length_0',  self.maLen0Str,        row, 0, 1)
        row += 1
        self.gbLength0  = SubFrameElement(self, fishingFrame, 'GB Length_0',  self.gbLen0Str,        row, 0, 1)
        row += 1
        fishingFrame.grid(row=0, column=0, padx=10, sticky='n')

        selectivityFrame = ttk.LabelFrame(self, text='Selectivity', style='Mortality.TFrame')
        row = 0
        self.maFSelectA       = SubFrameElement(self, selectivityFrame, 'MA FSelectA', self.maFSelAStr,          row, 0, 1)
        row += 1
        self.maFSelectB       = SubFrameElement(self, selectivityFrame, 'MA FSelectB', self.maFSelBStr,          row, 0, 1)
        row += 1
        self.gbClosedFSelectA = SubFrameElement(self, selectivityFrame, 'GB Closed FSelectA', self.gbClFSelAStr, row, 0, 1)
        row += 1
        self.gbClosedFSelectB = SubFrameElement(self, selectivityFrame, 'GB Closed FSelectB', self.gbClFSelBStr, row, 0, 1)
        row += 1
        self.gbOpenFSelectA   = SubFrameElement(self, selectivityFrame, 'GB Open FSelectA', self.gbOpFSelAStr,   row, 0, 1)
        row += 1
        self.gbOpenFSelectB   = SubFrameElement(self, selectivityFrame, 'GB Open FSelectB', self.gbOpFSelBStr,   row, 0, 1)
        row += 1
        selectivityFrame.grid(row=0, column=1, padx=10, sticky='n')
        
        lpueFrame   = ttk.LabelFrame(self, text='LPUE', style='Mortality.TFrame')
        row = 0
        self.lpueSlope   = SubFrameElement(self, lpueFrame, 'LPUE Slope', self.lpueSlStr,       row, 0, 1)
        row += 1
        self.lpueSlope2  = SubFrameElement(self, lpueFrame, 'LPUE Slope2', self.lpueSl2Str,     row, 0, 1)
        row += 1
        self.lpueIntcept = SubFrameElement(self, lpueFrame, 'LPUE Intercept', self.lpueIntcStr, row, 0, 1)
        row += 1
        self.maxPerDay   = SubFrameElement(self, lpueFrame, 'Max Per Day', self.maxPerDayStr,   row, 0, 1)
        row += 1
        self.maxTime     = SubFrameElement(self, lpueFrame, 'Max Time', self.maxTimeStr,        row, 0, 1)
        row += 1
        self.dredgeWth   = SubFrameElement(self, lpueFrame, 'Dredge Width', self.dredgeWdStr,   row, 0, 1)
        row += 1
        self.towSpeed    = SubFrameElement(self, lpueFrame, 'Towing Speed', self.towSpdStr,     row, 0, 1)
        row += 1
        lpueFrame.grid(row=0, column=2, padx=10, sticky='n')

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

    #--------------------------------------------------------------------------------------------------
    ##
    # Method to read Mortality Configuration file and set values accordingly
    #
    #--------------------------------------------------------------------------------------------------
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
