#======================================================================================================
## @page page3 Growth Frame
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
# These parameters are used to compute the scallop selectivity as a function of its length.
# @f$ selectivity = 1 / ( 1 + exp( select_a - select_b * (l_{shell} + 2.5))) @f$
# @subsection    p4p2p1 FSelectA
# @subsubsection p4p2p1p1 Value to use for MA
# @subsubsection p4p2p1p2 Value to use for GB Closed
# @subsubsection p4p2p1p3 Value to use for GB Open
# @subsection    p4p2p2 FSelectB
# @subsubsection p4p2p2p1 Value to use for MA
# @subsubsection p4p2p2p2 Value to use for GB Closed
# @subsubsection p4p2p2p3 Value to use for GB 
#
# @section    p4p3 Computing Landings Per Unit Effort, LPUE
# The simulation uses the following parameters to compute LPUE
#
# @f[ W_{expl} = \frac{EBMS}{N_{scallops}} \text{, weight in grams} @f]
#
# @f[ EBMS_{tow} = EBMS * Tow_{sqm} \text{, biomass in grams}  @f]
#
# @f[ slope_1 = lpue_{slope} * EBMS_{tow} + lpue_{intercept}  @f]
#
# @f[ slope_2 = LPUE_{slope_2} * EBMS_{tow}  @f]
#
# @f[ LPUE_{limit} = max_{per_{day}} * W_{expl} / 453.592 @f]
# 
# @f[ LPUE = min(slope_1, slope_2, LPUE_{limit})  @f]
#
# @subsection p4p3p1 LPUE Slope
#
# @subsection p4p3p2 LPUE Slope2
#
# @subsection p4p3p3 LPUE Intercept
#
# @subsection p4p3p4 Maximum Number of Scallops Shucked Per Day
#
# @subsection p4p3p5 Maximum Number of Hours Dredging Per Day
#
# @subsection p4p3p6 Dredge Width in meters
#
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
# This class allows the user to adjust parameters used in computing scallop growth
#
#===============================================================================================================
class Growth(ttk.Frame, MainApplication):
    ##
    # Constructor for Growth Class
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
        self.style.configure('Growth.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Growth.TFrame.Label', font=('courier', 10, 'bold'))
        fishingFrame= ttk.LabelFrame(self, text='Mortality', style='Growth.TFrame')
        self.fishMort   = SubFrameElement(self, fishingFrame, 'Fishing Mortality', self.fmortStr,       0, 0, 1)
        #self.fishSelect = SubFrameElement(self, fishingFrame, 'Fishing Effort\n(USD, BMS, or CAS)', self.fishStr, row, 0, 1)
        self.alphaMort  = SubFrameElement(self, fishingFrame, 'Alpha Mortality',self.alphStr,           1, 0, 1)
        self.maAdultMort= SubFrameElement(self, fishingFrame, 'MA Adult Mortality',self.maAdultMortStr, 2, 0, 1)
        self.gbAdultMort= SubFrameElement(self, fishingFrame, 'GB Adult Mortality',self.gbAdultMortStr, 3, 0, 1)
        self.maLength0  = SubFrameElement(self, fishingFrame, 'MA Length_0',  self.maLen0Str,           4, 0, 1)
        self.gbLength0  = SubFrameElement(self, fishingFrame, 'GB Length_0',  self.gbLen0Str,           5, 0, 1)
        fishingFrame.grid(row=0, column=0, padx=10, sticky='n')

        selectivityFrame = ttk.LabelFrame(self, text='Selectivity', style='Growth.TFrame')
        self.maFSelectA       = SubFrameElement(self, selectivityFrame, 'MA FSelectA', self.maFSelAStr,          0, 0, 1, width=10)
        self.maFSelectB       = SubFrameElement(self, selectivityFrame, 'MA FSelectB', self.maFSelBStr,          1, 0, 1, width=10)
        self.gbClosedFSelectA = SubFrameElement(self, selectivityFrame, 'GB Closed FSelectA', self.gbClFSelAStr, 2, 0, 1, width=10)
        self.gbClosedFSelectB = SubFrameElement(self, selectivityFrame, 'GB Closed FSelectB', self.gbClFSelBStr, 3, 0, 1, width=10)
        self.gbOpenFSelectA   = SubFrameElement(self, selectivityFrame, 'GB Open FSelectA', self.gbOpFSelAStr,   4, 0, 1, width=10)
        self.gbOpenFSelectB   = SubFrameElement(self, selectivityFrame, 'GB Open FSelectB', self.gbOpFSelBStr,   5, 0, 1, width=10)
        selectivityFrame.grid(row=0, column=1, padx=10, sticky='n')
        
        lpueFrame   = ttk.LabelFrame(self, text='LPUE', style='Growth.TFrame')
        self.lpueSlope   = SubFrameElement(self, lpueFrame, 'LPUE Slope', self.lpueSlStr,       0, 0, 1, width=10)
        self.lpueSlope2  = SubFrameElement(self, lpueFrame, 'LPUE Slope2', self.lpueSl2Str,     1, 0, 1, width=10)
        self.lpueIntcept = SubFrameElement(self, lpueFrame, 'LPUE Intercept', self.lpueIntcStr, 2, 0, 1, width=10)
        self.maxPerDay   = SubFrameElement(self, lpueFrame, 'Max Per Day', self.maxPerDayStr,   3, 0, 1, width=10)
        self.maxTime     = SubFrameElement(self, lpueFrame, 'Max Time', self.maxTimeStr,        4, 0, 1, width=10)
        self.dredgeWth   = SubFrameElement(self, lpueFrame, 'Dredge Width', self.dredgeWdStr,   5, 0, 1, width=10)
        self.towSpeed    = SubFrameElement(self, lpueFrame, 'Towing Speed', self.towSpdStr,     6, 0, 1, width=10)
        lpueFrame.grid(row=0, column=2, padx=10, sticky='n')

        incidentalFrame = ttk.LabelFrame(self, text='Incidental', style='Growth.TFrame')
        self.maIncident = SubFrameElement(self, incidentalFrame, 'MA Incidental', self.maIncidStr, 0, 0, 1)
        self.gbIncident = SubFrameElement(self, incidentalFrame, 'GB Incidental', self.gbIncidStr, 1, 0, 1)
        incidentalFrame.grid(row=1, column=0, padx=10)

        discardFrame   = ttk.LabelFrame(self, text='Discard', style='Growth.TFrame')
        self.maCullSize = SubFrameElement(self, discardFrame, 'MA Cull Size', self.maCullStr, 0, 0, 1)
        self.maDiscard  = SubFrameElement(self, discardFrame, 'MA Discard',   self.maDiscStr, 1, 0, 1)
        self.gbCullSize = SubFrameElement(self, discardFrame, 'GB Cull Size', self.gbCullStr, 0, 2, 3)
        self.gbDiscard  = SubFrameElement(self, discardFrame, 'GB Discard',   self.gbDiscStr, 1, 2, 3)
        discardFrame.grid(row=1, column=1, columnspan=2, padx=10)
        #-------------------------------------------------------------------------------------------
        self.style.configure("Help.TLabel", padding=6, relief="flat", foreground='white', background="#5783db")
        helpButton = ttk.Button(self, text= "Growth Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=2, column=1)

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

    #-------------------------------------------------------------------------------------
    ## 
    #-------------------------------------------------------------------------------------
    def pop_up(self):
        about = '''Mortality
    Define parameters to compute fishing mortality
    - Fishing Mortality: This is the default fishing mortality
    - Alpha Mortality: Uses as an exponent to set fishing mortality 
      proportional to LPUE
    - Adult Mortality and Length_0: Used to compute Natural Mortality
        ALPHA = 1 - 1 / ( 1 + exp( - (shell_length - length_0) /10._dp ) )
        NATURAL MORT = ALPHA * JuvenileMort + (1 - ALPHA) * AdultMort

Selectivity
    Region dependent values to compute Ring Size Selectivity:
    RingSizeSelectivity = 1 / 
        ( 1 + exp( select_a - select_b * (shell_length + 5/2)) )
        where 5 is the steps between shell lenghts, or 5 mm.

LPUE
    Parameters used to compute Landings per Unit Effort 
    - LPUE Slope, LPUE Slope2, LPUE Intercept, Max Per Day
        slope1 = LPUE slope * EBMS + LPUE Intercept, 
                where EBMS is the biomass in tow area in grams
        slope2 = LPUE slope2 * EBMS
        LPUE Limit = Max [shucking] Per Day * EBMS_lbs
        LPUE = min(slope1, slope2, LPUE Limit)
    - Max Time: ancillary computation to determine Dredge Time in hours
    - Dredge Width and Towing Speed used to determine the dredge area to 
      convert biomass density to grams.

Discard
    - Cull Size and Discard: Determines SetDiscard Value
      If Shell Length > Cull Size then SetDiscard = 0
                                  else SetDiscard = discard * selectivity

Incidental
    Used to compute overall mortality, M
    M = NaturalMortality 
        + FishingEffort * (Selectivity + Incidental + SetDiscard)
'''
        #about = re.sub("\n\s*", "\n", about) # remove leading whitespace from each line
        popup = tk.Toplevel()
        nrows = 37
        ncols = 80
        popup.geometry(str(int(ncols*8.5))+"x"+str(nrows*18))
        T = tk.Text(popup, width=ncols, height=nrows, padx=10)
        T.insert('end', about)
        T.config(state='disabled')
        T.grid()
        btn = tk.Button(popup, text ="Close", command= popup.destroy)
        btn.grid(row =1)
