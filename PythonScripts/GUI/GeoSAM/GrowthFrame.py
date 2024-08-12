## @page page3 Growth Frame
# Allows the user to modify parameters that are used to define mortality computations
#
# @section p4p1 Mortality
# @subsection p4p1p1 Fishing Mortality
# This is the default fishing mortality in lieu of any other definition
#
# @subsection p4p1p2 Alpha Mortality
#  So for open areas, an overall fishing mortality @f$F_{avg}@f$ would be specified and then @f$F@f$ at each location
#  would be computed so that:\n
#   -# The weighted average (by exploitable numbers), F, over all locations is equal to @f$F_{avg}@f$ and 
#   -# @f$F@f$ at each location is proportional to @f$LPUE^{alpha_{mort}}@f$.\n
#
#  This would also apply to special access areas, but each one would have their own specified F, 
#  and the average would only be for those points within that access area.
#
# @f$ f_{avg} = \frac{\vec{scallops_{num}} \cdot \vec{F_{mort_{raw}}}}{sum(\vec{scallops_{num})}} @f$
#
# @subsection p4p1p3 Adult Mortality
#
# <table>
# <caption id>Mortality</caption>
# <tr><th> <th>Adult<th>Length<SUB>0</SUB>
# <tr><th>MA<td>0.25<td>65.0
# <tr><th>GB<td>0.2<td>70.0
# </table>
# 
# @subsection p4p1p4 Computing Alpha
# Alpha is based on the lengths of the shell normalized by length<SUB>0</SUB>
# @f[
# \vec{alpha} = 1 - \frac{1}{1 + exp\left(-(\vec{length_{shell}} - length_0)/10 \right))}
# @f]
#
# @subsection p4p1p5 Computing Natural Mortality
# Then natural mortality is computed from juvenile natural mortality and adult natural mortality as
# @f[
# \vec{mort_{nat}} = \vec{alpha} * mort_{nat_{juv}} + \left(1 - \vec{alpha} \right) * mort_{nat_{adult}}
# @f]
# 
# @section p4p2 Selectivity
# These parameters are used to compute the scallop selectivity as a function of its length. MA and GB have
# respective values for each term. GB will also distinguish between open and closed areas.
# @f$ selectivity = 1 / ( 1 + exp( select_a - select_b * (l_{shell} + 2.5))) @f$
#
# <table>
# <caption id>Selectivity</caption>
# <tr><th> <th>MA<th>GB Open<th>GB Closed
# <tr><th>FSelect A<td>20.5079<td>17.72<td>21.7345
# <tr><th>FSelect B<td>0.19845<td>0.15795<td>0.2193
# </table>
# 
# @section    p4p3 Incidental
# <table>
# <caption id>Incidental</caption>
# <tr><th>MA<td>0.05
# <tr><th>GB<td>0.1
# </table>
#
# @section    p4p4 Discard
# Discard determines how many scallops are thrown out of a catch. It is determined by 
# scallop length and if the area is closed.
#@code{C}
# if ((length > cull_size) OR is_closed) then
#     SetDiscard = 0.0
# else
#     SetDiscard = discard * selectivity
#@endcode
#
# <table>
# <caption>Discard</caption>
# <tr><th> <th>Cull Size<th>Discard
# <tr><th>MA<td>90.0<td>0.2
# <tr><th>GB<td>100.0<td>0.2
# </table>
#
# @section p4p5 Overall Mortality, M
# @f$M = natural_{mortality} + Fishing_{effort} * (selectivity + incidental + discard))@f$
#
# @section p4p6 Computing Landings Per Unit Effort, LPUE
# The simulation uses the following parameters to compute LPUE
#
# <table>
# <caption id>LPUE</caption>
# <tr><th> <th>Default
# <tr><td>LPUE Slope<td>0.6556
# <tr><td>LPUE Slope2<td>2.3
# <tr><td>LPUE Intercept<td>1094.0
# <tr><td>Max # of Scallops Shucked Per Day<td>56000.0
# <tr><td>Max # of Hours Dredging Per Day<td>19.0
# <tr><td>Dredge Width in meters<td>9.144
# <tr><td>Towing Speed in knots<td>4.8
# </table>
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
import os
import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

from Widgets import *
from GeoSams import MainApplication

##
# This class allows the user to adjust parameters used in computing scallop growth
#
class Growth(ttk.Frame, MainApplication):
    ##
    # Constructor for Growth Class
    #
    def __init__(self, container, friend):
        super().__init__()
        self.root = os.getcwd() #os.environ['ROOT']
        self.growthStartDir = os.path.join(self.root, configDir, simCfgDir)

        # default values should file get corrupted
        self.friend = friend
        self.fmortStr = '0.4'
        self.alphStr = '1.0'
        self.maCullStr = '90.0'
        self.maDiscStr = '0.2'
        self.gbCullStr = '100.0'
        self.gbDiscStr = '0.2'
        self.maFSelAStr = '20.5079'
        self.maFSelBStr = '0.19845'
        self.gbClFSelAStr = '17.72'
        self.gbClFSelBStr = '0.15795'
        self.gbOpFSelAStr = '21.7345'
        self.gbOpFSelBStr = '0.2193'
        self.maAdultMortStr = '0.25'
        self.gbAdultMortStr = '0.2'
        self.maIncidStr = '0.05'
        self.gbIncidStr = '0.1'
        self.maLen0Str = '65.0'
        self.gbLen0Str = '70.0'
        self.fmortFileName = None
        self.lpueSlStr = '0.6556'
        self.lpueSl2Str = '2.3'
        self.lpueIntcStr = '1094'
        self.maxPerDayStr = '56000'
        self.maxTimeStr = '19'
        self.dredgeWdStr = '9.144'
        self.towSpdStr = '4.8'

        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        fishingFrame= ttk.LabelFrame(self, text='Mortality', style='SAMS.TFrame')
        self.fishMort   = SubFrameElement(self, fishingFrame, 'Fishing Mortality', self.fmortStr,       0, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.alphaMort  = SubFrameElement(self, fishingFrame, 'Alpha Mortality',self.alphStr,           1, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.maAdultMort= SubFrameElement(self, fishingFrame, 'MA Adult Mortality',self.maAdultMortStr, 2, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.gbAdultMort= SubFrameElement(self, fishingFrame, 'GB Adult Mortality',self.gbAdultMortStr, 3, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.maLength0  = SubFrameElement(self, fishingFrame, 'MA Length_0',  self.maLen0Str,           4, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.gbLength0  = SubFrameElement(self, fishingFrame, 'GB Length_0',  self.gbLen0Str,           5, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        fishingFrame.grid(row=1, column=0, padx=10, sticky='n')
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        selectivityFrame = ttk.LabelFrame(self, text='Selectivity', style='SAMS.TFrame')
        #-------------------------------------------------------------------------------------------
        self.maFSelectA       = SubFrameElement(self, selectivityFrame, 'MA FSelectA', self.maFSelAStr,          0, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.maFSelectB       = SubFrameElement(self, selectivityFrame, 'MA FSelectB', self.maFSelBStr,          1, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.gbClosedFSelectA = SubFrameElement(self, selectivityFrame, 'GB Closed FSelectA', self.gbClFSelAStr, 2, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.gbClosedFSelectB = SubFrameElement(self, selectivityFrame, 'GB Closed FSelectB', self.gbClFSelBStr, 3, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.gbOpenFSelectA   = SubFrameElement(self, selectivityFrame, 'GB Open FSelectA', self.gbOpFSelAStr,   4, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.gbOpenFSelectB   = SubFrameElement(self, selectivityFrame, 'GB Open FSelectB', self.gbOpFSelBStr,   5, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        selectivityFrame.grid(row=1, column=1, padx=10, sticky='n')
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        lpueFrame   = ttk.LabelFrame(self, text='LPUE', style='SAMS.TFrame')
        #-------------------------------------------------------------------------------------------
        self.lpueSlope   = SubFrameElement(self, lpueFrame, 'LPUE Slope', self.lpueSlStr,       0, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.lpueSlope2  = SubFrameElement(self, lpueFrame, 'LPUE Slope2', self.lpueSl2Str,     1, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.lpueIntcept = SubFrameElement(self, lpueFrame, 'LPUE Intercept', self.lpueIntcStr, 2, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.maxPerDay   = SubFrameElement(self, lpueFrame, 'Max Per Day', self.maxPerDayStr,   3, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.maxTime     = SubFrameElement(self, lpueFrame, 'Max Time', self.maxTimeStr,        4, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.dredgeWth   = SubFrameElement(self, lpueFrame, 'Dredge Width', self.dredgeWdStr,   5, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        self.towSpeed    = SubFrameElement(self, lpueFrame, 'Towing Speed', self.towSpdStr,     6, 0, 1, valCmd=floatCallback, width=10)
        #-------------------------------------------------------------------------------------------
        lpueFrame.grid(row=1, column=2, padx=10, sticky='n')
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        incidentalFrame = ttk.LabelFrame(self, text='Incidental', style='SAMS.TFrame')
        #-------------------------------------------------------------------------------------------
        self.maIncident = SubFrameElement(self, incidentalFrame, 'MA Incidental', self.maIncidStr, 0, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.gbIncident = SubFrameElement(self, incidentalFrame, 'GB Incidental', self.gbIncidStr, 1, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        incidentalFrame.grid(row=2, column=0, padx=10)
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        discardFrame   = ttk.LabelFrame(self, text='Discard', style='SAMS.TFrame')
        self.maCullSize = SubFrameElement(self, discardFrame, 'MA Cull Size', self.maCullStr, 0, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.maDiscard  = SubFrameElement(self, discardFrame, 'MA Discard',   self.maDiscStr, 1, 0, 1, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.gbCullSize = SubFrameElement(self, discardFrame, 'GB Cull Size', self.gbCullStr, 0, 2, 3, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        self.gbDiscard  = SubFrameElement(self, discardFrame, 'GB Discard',   self.gbDiscStr, 1, 2, 3, valCmd=floatCallback)
        #-------------------------------------------------------------------------------------------
        discardFrame.grid(row=2, column=1, columnspan=2, padx=10)
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        growthFrame = ttk.LabelFrame(self, text='Growth Configuration Files', style='SAMS.TFrame')
        self.growthCfgFile = SubFrameElement(self, growthFrame, 'Growth Config File   ', 'Growth.cfg',  0, 0, 1, width=20)
        #-------------------------------------------------------------------------------------------
        self.saveGrowthConfigButton = ttk.Button(growthFrame, text='Change/Save Growth File', style="BtnBluGrn.TLabel", command=self.GetGrowthConfigFName)
        self.saveGrowthConfigButton.grid(row=0, column=3)
        #-------------------------------------------------------------------------------------------
        self.loadGrowthConfigButton = ttk.Button(growthFrame, text='Load Growth Data', style="BtnGreen.TLabel", command=self.LoadGrowthData)
        self.loadGrowthConfigButton.grid(row=1, column=3)
        #-------------------------------------------------------------------------------------------
        growthFrame.grid(row=3, column=0, columnspan=2, padx=10)
        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Growth Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=1)

        self.UpdateValues()

        self.bind("<Visibility>", self.on_visibility)

    ##
    #
    def on_visibility(self, event):
        self.UpdateValues()
        self.UpdateWidgets()

    ## 
    # Calls the filedialog method asksaveasfilename to name a file to be used for the Mortality Configuration
    # file. It then writes out the defined parameters to this file using the 'tag = value' format. 
    #
    def GetGrowthConfigFName(self):
        file_path = filedialog.asksaveasfilename(title="Open Growth Config File", filetypes=[("CFG files", "*.cfg")],
                                                 defaultextension='cfg', initialdir=self.growthStartDir)
        f = file_path.split('/')
        if file_path:
            self.growthCfgFile.myEntry.delete(0,tk.END)
            self.growthCfgFile.myEntry.insert(0,f[-1])
            self.friend.WriteGrowthConfig()

    ##
    #
    def LoadGrowthData(self):
        filePath = filedialog.askopenfilename(title="Load Growth Config File", filetypes=[("CFG files", "*.cfg")],
                                             defaultextension='cfg', initialdir=self.growthStartDir)
        f = filePath.split('/')
        if filePath:
            self.growthCfgFile.myEntry.delete(0,tk.END)
            self.growthCfgFile.myEntry.insert(0,f[-1])
        self.UpdateValues()
        self.UpdateWidgets()

    ##
    #
    def UpdateWidgets(self):
        # restore entries
        self.fishMort.myEntry.delete(0,tk.END)
        self.fishMort.myEntry.insert(0, self.fmortStr)

        self.alphaMort.myEntry.delete(0,tk.END)  
        self.alphaMort.myEntry.insert(0, self.alphStr)

        self.maAdultMort.myEntry.delete(0,tk.END)
        self.maAdultMort.myEntry.insert(0, self.maAdultMortStr)

        self.gbAdultMort.myEntry.delete(0,tk.END)
        self.gbAdultMort.myEntry.insert(0, self.gbAdultMortStr)

        self.maLength0.myEntry.delete(0,tk.END)
        self.maLength0.myEntry.insert(0, self.maLen0Str)

        self.gbLength0.myEntry.delete(0,tk.END)
        self.gbLength0.myEntry.insert(0, self.gbLen0Str)

        self.maFSelectA.myEntry.delete(0,tk.END)
        self.maFSelectA.myEntry.insert(0, self.maFSelAStr)

        self.maFSelectB.myEntry.delete(0,tk.END)
        self.maFSelectB.myEntry.insert(0, self.maFSelBStr)

        self.gbClosedFSelectA.myEntry.delete(0,tk.END)
        self.gbClosedFSelectA.myEntry.insert(0, self.gbClFSelAStr)

        self.gbClosedFSelectB.myEntry.delete(0,tk.END)
        self.gbClosedFSelectB.myEntry.insert(0, self.gbClFSelBStr)

        self.gbOpenFSelectA.myEntry.delete(0,tk.END)
        self.gbOpenFSelectA.myEntry.insert(0, self.gbOpFSelAStr)

        self.gbOpenFSelectB.myEntry.delete(0,tk.END)
        self.gbOpenFSelectB.myEntry.insert(0, self.gbOpFSelBStr)

        self.lpueSlope.myEntry.delete(0,tk.END)
        self.lpueSlope.myEntry.insert(0, self.lpueSlStr)

        self.lpueSlope2.myEntry.delete(0,tk.END)
        self.lpueSlope2.myEntry.insert(0, self.lpueSl2Str)

        self.lpueIntcept.myEntry.delete(0,tk.END)
        self.lpueIntcept.myEntry.insert(0, self.lpueIntcStr)

        self.maxPerDay.myEntry.delete(0,tk.END)
        self.maxPerDay.myEntry.insert(0, self.maxPerDayStr)

        self.maxTime.myEntry.delete(0,tk.END)
        self.maxTime.myEntry.insert(0, self.maxTimeStr)

        self.dredgeWth.myEntry.delete(0,tk.END)
        self.dredgeWth.myEntry.insert(0, self.dredgeWdStr)

        self.towSpeed.myEntry.delete(0,tk.END)
        self.towSpeed.myEntry.insert(0, self.towSpdStr)

        self.maIncident.myEntry.delete(0,tk.END)
        self.maIncident.myEntry.insert(0, self.maIncidStr)

        self.gbIncident.myEntry.delete(0,tk.END)
        self.gbIncident.myEntry.insert(0, self.gbIncidStr)

        self.maCullSize.myEntry.delete(0,tk.END)
        self.maCullSize.myEntry.insert(0, self.maCullStr)

        self.maDiscard.myEntry.delete(0,tk.END)
        self.maDiscard.myEntry.insert(0, self.maDiscStr)

        self.gbCullSize.myEntry.delete(0,tk.END)
        self.gbCullSize.myEntry.insert(0, self.gbCullStr)

        self.gbDiscard.myEntry.delete(0,tk.END)
        self.gbDiscard.myEntry.insert(0, self.gbDiscStr)

    ##
    # Method to read Mortality Configuration file and set values accordingly
    #
    def UpdateValues(self):
        fName = os.path.join(self.root,configDir, simCfgDir, self.growthCfgFile.myEntry.get())
        # read current settings
        tags = self.ReadConfigFile(fName)
        for (tag, value) in tags:
            # Python 3.8 does not have match/case so using if elif
            if   tag == 'Fishing Mortality': self.fmortStr = value
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
            elif tag == 'Fishing Mortality File': self.fmortFileName = value
            elif tag == 'LPUE Slope':         self.lpueSlStr = value
            elif tag == 'LPUE Slope2':        self.lpueSl2Str = value
            elif tag == 'LPUE Intercept':     self.lpueIntcStr = value
            elif tag == 'Max Per Day':        self.maxPerDayStr = value
            elif tag == 'Max Time':           self.maxTimeStr = value
            elif tag == 'Dredge Width':       self.dredgeWdStr = value
            elif tag == 'Towing Speed':       self.towSpdStr = value

    ## 
    #
    def pop_up(self):
        about = '''Mortality
    Define parameters to compute fishing mortality
    - Fishing Mortality: This is the default fishing mortality
    - Alpha Mortality: Used as an exponent to set fishing mortality 
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
    - Region dependent Cull Size and Discard: Determines SetDiscard Value
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
        parentPosn = '+'+str(self.winfo_rootx()+helpXoffset)+'+'+str(self.winfo_rooty()+helpYoffset)
        popup.geometry(str(int(ncols*8.5))+"x"+str(nrows*18)+parentPosn)
        T = tk.Text(popup, width=ncols, height=nrows, padx=10)
        T.insert('end', about)
        T.config(state='disabled')
        T.grid()
        btn = tk.Button(popup, text ="Close", command= popup.destroy)
        btn.grid(row =1)
