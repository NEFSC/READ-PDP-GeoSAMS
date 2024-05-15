import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

import subprocess
import os
import sys
import csv
import platform


class MainApplication(tk.Tk):
    def __init__(self, title):
        super().__init__()

        # setup
        self.title(title)
        #self.geometry('800x800')
        self.style = ttk.Style()

        # vscode will not set this variable, must be done via conntrol panel
        # in command terminal on Windows, assuming user is in the install directory
        #   > set ROOT=%CD%
        self.root = os.environ['ROOT']

        self.notebook = ttk.Notebook(self)

        self.frame1 = Frame1(self.notebook)
        self.simConfigFile = os.path.join(self.root,'Configuration/'+self.frame1.simCfgFile.myEntry.get())
        # Read in configuration parameters
        [paramStr, paramVal, tsInYear, savedByStratum] = self.ReadSimConfigFile()
        self.frame2 = Frame2(self.notebook, tsInYear, paramVal, savedByStratum)
        self.frame3 = Frame3(self.notebook)

        self.notebook.add(self.frame1, text='Main')
        self.notebook.add(self.frame2, text='Outputs')
        self.notebook.add(self.frame3, text='Mortality')
        self.notebook.pack()

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#0F0")
        ttk.Button(self, text='START', style="Custom.TLabel", command=self.Run_Sim).pack()

    def Run_Sim(self):
        # No check for variables changed, therefore update all configuration files with current values in GUI
        # OR
        # Create new files based on names given by user.
        self.UpdateScallopConfig()
        self.UpdateRecruitmentConfig()
        self.UpdateMortalityConfig()

        ex = self.root+'/SRC/ScallopPopDensity'
        simCfgFile = self.frame1.simCfgFile.myEntry.get()
        startYear = self.frame1.startYr.myEntry.get()
        stopYear = self.frame1.stopYr.myEntry.get()
        dn = self.frame1.domainName.myEntry.get()
        cmd = [ex, simCfgFile, str(startYear), str(stopYear), dn]
        print(cmd)
        result = subprocess.run(cmd)
        if result.returncode == 0:
            messagebox.showinfo("GeoSAM Sim", f'Completed Successfully\n{result.args}')
        else:
            messagebox.showerror("GeoSAM Sim", f'Failed\n{result.args}\nReturn Code = {result.returncode}')

    def UpdateScallopConfig(self):
        simCfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.simCfgFile.myEntry.get())
        with open(simCfgFile, 'w') as f:
            f.write('Time steps per Year = ' + str(self.frame2.tsPerYear.myEntry.get())+'\n')
            f.write('Save By Stratum = '     + str(self.frame2.useStratum.myEntry.get())+'\n')
            f.write('Mortality Config File = '   + self.frame1.mortCfgFile.myEntry.get() + '\n')
            f.write('Recruit Config File = '     + self.frame1.recrCfgFile.myEntry.get() + '\n')
            f.write('Grid Manager Config File = '+ self.frame1.gmCfgFile.myEntry.get()   + '\n')
            if(self.frame2.abunVar):  f.write('Select Abundance          ='+'\n')
            if(self.frame2.bmsVar):   f.write('Select BMS                ='+'\n')
            if(self.frame2.ebmsVar):  f.write('Select Expl BMS           ='+'\n')
            if(self.frame2.feffVar):  f.write('Select Fishing Effort     ='+'\n')
            if(self.frame2.fmortVar): f.write('Select Fishing Mortality  ='+'\n')
            if(self.frame2.landVar):  f.write('Select Landings by Number ='+'\n')
            if(self.frame2.lndwVar):  f.write('Select Landings by Weight ='+'\n')
            if(self.frame2.lpueVar):  f.write('Select LPUE               ='+'\n')
            if(self.frame2.recrVar):  f.write('Select RECR               ='+'\n')
            f.close()

    def ConvertMonthDayToDay(self,month,day):
        daysInYear = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
        monDict = {'Jan':0, 'Feb':1, 'Mar':2, 'Apr':3, 'May':4, 'Jun':5, 'Jul':6, 'Aug':7, 'Sep':8, 'Oct':9, 'Nov':10, 'Dec':11} 
        return daysInYear[monDict[month]] + day - 1
            
    def UpdateRecruitmentConfig(self):
        cfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.recrCfgFile.myEntry.get())

        # TODO Assumes MMM D[D]
        periodStr = self.frame1.startDay.myEntry.get()
        m = len(periodStr)
        month = periodStr[0:3]
        day = int(periodStr[3:m])
        startPeriod = self.ConvertMonthDayToDay(month,day)

        periodStr = self.frame1.stopDay.myEntry.get()
        m = len(periodStr)
        month = periodStr[0:3]
        day = int(periodStr[3:m])
        stopPeriod = self.ConvertMonthDayToDay(month,day)

        with open(cfgFile, 'w') as f:
            f.write('Start Period = '+str(startPeriod)+'\n')
            f.write('Stop Period = '+str(stopPeriod)+'\n')
            f.close()
            
    def UpdateMortalityConfig(self):
        simCfgFile  = os.path.join(self.root,'Configuration/'+self.frame1.mortCfgFile.myEntry.get())
        with open(simCfgFile, 'w') as f:
            f.write('Fishing Mortality =' + self.frame3.fishMort.myEntry.get()   + '\n')
            f.write('Fishing = '          + self.frame3.fishSelect.myEntry.get() + '\n')
            f.write('Alpha Mortality ='   + self.frame3.alphaMort.myEntry.get()  + '\n')
            f.write('MA Cull size = '     + self.frame3.maCullSize.myEntry.get() + '\n')
            f.write('MA Discard = '       + self.frame3.maDiscard.myEntry.get() + '\n')
            f.write('GB Cull size = '     + self.frame3.gbCullSize.myEntry.get() + '\n')
            f.write('GB Discard = '       + self.frame3.gbDiscard.myEntry.get() + '\n')
            f.write('MA FSelectA = '       + self.frame3.maFSelectA.myEntry.get() + '\n')
            f.write('MA FSelectB = '       + self.frame3.maFSelectB.myEntry.get() + '\n')
            f.write('GB Closed FSelectA = '+ self.frame3.gbClosedFSelectA.myEntry.get() + '\n')
            f.write('GB Closed FSelectB = '+ self.frame3.gbClosedFSelectB.myEntry.get() + '\n')
            f.write('GB Open FSelectA = '  + self.frame3.gbOpenFSelectA.myEntry.get() + '\n')
            f.write('GB Open FSelectB = '  + self.frame3.gbOpenFSelectB.myEntry.get() + '\n')
            f.write('MA Adult Mortality = '+ self.frame3.maAdultMort.myEntry.get() + '\n')
            f.write('GB Adult Mortality = '+ self.frame3.gbAdultMort.myEntry.get() + '\n')
            f.write('MA Incidental = '     + self.frame3.maIncident.myEntry.get() + '\n')
            f.write('GB Incidental = '     + self.frame3.gbIncident.myEntry.get() + '\n')
            f.write('MA Length_0 = '       + self.frame3.maLength0.myEntry.get() + '\n')
            f.write('GB Length_0 = '       + self.frame3.gbLength0.myEntry.get() + '\n')
            f.write('Fishing Mortality File = '+ self.frame3.fishMortFile.myEntry.get() + '\n')
            f.write('LPUE Slope = '        + self.frame3.lpueSlope.myEntry.get() + '\n')
            f.write('LPUE Slope2 = '       + self.frame3.lpueSlope2.myEntry.get() + '\n')
            f.write('LPUE Intercept = '    + self.frame3.lpueIntcept.myEntry.get() + '\n')
            f.write('Max Per Day = '       + self.frame3.maxPerDay.myEntry.get() + '\n')
            f.write('Max Time = '          + self.frame3.maxTime.myEntry.get() + '\n')
            f.write('Dredge Width = '      + self.frame3.dredgeWth.myEntry.get() + '\n')
            f.write('Towing Speed = '      + self.frame3.towSpeed.myEntry.get() + '\n')
            f.close()


    def ReadSimConfigFile(self):
        # need to read Configuration/Scallop.cfg to determine which parameters are output
        tsInYear = 0
        paramStr = []
        paramVal = 0
        with open(self.simConfigFile, 'r') as f:
            while True:
                line = f.readline()
                if not line:
                    f.close()
                    break
                if (line[0] != '#'):
                    j = line.find('=')
                    tag = line[0:j].strip()
                    k = line.find('#')
                    if (k == 0):
                        k = len(line)
                    value = line[j+1:k].strip()

                    # Python 3.8 does not have match/case so using if elif
                    if (tag == 'Select Abundance'):
                        paramStr.append('ABUN_')
                        paramVal += 8
                    elif (tag == 'Select BMS'):
                        paramStr.append('BMMT_')
                        paramVal += 4
                    elif (tag == 'Select Expl BMS'):
                        paramStr.append('EBMS_')
                        paramVal += 2
                    elif (tag == 'Select Fishing Effort'):
                        paramStr.append('FEFF_')
                        paramVal += 64
                    elif (tag == 'Select Fishing Mortality'):
                        paramStr.append('FMOR_')
                        paramVal += 128
                    elif (tag == 'Select Landings by Number'):
                        paramStr.append('LAND_')
                        paramVal += 32
                    elif (tag == 'Select Landings by Weight'):
                        paramStr.append('LNDW_')
                        paramVal += 16
                    elif (tag == 'Select LPUE'):
                        paramStr.append('LPUE_')
                        paramVal += 1
                    elif (tag == 'Select RECR'):
                        paramStr.append('RECR_')
                        paramVal += 256
                    elif (tag == 'Time steps per Year'):
                        tsInYear = int(value)
                    elif (tag == 'Save By Stratum'):
                        savedByStratum = value[0] == 'T'

        return [paramStr, paramVal, tsInYear, savedByStratum]


class Frame1(ttk.Frame):
    def __init__(self, container):
        super().__init__()

        self.style = ttk.Style()
        self.style.configure('Frame1.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame1.TFrame.Label', font=('courier', 8, 'bold'))

        growthFrame = ttk.LabelFrame(self, text='Growth', style='Frame1.TFrame')
        self.startYr    = SubFrameElement(self, growthFrame, 'Start Year', '2015',        0, 0, 1)
        self.stopYr     = SubFrameElement(self, growthFrame, 'Stop Year ', '2017',        1, 0, 1)
        self.domainName = SubFrameElement(self, growthFrame, 'Domain Name', 'MA',         2, 0, 1)
        growthFrame.grid(row=0, column=0)

        recruitFrame = ttk.LabelFrame(self, text='Recruitment', style='Frame1.TFrame')
        self.startDay    = SubFrameElement(self, recruitFrame, 'Start Day', 'Jan 1',  0, 0, 1)
        self.stopDay     = SubFrameElement(self, recruitFrame, 'Stop Day ', 'Apr 11', 1, 0, 1)
        recruitFrame.grid(row=0, column=1)

        configFrame = ttk.LabelFrame(self, text='Configuration Files', style='Frame1.TFrame')
        self.mortCfgFile = SubFrameElement(self, configFrame, 'Mortality Config File', 'Mortality.cfg',  0, 0, 1)
        self.recrCfgFile = SubFrameElement(self, configFrame, 'Recruitment File',      'Recruitment.cfg',1, 0, 1)
        self.gmCfgFile   = SubFrameElement(self, configFrame, 'Grid Manager File',     'GridManager.cfg',2, 0, 1)
        self.simCfgFile  = SubFrameElement(self, configFrame, 'Config File',           'Scallop.cfg',    3, 0, 1)
        configFrame.grid(row=0, column=2)


class Frame2(ttk.Frame):
    def __init__(self, container, tsPerYear, selectedOutputs, useStratum):
        super().__init__()

        self.style = ttk.Style()
        self.style.configure('Frame2.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame2.TFrame.Label', font=('courier', 8, 'bold'))

        outputsFrame = ttk.LabelFrame(self, text='Outputs', style='Frame1.TFrame')
        self.tsPerYear    = SubFrameElement(self, outputsFrame, 'tsPerYear', str(tsPerYear),  0, 0, 1)
        self.useStratum     = SubFrameElement(self, outputsFrame, 'Stop Day ', str(useStratum), 1, 0, 1)
        outputsFrame.grid(row=0, column=1)

        # Check Buttons
        self.lpueVar = tk.IntVar(value=(selectedOutputs   )&1)
        self.ebmsVar = tk.IntVar(value=(selectedOutputs>>1)&1)
        self.bmsVar  = tk.IntVar(value=(selectedOutputs>>2)&1)
        self.abunVar = tk.IntVar(value=(selectedOutputs>>3)&1)
        self.lndwVar = tk.IntVar(value=(selectedOutputs>>4)&1)
        self.landVar = tk.IntVar(value=(selectedOutputs>>5)&1)
        self.feffVar = tk.IntVar(value=(selectedOutputs>>6)&1)
        self.fmortVar= tk.IntVar(value=(selectedOutputs>>7)&1)
        self.recrVar = tk.IntVar(value=(selectedOutputs>>8)&1)
        ttk.Checkbutton(self, text='Abundance',     variable=self.abunVar, command=self.CBSelected).grid(row=2, column=0)
        ttk.Checkbutton(self, text='Biomass',       variable=self.bmsVar,  command=self.CBSelected).grid(row=2, column=1)
        ttk.Checkbutton(self, text='ExplBiomass',   variable=self.ebmsVar, command=self.CBSelected).grid(row=2, column=2)
        ttk.Checkbutton(self, text='LPUE',          variable=self.lpueVar, command=self.CBSelected).grid(row=2, column=3)
        ttk.Checkbutton(self, text='Fish Mort',    variable=self.fmortVar, command=self.CBSelected).grid(row=3, column=0)
        ttk.Checkbutton(self, text='Fish Effort',  variable=self.feffVar , command=self.CBSelected).grid(row=3, column=1)
        ttk.Checkbutton(self, text='Lands By Num',  variable=self.landVar , command=self.CBSelected).grid(row=3, column=2)
        ttk.Checkbutton(self, text='Lands By Wght', variable=self.lndwVar , command=self.CBSelected).grid(row=3, column=3)
        ttk.Checkbutton(self, text='Recruitment',   variable=self.recrVar , command=self.CBSelected).grid(row=4, column=2)
    
    def CBSelected(self):
        self.desiredOutput = self.abunVar.get()*8 + self.bmsVar.get()*4 + self.ebmsVar.get()*2 + self.lpueVar.get()
        self.desiredOutput+= self.fmortVar.get()*128 + self.feffVar.get()*64 + self.landVar.get()*32 + self.lndwVar.get()*16
        self.desiredOutput+= self.recrVar.get()*256
        print(f'0X{self.desiredOutput:X}')

class Frame3(ttk.Frame):
    def __init__(self, container):
        super().__init__()

        self.style = ttk.Style()
        self.style.configure('Frame3.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Frame3.TFrame.Label', font=('courier', 8, 'bold'))
        
        fishingFrame= ttk.LabelFrame(self, text='Mortality', style='Frame3.TFrame')
        self.fishMort   = SubFrameElement(self, fishingFrame, 'Fishing Mort', '0.4',  0, 0, 1)
        self.fishSelect = SubFrameElement(self, fishingFrame, 'Fishing Select', 'BMS', 1, 0, 1)
        self.fishMortFile = SubFrameElement(self, fishingFrame, 'Fishing Mort File', 'FishingMortality.csv', 1, 0, 1)
        self.alphaMort  = SubFrameElement(self, fishingFrame, 'Alpha Mortality','1.0', 2, 0, 1)
        self.maAdultMort  = SubFrameElement(self, fishingFrame, 'MA Adult Mortality','0.25', 3, 0, 1)
        self.gbAdultMort  = SubFrameElement(self, fishingFrame, 'GB Adult Mortality','0.2', 4, 0, 1)
        fishingFrame.grid(row=0, column=0)

        ilfFrame = ttk.LabelFrame(self, text='Increasing Logistic Fcn', style='Frame3.TFrame')
        self.maFSelectA       = SubFrameElement(self, ilfFrame, 'MA FSelectA', '20.5079', 0, 0, 1)
        self.maFSelectB       = SubFrameElement(self, ilfFrame, 'MA FSelectB', '0.19845', 1, 0, 1)
        self.gbClosedFSelectA = SubFrameElement(self, ilfFrame, 'GB Closed FSelectA', '17.72', 2, 0, 1)
        self.gbClosedFSelectB = SubFrameElement(self, ilfFrame, 'GB Closed FSelectB', '0.15795', 3, 0, 1)
        self.gbOpenFSelectA   = SubFrameElement(self, ilfFrame, 'GB Open FSelectA', '21.7345', 4, 0, 1)
        self.gbOpenFSelectB   = SubFrameElement(self, ilfFrame, 'GB Open FSelectB', '0.2193', 5, 0, 1)
        ilfFrame.grid(row=0, column=1)
        
        lpueFrame   = ttk.LabelFrame(self, text='LPUE', style='Frame3.TFrame')
        self.lpueSlope   = SubFrameElement(self, lpueFrame, 'LPUE Slope', '0.6556', 0, 0, 1)
        self.lpueSlope2  = SubFrameElement(self, lpueFrame, 'LPUE Slope2', '2.3', 1, 0, 1)
        self.lpueIntcept = SubFrameElement(self, lpueFrame, 'LPUE Intercept', '1094', 2, 0, 1)
        self.maxPerDay   = SubFrameElement(self, lpueFrame, 'Max Per Day', '56000', 3, 0, 1)
        self.maxTime     = SubFrameElement(self, lpueFrame, 'Max Time', '19', 4, 0, 1)
        self.dredgeWth   = SubFrameElement(self, lpueFrame, 'Dredge Width', '9.144', 5, 0, 1)
        self.towSpeed    = SubFrameElement(self, lpueFrame, 'Towing Speed', '4.8', 6, 0, 1)
        lpueFrame.grid(row=0, column=2)

        discardFrame   = ttk.LabelFrame(self, text='Discard', style='Frame3.TFrame')
        self.maCullSize = SubFrameElement(self, discardFrame, 'MA Cull Size', '90.0', 0, 0, 1)
        self.maDiscard  = SubFrameElement(self, discardFrame, 'MA Discard',   '0.2',    1, 0, 1)
        self.gbCullSize = SubFrameElement(self, discardFrame, 'GB Cull Size', '100.0', 0, 2, 3)
        self.gbDiscard  = SubFrameElement(self, discardFrame, 'GB Discard',   '0.2',    1, 2, 3)
        discardFrame.grid(row=1, column=1)

        incidentalFrame = ttk.LabelFrame(self, text='Incidental', style='Frame3.TFrame')
        self.maIncident = SubFrameElement(self, incidentalFrame, 'MA Incidental', '0.05', 0, 0, 1)
        self.gbIncident = SubFrameElement(self, incidentalFrame, 'GB Incidental', '0.1',  1, 0, 1)
        self.maLength0  = SubFrameElement(self, incidentalFrame, 'MA Length_0',  '65.0',  0, 2, 3)
        self.gbLength0  = SubFrameElement(self, incidentalFrame, 'GB Length_0',  '70.0',  1, 2, 3)
        incidentalFrame.grid(row=1, column=0)

class SubFrameElement(tk.Frame):
        def __init__(self, container, parent, label, value, elementRow, labelCol, entryCol):
            super().__init__()
            self.myEntry=ttk.Entry(parent)
            self.myEntry.grid(row=elementRow, column=entryCol, rowspan=1, sticky='nsew', pady=10)
            self.myEntry.insert(0, value)
            self.myLabel = ttk.Label(parent, text=label)
            self.myLabel.grid(row=elementRow, column=labelCol, rowspan=1, sticky='nsew', pady=10)


def main():
    r = MainApplication('GeoSAM')
    r.mainloop()

if __name__ == "__main__":
    main()
