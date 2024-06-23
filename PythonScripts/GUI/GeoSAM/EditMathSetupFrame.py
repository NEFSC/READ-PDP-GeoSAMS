#======================================================================================================
## @page MathSetupFile Math Setup Frame
#
#======================================================================================================
import os
import tkinter as tk

from tkinter import ttk
from tkinter import messagebox

from Widgets import *

#===============================================================================================================
##
# This class allows the user to edit the Matlab/Octave setup files to fit their environment.
#
#===============================================================================================================
class EditMathSetup(ttk.Frame):
    ##
    # Constructor for Growth Class
    #
    def __init__(self, container):
        super().__init__()
        self.matlabFName = 'startup.m'
        self.octaveFName = '.octaverc'
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.editMathFrame = ttk.LabelFrame(self, text='Math Setup Text', style='SAMS.TFrame')
        # --------------------------------------------------------------------------------------------------------
        self.editText = tk.Text(self.editMathFrame, height = 15, width = 90)
        self.editText.grid(row=0, column=0, columnspan=3, padx=5, sticky='sw')
        self.editTextButton = ttk.Button(self.editMathFrame, text='Write Startup File', style="BtnGreen.TLabel", command=self.WriteStartupFile)
        self.editTextButton.grid(row=1, column=0)
        #-------------------------------------------------------------------------------------------
        # Radio Buttons, it's either Matlab or Octave
        self.isMatlab = True
        self.usingMatlab = tk.BooleanVar(self.editMathFrame, self.isMatlab)
        self.useMatlabRB = ttk.Radiobutton(self.editMathFrame, text='Matlab', value=True, variable=self.usingMatlab, command=self.LoadStartupFile)
        self.useMatlabRB.grid(row=1, column=1)
        self.useOctaveRB = ttk.Radiobutton(self.editMathFrame, text='Octave', value=False, variable=self.usingMatlab, command=self.LoadStartupFile)
        self.useOctaveRB.grid(row=1, column=2)
        #-------------------------------------------------------------------------------------------
        self.editMathFrame.grid(row=1, column=0, padx=5, sticky='sw')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        helpButton = ttk.Button(self, text= "Math Help", style="Help.TLabel", command = self.pop_up)
        helpButton.grid(row=0, column=0)

        self.bind("<Visibility>", self.on_visibility)

    #---------------------------------------------------------------------------------------------------------
    ## Opens either startup.m or .octaverc depending if user selected Matlab or Octave resp
    #---------------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
        self.ReadFile(False)

    def LoadStartupFile(self):
        self.ReadFile(True)

    def ReadFile(self, showMsg):
        if self.usingMatlab.get():
            if os.path.isfile(self.matlabFName):
                # Unpack.sh has not renamed Matlab startup.m
                self.usingMatlab.set(True)
                self.editText.delete(1.0,tk.END)
                with open(self.matlabFName, 'r') as f:
                    while True:
                        line = f.readline()
                        if not line:
                            f.close()
                            break
                        self.editText.insert('current',line)
            else:
                # File does not exist default to Octave
                if showMsg: messagebox.showinfo('Read Startup File','File has been renamed by Unpack.sh')
                self.isMatlab = False
                self.usingMatlab.set(False)

        if not self.usingMatlab.get():
            # Unpack.sh has not renamed Matlab startup.m
            self.editText.delete(1.0,tk.END)
            with open(self.octaveFName, 'r') as f:
                while True:
                    line = f.readline()
                    if not line:
                        f.close()
                        break
                    self.editText.insert('current',line)

    #-------------------------------------------------------------------------------------
    ## 
    #-------------------------------------------------------------------------------------
    def WriteStartupFile(self):
        lines = self.editText.get(1.0,tk.END)
        n = len(lines)
        # remove extr CRLF
        if lines[n-1] == '\n':  lines = lines[0:n-1]
        if self.usingMatlab.get():
            fname = self.matlabFName
        else:
            fname = self.octaveFName
        f = open(fname, 'w')
        f.write(lines)
        f.close()
        messagebox.showinfo('WRITING TO STARTUP FILE', 'File successfully writen.')
        

    #-------------------------------------------------------------------------------------
    ## 
    #-------------------------------------------------------------------------------------
    def pop_up(self):
        about = '''(This frame is scrollable, use mouse wheel)
This frame allows the user to modify the Matlab/Octave startup files.

Matlab should not need any modification as these are the installed directories.
The user should not need to run any Matlab scripts as these are called from the
GUI.

Octave on the other hand does require some setup. The user will need to install
the desired packages from https://gnu-octave.github.io/packages/
  - io
  - geometry
  - mapping
  - statistics

The user then needs to modify .octaverc to point to where these are installed.
Either click the Octave radio button or on a non-Windows platform the Unpack.sh
renames startup.m to startup.xxx such that this frame will default to .octaverc

At this point simply edit the text box to reflect your environment and click
Write Startup File.
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

