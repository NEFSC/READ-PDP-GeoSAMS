#======================================================================================================
## @page page2 Special Access Areas
# Allows the user to view files used to define the special access areas and the fishming mortality
# associated with those areas and the years for which the data applies.
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

##
# This class is used to allow the user to open and view Special Access Area files
#
class SpecialAccess(ttk.Frame):

    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief constructor for SpecialAccess Class
    #
    #--------------------------------------------------------------------------------------------------
    def __init__(self, container):
        super().__init__()
        self.root = os.environ['ROOT']

        self.style = ttk.Style()
        self.style.configure('MainInput.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('MainInput.TFrame.Label', font=('courier', 8, 'bold'))
        #-------------------------------------------------------------------------------------------
        specialAccFrame = ttk.LabelFrame(self, text='Special Access', style='MainInput.TFrame')
        self.specAccFile  = SubFrameElement(self, specialAccFrame, 'Special Access Points', '', 0, 0, 1)
        self.fishMortFile = SubFrameElement(self, specialAccFrame, 'Fishing Mort File', '', 1, 0, 1)

        self.style.configure("Custom.TLabel", padding=6, relief="flat", background="#080")
        self.openFishCSVButton = ttk.Button(specialAccFrame, text='View', style="Custom.TLabel", command=self.OpenSpecAccCSV)
        self.openFishCSVButton.grid(row=0, column=2)

        self.tree = ttk.Treeview(specialAccFrame, show="headings", height=20)
        self.tree.grid(row=2, column=0, columnspan=5, padx=10)
        specialAccFrame.grid(row=0, column=0, columnspan=4, sticky='w')
        #-------------------------------------------------------------------------------------------


    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Used to open CSV File Dialog Window
    #
    #--------------------------------------------------------------------------------------------------
    def OpenSpecAccCSV(self):
        startDir = os.path.join(self.root, 'Configuration')
        file_path = filedialog.askopenfilename(title="Open CSV File", filetypes=[("CSV files", "*.csv")], initialdir=startDir)
        if file_path:
            self.DisplayCSVData(file_path)

    #--------------------------------------------------------------------------------------------------
    ## 
    #  @brief Displays CSV file contents
    #
    #--------------------------------------------------------------------------------------------------
    def DisplayCSVData(self, file_path):
        try:
            with open(file_path, 'r', newline='') as file:
                csv_reader = csv.reader(file)
                header = next(csv_reader)  # Read the header row
                self.tree.delete(*self.tree.get_children())  # Clear the current data

                self.tree["columns"] = header
                for col in header:
                    self.tree.column(col)
                    self.tree.heading(col, text=col)

                for row in csv_reader:
                    self.tree.insert("", "end", values=row)

        except Exception as e:
            messagebox.showerror("GeoSAM Sim", f"Error: {str(e)}")
