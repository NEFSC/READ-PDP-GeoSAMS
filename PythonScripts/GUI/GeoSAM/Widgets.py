import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

class SubFrameElement(tk.Frame):
    def __init__(self, container, parent, label, value, elementRow, labelCol, entryCol):
        super().__init__()
        self.myEntry=ttk.Entry(parent)
        self.myEntry.grid(row=elementRow, column=entryCol, rowspan=1, sticky='nsew', pady=10)
        self.myEntry.insert(0, value)
        self.myLabel = ttk.Label(parent, text=label, wraplength=200, anchor='nw', justify='right')
        self.myLabel.grid(row=elementRow, column=labelCol, rowspan=1, sticky='nsew', pady=10)


class SubFrameInterpFunction(tk.Frame):
    def __init__(self, container, parent, funcNum, dim, shape, preconNum, elementRow, elementCol):
        super().__init__()

        self.funcFrame = ttk.LabelFrame(parent, text='Function '+str(funcNum))
        dimFrame = ttk.LabelFrame(self.funcFrame, text='dim')
        self.dimVal = tk.StringVar(dimFrame, dim)
        self.myDimRBx = ttk.Radiobutton(dimFrame, text='x', value='x', variable=self.dimVal, command=self.GetDimRB).pack()
        self.myDimRBy = ttk.Radiobutton(dimFrame, text='y', value='y', variable=self.dimVal, command=self.GetDimRB).pack()
        self.myDimRBz = ttk.Radiobutton(dimFrame, text='z', value='z', variable=self.dimVal, command=self.GetDimRB).pack()
        dimFrame.grid(row=elementRow, column=0, rowspan=3, sticky='nsew', pady=10)

        shapeFrame = ttk.LabelFrame(self.funcFrame, text='shape')
        self.shapeVal = tk.StringVar(shapeFrame, shape)
        self.myShapeG = ttk.Radiobutton(shapeFrame, text='Gaussian', value='Gaussian', variable=self.shapeVal, command=self.GetShapeRB).pack()
        self.myShapeL = ttk.Radiobutton(shapeFrame, text='Logistic', value='Logistic', variable=self.shapeVal, command=self.GetShapeRB).pack()
        self.myShapeS = ttk.Radiobutton(shapeFrame, text='SinExp',   value='SinExp',   variable=self.shapeVal, command=self.GetShapeRB).pack()
        self.myShapeC = ttk.Radiobutton(shapeFrame, text='CosExp',   value='CosExp',   variable=self.shapeVal, command=self.GetShapeRB).pack()
        shapeFrame.grid(row=elementRow, column=1, rowspan=3, sticky='nsew', pady=10)

        self.precon = ttk.Label(self.funcFrame, text='precon')
        self.precon.grid (row=elementRow, column=2, columnspan=1, sticky='n', pady=10)
        self.myEntry=ttk.Entry(self.funcFrame)
        self.myEntry.grid(row=elementRow, column=2, columnspan=1, sticky='s', pady=10)
        self.myEntry.insert(0, preconNum)

        self.funcFrame.grid(row=elementRow, column=elementCol)

    def GetDimRB(self):
        print(self.dimVal.get())

    def GetShapeRB(self):
        print(self.shapeVal.get())
