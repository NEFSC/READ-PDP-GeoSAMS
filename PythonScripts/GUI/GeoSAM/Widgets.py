import tkinter as tk
import platform
from tkinter import ttk

# *************************************************************************************************
# *************************************************************************************************
class SubFrameElement(tk.Frame):
    def __init__(self, container, parent, label, value, elementRow, labelCol, entryCol, width=5,
                 valCmd=None, enterCmd=None):
        super().__init__()
        if valCmd == None: self.myEntry=ttk.Entry(parent, width=width)
        else: 
            self.myEntry=ttk.Entry(parent, validatecommand=valCmd, width=width)
            reg=self.myEntry.register(valCmd)
            self.myEntry.configure(validate='key', validatecommand=(reg, '%P'))
        
        if not enterCmd == None:
            self.myEntry.focus()
            self.myEntry.bind('<Return>', enterCmd)

        self.myEntry.insert(0, value)
        self.myEntry.grid(row=elementRow, column=entryCol, sticky='n', padx=5, pady=10)
        self.myLabel = ttk.Label(parent, text=label, wraplength=200, anchor='n', justify='right')
        self.myLabel.grid(row=elementRow, column=labelCol, sticky='n', padx=5, pady=10)

# *************************************************************************************************
# *************************************************************************************************
class SubFrameInterpFunction(tk.Frame):
    def __init__(self, container, parent, funcNum, dim, shape, preconNum, elementRow, elementCol):
        super().__init__()

        self.funcFrame = ttk.LabelFrame(parent, text='Function '+str(funcNum))
        dimFrame = ttk.LabelFrame(self.funcFrame, text='dim')
        self.dimVal = tk.StringVar(dimFrame, dim)
        self.myDimRBx = ttk.Radiobutton(dimFrame, text='x', value='x', variable=self.dimVal).pack()
        self.myDimRBy = ttk.Radiobutton(dimFrame, text='y', value='y', variable=self.dimVal).pack()
        self.myDimRBz = ttk.Radiobutton(dimFrame, text='z', value='z', variable=self.dimVal).pack()
        dimFrame.grid(row=elementRow, column=0, rowspan=3, sticky='nsew', padx=5, pady=10)

        shapeFrame = ttk.LabelFrame(self.funcFrame, text='shape')
        self.shapeVal = tk.StringVar(shapeFrame, shape)
        self.myShapeG = ttk.Radiobutton(shapeFrame, text='Gaussian', value='Gaussian', variable=self.shapeVal).pack()
        self.myShapeL = ttk.Radiobutton(shapeFrame, text='Logistic', value='Logistic', variable=self.shapeVal).pack()
        self.myShapeS = ttk.Radiobutton(shapeFrame, text='SinExp',   value='SinExp',   variable=self.shapeVal).pack()
        self.myShapeC = ttk.Radiobutton(shapeFrame, text='CosExp',   value='CosExp',   variable=self.shapeVal).pack()
        shapeFrame.grid(row=elementRow, column=1, rowspan=3, sticky='nsew', padx=5, pady=10)

        self.preconLabel = ttk.Label(self.funcFrame, text='precon')
        self.preconLabel.grid (row=elementRow, column=2, columnspan=1, sticky='n', padx=5, pady=10)
        self.preconEntry=ttk.Entry(self.funcFrame, width=5)
        self.preconEntry.grid(row=elementRow, column=2, columnspan=1, sticky='s', padx=5, pady=10)
        self.preconEntry.insert(0, preconNum)

        self.funcFrame.grid(row=elementRow, column=elementCol)

# *************************************************************************************************
# *************************************************************************************************
class SubFrameXY(tk.Frame):
    def __init__(self, container, parent, fieldNum, elementRow, elementCol, lableArr):
        super().__init__()
        fieldText = lableArr[0]
        xText = lableArr[1]
        yText = lableArr[2]
        xVal = lableArr[3]
        yVal = lableArr[4]

        self.cornerFrame = ttk.LabelFrame(parent, text=fieldText+str(fieldNum))
        self.longitude  = SubFrameElement(self, self.cornerFrame, xText, xVal,  0, 0, 1, width=10)
        self.latitude   = SubFrameElement(self, self.cornerFrame, yText, yVal,   1, 0, 1, width=10)
        self.cornerFrame.grid(row=elementRow, column=elementCol, padx=5)


# ************************
# Scrollable Frame Class
# from https://gist.github.com/mp035/9f2027c3ef9172264532fcd6262f3b01
# ************************
class ScrollFrame(tk.Frame):
    def __init__(self, parent):
        super().__init__(parent) # create a frame (self)

        self.canvas = tk.Canvas(self, borderwidth=0, background="#ffffff")          #place canvas on self
        self.viewPort = tk.Frame(self.canvas, background="#ffffff")                    #place a frame on the canvas, this frame will hold the child widgets 
        vsb = tk.Scrollbar(self, orient="vertical", command=self.canvas.yview) #place a scrollbar on self 
        hsb = tk.Scrollbar(self, orient="horizontal", command=self.canvas.xview) #place a scrollbar on self 
        self.canvas.config(width=1000, height=600, xscrollcommand=hsb.set, yscrollcommand=vsb.set)  #attach scrollbar action to scroll of canvas

        vsb.grid(row=0, rowspan=20, column=6, sticky='ns')
        hsb.grid(row=10, column=0, sticky='ew')
        self.canvas.grid(row=0, column=0,sticky='nsew')

        self.canvas_window = self.canvas.create_window((4,4), window=self.viewPort, anchor="nw", tags="self.viewPort") #add view port frame to canvas

        self.viewPort.bind("<Configure>", self.onFrameConfigure) #bind an event whenever the size of the viewPort frame changes.
        self.viewPort.bind('<Enter>', self.onEnter)              # bind wheel events when the cursor enters the control
        self.viewPort.bind('<Leave>', self.onLeave)              # unbind wheel events when the cursorl leaves the control

        self.onFrameConfigure(None)                              #perform an initial stretch on render, otherwise the scroll region has a tiny border until the first resize

    #---------------------------------------------------------------------------------
    ## whenever the size of the frame changes, alter the scroll region respectively.
    #---------------------------------------------------------------------------------
    def onFrameConfigure(self, event):                 
        '''Reset the scroll region to encompass the inner frame'''
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))

    #---------------------------------------------------------------------------------
    ## cross platform scroll wheel event
    #---------------------------------------------------------------------------------
    def onMouseWheel(self, event):
        if platform.system() == 'Windows':
            self.canvas.yview_scroll(int(-1* (event.delta/120)), "units")
        elif platform.system() == 'Darwin':
            self.canvas.yview_scroll(int(-1 * event.delta), "units")
        else:
            if event.num == 4:
                self.canvas.yview_scroll( -1, "units" )
            elif event.num == 5:
                self.canvas.yview_scroll( 1, "units" )
    
    #---------------------------------------------------------------------------------
    ## bind wheel events when the cursor enters the control
    #
    #---------------------------------------------------------------------------------
    def onEnter(self, event):
        if platform.system() == 'Linux':
            self.canvas.bind_all("<Button-4>", self.onMouseWheel)
            self.canvas.bind_all("<Button-5>", self.onMouseWheel)
        else:
            self.canvas.bind_all("<MouseWheel>", self.onMouseWheel)

    #---------------------------------------------------------------------------------
    ## unbind wheel events when the cursorl leaves the control
    #
    #---------------------------------------------------------------------------------
    def onLeave(self, event):
        if platform.system() == 'Linux':
            self.canvas.unbind_all("<Button-4>")
            self.canvas.unbind_all("<Button-5>")
        else:
            self.canvas.unbind_all("<MouseWheel>")

# *************************************************************************************************
# *************************************************************************************************
def numbersCallback(input):
    """Only allows numeric for input """
    if input.isdigit(): return True  
    elif input == ".": return True
    elif input == "": return True
    else: return False

# *************************************************************************************************
# *************************************************************************************************
def floatCallback(inp):
    if input.isdigit(): return True  
    elif input == ".": return True
    elif input == "": return True
    else:
        try:
            float(inp)
        except:
            return False
        return True

