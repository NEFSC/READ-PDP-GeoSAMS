import tkinter as tk
import platform
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

class SubFrameElement(tk.Frame):
    def __init__(self, container, parent, label, value, elementRow, labelCol, entryCol, valCmd=None):
        super().__init__()
        self.myEntry=ttk.Entry(parent, validatecommand=valCmd)
        self.myEntry.grid(row=elementRow, column=entryCol, rowspan=1, sticky='n', pady=10)
        self.myEntry.insert(0, value)
        self.myLabel = ttk.Label(parent, text=label, wraplength=200, anchor='n', justify='right')
        self.myLabel.grid(row=elementRow, column=labelCol, rowspan=1, sticky='n', pady=10)


class SubFrameInterpFunction(tk.Frame):
    def __init__(self, container, parent, funcNum, dim, shape, preconNum, elementRow, elementCol):
        super().__init__()

        self.funcFrame = ttk.LabelFrame(parent, text='Function '+str(funcNum))
        dimFrame = ttk.LabelFrame(self.funcFrame, text='dim')
        self.dimVal = tk.StringVar(dimFrame, dim)
        self.myDimRBx = ttk.Radiobutton(dimFrame, text='x', value='x', variable=self.dimVal).pack()
        self.myDimRBy = ttk.Radiobutton(dimFrame, text='y', value='y', variable=self.dimVal).pack()
        self.myDimRBz = ttk.Radiobutton(dimFrame, text='z', value='z', variable=self.dimVal).pack()
        dimFrame.grid(row=elementRow, column=0, rowspan=3, sticky='nsew', pady=10)

        shapeFrame = ttk.LabelFrame(self.funcFrame, text='shape')
        self.shapeVal = tk.StringVar(shapeFrame, shape)
        self.myShapeG = ttk.Radiobutton(shapeFrame, text='Gaussian', value='Gaussian', variable=self.shapeVal).pack()
        self.myShapeL = ttk.Radiobutton(shapeFrame, text='Logistic', value='Logistic', variable=self.shapeVal).pack()
        self.myShapeS = ttk.Radiobutton(shapeFrame, text='SinExp',   value='SinExp',   variable=self.shapeVal).pack()
        self.myShapeC = ttk.Radiobutton(shapeFrame, text='CosExp',   value='CosExp',   variable=self.shapeVal).pack()
        shapeFrame.grid(row=elementRow, column=1, rowspan=3, sticky='nsew', pady=10)

        self.precon = ttk.Label(self.funcFrame, text='precon')
        self.precon.grid (row=elementRow, column=2, columnspan=1, sticky='n', pady=10)
        self.myEntry=ttk.Entry(self.funcFrame)
        self.myEntry.grid(row=elementRow, column=2, columnspan=1, sticky='s', pady=10)
        self.myEntry.insert(0, preconNum)

        self.funcFrame.grid(row=elementRow, column=elementCol)

class SubFrameLongLat(tk.Frame):
    def __init__(self, container, parent, cornerNum, long, lat, elementRow, elementCol):
        super().__init__()
        self.cornerFrame = ttk.LabelFrame(parent, text='Corner '+str(cornerNum))
        self.longitude  = SubFrameElement(self, self.cornerFrame, 'Long ', long,  0, 0, 1)
        self.latitude   = SubFrameElement(self, self.cornerFrame, 'Lat ',  lat,   1, 0, 1)
        self.cornerFrame.grid(row=elementRow, column=elementCol)


class SubFrameArea(tk.Frame):
    def __init__(self, container, parent, areaNum, numCorners, numCornersMax, elementRow, elementCol):
        super().__init__()
        self.numCorners = numCorners
        self.numCornersMax = numCornersMax
        self.corners = [None for i in range(self.numCornersMax)]

        self.areaFrame = ttk.LabelFrame(parent, text='Area '+str(areaNum+1))
        self.numCornersEntry = SubFrameElement(self, self.areaFrame, '# corners ',  numCorners,   elementRow, 0, 1, valCmd=numbersCallback)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numCornersButton = ttk.Button(self.areaFrame, text='Update', command=self.NumCornersUpdate)
        self.numCornersButton.grid(row=elementRow, column=0, sticky='s')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        for i in range(numCornersMax):
            self.corners[i] = SubFrameLongLat(self, self.areaFrame, str(i+1), '-70', '45', elementRow, i+2)
        # now hide unwanted corners
        for i in range(numCorners, numCornersMax):
            self.corners[i].cornerFrame.grid_remove()

        self.areaFrame.grid(row=elementRow, column=elementCol, sticky='w')

    def NumCornersUpdate(self):
        for i in range(self.numCornersMax):
            self.corners[i].cornerFrame.grid_remove()

        n = int(self.numCornersEntry.myEntry.get())
        if n > self.numCornersMax:
            messagebox.showerror("Number of Corners", f'Max is {self.numCornersMax}\nSetting to max')
            n = self.numCornersMax
            self.numCornersEntry.myEntry.delete(0,3)
            self.numCornersEntry.myEntry.insert(0, str(n))
        self.numCorners = n
        # Now update desired funtion definitions
        for i in range(self.numCornersMax):
            self.corners[i].cornerFrame.grid_remove()
        for i in range(self.numCorners):
            self.corners[i].cornerFrame.grid()


# ************************
# Scrollable Frame Class
# from https://gist.github.com/mp035/9f2027c3ef9172264532fcd6262f3b01
# ************************
class ScrollFrame(tk.Frame):
    def __init__(self, parent):
        super().__init__(parent) # create a frame (self)

        self.canvas = tk.Canvas(self, borderwidth=0, background="#ffffff")          #place canvas on self
        #self.canvas.config(width=1050, height=600)
        self.viewPort = tk.Frame(self.canvas, background="#ffffff")                    #place a frame on the canvas, this frame will hold the child widgets 
        vsb = tk.Scrollbar(self, orient="vertical", command=self.canvas.yview) #place a scrollbar on self 
        #self.canvas.configure(yscrollcommand=vsb.set)                          #attach scrollbar action to scroll of canvas

        hsb = tk.Scrollbar(self, orient="horizontal", command=self.canvas.xview) #place a scrollbar on self 
        self.canvas.config(width=1050, height=600, xscrollcommand=hsb.set, yscrollcommand=vsb.set)  #attach scrollbar action to scroll of canvas

        #vsb.pack(side="right", fill="y")                                       #pack scrollbar to right of self
        vsb.grid(row=0, rowspan=20, column=6, sticky='ns')
        hsb.grid(row=10, column=0, sticky='ew')
        #self.canvas.pack(side="left", fill="both", expand=True)                     #pack canvas to left of self and expand to fil
        self.canvas.grid(row=0, column=0,sticky='nsew')

        self.canvas_window = self.canvas.create_window((4,4), window=self.viewPort, anchor="nw", tags="self.viewPort") #add view port frame to canvas

        self.viewPort.bind("<Configure>", self.onFrameConfigure)                    #bind an event whenever the size of the viewPort frame changes.
            
        self.viewPort.bind('<Enter>', self.onEnter)                                 # bind wheel events when the cursor enters the control
        self.viewPort.bind('<Leave>', self.onLeave)                                 # unbind wheel events when the cursorl leaves the control

        self.onFrameConfigure(None)                                                 #perform an initial stretch on render, otherwise the scroll region has a tiny border until the first resize

    def onFrameConfigure(self, event):                                              
        '''Reset the scroll region to encompass the inner frame'''
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))                 #whenever the size of the frame changes, alter the scroll region respectively.

    def onMouseWheel(self, event):                                                  # cross platform scroll wheel event
        if platform.system() == 'Windows':
            self.canvas.yview_scroll(int(-1* (event.delta/120)), "units")
        elif platform.system() == 'Darwin':
            self.canvas.yview_scroll(int(-1 * event.delta), "units")
        else:
            if event.num == 4:
                self.canvas.yview_scroll( -1, "units" )
            elif event.num == 5:
                self.canvas.yview_scroll( 1, "units" )
    
    def onEnter(self, event):                                                       # bind wheel events when the cursor enters the control
        if platform.system() == 'Linux':
            self.canvas.bind_all("<Button-4>", self.onMouseWheel)
            self.canvas.bind_all("<Button-5>", self.onMouseWheel)
        else:
            self.canvas.bind_all("<MouseWheel>", self.onMouseWheel)

    def onLeave(self, event):                                                       # unbind wheel events when the cursorl leaves the control
        if platform.system() == 'Linux':
            self.canvas.unbind_all("<Button-4>")
            self.canvas.unbind_all("<Button-5>")
        else:
            self.canvas.unbind_all("<MouseWheel>")

def numbersCallback(input):
    """Only allows numeric for input """
    if input.isdigit(): return True  
    elif input == ".": return True
    elif input == "": return True
    else: return False

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
