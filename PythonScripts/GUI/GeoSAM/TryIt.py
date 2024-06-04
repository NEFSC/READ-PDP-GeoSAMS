# import tkinter as tk
# import re

# class CustomText(tk.Text):
#     '''A text widget with a new method, HighlightPattern 

#     example:

#     text = CustomText()
#     text.tag_configure("red",foreground="#ff0000")
#     text.HighlightPattern("this should be red", "red")

#     The HighlightPattern method is a simplified python 
#     version of the tcl code at http://wiki.tcl.tk/3246
#     '''
#     def __init__(self, *args, **kwargs):
#         tk.Text.__init__(self, *args, **kwargs)

#     def HighlightPattern(self, pattern, tag, start="1.0", end="end", regexp=True):
#         '''Apply the given tag to all text that matches the given pattern'''

#         start = self.index(start)
#         end = self.index(end)
#         self.mark_set("matchStart",start)
#         self.mark_set("matchEnd",end)
#         self.mark_set("searchLimit", end)

#         count = tk.IntVar()
#         while True:
#             index = self.search(pattern, "matchEnd","searchLimit",count=count, regexp=regexp)
#             if index == "": break
#             self.mark_set("matchStart", index)
#             self.mark_set("matchEnd", "%s+%sc" % (index,count.get()))
#             self.tag_add(tag, "matchStart","matchEnd")

# def aboutF():
#      win = tk.Toplevel()
#      win.title("About")
#      about = '''Top/bottom 3 - Reports only the top/bottom 3 rows for a param you will later specify.
#         Set noise threshold - Filters results with deltas below the specified noise threshold in ps.
#         Sort output - Sorts by test,pre,post,unit,delta,abs(delta).
#         Top 2 IDD2P/IDD6 registers - Reports only the top 2 IDD2P/IDD6 registers.
#         Only critical registers - Reports only critical registers.
#         Use tilda output format - Converts the output file from csv to tilda.
#         Use html output format - Converts the output file from csv to html.'''
#      about = re.sub("\n\s*", "\n", about) # remove leading whitespace from each line
#      t=CustomText(win, wrap="word", width=100, height=10, borderwidth=0)
#      t.tag_configure("blue", foreground="blue")
#      t.pack(sid="top",fill="both",expand=True)
#      t.insert("1.0", about)
#      t.HighlightPattern("^.*? - ", "blue")
#      tk.Button(win, text='OK', command=win.destroy).pack()

# root=tk.Tk()
# aboutF()
# root.mainloop()

import tkinter as tk
 
 
root = tk.Tk()
 
# # specify size of window.
root.geometry("250x170")
 
# Create text widget and specify size.
T = tk.Text(root, height = 5, width = 52)
 
# Create label
l = tk.Label(root, text = "Fact of the Day")
l.config(font =("Courier", 14))
 
Fact = """A man can be arrested in
Italy for wearing a skirt in public."""
 
# Create button for next text.
b1 = tk.Button(root, text = "Next", )
 
# Create an Exit button.
b2 = tk.Button(root, text = "Exit",
            command = root.destroy) 
 
l.pack()
T.pack()
b1.pack()
b2.pack()
 
# Insert The Fact.
T.insert(tk.END, Fact)
 
tk.mainloop()