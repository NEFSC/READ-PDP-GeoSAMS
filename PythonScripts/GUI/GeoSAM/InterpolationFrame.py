#======================================================================================================
## @page page4 Interpolation Frame
# @section m2p1 Implementation of Interpolation
#
# @subsection m2p1p1 Interpolation Algorithm
# The interpolation of geospatial data is carried out via an Universal Kriging (UK) algorithm. 
#
# @subsubsection m2p1p1p1 Universal Kriging
# Universal kriging (UK) is a generalization of ordinary kriging in which a set of spatial functions are used to model the
# trend of a set of point observations.  The underlying model is:
# @f[
# f(x,y,H(x,y),\lambda)=\sum_{k=1}^{n_f} f_k(x,y,H(x,y),\lambda_k) +\epsilon(x,y) 
# @f] 
# where @f$f_k@f$ are the known spatial functions and @f$\epsilon(x,y)@f$ is a zero mean, spatially correlated,  stationary random 
# process with semi-variogram @f$\gamma(s)@f$. For a summary of UK see @cite Cressie 1993, pages 151 -180.\n
# The spatially variable @f$x@f$ here is taken to include latitude, longitude and, bathymetric depth(@f$x=[lat,lon,z(lat,lon)]@f$). 
# 
# @subsubsection m2p1p1p2 Spatial functions
# The spatial functions (SF) used here are  a set of one dimensional, bounded, C-infinity functions with two parameters, 
# 
# Gaussian Bump:
# @f[
# f_a (s,\lambda,x_0) = \exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
# @f]
# Logistic curve:
# @f[
# f_b (s,\lambda,x_0) = \frac{1}{1+\exp( -\frac{s-x_0}{\lambda} ) }
# @f]
# Sin Exp curve:
# @f[
# f_c (s,\lambda,x_0) = \sin(\frac{s-x_0}{\lambda})\exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
# @f]
# Cos Exp curve:
# @f[
# f_c (s,\lambda,x_0) = \cos(\frac{s-x_0}{\lambda})\exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
# @f]
#
# In all of the function form @f$\lambda@f$ controls the width of the transition and @f$x_0@f$ the transition point. 
# 
# After fitting these to the bathymetric variable (H) we can introduce interaction. Allowing interaction terms for the 
# spatial functions depending on bathymetry only we can define, @f$g_j(x,H,\lambda^j,{x_0}^j,\lambda_k,{x_0}^k)=f_j(x)f_k ( H )@f$
# @f[
# f(x,y,H)=\sum_i f_i(H,\lambda^i,z_0^i) + \sum_j f_{j_x}(x,\lambda^{j_x},x_0^{j_x}) f_k(z,\lambda^k,x_0^k)+ \sum_j f_{j_y}(y,\lambda^{j_y},x_0^{j_y}) f_k(z,\lambda^k,x_0^k)
# @f]
#
# Here @f$z@f$ is bathymetric depth. We start by fitting nonlinear parameters @f$\lambda^{c,s}@f$ and @f$x_0^{c,s}@f$ to log 
# recruitment for "cross shelf" structure.  
# @f[
# f(x,y,z)=\beta_0+\sum_i \beta_i f_i(z) + \sum_j \beta_j g_j(x,z)+\sum_k \beta_k g_k(y,z)+ \epsilon
# @f]
# where @f$\beta_i@f$ are coefficients for the spatial functions and @f$\epsilon@f$ is the zero mean noise process associated with UK.
# 
# @subsubsection m2p1p1p3 Fitting non-linear parameters
# A brute force approach is taken to fitting the nonlinear parameters @f$x_0@f$ and @f$\lambda@f$.  A search range is 
# determined based on the geographic range of the observations.  The parameters are then fit to minimize the misfit to 
# observations. 
#
# Subroutine @a NLSF_Fit_Function parameter np).  The nonlinear parameters are fit by minimizing RMS misfit to the simple least 
# squares fit with a smoothness penalty,
# @f[
# J(x_0,\lambda)=\sqrt{ \frac{1}{n} \sum_i (d_i-a - b f(x_i|\lambda,x_0))^2 }+S(\lambda,x_0)
# @f]
# Where @f$S(\lambda,x_0)=\int_{-\infty}^\infty f''(x) ^2 d x= S(\lambda)@f$ is a roughness penalty, @f$a@f$ and @f$b@f$ are 
# temporarily assigned (by least squares) constants fit to minimize @f$J@f$.  @f$S@f$ is proportional to @f$\lambda^{-3}@f$ 
# for all examples used here (see subroutine @a NLSF_Smooth_Penalty).  Other one dimensional function forms can be added to the 
# software in subroutine @a NLSF_Eval_Semivariance and @a NLSFFuncPen.
# 
# A smoothness penalty is imposed for each function based on the analytic 
# 
# @subsection m2p1p2 Residual process
# After performing an ordinary least squares fit for the SF coeficients, @f$\beta@f$, we have an estimate of @f$\epsilon@f$. 
# An empirical variogram is computed subroutine @a Krig_Comp_Emp_Variogram, and variogram parameters are fit (again by brute force).  
# The variogram forms allowed are "spherical", "exponential", and "gaussian".  The form is defaults to 'spherical' if not specified 
# by the UK configuration file.
#
# @subsubsection m2p1p2p1 Posterior sampling
# With the fitting of the residual we have a covariance for @f$\epsilon@f$ and the estimation problem becomes one of 
# Generalized Least Squares (LSF_Generalized_Least_Squares).  Posterior sampling is then conducted  achieved posterior sampling is Treating the TBD
#
# @section m2p2 Non Linear Spatial function fitting for UK
# The universal kriging algorithm described above is used to build a distribution based on the historical recruitment data 
# (1979-present).  Spatial function forms of one variable were selected for smoothness and boundedness. We have:\n
# Gaussian bump
# 
# The nonlinear parameters are fit by minimizing RMS misfit to the simple least squares fit. 
# @f[
# J(x_0,\lambda)=\sqrt{ \frac{1}{n} \sum_i (d_i-a - b f(x_i|\lambda,x_0))^2 }+S(\lambda,x_0)
# @f]
# Where @f$S(\lambda,x_0)=\int_{-\infty}^\infty f''(x) ^2 d x@f$ is a roughness penalty, @f$a@f$ and @f$b@f$ are temporarily 
# assigned constants fit to minimize @f$J@f$.  @f$S@f$ is proportional to @f$\lambda^{-3}@f$ for all examples used here.  
# Other one dimensional function forms can be added to the software in subroutine @a NLSF_Eval_Semivariance and @a NLSFFuncPen.
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

#======================================================================================================
##
# This class is used to present the parameters to the user to customize how the interpolation is performed.\n
# Testing has shown that 
# - MA works best with 9 spatial functions
# - GB works best with 5 spatial functions

#======================================================================================================
class Interpolation(ttk.Frame):
    def __init__(self, container, get):
        super().__init__()

        self.okToRepaintFunctions = True
        self.nsfMax = 20
        self.functions = [None for _ in range(self.nsfMax)]
        self.myget = get # pointer function to domain name entry
        self.domainName = self.myget()
        if self.domainName == 'MA':
            self.nsf = 9
        else:
            self.nsf = 5
        self.style = ttk.Style()
        self.style.configure('Interpolation.TFrame', borderwidth=10, relief='solid', labelmargins=20)
        self.style.configure('Interpolation.TFrame.Label', font=('courier', 8, 'bold'))

        self.scrollFrame = ScrollFrame(self) # add a new scrollable frame.

        # --------------------------------------------------------------------------------------------------------
        self.funcFrame = ttk.LabelFrame(self.scrollFrame.viewPort, text='Spatial Functions', style='Interpolation.TFrame', width=400, height=200)

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFncsLabel = ttk.Label(self.funcFrame, text='# of Functions')
        self.numFncsLabel.grid(row=0, column=0, sticky='w')
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFcnsEntry=ttk.Entry(self.funcFrame, validatecommand=numbersCallback)
        self.numFcnsEntry.insert(0, str(self.nsf))
        self.numFcnsEntry.grid(row=0, column=0, sticky='e')
        reg=self.numFcnsEntry.register(numbersCallback)
        self.numFcnsEntry.configure(validate='key', validatecommand=(reg, '%P'))
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        self.numFncsButton = ttk.Button(self.funcFrame, text='Update', command=self.NumFuncsUpdate)
        self.numFncsButton.grid(row=0, column=1)
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # The grid manager keep all widgets once defined. We are just going to decide which are shown
        # paint first row to show x, y, and z
        # remaining rows are just x, y
        desNumCol = 2
        for i in range(self.nsfMax):
            if i < 3:
                row = 1
                precon = 0
                self.functions[i] = SubFrameInterpFunction(self, self.funcFrame, str(i+1), 'z', 'Logistic', precon, row, i%3)
            else:
                row = ((i-3) // desNumCol) + 2
                col = (i-3) % desNumCol
                precon = row - 1
                self.functions[i] = SubFrameInterpFunction(self, self.funcFrame, str(i+1), chr(120+col), 'Logistic', precon, row, col)
        self.functions[1].dimVal.set('x')
        self.functions[2].dimVal.set('y')
        # Now hide undesired funtion definitions
        for i in range(self.nsf, self.nsfMax):
            self.functions[i].funcFrame.grid_remove()

        self.funcFrame.grid(row=0, column=0, sticky='n')
        # --------------------------------------------------------------------------------------------------------
        # --------------------------------------------------------------------------------------------------------
        paramFrame= ttk.LabelFrame(self.scrollFrame.viewPort, text='Parameters', style='Interpolation.TFrame')
        self.highLimit   = SubFrameElement(self, paramFrame, 'High Limit Factor ', '1.5',  0, 0, 1)
        self.form = SubFrameElement(self, paramFrame, 'variogram form', 'spherical', 1, 0, 1)
        self.useLogTrans  = SubFrameElement(self, paramFrame, 'Use Log Transfrom', 'True', 2, 0, 1)
        self.powerTrans  = SubFrameElement(self, paramFrame, 'Power Tranform\n(Not used if Log = True)', '1.0', 3, 0, 1)
        self.spatCfgFile  = SubFrameElement(self, paramFrame, 'Spatial Fcn Config File', 'SpatialFcns.cfg', 4, 0, 1)
        paramFrame.grid(row=0, column=4, sticky='n')
        # --------------------------------------------------------------------------------------------------------
        self.scrollFrame.grid(row=0, column=0, sticky='nsew')

        self.bind("<Visibility>", self.on_visibility)

    #--------------------------------------------------------------------------------------------------
    ## 
    # This method is used to update widgets each time the user switches to this tab
    #
    #--------------------------------------------------------------------------------------------------
    def on_visibility(self, event):
            if self.okToRepaintFunctions:
                self.domainName = self.myget()
                self.numFcnsEntry.delete(0,2)
                if self.domainName == 'MA':
                    self.nsf = 9
                else:
                    self.nsf = 5
                self.numFcnsEntry.insert(0, str(self.nsf))

                # Now update desired funtion definitions
                for i in range(self.nsfMax):
                    self.functions[i].funcFrame.grid_remove()
                for i in range(self.nsf):
                    self.functions[i].funcFrame.grid()

    #--------------------------------------------------------------------------------------------------
    ##
    # This method updates the number of spatial functions. Overrides default value for MA and GB
    #
    #--------------------------------------------------------------------------------------------------
    def NumFuncsUpdate(self):
        # Once a new value is manually enterred, prevent changing tabs from repainting spatial fucntions
        self.okToRepaintFunctions = False            
        for i in range(self.nsfMax):
            self.functions[i].funcFrame.grid_remove()

        n = int(self.numFcnsEntry.get())
        if n > self.nsfMax:
            messagebox.showerror("Number of Spatial functions", f'Max is {self.nsfMax}\nSetting to max')
            n = self.nsfMax
            self.numFcnsEntry.delete(0,3)
            self.numFcnsEntry.insert(0, str(n))
        self.nsf = n
        # Now update desired funtion definitions
        for i in range(self.nsfMax):
            self.functions[i].funcFrame.grid_remove()
        for i in range(self.nsf):
            self.functions[i].funcFrame.grid()
