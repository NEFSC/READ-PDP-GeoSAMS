import platform
import tkinter as tk

configDir = 'Configuration'
interCfgDir = 'Interpolation'
simCfgDir = 'Simulation'
specAccCfgDir = 'SpecialAccess'
surveyDataDir = 'OriginalData'
comboTFStr = ['T', 'F']
cornerLabelArr = ['Corner', 'Long', 'Lat ', '0.0', '0.0']
frameWidth = 400
frameHeight= 200
scrollFrameHeight = 600
helpXoffset = 700
helpYoffset = 50
meters_per_naut_mile = 1852
grid_area_sqm = meters_per_naut_mile**2
ABUN = 'ABUN_'
BIOM = 'BIOM_'
EBMS = 'EBMS_'
FEFF = 'FEFF_'
FMOR = 'FMOR_'
LAND = 'LAND_'
LNDW = 'LNDW_'
LPUE = 'LPUE_'
RECR = 'RECR_'

if platform.system() == 'Windows':
    scrollFrameWidth = 900
    geometryStr = '920x725+10+10'
else:
    scrollFrameWidth = 1200
    geometryStr = '1200x725+10+10'

def DetermineUnitsScale(desiredParam):
    if desiredParam == ABUN:
        return('K Scallops/m2', 1e-3)
    elif desiredParam == BIOM:
        return('metric tons', 1.0)
    elif desiredParam == EBMS:
        return('metric tons', 1.0)
    elif desiredParam == FEFF:
        return('average', 1.0)
    elif desiredParam == FMOR:
        return('average', 1.0)
    elif desiredParam == LAND:
        return('Scallops', 1.0)
    elif desiredParam == LNDW:
        return('grams', 1.0)
    elif desiredParam == LPUE:
        return('land/day', 1.0)
    elif desiredParam == RECR:
        return('scallops/m2' , 1.0)
    else:
        return(f'Unknown Parameter {desiredParam}', 0)
    
##
#
def UpdateEntry(entry, val):
    entry.delete(0,tk.END)
    entry.insert(0, val)
