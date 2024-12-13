# -----------------------------------------------------------------------------
# Call R_GAM_GeoSAMS.r to compute the residuals and predict. 
# The residuals will be on the survey grid, predict on the region grid
#
# INPUTS: 
# Growth Estimates: X_Y_<PARAM>_DNYYYY_[GB | MA].csv
# Region Grid: [GB | MA]xyzLatLonRgn.csv
#
# -----------------------------------------------------------------------------
# Interim results
# -----------------------------------------------------------------------------
# OUTPUTS
# Residuals to be read in for IDW interpolation: X_Y_<PARAM>_DNYYYY_[GB | MA]_IDW.csv
# Prediction for further processing:  X_Y_<PARAM>_DNYYYY_[GB | MA]_PRED.csv
#
# Call InverseDistanceWeighting to interpolate the residuals onto the region grid
# INPUTS: 
# Residuals:  X_Y_<PARAM>_DNYYYY_[GB | MA]_IDW.csv
# Region Grid: [GB | MA]xyzLatLonRgn.csv
# 
# Add together predict values and interpolated residual values at each grid location, i.e. 
# X_Y_<PARAM>_DNYYYY_[GB | MA]_PRED.csv + Lat_Lon_Grid_<PARAM>_DNYYYY_[GB | MA]_IDW.csv
# -----------------------------------------------------------------------------
# Final Output Results are written to
#   Results/Lat_Lon_Grid_<PARAM>_DNYYYY_[GB | MA]_IDW.csv
# -----------------------------------------------------------------------------

import subprocess
import os
import sys
sys.path.append("PythonScripts/GUI/GeoSAM")
from InverseDistanceWeigthing import *

obsFile = sys.argv[1]
gridFile = sys.argv[2]

print(obsFile, gridFile)

# Compute Residuals
ex = os.path.join('GAM', 'R_GAM_GeoSAMS.r')
cmd = ['Rscript', ex, obsFile, gridFile ]
result = subprocess.run(cmd)

idw = InverseDistanceWeighting(obsFile, gridFile)
idw.Interpolate()

sys.exit(0)
