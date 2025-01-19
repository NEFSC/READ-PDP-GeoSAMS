############Note############
###v1: 12/17/2024: a. kriging by modeling region, region defined by provided shapefiles
###                b. spherical, utm, isotropic kriging model
###
###v2: 01/12/2025: a. no model will be estimated for positive data <= 5%
############################

import sys
import os
import pandas as pd
import numpy as np
import statistics
from pykrige.ok import OrdinaryKriging

# Get Input Files
inputFile = sys.argv[1]
surveyFile = os.path.join('Data', inputFile+'_BUFFER_RESID.csv')
predFile = os.path.join('Data', inputFile+'_BUFFER_PRED.csv')

outFile = os.path.join('Results', 'Lat_Lon_Grid_'+inputFile[4:]+'_REGION_KRIGE.csv')
print('Reading: ', surveyFile, predFile)

### read in data

#df = pd.read_csv('X_Y_FMOR_AL2026_MA_BUFFER_RESID.csv') #survey data with gam residuals
df = pd.read_csv(surveyFile)  #survey data with gam residuals


#df_grid = pd.read_csv('X_Y_FMOR_AL2026_MA_BUFFER_PRED.csv') #prediction grid with gam predictions
df_grid = pd.read_csv(predFile) #prediction grid with gam predictions
####print('GRID HEADER   :', df_grid.columns.tolist())
####print('GRID LENGTH   :', len(df_grid))

#start the data frame to store final result with grids outside of modeling regions
df_grid_result = df_grid[df_grid['REGION'].isna()]

### perform OK using the spherical/exponential variogram model

df_region = df['REGION']
region = pd.Series(df_region.dropna()).unique() #obtain unique values of regions

for i in region:
    #buffer here is the buffer of the region, use to query data outside of the modeling region
    df_temp = df[df['BUFFER']==i] 
    #prediction grid within the modeling region
    df_grid_temp = df_grid[df_grid['REGION']==i] 
    if sum(df_temp['GAMPREDICTM2'])==0:
        # assign 0s back to the temp file
        df_grid_temp = df_grid_temp.assign(KRIGEPREDICTM2=0, KRIGEVAR=0)
        # concat the outputs from different regions
        df_grid_result = pd.concat([df_grid_result, df_grid_temp], ignore_index=True)
    else:
        #use utm to do kriging so that the range parameter is meanful in this case
        utm_y = df_temp['UTM_Y'].values 
        utm_x = df_temp['UTM_X'].values
        resi_value = df_temp['GAMRESIDUALM2'].values

        grid_x = df_grid_temp['UTM_X'].tolist()
        grid_y = df_grid_temp['UTM_Y'].tolist()
        #need to change the type of the utms otherwise kriging function read in as a mask, not sure why
        grid_x = np.array(grid_x, dtype = np.float64) 
        grid_y = np.array(grid_y, dtype = np.float64)

        OK = OrdinaryKriging(utm_x,
                            utm_y,
                            resi_value,
                            coordinates_type= 'euclidean', #default is euclidean for interpolation on a plane
                            #coordinates_type= 'geographic', #for interpolation on a sphere, noted that the function expected lon 0 - 360 and lat -90 - 90 so will need to transform the lat/lon if one wants to do geographic
                            #variogram_model = 'exponential',
                            variogram_model = 'spherical',
                            verbose = True,
                            enable_plotting = False, #turn off to run without pausing by plots
                            nlags = 15, #tested 20 and 30 for whole gb or mab 30 was a little too much so was using 20, set 15 for smaller regions, didn't test further tho
                            pseudo_inv = True, #pseudo inverted kriging matrix to solve for estimates so that the estimates are more numerically stable
                            #pseudo_inv_type = 'pinvh', #use eigen-values or least square solution, doest seem to make a big difference, default is least square pinv
                            #weight=True, #didn't change the results much with weighting the points closer more important (the weights are hard-wired so not sure if it is the best)
                            exact_values = True) #true
        z_interp, ss = OK.execute('points', grid_x, grid_y,mask=False) #calcuate interpolated values for utm interpolation

        #if the kriging is not working, it will produce same values for the entire surface, so use mean and var of the interpolated values to check and see whether kriging is working
        print('SHOULD NOT BE THE SAME VALUES FOR ENTIRE SURFACE')
        print(statistics.mean(z_interp)) 
        print(statistics.variance(z_interp))

        #assign kriging results back to the temp file
        df_grid_temp = df_grid_temp.assign(KRIGEPREDICTM2 = z_interp, KRIGEVAR = ss)
        #concat the outputs from different regions
        df_grid_result = pd.concat([df_grid_result, df_grid_temp], ignore_index=True) 

#add two columns to calculate the final results
df_grid_result = df_grid_result.assign(
    FINALPREDICTM2 = df_grid_result['GAMPREDICTM2'] + df_grid_result['KRIGEPREDICTM2'],
    FINALPREDICT = (df_grid_result['GAMPREDICTM2'] + df_grid_result['KRIGEPREDICTM2']) * (1.852**2)) 
#if the FINALPREDICT is negative, set the value to zero
df_grid_result['FINALPREDICT'] = df_grid_result['FINALPREDICT'].apply(lambda x : (x if x > 0 else 0)) 


#df_grid_result.to_csv('MAxyzLatLonRgn_REGION_GAM_KRIGE.csv',sep = ',',index=False) #output the prediction grid
print('Writing results to: ', outFile)
df_grid_result.to_csv(outFile,sep = ',',index=False) #output the prediction grid
