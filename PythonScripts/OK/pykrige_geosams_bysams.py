import pandas as pd
import numpy as np
import statistics
from pykrige.ok import OrdinaryKriging

# read in data

df = pd.read_csv('X_Y_BIOM_AL2022_GB_GAM_REGION.csv')
print(df.head())

df_grid = pd.read_csv('GBxyzLatLonRgn_GAM_REGION.csv')
print(df_grid.head())
print(len(df_grid))

# perform OK using the spherical variogram model

df_sams = df['NEWSAMS']
sams = pd.Series(df_sams.dropna()).unique()
print(sams)

df_grid_result = df_grid[df_grid['NEWSAMS'].isna()]
print(len(df_grid_result))

for i in sams:
    df_temp = df[df['NEWSAMS']==i]
    df_grid_temp = df_grid[df_grid['NEWSAMS']==i]

    utm_y = df_temp['UTM_Y'].values
    utm_x = df_temp['UTM_X'].values
    #lat = df_temp['LAT'].values
    #lon = df_temp['LON'].values
    resi_value = df_temp['GAMRESIDUAL'].values

    grid_x = df_grid_temp['UTM_X'].tolist()
    grid_y = df_grid_temp['UTM_Y'].tolist()
    grid_x = np.array(grid_x, dtype = np.float64)
    grid_y = np.array(grid_y, dtype = np.float64)
    #grid_lat = df_grid_temp['LAT'].values
    #grid_lon = df_grid_temp['LON'].values

    OK = OrdinaryKriging(utm_x,
                         utm_y,
                         resi_value,
                         #coordinates_type= 'geographic',
                         variogram_model = 'spherical',
                         verbose = True,
                         enable_plotting = True,
                         nlags = 20,
                         pseudo_inv = True,
                         pseudo_inv_type = 'pinvh',
                         #weight=True,
                         exact_values = True)
    z_interp, ss = OK.execute('points', grid_x, grid_y,mask=False) #calcuate interpolated values
    #z_interp, ss = OK.execute('points', grid_lon, grid_lat)  # calcuate interpolated values

    #print(z_interp)
    print(statistics.mean(z_interp))
    print(statistics.variance(z_interp))
    #print(ss)

    df_grid_temp = df_grid_temp.assign(KRIGEPREDICT = z_interp,
                                       KRIGEVAR = ss)
    df_grid_result = pd.concat([df_grid_result, df_grid_temp], ignore_index=True)

df_grid_result = df_grid_result.assign(
    FINALPREDICTM2 = df_grid_result['GAMPREDICT'] + df_grid_result['KRIGEPREDICT'],
    FINALPREDICT = (df_grid_result['GAMPREDICT'] + df_grid_result['KRIGEPREDICT']) * (1.852**2))
df_grid_result['FINALPREDICT'] = df_grid_result['FINALPREDICT'].apply(lambda x : (x if x > 0 else 0))

print(df_grid_result.head())
print(len(df_grid_result))

print(df_grid_result.groupby('NEWSAMS').agg({'FINALPREDICT':'sum'}))
print(df_grid_result.agg({'FINALPREDICT':'sum'}))

df_grid_result.to_csv('GBxyzLatLonRgn_GAM_REGION_KRIGE.csv',sep = '\t')


