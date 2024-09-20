import platform
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
