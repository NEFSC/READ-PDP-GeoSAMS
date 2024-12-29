#########################################################################################
###                           WORK IN PROGRESS                                        ###
#########################################################################################

import sys
import os
import pandas as pd

# Dredge CSV File Header
# area	subarea	cruise6	year	month	day	time	stratum	tow	station	statype	SVGEAR	haul	
# gearcon	sdefid	newstra	clop	lat	lon	tnms	setdpth	bottemp	dopdisb	distused	towadj	
# towdur	sizegrp	catchnu	catchwt	surv_n	partrecn	fullrecn	surv_b	partrecb	fullrecb	
# postow	datasource	lwarea	SETLW	SVSPP	PropChains	AREAKIND	STRATMAP	SQNM	
# UTM X	UTM Y

refYear = int(sys.argv[1])
domain = sys.argv[2]

dredgeFile = os.environ['DredgeFile']
dataFile = os.path.join('OriginalData', dredgeFile+'.csv')

M = pd.read_csv(dataFile)

M = M[M['month']>0]

#------- new M table ----------------------

if domain != 'AL':
    if domain == 'GB':
        M = M[M['lon']>-70.5]
    else:
        M = M[M['lon']<=-70.5]
    print(len(M))

M = M[M['year']==refYear]
#and
M = M[M['sizegrp']==3.0]
print(len(M))

if len(M) == 0:
    print('Skipping {} Year {}'.format(domain, refYear))
    quit()

print('Working on {} Year {}'.format(domain, refYear))

