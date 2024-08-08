#!/bin/bash

# This script will
#  1) Unzip the dredge data
#  2) Create the necessary subdirectories
#  3) Pull out the initial state and recruit data from the dredge survey file.
#  4) Compile the source code
#  5) Run the sim to create the growth data
#  6) Run the UK to interpolate the results to the region grid locations, does this separately for MA and GB
#  7)Call Octave to plot the data and produce pdf output files located in the Results subdirectory
#    - Lat_Lon_Grid_AAAA_DN_YYYY_scale.pdf
#      -- AAAA is
#        EBMS: exploitable biomass in metric tons
#        LAND: landings by number
#        LPUE: Landings per Unit Effort
#        FEFF: Fishing Effort
#        RECR: Recruits
#      -- DN is MA or GB
#      -- YYYY is the year 2005 - 2008
#      -- Scale is the maximum value shown. The final plot is scaled to show the range 0 to 50
#    - Lat_Lon_Surv_AAAA_DN_YYYY_scale.pdf
#       same as above shows the original data plotted at the survey locations.

if [ "'$HabCamFile'" == "''" ]
then
    echo [31mEnv Variable Not Set[0m
    echo "For example > export HabCamFile='Habcam_BySegment_2000_2014-2020'"
    exit 1
fi

if [ "'$DredgeFile'" == "''" ]
then
    echo [31mEnv Variable Not Set[0m
    echo "For example > export DredgeFile='dredgetowbysize7917'"
    exit 1
fi

if [ $# -ne 5 ] 
then
    echo [31mMissing arguments[0m
    echo Expecting: Unpack.sh ReferenceYr RecrYrStrt RecrYrStop Domain "M|O"
    echo "Domain"
    echo "    MA"
    echo "    GB"
    echo "    ALL, both MA and GB"
    echo "Math Args"
    echo "    M: use Matlab"
    echo "    O: use Octave"
    exit
fi

if [[ "$4" != "MA" && "$4" != "GB" && "$4" != "AL" ]] 
then
    echo [31mInvalid Domain: [0m "$4"
    echo Domain
    echo "    'MA'"
    echo "    'GB'"
    echo "    'AL', both MA and GB"
    exit
fi

if [[ "$5" != "M" && "$5" != "O" ]] 
then
    echo [31mInvalid Math Arg: [0m "$5"
    echo Math Arg
    echo "    M: Use Matlab"
    echo "    O: Use Octave"
    exit
fi

# unzip Dredge Data
if [ "$DredgeFile" != "NONE" ]; then 
if [ ! -f "OriginalData/$DredgeFile.csv" ]; then
    cd "OriginalData/"
    unzip $DredgeFile.zip
    cd ..
fi
fi
# unzip HabCam Data
if [ ! -f "OriginalData/$HabCamFile.csv" ]; then
    cd "OriginalData/"
    unzip $HabCamFile.zip
    cd ..
fi

# Create Directories used by GeoSAMS
if [ ! -d "Data" ]; then
    mkdir Data
fi
if [ ! -d "GrowthOutput" ]; then
    mkdir GrowthOutput
fi
if [ ! -d "Results" ]; then
    mkdir Results
fi
if [ ! -d "RecruitField" ]; then
    mkdir RecruitField
fi
if [ ! -d "KrigingEstimates" ]; then
    mkdir KrigingEstimates
fi
if [ ! -d "RecruitEstimates" ]; then
    mkdir RecruitEstimates
fi

# startup.m is for use with MATLAB, not used by octave instead see .octaverc
mv startup.m startup.xxx

# Make GeoSam executables
cd SRC
if [ ! -d "mod" ]; then
    mkdir mod
fi
if [ ! -d "obj" ]; then
    mkdir obj
fi
make

# Make UK executables
cd ../UKsrc
if [ -f IORoutines.f90 ]; then
# if files already exist, make writeable
    chmod 664 IORoutines.f90
    chmod 664 Globals.f90
fi
cp ../SRC/IORoutines.f90 .
cp ../SRC/Globals.f90 .
# change back to read only to remind user not to modifying local copy
chmod 444 IORoutines.f90
chmod 444 Globals.f90

if [ ! -d "mod" ]; then
    mkdir mod
fi
if [ ! -d "obj" ]; then
    mkdir obj
fi
make

# finish with preprocessing
cd ..

# Pull Out Survey Data --------------------------------------------------------------
# Both Dredge and appending HabCam
if [ "$5" == "M" ]; then 
    echo [33mmatlab.exe -batch "TrawlData5mmbin(%1, '%4'); exit;"[0m
    matlab.exe -batch "TrawlData5mmbin(%1, '%4'); exit;"
    if [ $? != 0 ]; then
        echo [31mError in Matlab TrawlData5mmbin. Stopping[0m
        exit 1
    fi

    echo [33mmatlab.exe -batch "HabCamData5mmbin(%1, '%4', 'T'); exit;"[0m
    matlab.exe -batch "HabCamData5mmbin(%1, '%4', 'T'); exit;"
    if [ $? != 0 ]; then
        echo [31mError in Matlab HabCamData5mmbin. Stopping[0m
        exit 1
    fi
fi # end if MATLAB

if [ "$5" == "O" ]; then 
    echo [33moctave PreProcess/TrawlData5mmbin.m $1 $4[0m
    octave PreProcess/TrawlData5mmbin.m $1 $4
    if [ $? != 0 ]; then
        echo [31mError in Octave TrawlData5mmbin. Stopping[0m
        exit 1
    fi

    echo [33moctave PreProcess/HabCamData5mmbin.m $1 $4 T[0m
    octave PreProcess/HabCamData5mmbin.m $1 $4 T
    if [ $? != 0 ]; then
        echo [31mError in Octave HabCamData5mmbin. Stopping[0m
        exit 1
    fi
fi # end using OCTAVE

# Pull Out Recruit Data --------------------------------------------------------------
if [ "$5" == "M" ]; then 
    echo [33mmatlab.exe -batch "PullOutRecruitData('F', 'F'); exit;"[0m
    matlab.exe -batch "PullOutRecruitData('F', 'F'); exit;"
    if [ $? != 0 ]; then
        echo [31mError in Matlab PullOutRecruitData Dredge. Stopping[0m
        exit 2
    fi

    echo [33mmatlab.exe -batch "PullOutRecruitData('T', 'T'); exit;"[0m
    matlab.exe -batch "PullOutRecruitData('T', 'T'); exit;"
    if [ $? != 0 ]; then
        echo [31mError in Matlab PullOutRecruitData HabCam. Stopping[0m
        exit 2
    fi
fi #end if MATLAB

if [ "$5" == "O" ]; then 
    echo [33moctave PreProcess/PullOutRecruitData.m F F[0m
    octave PreProcess/PullOutRecruitData.m F F
    if [ $? != 0 ]; then
        echo [31mError in Octave PullOutRecruitData. Stopping[0m
        exit 2
    fi

    echo [33moctave PreProcess/PullOutRecruitData.m T T[0m
    octave PreProcess/PullOutRecruitData.m T T
    if [ $? != 0 ]; then
        echo [31mError in Octave PullOutRecruitData. Stopping[0m
        exit 2
    fi
fi # end if using OCTAVE

# Process Recruit Data --------------------------------------------------------------
if [ "$5" == "M" ]; then
echo [33mmatlab.exe -batch "ProcessRecruitData($2, $3, '$4'); exit;"[0m
matlab.exe -batch "ProcessRecruitData($2, $3, '$4', $hcChar); exit;"
else
echo [33moctave PreProcess/ProcessRecruitData.m $2 $3 $4 [0m
octave PreProcess/ProcessRecruitData.m $2 $3 $4
fi
if [ $? != 0 ]; then
    echo [31mError in octave ProcessRecruitData. Stopping[0m
    exit 3
fi

# Expand Nearest Neighbor to first year Survey Grid----------------------------------
if [ "$5" == "M" ]; then 
echo [33mmatlab.exe -batch "NearestNeighborRecInterp($2, $3, '$4', $1); exit;"[0m
matlab.exe -batch "NearestNeighborRecInterp($2, $3, '$4', $1); exit;"
else
echo [33moctave mfiles/NearestNeighborRecInterp.m $2 $3 $4 $1[0m
octave mfiles/NearestNeighborRecInterp.m $2 $3 $4 $1
fi
if [ $? != 0 ]; then
    echo [31mError in octave NearestNeighborRecInterp. Stopping[0m
    exit 4
fi
