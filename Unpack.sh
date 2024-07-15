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


if [ $# -ne 6 ] 
then
    echo [31mMissing arguments[0m
    echo Expecting: Unpack.sh YYYYstart YYYYend DataSource# Domain "M|O" "H|D"
    echo Data Source
    echo "    NMFS_ALB ==> 1111"
    echo "    CANADIAN ==> 2222"
    echo "    F/V_TRAD ==> 3333"
    echo "    VIMSRSA  ==> 4444"
    echo "    NMFSSHRP ==> 5555"
    echo "    ALL      ==> 0"
    echo "Domain"
    echo "    MA"
    echo "    GB"
    echo "    ALL, both MA and GB"
    echo "Math Args"
    echo "    M: use Matlab"
    echo "    O: use Octave"
    echo "Data Source Args"
    echo "    H: pull data from HabCam Data"
    echo "    D: pull data from Dredge Survey Data"
    exit
fi

if [[ $3 != 1111 && $3 != 2222 && $3 != 3333 && $3 != 4444 && $3 != 5555 && $3 != 0 ]] 
then
    echo [31mInvalid SRC: [0m "$3"
    echo "Data Source"
    echo "    NMFS_ALB ==> 1111"
    echo "    CANADIAN ==> 2222"
    echo "    F/V_TRAD ==> 3333"
    echo "    VIMSRSA  ==> 4444"
    echo "    NMFSSHRP ==> 5555"
    echo "    ALL      ==> 0"
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

if [[ "$6" != "H" && "$6" != "D" ]] 
then
    echo [31mInvalid Data Source Arg: [0m "$6"
    echo Math Arg
    echo "    H: Pull From HabCam Data"
    echo "    D: Pull From Dredge Survey Data"
    exit
fi

if [ "$6" == "D" ]; then 
# unzip dredge data
if [ ! -f "OriginalData/dredgetowbysize7917.csv" ]; then
    cd "OriginalData/"
    unzip dredgetowbysize7917.zip
    cd ..
fi
else
if [ ! -f "OriginalData/Habcam_BySegment_2000_2014-2019.csv" ]; then
    cd "OriginalData/"
    unzip Habcam_BySegment_2000_2014-2019.zip
    cd ..
fi
fi
# Create Directories used by GeoSAMS
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
if [ "$5" == "M" && "$6" == "D"]; then 
echo [33mmatlab.exe -batch "TrawlData5mmbin(%1, %2, %3, '%4'); exit;"[0m
matlab.exe -batch "TrawlData5mmbin(%1, %2, %3, '%4'); exit;"
fi
if [ $? != 0 ]; then
    echo [31mError in Matlab TrawlData5mmbin. Stopping[0m
    exit 1
fi

if [ "$5" == "M" && "$6" == "H"]; then 
echo [33mmatlab.exe -batch "HabCamData5mmbin(%1, %2, '%4'); exit;"[0m
matlab.exe -batch "HabCamData5mmbin(%1, %2, '%4'); exit;"
fi
if [ $? != 0 ]; then
    echo [31mError in Matlab HabCamData5mmbin. Stopping[0m
    exit 1
fi

if [ "$5" == "O" && "$6" == "D"]; then 
echo [33moctave PreProcess/TrawlData5mmbin.m $1 $2 $3 $4[0m
octave PreProcess/TrawlData5mmbin.m $1 $2 $3 $4
fi
if [ $? != 0 ]; then
    echo [31mError in Octave TrawlData5mmbin. Stopping[0m
    exit 1
fi

if [ "$5" == "O" && "$6" == "H"]; then 
echo [33moctave PreProcess/HabCamData5mmbin.m $1 $2 $4[0m
octave PreProcess/HabCamData5mmbin.m $1 $2 $4
fi
if [ $? != 0 ]; then
    echo [31mError in Octave HabCamData5mmbin. Stopping[0m
    exit 1
fi

# Pull Out Recruit Data --------------------------------------------------------------
if [ "$6" == 'H"']; then
hcChar='T'
else
hcChar='F'
fi
if [ "$5" == "M" ]; then 
echo [33mmatlab.exe -batch "PullOutRecruitData(%3, $hcChar); exit;"[0m
matlab.exe -batch "PullOutRecruitData(%3); exit;"
fi
if [ $? != 0 ]; then
    echo [31mError in Matlab PullOutRecruitData. Stopping[0m
    exit 2
fi

if [ "$5" == "O" ]; then 
echo [33moctave PreProcess/PullOutRecruitData.m $3 $hcChar[0m
octave PreProcess/PullOutRecruitData.m $3 $hcChar
fi
if [ $? != 0 ]; then
    echo [31mError in Octave PullOutRecruitData. Stopping[0m
    exit 2
fi

# Process Recruit Data --------------------------------------------------------------
if [ "$5" == "M" ]; then
echo [33mmatlab.exe -batch "ProcessRecruitData($1, $2, '$4', $hcChar); exit;"[0m
matlab.exe -batch "ProcessRecruitData($1, $2, '$4', $hcChar); exit;"
else
echo [33moctave PreProcess/ProcessRecruitData.m $1 $2 $4 $hcChar[0m
octave PreProcess/ProcessRecruitData.m $1 $2 $4 $hcChar
fi
if [ $? != 0 ]; then
    echo [31mError in octave ProcessRecruitData. Stopping[0m
    exit 3
fi

# Expand Nearest Neighbor to first year Survey Grid----------------------------------
if [ "$5" == "M" ]; then 
echo [33mmatlab.exe -batch "NearestNeighborRecInterp($1, $2, '$4'); exit;"[0m
matlab.exe -batch "NearestNeighborRecInterp($1, $2, '$4'); exit;"
else
echo [33moctave mfiles/NearestNeighborRecInterp.m $1 $2 $4[0m
octave mfiles/NearestNeighborRecInterp.m $1 $2 $4
fi
if [ $? != 0 ]; then
    echo [31mError in octave NearestNeighborRecInterp. Stopping[0m
    exit 4
fi
