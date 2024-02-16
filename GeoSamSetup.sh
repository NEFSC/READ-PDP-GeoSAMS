#!/bin/bash
if [ $# \< 2 ] 
then
    echo No year inputs
    echo Expecting: GeoSamSetup.sh YYYYstart YYYYend
    exit
fi

# unzip dredge data
if [ ! -f "OriginalData/dredgetowbysize7917.csv" ]; then
    cd "OriginalData/"
    unzip dredgetowbysize7917.zip
    cd ..
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
dirGB="KrigingEstimates/SimGB"
dirMA="KrigingEstimates/SimMA"
#
for ((i = $(($1)); i <= $(($2)); i++ )); do
    if [ ! -d $dirGB$i ]; then
       mkdir $dirGB$i
    fi
    if [ ! -d $dirMA$i ]; then
       mkdir $dirMA$i
    fi
done

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
if [ ! -d "mod" ]; then
    mkdir mod
fi
if [ ! -d "obj" ]; then
    mkdir obj
fi
make

# finish with preprocessing
cd ..

octave PreProcess/TrawlData5mmbin.m $1 $2
octave PreProcess/PullOutRecruitData.m
octave PreProcess/ProcessRecruitData.m $1 $2
octave mfiles/NearestNeighborRecInterp.m $1 $2

# Not used with NearestNeighborRecInterp
# python PythonScripts/EstimateRecruitFields.py %1 %2
