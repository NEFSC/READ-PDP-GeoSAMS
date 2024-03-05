#!/bin/bash
if [ $# \< 3 ] 
then
    echo No year inputs
    echo Expecting: GeoSamSetup.sh YYYYstart YYYYend DataSource#
    echo Data Source
    echo NMFS_ALB ==> 1111
    echo CANADIAN ==> 2222
    echo F/V_TRAD ==> 3333
    echo VIMSRSA ==> 4444
    echo NMFSSHRP ==> 5555
    echo ALL ==> 0
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

octave PreProcess/TrawlData5mmbin.m $1 $2 $3
if [ $? != 0 ]; then
    echo [31mError in MATLAB TrawlData5mmbin. Stopping[0m
    exit
fi

octave PreProcess/PullOutRecruitData.m $3
if [ $? != 0 ]; then
    echo [31mError in MATLAB PullOutRecruitData. Stopping[0m
    exit
fi

octave PreProcess/ProcessRecruitData.m $1 $2
if [ $? != 0 ]; then
    echo [31mError in MATLAB ProcessRecruitData. Stopping[0m
    exit
fi

octave mfiles/NearestNeighborRecInterp.m $1 $2
if [ $? != 0 ]; then
    echo [31mError in MATLAB NearestNeighborRecInterp. Stopping[0m
    exit
fi


# Not used with NearestNeighborRecInterp
# python PythonScripts/EstimateRecruitFields.py $1 $2
# if [ $? != 0 ]; then
#     echo [31mError in MATLAB EstimateRecruitFields. Stopping[0m
#     exit
# fi

./SRC/ScallopPopDensity Scallop.cfg MA $1 $2
if [ $? != 0 ]; then
    echo [31mError in MATLAB ScallopPopDensity MA. Stopping[0m
    exit
fi

./SRC/ScallopPopDensity Scallop.cfg GB $1 $2
if [ $? != 0 ]; then
    echo [31mError in MATLAB ScallopPopDensity GB. Stopping[0m
    exit
fi

python3 ./PythonScripts/ProcessResults.py $1 $2
if [ $? == 0 ]; then
    python3 ./PythonScripts/ConcatCsvResults.py $1 $2
else 
    echo [31mError in ProcessResults.py. Stopping[0m
fi
