@echo off
if [%1] == [] (
    echo No year inputs
    echo Expecting: GeoSamSetup.bat YYYYstart YYYYend
    exit
)
if [%2] == [] (
    echo No year inputs
    echo Expecting: GeoSamSetup.bat YYYYstart YYYYend
    exit
)
@REM Create Directories used by GeoSAMS
if not exist GrowthOutput\ (
    mkdir GrowthOutput\
)
if not exist Results\ (
    mkdir Results\
)
if not exist RecruitField\ (
    mkdir RecruitField\
)
if not exist KrigingEstimates\ (
    mkdir KrigingEstimates\
)
set dirGB=KrigingEstimates\SimGB
set dirMA=KrigingEstimates\SimMA
for /l %%y in (%1, 1, %2) do (
    if not exist %dirGB%%%y\ (
    mkdir %dirGB%%%y\
    )
    if not exist %dirMA%%%y\ (
    mkdir %dirMA%%%y\
    )
)

@REM Make GeoSam executables
cd SRC
if not exist mod\ (
    mkdir mod\
)
if not exist obj\ (
    mkdir obj\
)
make clean
make

@REM Make UK executables
cd ..\UKsrc
if not exist mod\ (
    mkdir mod\
)
if not exist obj\ (
    mkdir obj\
)
make clean
make

@REM finish with preprocessing
cd ..
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; TrawlData5mmbin(%1, %2); exit;"
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; PullOutRecruitData; exit;"
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; ProcessRecruitData(%1, %2); exit;"
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; NearestNeighborRecInterp(%1, %2); exit;"

@REM Not used with NearestNeighborRecInterp
@REM python PythonScripts/EstimateRecruitFields.py %1 %2