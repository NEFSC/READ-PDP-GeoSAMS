@echo off
if [%1] == [] (
    echo No year inputs
    echo Expecting: GeoSamSetup.bat YYYYstart YYYYend
    exit /b
)
if [%2] == [] (
    echo No year inputs
    echo Expecting: GeoSamSetup.bat YYYYstart YYYYend
    exit /b
)

@REM unzip dredge data
if not exist OriginalData\dredgetowbysize7917.csv (
    cd "OriginalData/"
    "C:\Program Files\7-Zip\7z" e dredgetowbysize7917.zip
    cd ..
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
make

@REM Make UK executables
cd ..\UKsrc
if not exist mod\ (
    mkdir mod\
)
if not exist obj\ (
    mkdir obj\
)
make

@REM finish with preprocessing
cd ..

@REM PullOutRecruitData compiles recruit data for all years from the dredge data.
@REM For Matlab: %3 defines the source data for the dredge
@REM before 2008 use NMFS_ALB
@REM 2008 and after use VIMSRSA
@REM 0 : "ALL"
@REM 1 : "NMFS_ALB"
@REM 2 : "VIMSRSA" 
@REM font color take from
@REM The ESC special character can be entered with ALT+0+2+7 on the numeric keypad
@REM However, vscode does not see it so enter via notepad and cut & paste to here.
@REM https://stackoverflow.com/questions/2048509/how-to-echo-with-different-colors-in-the-windows-command-line
@REM https://gist.githubusercontent.com/mlocati/fdabcaeb8071d5c75a2d51712db24011/raw/b710612d6320df7e146508094e84b92b34c77d48/win10colors.cmd
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; PullOutRecruitData(%3); exit;"
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in MATLAB PullOutRecruitData. Stopping[0m
    exit
)
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; TrawlData5mmbin(%1, %2, %3); exit;"
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in MATLAB TrawlData5mmbin. Stopping[0m
    exit
)
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; ProcessRecruitData(%1, %2); exit;"
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in MATLAB ProcessRecruitData. Stopping[0m
    exit
)
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; NearestNeighborRecInterp(%1, %2); exit;"
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in MATLAB NearestNeighborRecInterp. Stopping[0m
    @echo 
    exit
)

@REM Not used with NearestNeighborRecInterp
@REM python PythonScripts/EstimateRecruitFields.py %1 %2

.\SRC\ScallopPopDensity.exe Scallop.cfg MA %1 %2
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in ScallopPopDensity MA. Stopping[0m
    exit
)

.\SRC\ScallopPopDensity.exe Scallop.cfg GB %1 %2
IF %ERRORLEVEL% NEQ 0 (
    @echo [31mError in ScallopPopDensity GB. Stopping[0m
    exit
)

python .\PythonScripts\ProcessResults.py %1 %2
IF %ERRORLEVEL% == 0 (
    python .\PythonScripts\ConcatCsvResults.py %1 %2
) else (
    @echo [31mError in ProcessResults.py. Stopping[0m
)
