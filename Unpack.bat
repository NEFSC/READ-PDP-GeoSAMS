@REM This script will
@REM  1) Unzip the dredge data
@REM  2) Create the necessary subdirectories
@REM  3) Pull out the initial state and recruit data from the dredge survey file.
@REM  4) Compile the source code
@REM  5) Run the sim to create the growth data
@REM  6) Run the UK to interpolate the results to the region grid locations, does this separately for MA and GB
@REM  7)Call Octave to plot the data and produce pdf output files located in the Results subdirectory
@REM    - Lat_Lon_Grid_AAAA_DN_YYYY_scale.pdf
@REM      -- AAAA is
@REM        EBMS: exploitable biomass in metric tons
@REM        LAND: landings by number
@REM        LPUE: Landings per Unit Effort
@REM        FEFF: Fishing Effort
@REM        RECR: Recruits
@REM      -- DN is MA or GB
@REM      -- YYYY is the year 2005 - 2008
@REM      -- Scale is the maximum value shown. The final plot is scaled to show the range 0 to 50
@REM    - Lat_Lon_Surv_AAAA_DN_YYYY_scale.pdf
@REM       same as above shows the original data plotted at the survey locations.

@echo off

if "'%DredgeFile%'" EQU "''" (
    echo [31mEnv Variable Not Set[0m
    echo "For example > set DredgeFile=dredgetowbysize7917"
    exit /b 10
)

if "'%HabCamFile%'" EQU "''" (
    @echo [31mEnv Variable Not Set[0m
    @echo "For example > set HabCamFile=Habcam_BySegment_2000_2014-2020"
    exit /b 10
)

set argCount=0
for %%x in (%*) do (
   set /A argCount+=1
   set "argVec[!argCount!]=%%~x"
)

if %argCount% NEQ 5 goto args_count_wrong
goto args_count_ok

:args_count_wrong
    @echo [31mMissing arguments[0m
    @echo "Expecting: Unpack.bat YYYYstart YYYYend DataSource# Domain [M|O]"
    @echo "Data Source"
    @echo "    NMFS_ALB ==> 1111"
    @echo "    CANADIAN ==> 2222"
    @echo "    F/V_TRAD ==> 3333"
    @echo "    VIMSRSA  ==> 4444"
    @echo "    NMFSSHRP ==> 5555"
    @echo "    ALL      ==> 0"
    @echo "Domain"
    @echo "    MA"
    @echo "    GB"
    @echo "    ALL, both MA and GB"
    @echo "[M|O]"
    @echo "    M: Use Matlab for numerical processing"
    @echo "    O: Use Octave for numerical processing"
    exit /b 11

:args_count_ok
@REM batch does not support AND, OR statements in IF
if "%3" EQU "1111" goto check_domain_arg
if "%3" EQU "2222" goto check_domain_arg
if "%3" EQU "3333" goto check_domain_arg
if "%3" EQU "4444" goto check_domain_arg
if "%3" EQU "5555" goto check_domain_arg
if "%3" EQU "0" goto check_domain_arg
    @echo [31mInvalid SRC: [0m "%3"
    @echo "Data Source"
    @echo "    NMFS_ALB EQU> 1111"
    @echo "    CANADIAN EQU> 2222"
    @echo "    F/V_TRAD EQU> 3333"
    @echo "    VIMSRSA  EQU> 4444"
    @echo "    NMFSSHRP EQU> 5555"
    @echo "    ALL      EQU> 0"
    exit /b 12

:check_domain_arg
if "%4" EQU "MA" goto check_math_arg
if "%4" EQU "GB" goto check_math_arg
if "%4" EQU "AL" goto check_math_arg
:domain_wrong
    @echo [31mInvalid Domain Arg: [0m %4
    @echo "Domain"
    @echo "    MA"
    @echo "    GB"
    @echo "    AL, both MA and GB"
    exit /b 13

:check_math_arg
if "%5" EQU "M" goto continue
if "%5" EQU "O" goto continue
:math_arg_wrong
    @echo [31mInvalid Math Arg: [0m %5
    @echo "Math Arg"
    @echo "    M EQU> Use Matlab"
    @echo "    O EQU> Use Octave"
    exit /b 14

:continue
@REM Unzip both survey files
@REM unzip dredge data
@echo %DredgeFile%
if "%DredgeFile%" NEQ "NONE" (
if not exist OriginalData\%DredgeFile%.csv (
    cd OriginalData
    "C:\Program Files\7-Zip\7z" e %DredgeFile%.zip
    cd ..
)
)

@REM unzip HabCam data
if not exist OriginalData\%HabCamFile%.csv (
    cd OriginalData
    "C:\Program Files\7-Zip\7z" e %HabCamFile%.zip
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
if not exist RecruitEstimates\ (
    mkdir RecruitEstimates\
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
attrib -R IORoutines.f90
attrib -R Globals.f90
copy ..\SRC\IORoutines.f90 .
copy ..\SRC\Globals.f90 .
@REM prevent modification of these file in this directory
attrib +R .\IORoutines.f90
attrib +R Globals.f90

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

@REM Pull Out Survey Data --------------------------------------------------------------
@REM Always pull dregde data
@REM if "%5" EQU "M" if "%6" EQU "D" (
if "%5" EQU "M" (
    @echo [33mmatlab.exe -batch "TrawlData5mmbin(%1, %2, %3, '%4'); exit;"[0m
    matlab.exe -batch "TrawlData5mmbin(%1, %2, %3, '%4'); exit;"
    IF ERRORLEVEL 1 (
        @echo [31mError in MATLAB TrawlData5mmbin. Stopping[0m
        exit 1/b
    )

    @echo [33mmatlab.exe -batch "HabCamData5mmbin(%1, %2, '%4', 'T'); exit;"[0m
    matlab.exe -batch "HabCamData5mmbin(%1, %2, '%4', 'T'); exit;"
    IF ERRORLEVEL 1 (
        @echo [31mError in MATLAB HabCamData5mmbin. Stopping[0m
        exit 1/b
    )
)

@REM Always pull dredge data
@REM if "%5" EQU "O" if "%6" EQU "D" (
if "%5" EQU "O" (
    @echo [33moctave PreProcess/TrawlData5mmbin.m $1 $2 $3 $4 T[0m
    octave PreProcess/TrawlData5mmbin.m $1 $2 $3 $4 'T'
    IF ERRORLEVEL 1 (
        @echo [31mError in Octave TrawlData5mmbin. Stopping[0m
        exit 1/b
    )

    @REM Alwyas pull HabCam data and append to dredge data
    @REM if "%5" EQU "O" if "%6" EQU "H" (
    @echo [33moctave PreProcess/HabCamData5mmbin.m $1 $2 $4 'T'[0m
    octave PreProcess/HabCamData5mmbin.m $1 $2 $4 'T'
    IF ERRORLEVEL 1 (
        @echo [31mError in Octave TrawlData5mmbin. Stopping[0m
        exit 1/b
    )
)

@REM Pull Out Recruit Data --------------------------------------------------------------
@REM Pull Out Dredge data then pull out HabCam Data
if "%5" EQU "M" (
    @echo [33mmatlab.exe -batch "PullOutRecruitData(%3, 'F', 'F'); exit;"[0m
    matlab.exe -batch "PullOutRecruitData(%3, 'F', 'F'); exit;"
    IF ERRORLEVEL 1 (
        @echo [31mError in MATLAB PullOutRecruitData. Stopping[0m
        exit 2/b
    )

    @echo [33mmatlab.exe -batch "PullOutRecruitData(%3, 'T', 'T'); exit;"[0m
    matlab.exe -batch "PullOutRecruitData(%3, 'T', 'T'); exit;"
    IF ERRORLEVEL 1 (
        @echo [31mError in MATLAB PullOutRecruitData. Stopping[0m
        exit 2/b
    )
) 

if "%5" EQU "O" (
    @echo [33moctave PreProcess/PullOutRecruitData.m %3 F F[0m
    octave PreProcess/PullOutRecruitData.m %3 F F
    IF ERRORLEVEL 1 (
        @echo [31mError in Octave PullOutRecruitData. Stopping[0m
        exit 2/b
    )

    @echo [33moctave PreProcess/PullOutRecruitData.m %3 T T[0m
    octave PreProcess/PullOutRecruitData.m %3 T T
    IF ERRORLEVEL 1 (
        @echo [31mError in Octave PullOutRecruitData. Stopping[0m
        exit 2/b
    )

)

@REM Process Recruit Data --------------------------------------------------------------
if "%5" EQU "M" (
    @echo [33mmatlab.exe -batch "ProcessRecruitData(%1, %2, '%4'); exit;"[0m
    matlab.exe -batch "ProcessRecruitData(%1, %2, '%4'); exit;"
)
if "%5" EQU "O" (
    @echo [33mPreProcess/ProcessRecruitData.m %1 %2 %4[0m
    octave PreProcess/ProcessRecruitData.m %1 %2 %4 
)
IF ERRORLEVEL 1 (
    @echo [31mError in MATLAB ProcessRecruitData. Stopping[0m
    exit 3/b
)

@REM Expand Nearest Neighbor to first year Survey Grid----------------------------------
if "%5" EQU "M" matlab.exe -batch "NearestNeighborRecInterp(%1, %2, '%4'); exit;"
if "%5" EQU "O" octave mfiles/NearestNeighborRecInterp.m %1 %2 %4
IF ERRORLEVEL 1 (
    @echo [31mError in MATLAB NearestNeighborRecInterp. Stopping[0m
    exit 4/b
)
