!--------------------------------------------------------------------------------------------------
!> @mainpage Universal Kriging
!>
!> This program is designed to interpolate geospatial data from a smaller, arbritrary grid to a larger, 
!> evenly spaced grid, albeit evenly is not required.
!>
!> @section m0p1 Universal Kriging, UK, Startup
!>
!> UK begins by gathering parameters used to define how the program executes
!>
!> @subsection m0p1p1 UK Configuration File
!>
!> This file is specified on the command line. It has the following parameters
!> * High Limit Factor
!> * Number of Random Fields
!> * Kriging variogram form
!> * Log Transform
!> * Power Transform Parameter
!> * NLS Spatial Fcn File Name
!> * Save Data
!> 
!> @subsubsection m0p1p1p1 High Limit Factor
!> 
!> This configuration item is used to set an upper limit for the observed data. That is,
!> * fmax = fmax * maxval(observed data)\n
!> Used with logical IsHighLimit. IsHighLimit is the inverse of Log Transform. Typically runs as false. 
!> Not fully tested.
!>
!> @subsubsection m0p1p1p2 Number of Random Fields
!>
!> This configuration item sets the number of random fields, NRand. DEPRECATED
!>
!> @subsubsection m0p1p1p3 Kriging variogram form
!> * 'spherical'
!> * 'exponential'
!> * 'gaussian'
!> * 'matern'
!>
!> Default value is 'spherical' with no entry in configuration file
!>
!> @subsubsection m0p1p1p4 Log Transform
!> If set to true, interpolation uses the log of the input data instead of linear transform.
!>
!> @subsubsection m0p1p1p5 Power Transform Parameter
!>
!> Power transform interpolates f(x)^alpha, generally 0< alpha < 1 but this has not been tested .
!> Not used if "Log Transform = T"
!>
!> @subsubsection m0p1p1p6 NLS Spatial Fcn File Name
!> The name of the file that defines the spatial functions. 
!> Configuration files are expected to be in the <em><b>./Configuration</b></em> subdirectory
!>
!> @subsubsection m0p1p1p7 Save Data
!> This configuration item determines where the output is saved.
!>
!> F: Written to the root directory\n
!> T: Written to the <em><b>./Results</b></em> with files named using the template Lat_Lon_Grid_PARM_DN_YYYY.csv
!>
!> * PARM, includes but not limited to
!>   - EBMS: Exploitable BioMasS
!>   - LPUE: Landings Per Unit Effort
!>   - RECR: Recruitment
!> * DN: Domain Name
!>   - MA: Mid Atlantic
!>   - GB: Georges Bank
!>
!-----------------------------------------------------------------------
program UKsimulation
use globals
use Grid_Manager_Mod
use LSF_Mod
use NLSF_Mod
use Krig_Mod
!NORF!use Random_Field_Mod
implicit none

real(dp), allocatable :: beta(:), Cbeta(:,:), eps(:), Ceps(:,:), residual_cov(:,:)
real(dp), allocatable :: spatial_fcn(:,:), residual(:), trndOBS(:), resOBS(:)
real(dp), allocatable :: dist(:,:)
integer num_points, num_spat_fcns, num_obs_points, j
real(dp)  fmax, SF
! variables for nonlinear fitting
integer nsf
logical IsHiLimit, IsLogT
character(domain_len) domain_name
type(Grid_Data_Class):: grid
type(Grid_Data_Class):: obs
type(Krig_Class):: par
type(NLSF_Class), allocatable ::nlsf(:)
real(dp) alpha
logical save_data
real(dp) f0_max
real e, etime, t(2)
e = etime(t)         !  Startup etime - do not use result

e = etime(t)

par = Krig_Class(0.0, 0.0, 0.0, 'spherical')

write (*,*) term_grn, "PROGRAM STARTING  ", &
& 'elapsed:', term_yel, e, term_grn, ', user:', term_yel, t(1), term_grn, ', sys:', term_yel, t(2), term_blk

call Read_Startup_Config(domain_name, IsLogT, IsHiLimit, fmax, par, alpha, save_data, f0_max)

call random_seed( )

!--------------------------------------------------------------------------
! Get observation data and regional grid data
!--------------------------------------------------------------------------
call GridMgr_Set_Grid_Manager(obs, grid, alpha, num_obs_points, num_points, fmax)

allocate( eps(1:num_points), Ceps(1:num_points, 1:num_points))
allocate(residual_cov(1:num_obs_points, 1:num_obs_points))
allocate(residual(1:num_obs_points))
allocate(trndOBS(1:num_obs_points), resOBS(1:num_obs_points))
allocate(dist(1:num_obs_points, 1:num_obs_points))

!-------------------------------------------------------------------------  
! nonlinear curve fitting for spatial functions
!-------------------------------------------------------------------------  
! Determine how many spatial functions are defined.
nsf = NLSF_Count_Defined_Functions()
allocate(nlsf(1:nsf))
nsf = NLSF_Define_Functions(nlsf, grid, f0_max)

write (*,*) term_blu,"Reading ", domain_name
write(*,*) 'Observation file:  ', trim(GridMgr_Get_Obs_Data_File_Name())
write(*,*) 'Logtransorm:       ', IsLogT
write(*,*) 'Using High Limit:  ', IsHiLimit
write(*,'(A,F10.4)') ' High limit fmax:   ', fmax
write(*,*) 'Form of variagram: ', par%form
if (save_data) then
    write(*,*) 'All data is saved'
else
    write(*,*) 'Only resulting estimate is saved'
endif
if (NLSF_Get_Use_Orig_Data()) then
    write(*,*) 'Force 1D fit to original data'
else
    write(*,*) 'Force 1D fit to residual data'
endif 
write(*,*) term_blk
write(*,*)'num_obs_points=', num_obs_points, 'nsf limit=', NLSF_Get_NSF_Limit(), ' alpha = ', alpha
write(*,*)'num_grid_points=', num_points
write(*,'(A,L2)') 'Is Truncate Range: ', NLSF_Get_Is_Truncate_Range()
write(*,'(A,L2)') 'Using Greedy Fit:  ', NLSF_Get_Use_Greedy_Fit()
f0_max = NLSF_Get_Z_F0_Max()
if (f0_max > 0._dp) then
    write(*,*) 'Using f0_max setting:', f0_max, term_blk
else
    write(*,*) 'Using f0_max setting: Determined by algorithm', term_blk
endif
   
if(IsLogT) then
    SF = Compute_MEAN(obs%field(1:num_obs_points), num_obs_points) / 5.D0
    obs%field(1:num_obs_points) = log((one_scallop_per_tow + obs%field(1:num_obs_points)) / SF)
endif

!-----------------------------------------------------------------------------------------------------
! Performs a brute force least squares fit of nonlinear spatial function parameters to data points in obs.
!-----------------------------------------------------------------------------------------------------
call NLSF_Select_Fit(obs, nlsf, save_data)
! nsf may have been modified during NLSF_Select_Fit
nsf = NLSF_Get_NSF()
if (nsf>0) then
    write(*,*)
    write(*,*) 'ax   Form               f0            lambda'
    write(*,*) '-- ----------   ---------------  --------------'
    do j=1, nsf
        write(*,'(A4,A12,2F16.6)') nlsf(j)%axis, nlsf(j)%form, nlsf(j)%f0, nlsf(j)%lambda
    enddo
    write(*,*)
endif
residual_cov(1:num_obs_points, 1:num_obs_points)=0.D0
do j=1, num_obs_points
    residual_cov(j, j)=1.D0
enddo

!-------------------------------------------------------------------------
! Ordinary Least Square fit with spatial functions
!-------------------------------------------------------------------------
num_spat_fcns = nsf + 1
allocate(spatial_fcn(1:num_obs_points, 1:num_spat_fcns))
allocate( beta(1:num_spat_fcns), Cbeta(1:num_spat_fcns, 1:num_spat_fcns) )
spatial_fcn = Krig_Eval_Spatial_Function(obs, num_spat_fcns, num_obs_points, nlsf, save_data)
residual = LSF_Generalized_Least_Squares&
&       (obs%field, spatial_fcn, residual_cov, num_obs_points, num_spat_fcns, beta, Cbeta, save_data)
write(*,'(A,F10.6)')'Ordinary Least Sq Residual:', Compute_RMS( residual(:), num_obs_points)
if (save_data) then
    call Write_Vector_Scalar_Field(num_obs_points, residual, 'OLSresidual.txt')
    call Write_Vector_Scalar_Field(num_obs_points, obs%field, 'data.txt')
    call Write_CSV(num_spat_fcns, num_spat_fcns, Cbeta, 'LSFCovBeta.csv', num_spat_fcns, .false.)
endif

!-------------------------------------------------------------------------
! Fit variogram parameters to Ordinary Least Square residual
!-------------------------------------------------------------------------
dist = Krig_Compute_Distance(obs, obs, num_obs_points, obs%num_points)
call Krig_Comp_Emp_Variogram(num_obs_points, dist, num_obs_points, residual, par)

if (save_data) then
    open(63, file='KRIGpar.txt')
    write(63,*)par%sill, par%nugget, par%alpha
    close(63)
endif

!-------------------------------------------------------------------------
! Compute Universal Kriging estimate of field on grid (fest) given
! observations x_obs, y_obs, z_obs, field_obs. Also returns the estimate of spatial function
! coeficients, beta, and posterior covariance of beta(Cbeta).
!-------------------------------------------------------------------------
call Krig_Generalized_Least_Sq(grid, obs, num_spat_fcns, par, beta, Cbeta, eps, Ceps, nlsf, save_data)

! COMPUTED BUT NOT USED except for GLSres
trndOBS =  matmul(spatial_fcn, beta)
resOBS(1:num_obs_points) = obs%field(1:num_obs_points) - trndOBS(1:num_obs_points)
write(*,'(A,F10.6)')'GLSres:', Compute_RMS(resOBS(:), num_obs_points)

if (save_data) then
    call OutputUK(num_points, num_spat_fcns, grid, nlsf, beta, eps, Ceps, Cbeta, fmax, SF, IsLogT, IsHiLimit, alpha)
else
    !DEPRECATE trend!call OutputEstimates(num_points, num_spat_fcns, grid, Ceps, IsLogT, IsHiLimit, fmax, SF, domain_name, alpha)
    call OutputEstimates(num_points, grid, Ceps, IsLogT, IsHiLimit, fmax, SF, alpha)
endif

write(*,*)'num_points, num_survey', num_points, num_obs_points

deallocate(beta, Cbeta, nlsf)
deallocate(eps, Ceps)
deallocate(residual_cov, spatial_fcn, residual)
deallocate(trndOBS, resOBS)
deallocate(dist)

e = etime(t)
write (*,*) term_grn, "PROGRAM STOP  ", &
& 'elapsed:', term_yel, e, term_grn, ', user:', term_yel, t(1), term_grn, ', sys:', term_yel, t(2), term_blk

stop 0
endprogram

!--------------------------------------------------------------------------------------------------
!> Purpose: Read parameter values, flags, etc. from an ascii text input file:"UK.cfg".  Parameters etc. 
!> to be read from UK.cfg are identified by tag before '='.  Values are read from the 
!> line to the right of an "=" character. Logical variables are read from 'T','F'.
!>
!> This method also considers arguments passed on the command line that determine
!> * Name of the configuration file
!> * Name of the observation file
!> Or to override config file settings used in batch mode
!> * Domain
!> * Grid File
!> * Z f0 max
!>
!> outputs: 
!>       all variables
!>
!--------------------------------------------------------------------------------------------------
! Keston Smith, Tom Callaghan (IBSS) 2024
!--------------------------------------------------------------------------------------------------
subroutine Read_Startup_Config(domain_name, IsLogT, IsHiLimit, fmax, par, alpha, save_data, f0_max)
use globals
use Krig_Mod
use NLSF_Mod, only : NLS_Set_Config_File_Name => Set_Config_File_Name
implicit none
character(2),intent(out):: domain_name
type(Krig_Class), intent(out):: par
integer j, k, io

logical IsLogT,IsHiLimit
character(72) :: input_string
character(tag_len) tag
character(value_len) value
real(dp),intent(out)::  fmax,alpha
logical, intent(out) :: save_data
real(dp), intent(out) :: f0_max

integer ncla
character(fname_len) cfg_file_name
character(fname_len) cmd, obsfile, gridfile
logical exists

! set default values for parameters not in file
IsLogT=.true.
IsHiLimit=.false.
alpha=1.D0
save_data = .false.
f0_max = 0._dp

! Check what was entered on command line
ncla=command_argument_count()
if (ncla .eq. 0) then
    write(*,*) term_red, 'No UK configuration file given', term_blk
    call get_command(cmd)
    write(*,*) term_blu,'Typical use: '

    write(*,*) term_blu,'  exe   ConfigFile  Domain  ObservFile              ZF0Max(optional)'
    write(*,*) term_yel,' .\UKsrc\UK UK_MA.cfg MA   X_Y_EBMS_MA2005_0.csv   0.0 | 70.0'
    !                      arg#        1       2              3                  4'
    write(*,*) term_blu,'                                                    | --- optional but need both ---|'
    write(*,*) term_blu,'  exe   ConfigFile  Domain  ObservFile                GridFile            ZF0Max'
    write(*,*) term_yel,' .\UKsrc\UK UK_GB.cfg GB   X_Y_EBMS_GB2005_0_SW.csv  GBxyzLatLon_SW.csv   0.0 | 70.0', term_blk
    !                      arg#        1       2              3                  4                   5
    stop 1
endif

if(ncla.ge.1) call get_command_argument(1, cfg_file_name)
cfg_file_name = config_dir//trim(cfg_file_name)
inquire(file=cfg_file_name, exist=exists)
if (exists) then
    PRINT *, term_blu, trim(cfg_file_name), ' FOUND', term_blk
else
    PRINT *, term_red, trim(cfg_file_name), ' NOT FOUND', term_blk
    stop 1
endif

open(69,file=cfg_file_name)
do
    input_string=""
    read(69,'(a)',iostat=io) input_string
    if (io.lt.0) exit

    if (input_string(1:1) .NE. '#') then
        j = scan(input_string,"=",back=.true.)
        tag = trim(adjustl(input_string(1:j-1)))
        ! explicitly ignore inline comment
        k = scan(input_string,"#",back=.true.)
        if (k .EQ. 0) k = len(input_string) + 1
        value =  trim(adjustl(input_string(j+1:k-1)))

        select case (tag)
        case('Domain Name')
            domain_name = value(1:2)
            if (.not. ( any ((/ domain_name.eq.'MA', domain_name.eq.'GB', domain_name.eq.'AL'/)) )) then
                write(*,*) term_red, ' **** INVALID DOMAIN NAME: ', domain_name, term_blk
                stop 1
            endif
            ! Force Grid file name form MA and GB. 
            ! AL sets it on the command line, as does GB if saveByStratum was used during sim
            value = domain_name//'xyzLatLon.csv'
            if ( any ((/ domain_name.eq.'MA', domain_name.eq.'GB'/)) ) then
                Call GridMgr_Set_Grid_Data_File_Name(value)
            endif

        case('Observation File')
            Call GridMgr_Set_Obs_Data_File_Name(value)

        case('Log Transform')
            read(value,*) IsLogT

        case('High Limit Factor')
            read(value, *) fmax

        case('Power Transform Parameter')
            if (IsLogT) then
                ! Forcing alpha to 1.0
                alpha=1.D0
                write(*,*) term_yel, 'Log Transform is T, ignoring Power Transform Param', term_blk
            else
                read(value,*) alpha
                IsHiLimit = .true.
                write(*,*)'Is this the fmax you were searching for?', fmax
            endif

        case('Kriging variogram form')
            read(value,*) par%form

        case('NLS Spatial Fcn File Name')
            call NLS_Set_Config_File_Name(value)

        case('Save Data')
            read(value,*) save_data

        case default
            write(*,*) term_red, 'ReadInput: Unrecognized line in UK.cfg'
            write(*,*) 'Unrecognized Line->',input_string, term_blk
            STOP 1
    end select
    endif
end do
close(69)

!========================== override config file =================================
!override UK.cfg with command line arguments if present. Used by python scripts
if(ncla.ge.2) then 
    call get_command_argument(2, domain_name)
    if (.not. ( any ((/ domain_name.eq.'MA', domain_name.eq.'GB', domain_name.eq.'AL'/)) )) then
        write(*,*) term_red, ' **** INVALID DOMAIN NAME: ', domain_name, term_blk
        stop 1
    endif
    ! Force default grid file name
    cmd = domain_name//'xyzLatLon.csv'
    ! AL sets it via command line, as does GB if saveByStratum was used during sim
    if ( any ((/ domain_name.eq.'MA', domain_name.eq.'GB'/)) ) then
        Call GridMgr_Set_Grid_Data_File_Name(cmd)
    endif
endif

if(ncla.ge.3) then
    call get_command_argument(3, obsfile)
    call GridMgr_Set_Obs_Data_File_Name(obsfile)
endif

! Typically, for MA, which uses default grid file, only reads f0_max override
if (ncla .ge. 4) then
    if (ncla .eq. 4) then
        call get_command_argument(4, cmd)
        if (len_trim(cmd) .NE. 0) then
            ! did not read null spaces
            read(cmd, *) f0_max
        endif
    else
        ! GB breaks up GridFile into four regions
        ! 1) GBxyzLatLon_N
        ! 2) GBxyzLatLon_S
        ! 3) GBxyzLatLon_SW
        ! 4) GBxyzLatLon_W
        ! AL adds a fifth Region, but uses the same grid as MA
        ! 5) MAxyzLatLon
        call get_command_argument(4, gridfile)
        call GridMgr_Set_Grid_Data_File_Name(gridfile)

        call get_command_argument(5, cmd)
        read(cmd, *) f0_max
    endif
endif

endsubroutine Read_Startup_Config
        
!---------------------------------------------------------------------------------------------------
!! Purpose: This subroutine writes files for output.  This includes a central prediction: 
!> "KrigingEstimate.txt" Predictor standard deviation  is output to "KrigSTD.txt".
!> Function coefficient 
!---------------------------------------------------------------------------------------------------
subroutine OutputUK(num_points, num_spat_fcns, grid, nlsf, beta, eps, Ceps, Cbeta, fmax, SF, &
    &                  IsLogT, IsHiLimit, alpha)
use globals
use Grid_Manager_Mod
use NLSF_Mod
use LSF_Mod
use Krig_Mod
implicit none

type(Grid_Data_Class), intent(inout) :: grid
type(NLSF_Class), intent(in)::nlsf(*)
                                    
integer, intent(in) :: num_points, num_spat_fcns
real(dp), intent(in) :: eps(*), beta(*), Cbeta(num_spat_fcns,*), fmax, SF, alpha
real(dp), intent(inout) :: Ceps(num_points,*)
logical, intent(in) ::IsLogT, IsHiLimit
integer n 
real(dp) trend(num_points), V(num_points), Fg(num_points, num_spat_fcns)
real(dp) logf(num_points)
logical, parameter :: save_data = .true.

write(*,*)'output fmax, SF, A=', fmax, SF, one_scallop_per_tow
do n=1, num_points
    V(n)=Ceps(n, n)
enddo
grid%field(1:num_points) = grid%field(1:num_points)**(1./alpha)

logf(1:num_points) = grid%field(1:num_points)
if(IsLogT) grid%field(1:num_points) = SF * exp( grid%field(1:num_points) + V(1:num_points)/2. ) - one_scallop_per_tow  ! adjusted inverse log(one_scallop_per_tow+f)
if(IsHiLimit) call LSF_Limit_Z(num_points, grid%field, grid%z, fmax, grid%lon)
call Write_Vector_Scalar_Field(num_points, grid%field, 'KrigingEstimate.txt')

Fg = Krig_Eval_Spatial_Function(grid, num_spat_fcns, num_points, nlsf, save_data)
trend(1:num_points) = matmul( Fg(1:num_points, 1:num_spat_fcns), beta(1:num_spat_fcns)) 

if(IsLogT) trend(1:num_points) = SF*exp(trend(1:num_points))-one_scallop_per_tow
call Write_Vector_Scalar_Field(num_points, trend, 'SpatialTrend.txt')

call Write_Vector_Scalar_Field(num_points, eps, 'epsilon.txt')
call Write_Vector_Scalar_Field(num_spat_fcns, beta, 'beta.txt')

call Write_CSV(num_spat_fcns, num_spat_fcns, Cbeta, 'CovBeta.csv', num_spat_fcns, .false.)

Fg = Krig_Eval_Spatial_Function(grid, num_spat_fcns, num_points, nlsf, save_data)
Ceps(1:num_points, 1:num_points) = Ceps(1:num_points, 1:num_points)&
&    + matmul( Fg(1:num_points, 1:num_spat_fcns) , &
&              matmul(Cbeta(1:num_spat_fcns, 1:num_spat_fcns), transpose(Fg(1:num_points, 1:num_spat_fcns)) ) )
do n=1, num_points
   V(n) = Ceps(n, n)
enddo
if(IsLogT) call Write_Vector_Scalar_Field(num_points, SF*exp(sqrt(V(1:num_points)))-one_scallop_per_tow, 'KrigSTD.txt')
if(.not.IsLogT) call Write_Vector_Scalar_Field(num_points, sqrt(V(1:num_points)), 'KrigSTD.txt')

endsubroutine OutputUK

!---------------------------------------------------------------------------------------------------
!! Purpose: This subroutine only write the interpolated results file for output. Namely, the central 
!> prediction. The file name is take for the observation file name that resides in the Data
!> subdirectory and then written to the Results subdirectory. 
!>
!> Also want to change the name from X_Y_ (UTM) to Lat_Lon_Grid_ (Navigation)
!>
!> That is moving from survey data locations to MA/GB grid locations
!---------------------------------------------------------------------------------------------------
!DEPRECATE trend!subroutine OutputEstimates(num_points, num_spat_fcns, grid, Ceps, IsLogT, IsHiLimit, fmax, SF, domain_name, nlsf, alpha, beta)
subroutine OutputEstimates(num_points, grid, Ceps, IsLogT, IsHiLimit, fmax, SF, alpha)
use globals
use Grid_Manager_Mod
use NLSF_Mod
use LSF_Mod
use Krig_Mod

implicit none

integer, intent(in) :: num_points!DEPRECATE trend!, num_spat_fcns
type(Grid_Data_Class), intent(inout) :: grid
real(dp), intent(in) :: Ceps(num_points,*)
logical, intent(in) ::IsLogT, IsHiLimit
real(dp), intent(in) :: fmax, SF

!DEPRECATE trend!type(NLSF_Class), intent(in)::nlsf(*)
real(dp), intent(in) :: alpha
!DEPRECATE trend!real(dp), intent(in) :: beta(*)

integer n
real(dp) V(num_points)
character(fname_len) fname, fout  !DEPRECATE trend!, ftrend
character(80) fmtstr

!DEPRECATE trend!real(dp) trend(num_points), Fg(num_points, num_spat_fcns)

do n=1, num_points
    V(n)=Ceps(n, n)
enddo
grid%field(1:num_points) = grid%field(1:num_points)**(1./alpha)    
if(IsLogT) grid%field(1:num_points) = SF * exp( grid%field(1:num_points) + V(1:num_points)/2. ) - one_scallop_per_tow  ! adjusted inverse log(one_scallop_per_tow+f)
if(IsHiLimit) call LSF_Limit_Z(num_points, grid%field, grid%z, fmax, grid%lon)

fname = GridMgr_Get_Obs_Data_File_Name()
! Change output directory and file prefix
! /X_Y_...
! n12345
n = index(fname, '/') + 5
fout = output_dir//'Lat_Lon_Grid_'//fname(n:)
!DEPRECATE trend!ftrend = output_dir//'Lat_Lon_Grid_Trend-'//fname(n:)
fmtstr='(2(ES14.7 : ", ") (ES14.7 : ))'

write(*,'(A,A,3F12.6)') term_blu, 'output fmax, SF, A=', fmax, SF, one_scallop_per_tow
write(*,*) 'Writing ouput to: ', trim(fout)
!DEPRECATE trend! write(*,*) 'Writing ouput to: ', trim(ftrend), term_blk

! Save results along with location information
open(63,file=trim(fout))
do n=1, num_points
    write(63, fmtstr) grid%lat(n), grid%lon(n), grid%field(n)
enddo
close(63)

! DEPRECATE trend --------------------------------------------------------------------------
! ! Now let's save the trend information in the same manner
! Fg = Krig_Eval_Spatial_Function(grid, num_spat_fcns, num_points, nlsf, .false.)
! trend(1:num_points) = matmul( Fg(1:num_points, 1:num_spat_fcns), beta(1:num_spat_fcns)) 

! if(IsLogT) trend(1:num_points) = SF*exp(trend(1:num_points))-one_scallop_per_tow
! !call Write_Vector_Scalar_Field(num_points, trend, 'SpatialTrend.txt')
! open(63,file=trim(ftrend))
! do n=1, num_points
!     write(63, fmtstr) grid%lat(n), grid%lon(n), trend(n)
! enddo
! close(63)
! ------------------------------------------------------------------------------------
endsubroutine OutputEstimates
