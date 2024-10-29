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
!> * Kriging variogram form
!> * NLS Spatial Fcn File Name
!> * Save Data
!> 
!> @subsubsection m0p1p1p1 High Limit Factor DEPRECATED
!> 
!> This configuration item is used to set an upper limit for the observed data. That is,
!> * fmax = fmax_multiplier * maxval(observed data)\n
!> Used with logical IsHighLimit. IsHighLimit is the inverse of Log Transform. Typically runs as false. 
!> Not fully tested.
!>
!> @subsubsection m0p1p1p2 Kriging variogram form
!> * 'spherical'
!> * 'exponential'
!> * 'gaussian'
!> * 'matern'
!>
!> Default value is 'spherical' with no entry in configuration file
!>
!> @subsubsection m0p1p1p3 Log Transform
!> If set to true, interpolation uses the log of the input data instead of linear transform.
!>
!> @subsubsection m0p1p1p4 NLS Spatial Fcn File Name
!> The name of the file that defines the spatial functions. 
!> Configuration files are expected to be in the <em><b>./Configuration</b></em> subdirectory
!>
!> @subsubsection m0p1p1p5 Save Data
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
!>   - AL: Both of the above
!>


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
real(dp)  SF
! variables for nonlinear fitting
integer nsf
type(Grid_Data_Class):: grid
type(Grid_Data_Class):: obs
type(Krig_Class):: par
type(NLSF_Class), allocatable ::nlsf(:)
real(dp) alpha_obs
logical save_data
real(dp) f0_max
real(dp) fmax
real(dp) domain_avg
real(dp) tmp

real e, etime, t(2)
e = etime(t)         !  Startup etime - do not use result

e = etime(t)

par = Krig_Class(0.0, 0.0, 0.0, 'spherical')

write (*,*) term_grn, "PROGRAM STARTING  ", &
& 'elapsed:', term_yel, e, term_grn, ', user:', term_yel, t(1), term_grn, ', sys:', term_yel, t(2), term_blk

call Read_Startup_Config(par, alpha_obs, save_data, f0_max)

call random_seed( )

!--------------------------------------------------------------------------
! Get observation data and regional grid data
!--------------------------------------------------------------------------
call GridMgr_Set_Grid_Manager(obs, grid, alpha_obs, num_obs_points, num_points, fmax, domain_avg) !, fmax_multiplier, fmax)

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

write(*,*) term_blu, 'Observation file:  ', term_blk, trim(GridMgr_Get_Obs_Data_File_Name())
! write(*,*) 'Logtransorm:       ', IsLogT
! write(*,*) 'Using High Limit:  ', IsHiLimit
write(*,'(A,F10.4)') term_blu//' High limit fmax:   '//term_blk, fmax
write(*,*) term_blu, 'Form of variagram: ', term_blk, par%form
if (save_data) then
    write(*,*) 'All data is saved'
else
    write(*,*) 'Only interpolated data is saved'
endif
if (NLSF_Get_Use_Orig_Data()) then
    write(*,*) 'Force 1D fit to original data'
else
    write(*,*) 'Force 1D fit to residual data'
endif 
write(*,*) 
write(*,*) term_blu, 'num_obs_points=', term_blk, num_obs_points, term_blu, 'nsf limit=', term_blk, NLSF_Get_NSF_Limit(), &
&          term_blu, ' alpha = ', term_blk, alpha_obs
write(*,*) term_blu, 'num_grid_points=', term_blk, num_points, term_blu
write(*,'(A,A,A, L2)') term_blu, ' Is Truncate Range: ', term_blk, NLSF_Get_Is_Truncate_Range()
f0_max = NLSF_Get_Z_F0_Max()
if (f0_max > 0._dp) then
    write(*,*) term_blu, 'f0_max setting: ', term_blk, f0_max
else
    write(*,*) term_blu, 'f0_max setting: ', term_blk, 'Determined by algorithm'
endif

!===========================
! setup before all 0.0 check
!===========================

!
! convert observed data to Log space
!
SF = (sum(obs%field(1:num_obs_points)) / float(num_obs_points)) / 5.D0
obs%field(1:num_obs_points) = log((one_scallop_per_tow + obs%field(1:num_obs_points)) / SF)

!-----------------------------------------------------------------------------------------------------
! Performs a brute force least squares fit of nonlinear spatial function parameters to data points in obs.
!-----------------------------------------------------------------------------------------------------
call NLSF_Least_Sq_Fit(obs, nlsf, save_data)
! nsf may have been modified during NLSF_Select_Fit
nsf = NLSF_Get_NSF()
num_spat_fcns = nsf + 1
allocate(spatial_fcn(1:num_obs_points, 1:num_spat_fcns))
allocate( beta(1:num_spat_fcns), Cbeta(1:num_spat_fcns, 1:num_spat_fcns) )
!===========================

!SPECIAL CASE, ALL OBSERVED DATA POINTS ARE 0.0
if (abs(sum(obs%field(1:num_obs_points))) .GE. zero_threshold) then
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
    spatial_fcn = Krig_Eval_Spatial_Function(obs, num_spat_fcns, num_obs_points, nlsf, save_data)
    residual = LSF_Generalized_Least_Squares&
    &       (obs%field, spatial_fcn, residual_cov, num_obs_points, num_spat_fcns, beta, Cbeta, save_data)
    tmp = sqrt(sum(residual(1:num_obs_points)**2) / float(num_obs_points))

    if (Is_NAN(tmp)) then ! (isnan(tmp)) then
        write(*,*) term_red, 'Ordinary Least Sq Residual Failed', term_blk
        STOP 1
    else
        write(*,'(A,F10.6)')'Ordinary Least Sq Residual and result:', tmp
    endif

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
    write(*,'(A,F10.6)')'GLSres:', sqrt(sum(resOBS(1:num_obs_points)**2) / float(num_obs_points))

else
    ! THEREFORE, ALL INTERPOLATED DATA WOULD BE 0.0
    num_points = grid%num_points
    grid%field(1:num_points) = 0.0_dp
    write(*,*) term_yel, 'Obsevation data is all 0, therefore interpolated data is will be all 0', term_blk
endif
if (save_data) then
    call OutputUK(num_points, num_spat_fcns, grid, nlsf, beta, eps, Ceps, Cbeta, SF, alpha_obs)
else
    call OutputEstimates(num_points, num_spat_fcns, grid, nlsf, beta, eps, Ceps, SF, alpha_obs, fmax, domain_avg)
endif

write(*,*)'num_points, num_survey', num_points, num_obs_points

deallocate(spatial_fcn)
deallocate(beta, Cbeta)
deallocate(nlsf)
deallocate(eps, Ceps)
deallocate(residual_cov, residual)
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
subroutine Read_Startup_Config(par, alpha_obs, save_data, f0_max)
use globals
use Krig_Mod
use NLSF_Mod, only : NLS_Set_Config_File_Name => Set_Config_File_Name
implicit none
type(Krig_Class), intent(out):: par
integer j, k, io

character(72) :: input_string
character(tag_len) tag
character(value_len) value
real(dp), intent(out) :: alpha_obs
logical, intent(out)  :: save_data
real(dp), intent(out) :: f0_max

integer ncla
character(fname_len) cfg_file_name
character(fname_len) cmd, obsfile, gridfile
character(form_len) form
logical exists

! set default values for parameters not in file
alpha_obs=1.D0
save_data = .false.
f0_max = 0._dp

! Check what was entered on command line
ncla=command_argument_count()
if (ncla .ne. 4) then
    write(*,*) term_red, 'No UK configuration file given', term_blk
    call get_command(cmd)
    write(*,*) term_blu,'Typical use: '

    write(*,*) term_blu,'                                                    | --- optional but need both ---|'
    write(*,*) term_blu,'  exe      ConfigFile  ObservFile                GridFile            ZF0Max'
    write(*,*) term_yel,' UKsrc\UK   UK_GB.cfg  X_Y_EBMS_GB2005.csv   GBxyzLatLon_SW.csv    0.0 | 70.0', term_blk
    !                      arg#        1                 2                  3                   4
    stop 1
endif

call get_command_argument(1, cfg_file_name)
cfg_file_name = config_dir_interp//trim(cfg_file_name)
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
        ! Now used via python scripts, these values are determined on command line

        ! case('Observation File')
        !     Call GridMgr_Set_Obs_Data_File_Name(value)

        ! case('Log Transform')
        !     read(value,*) IsLogT

        ! case('High Limit Factor')
        !     read(value, *) fmax_multiplier

        ! case('Power Transform Parameter')
        !     if (IsLogT) then
        !         ! Forcing alpha to 1.0
        !         alpha=1.D0
        !         write(*,*) term_yel, 'Log Transform is T, ignoring Power Transform Param', term_blk
        !     else
        !         read(value,*) alpha
        !         IsHiLimit = .true.
        !         !write(*,*)'Is this the fmax you were searching for?', fmax
        !     endif

        case('Kriging variogram form')
            read(value,*) form
            form=trim(form)
            if (.not. ( any ((/ form.eq.'spherical', form.eq.'exponential', form.eq.'gaussian', form.eq.'matern'/)) )) then
                write(*,*) term_red, ' **** INVALID FORM NAME: ', form, term_blk
                stop 1
            endif
            par%form = form

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

!========================== override config file/get remaining configs =================================
call get_command_argument(2, obsfile)
call GridMgr_Set_Obs_Data_File_Name(obsfile)

! GB breaks up GridFile into four regions DEPRECATED
! 1) GBxyzLatLon_N
! 2) GBxyzLatLon_S
! 3) GBxyzLatLon_SW
! 4) GBxyzLatLon_W
! AL adds a fifth Region, but uses the same grid as MA
! 5) MAxyzLatLon
call get_command_argument(3, gridfile)
call GridMgr_Set_Grid_Data_File_Name(gridfile)

call get_command_argument(4, cmd)
read(cmd, *) f0_max

endsubroutine Read_Startup_Config
        
!---------------------------------------------------------------------------------------------------
!! Purpose: This subroutine writes files for output.  This includes a central prediction: 
!> "KrigingEstimate.txt" Predictor standard deviation  is output to "KrigSTD.txt".
!> Function coefficient 
!---------------------------------------------------------------------------------------------------
subroutine OutputUK(num_points, num_spat_fcns, grid, nlsf, beta, eps, Ceps, Cbeta, SF, alpha_obs)
use globals
use Grid_Manager_Mod
use NLSF_Mod
use LSF_Mod
use Krig_Mod
implicit none

type(Grid_Data_Class), intent(inout) :: grid
type(NLSF_Class), intent(in)::nlsf(*)
                                    
integer, intent(in) :: num_points, num_spat_fcns
real(dp), intent(in) :: eps(*), beta(*), Cbeta(num_spat_fcns,*), SF, alpha_obs! , fmax
real(dp), intent(inout) :: Ceps(num_points,*)
integer n 
real(dp) trend(num_points), V(num_points), Fg(num_points, num_spat_fcns)
logical, parameter :: save_data = .true.

write(*,*)'output SF, A=', SF, one_scallop_per_tow
do n=1, num_points
    V(n)=Ceps(n, n)
enddo
grid%field(1:num_points) = grid%field(1:num_points)**(1._dp/alpha_obs)

grid%field(1:num_points) = SF * exp( grid%field(1:num_points) + V(1:num_points)/2. ) - one_scallop_per_tow  ! adjusted inverse log(one_scallop_per_tow+f)
call Write_Vector_Scalar_Field(num_points, grid%field, 'KrigingEstimate.txt')

Fg = Krig_Eval_Spatial_Function(grid, num_spat_fcns, num_points, nlsf, save_data)
trend(1:num_points) = matmul( Fg(1:num_points, 1:num_spat_fcns), beta(1:num_spat_fcns)) 

trend(1:num_points) = SF*exp(trend(1:num_points))-one_scallop_per_tow
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

call Write_Vector_Scalar_Field(num_points, SF*exp(sqrt(V(1:num_points)))-one_scallop_per_tow, 'KrigSTD.txt')

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
subroutine OutputEstimates(num_points, num_spat_fcns, grid, nlsf, beta, eps, Ceps, SF, alpha_obs, fmax, domain_avg)
use globals
use Grid_Manager_Mod
use NLSF_Mod
use LSF_Mod
use Krig_Mod

implicit none

integer, intent(in) :: num_points, num_spat_fcns
type(Grid_Data_Class), intent(inout) :: grid
type(NLSF_Class), intent(in)::nlsf(*)
real(dp), intent(in) :: Ceps(num_points,*), beta(*), eps(*)
real(dp), intent(in) :: SF,fmax
real(dp), intent(in) :: alpha_obs
real(dp), intent(in) :: domain_avg
real(dp) MuY

integer n
real(dp), allocatable :: trend(:), V(:), Fg(:,:)
character(fname_len) fname, fout
character(80) fmtstr

!=======================================================================================
character(fname_len) feps, fdata, ftrend, fstd
!=======================================================================================

allocate(trend(1:num_points), V(1:num_points), Fg(1:num_points, 1:num_spat_fcns))

! SPECIAL CASE, if all data points are 0 then no reverse processing necessary
if (sum(grid%field(1:num_points)) .NE. 0.0) then

    ! compute Fg using grid in logT space
    Fg = Krig_Eval_Spatial_Function(grid, num_spat_fcns, num_points, nlsf, .FALSE.)

    do n=1, num_points
        V(n)=Ceps(n, n)
    enddo
    grid%field(1:num_points) = grid%field(1:num_points)**(1./alpha_obs)    
!!!!    grid%field(1:num_points) = SF * exp( grid%field(1:num_points) + V(1:num_points)/2. ) - one_scallop_per_tow  ! adjusted inverse log(one_scallop_per_tow+f)

    !
    ! convert results back to linear space
    !
    grid%field(1:num_points) = SF * exp( grid%field(1:num_points) ) - one_scallop_per_tow  ! adjusted inverse log(one_scallop_per_tow+f)
    write(*,'(A,A,2F12.6,A)') term_blu, 'output SF, A=', SF, one_scallop_per_tow, term_blk
endif

! grid is in linear space
call Limit_Field(num_points, grid, fmax)
MuY = sum( grid%field(1:num_points) )/float(num_points)
grid%field(:) = domain_avg * grid%field(:) / MuY
call Limit_Field(num_points, grid, fmax)

! Save results along with location information
fname = GridMgr_Get_Obs_Data_File_Name()
! Change output directory and file prefix
! /X_Y_...
! n12345
n = index(fname, '/') + 5
fout = output_dir//'Lat_Lon_Grid_'//fname(n:)
fmtstr='(2(ES14.7 : ", ") (ES14.7 : ))'
write(*,*) term_blu,'Writing ouput to: ', trim(fout), term_blk
open(63,file=trim(fout))
do n=1, num_points
    write(63, fmtstr) grid%lat(n), grid%lon(n), grid%field(n)
enddo
close(63)

!=======================================================================================
! post run analyze results

! Change output directory and file prefix
! /X_Y_...
! n12345
n = index(fname, '/') + 5
feps   = anal_dir//'Lat_Lon_Grid_Epsilon-'//fname(n:)
fdata  = anal_dir//'Lat_Lon_Grid_Field-'//fname(n:)
ftrend = anal_dir//'Lat_Lon_Grid_SpatialTrend-'//fname(n:)
fstd   = anal_dir//'Lat_Lon_Grid_KrigSTD-'//fname(n:)
fmtstr='(2(ES14.7 : ", ") (ES14.7 : ))'

open(63,file=trim(feps))
open(64,file=trim(fdata))
open(65,file=trim(ftrend))
open(66,file=trim(fstd))

! Fg is in LogT space
trend(1:num_points) = matmul( Fg(1:num_points, 1:num_spat_fcns), beta(1:num_spat_fcns)) 
do n=1, num_points
    write(63, fmtstr) grid%lat(n), grid%lon(n), SF * exp(eps(n)) - one_scallop_per_tow
    write(64, fmtstr) grid%lat(n), grid%lon(n), grid%field(n)   ! already in linear space
    write(65, fmtstr) grid%lat(n), grid%lon(n), SF * exp(trend(n))-one_scallop_per_tow
    write(66, fmtstr) grid%lat(n), grid%lon(n), SF * exp(sqrt(V(n))) - one_scallop_per_tow
enddo
close(66)
close(65)
close(64)
close(63)
!=======================================================================================

deallocate(trend, V, Fg)

endsubroutine OutputEstimates

!----------------------------------------------------------------------------------------
!subroutine Limit_Field(n,grid,fpeak)
!
! Purpose: Restrict maximum output 
! The curves here are derived from scallop dredge survey data from 1979 to 2018.
!
! inputs: 
!   n - length of vectors f and z
!   fpeak - highest observed value of survey growth
!
! input/output
!   grid%field - [n] intepolated data.
! Keston Smith 2022
!----------------------------------------------------------------------------------------
subroutine Limit_Field(n,grid,fpeak)
    use globals
    use Grid_Manager_Mod
        
    integer, intent(in) :: n
    type(Grid_Data_Class), intent(inout) :: grid
    real(dp), intent(in) :: fpeak
    real(dp) a,w,zc,fmax 
    integer j,kappa
    
    do j=1,n
        if (grid%lon(j) > ma_gb_border) then  ! GB
            zc = 70._dp
            w = 50._dp
            a = 0.01_dp
            kappa = 20
        else
            zc = 60._dp
            w = 33._dp
            a = 0.01_dp
            kappa = 10
        endif
    
        grid%field(j) = max( grid%field(j), 0._dp )
        fmax= fpeak * (a + exp(-((grid%z(j) - zc) / w )**kappa))
        grid%field(j) = min( grid%field(j) , fmax )
    enddo    
    return
endsubroutine
