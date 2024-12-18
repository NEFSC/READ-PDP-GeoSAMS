!--------------------------------------------------------------------------------------------------
!> @page page2 Non Linear Spatial Functions, NLSF
!>
!> TBD - More information on what these methods do.
!>
!> To easily change how the NLSF functions perform a configuration file is used. 
!> The NLSF configuration file is named by the UK configuration file, configuration item 
!> <b>NLS Spatial Fcn File Name</b>. The subroutine <em><b>NLSF::Set_Config_File_Name</b></em> is 
!> called to set this value. When UK first starts is must call <em><b>NLSF_Count_Defined_Functions</b></em> 
!> to determine how many spatial functions are defined.
!>
!> The following configuration items are defined.
!> * Function String
!>
!> @section m3p1 Function String
!>
!> @subsection m3p1p1 Function
!> Define non linear spatial functions and paramater search range.
!>
!> - "Function 1, dim=z, shape=Logistic, precon=0 "
!> - "Function 2, dim=z, shape=Gaussian, precon=0 "
!> - "Function 3, dim=x, shape=Logistic, precon=1 "
!>
!> These define spatial functions for setting the spatial trend in the universal kriging algorithm. 
!>
!> The precon=0 term means that the function is not multiplied by another function. For example,\n
!> @verbatim
!>    "Function 3, dim=x, shape=Logistic, precon=1 "\n
!> @endverbatim
!> indicates that the third function is multiplied by the first function.  
!> This is true for fitting the nonlinear parameters of function 3 hence 
!> the parameters of function 1 must be fit before the parameters of function 3.
!>
!> @subsection m3p1p2 dim
!> * 'x'
!> * 'y'
!> * 'z'
!> * 'x+y'
!>
!> @subsection m3p1p3 shape
!> Let @f$\vec{A} = \vec{dim} - f0@f$
!> * 'Gaussian': @f$exp( - ( \vec{A} / \lambda )^2 )@f$
!> * 'Logistic': @f$1 / \left( 1 + exp( - \vec{A} / \lambda ) \right)@f$
!> * 'SinExp': @f$sin(\vec{A}/\lambda ) * exp(-(\vec{A}/\lambda )^2)@f$
!> * 'CosExp': @f$cos(\vec{A}/\lambda ) * exp(-(\vec{A}/(2\lambda) )^2)@f$
!>
!> @section m3p3 IsTruncateRange
!> * Set to F to extrapolate beyond observation range
!> * Set to T to restrict within observation range
!>
!> @section m3p4 ZF0Max
!> This configuration item is optional. The default is blank which then allows the algorithm to set
!> the maximum value for Z, i.e. depth. A nonzero value will limit the z value to that setting.
!> This configuration item can also be set on the command line.
!>
!> @section m3p5 Use Original Data
!> On each loop for the least squares fit, if true, the algorithm restores the data to the original data.
!> If false, the algorithm will used the residual data.
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS) 2024
!--------------------------------------------------------------------------------------------------

module NLSF_Mod
use globals
use Grid_Manager_Mod
use LSF_Mod
implicit none

type NLSF_Class
    real(dp) lambda, f0
    real(dp) lambda_min, lambda_max
    real(dp) f0_min, f0_max
    real(dp) rms
    character(12) form
    character(3) axis
    integer precon
end type NLSF_Class

character(fname_len), PRIVATE :: config_file_name
integer, PRIVATE :: nsf
integer, PRIVATE :: nsflim
logical, PRIVATE :: is_truncate_range
logical, PRIVATE :: use_orig_data
real(dp), PRIVATE :: z_f0_max

CONTAINS

!-----------------------------------------------------------------------------------------------
!> Getter functions
!-----------------------------------------------------------------------------------------------
logical function NLSF_Get_Is_Truncate_Range()
    NLSF_Get_Is_Truncate_Range = is_truncate_range
endfunction NLSF_Get_Is_Truncate_Range

logical function NLSF_Get_Use_Orig_Data()
    NLSF_Get_Use_Orig_Data = use_orig_data
endfunction NLSF_Get_Use_Orig_Data

integer function NLSF_Get_NSF_Limit()
    NLSF_Get_NSF_Limit = nsflim
endfunction NLSF_Get_NSF_Limit

integer function NLSF_Get_NSF()
    NLSF_Get_NSF = nsf
endfunction NLSF_Get_NSF

real(dp) function NLSF_Get_Z_F0_Max()
NLSF_Get_Z_F0_Max = z_f0_max
endfunction NLSF_Get_Z_F0_Max

!-----------------------------------------------------------------------------------------------
!> Used during instantiation to set the name of the file to read to for configuration parameters
!> @brief Read Input File
!> 
!> Sets name of a configuration file, 'config_file_name.cfg'
!-----------------------------------------------------------------------------------------------
subroutine Set_Config_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    config_file_name = config_dir_interp//fname
    inquire(file=config_file_name, exist=exists)

    if (exists) then
        PRINT *, term_blu, trim(config_file_name), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(config_file_name), ' NOT FOUND', term_blk
        stop 1
    endif
endsubroutine Set_Config_File_Name

!--------------------------------------------------------------------------------------------------
!! Count the number of defined spatial functions
!> 
!--------------------------------------------------------------------------------------------------
integer function NLSF_Count_Defined_Functions()
integer n, io
character(input_str_len) :: input_string

open(69,file=config_file_name)
n = 0
do
    input_string = ""
    read(69,'(a)',iostat=io) input_string
    if (io.lt.0) exit
    if(input_string(1:1).eq.'F') n = n+1
end do
close(69)
! if (n .eq. 0) then
!     write(*,*) term_red, 'No spatial functions are defined. Cannot proceed.', term_blk
!     stop 1
! endif
NLSF_Count_Defined_Functions = n
nsf = n
return
endfunction NLSF_Count_Defined_Functions

!--------------------------------------------------------------------------------------------------
!> Define non linear spatial functions(NLSF) and paramater search range.
!>
!> The file "SpatialFcns.cfg" contains a set of lines of the form
!> - "Function 1, dim=z, shape=Logistic, precon=0 "
!> - "Function 2, dim=z, shape=Gaussian, precon=0 "
!> - "Function 3, dim=x, shape=Logistic, precon=1 "
!>
!> These define spatial functions for setting the spatial trend in the universal kriging algorithm. 
!>
!> The precon=0 term means that the function is not multiplied by another function.
!> "Function 3, dim=x, shape=Logistic, precon=1 " indicates that the third function is multiplied by
!> the first function.  This is true for fitting the nonlinear parameters of function 3 hence 
!> the parameters of function 1 must be fit before the parameters of function 3.
!>
!> The function is called twice from the main program.  In the first call
!> InitiialCallFlag= T and the number of nonlinear spatial functions is returned.
!>
!> In the second call InitiialCallFlag= F and all of the functions are defined.
!> with precon=0 the nonlinear paraters function is fit in a least
!--------------------------------------------------------------------------------------------------
integer function NLSF_Define_Functions(nlsf, p, f0_max)
type(NLSF_Class) :: nlsf(*)
type(Grid_Data_Class):: p
integer j, io, n, num_points
real(dp), intent(in) :: f0_max
character(input_str_len) :: input_string

is_truncate_range = .true. !default
use_orig_data = .true.
z_f0_max = f0_max ! either default to 0.0 or read in on command line

open(69,file=config_file_name)
n=0
do
    input_string = ""
    read(69, '(a)', iostat=io) input_string
    if (io.lt.0) exit

    select case (input_string(1:1))
    case('#')
        ! Ignore comment

    case('F')
        n = n+1
        j = index(input_string, 'dim=',back=.true.)
        nlsf(n)%axis= trim( adjustl( input_string(j+4:j+4) ) )
        j = index(input_string, 'shape=',back=.true.)
        select case (input_string(j+6:j+6))
            case('G')
                nlsf(n)%form = 'Gaussian'
            case('L')
                nlsf(n)%form = 'Logistic'
            case('S')
                nlsf(n)%form = 'SinExp'
            case('C')
                nlsf(n)%form = 'CosExp'
            case default
                write(*,*) term_red,'Unrecognized function Form in ', trim(config_file_name) , ': ', term_blk, input_string
                stop 1
        end select
        j = index(input_string,"precon=",back=.true.)
        read( input_string(j+7:),* )nlsf(n)%precon

    case ('I')
        j = index(input_string,"=",back=.true.)
        read( input_string(j+1:),* ) is_truncate_range

    case ('Z')
        j = index(input_string,"=",back=.true.)
        read( input_string(j+1:),* ) z_f0_max
        ! if non-zero overwrite config file setting
        if (f0_max > 0._dp) z_f0_max = f0_max

    case ('U')
        j = index(input_string,"=",back=.true.)
        read( input_string(j+1:),* ) use_orig_data

    case default
        write(*,*) term_red,'Unrecognized Configuration Item in ', trim(config_file_name) , ': ', term_blk, input_string
        stop 1
endselect
end do
close(69)

! set private variables to the number of detected spatial functions
if (n.NE.nsf) then
    write(*,*) term_red, 'OOPS, something is wrong, Number of Spatial Functions do no match', term_blk, n, nsf
    STOP 1
endif
nsflim = n
NLSF_Define_Functions = n

num_points = p%num_points
do n = 1, nsf
    select case (trim(nlsf(n)%axis))
        case ('x')
            nlsf(n)%f0_min = minval(p%x(1:num_points))
            nlsf(n)%f0_max = maxval(p%x(1:num_points))
            nlsf(n)%lambda_min = 5000.
        case ('y')
            nlsf(n)%f0_min = minval(p%y(1:num_points))
            nlsf(n)%f0_max = maxval(p%y(1:num_points))
            nlsf(n)%lambda_min = 5000.
        case ('z')
            nlsf(n)%f0_min = minval(p%z(1:num_points))
            if (z_f0_max > 0._dp) then
                nlsf(n)%f0_max = z_f0_max
            else
                nlsf(n)%f0_max = maxval(p%z(1:num_points))
            endif
            nlsf(n)%lambda_min = 5.
        case ('x+y')
            nlsf(n)%f0_min = minval(p%x(1:num_points)+p%y(1:num_points))
            nlsf(n)%f0_max = maxval(p%x(1:num_points)+p%y(1:num_points))
            nlsf(n)%lambda_min = 5000.
        case default
            write(*,*) 'Error unkown spatial dimension in nlsf(b):', n, trim(nlsf(n)%axis)
            stop 1
    end select
    nlsf(n)%lambda_max = nlsf(n)%f0_max - nlsf(n)%f0_min
enddo
endfunction

!--------------------------------------------------------------------------------------------------
!> Purpose: Performs a brute force least squares fit of nonlinear spatial function parameters to data points in 
!> obs.
!>
!> inputs:
!> - obs: Grid_Manager_Mod(see UniversalKriging.f90) Defines observations to be fit. 
!> - the residual from the preceeding function is fit. 
!>
!> inputs/output:
!>  - nlsf: Nonlinear spatial function(see UniversalKriging.f90). Defines a vector of nonlinear 
!>          spatial functions. On return nlsf(1:nsf)%f0 and nlsf(1:nsf)%lambda are specified.
!>
!> outputs: 
!>  - f: p%num_points length vector of values of nlsf at points defined in p.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine NLSF_Least_Sq_Fit(obs, nlsf, save_data)
type(Grid_Data_Class), intent(in) :: obs
type(NLSF_Class), intent(inout) ::nlsf(*)
logical, intent(in) :: save_data

integer j, num_obs_points, k
real(dp), allocatable :: residual_vect(:), field_precond(:), residuals(:, :), rms(:)
integer, allocatable:: RankIndx(:)

real(dp) mu,rms0
integer nsf_local, nsflim_local

character(10) buf
logical, parameter :: sort = .true.

if (nsf > 0) then
    nsf_local = nsf
    nsflim_local = nsflim

    num_obs_points = obs%num_points

    write(*,'(A,A,A,I5,A,A,A,I3)') term_blu, 'NLSF_Least_Sq_Fit: Number Obs Points: ', term_blk, num_obs_points, &
    &                             term_blu,'  Number Spatial Functions Defined: ', term_blk, nsf_local
            
    allocate(residual_vect(1:num_obs_points), field_precond(1:num_obs_points), residuals(1:num_obs_points, 1:nsf_local))
    allocate(rms(1:nsf_local), RankIndx(1:nsf_local))

    ! initialize residual 0
    residual_vect(:) = obs%field(1:num_obs_points)
    write(*,'(A,F10.6)') 'residual  0: ', sqrt(sum(residual_vect(1:num_obs_points)**2) / float(num_obs_points))

    do j = 1, nsf_local
        if(use_orig_data) residual_vect(:) = obs%field(1:num_obs_points)
        k = nlsf(j)%precon
        if (k.eq.0) then
            !no preconditioning
            field_precond(:) = 1.D0
        else
            if (k.ge.j)then
                write(*,'(A, I2, I2)') 'Preconditiong function must be indexed first:', k, j
                stop 1
            endif
            field_precond(:) = NLSF_Eval_Semivariance(obs, nlsf(k))
        endif
        residual_vect = NLSF_Fit_Function(obs, nlsf(j), residual_vect, field_precond)
        nlsf(j)%rms = sqrt(sum(residual_vect(1:num_obs_points)**2) / float(num_obs_points))
        rms(j) = nlsf(j)%rms
        residuals(:, j) = residual_vect(:)
        write(*,'(A,I2,A,F10.6, A, A3, A, I2, A, I2)')'residual ', j, ': ', nlsf(j)%rms, &
        &    '  Precon Axis: ', trim(nlsf(j)%axis), '  Precon Number', k, '  Function Number', j
    enddo

    if (save_data) then
        call Write_CSV(num_obs_points, nsf_local, residuals, 'residuals.csv', num_obs_points, .false.)
        open(63, file = 'NLSF_Class.csv')
        do j = 1, nsf_local
            write(63,*)nlsf(j)%axis, ', ', trim(nlsf(j)%form), ', ', nlsf(j)%f0, ', ', nlsf(j)%lambda
        enddo
        close(63)
    endif

    if ((use_orig_data) .and. sort) then
        !sort functions into increasing rms
        write(buf,'(I3)') nsf_local
        write(*,'(A,A,'//trim(buf)//'(F10.6))') term_grn, 'rms:',rms(1:nsf_local)
        do k = 1,nsf_local
            j = minloc(rms(1:nsf_local),1)
            write(*,'(I3,I3,F10.6)') k, j, rms(j)
            RankIndx(k) = j
            rms(j) = huge(1.D0)
        enddo
        write(buf,'(I3)') nsf_local
        write(*,'(A,'//trim(buf)//'(I2))') 'function rank:', RankIndx(1:nsf_local)
        nlsf(1:nsf_local) = nlsf(RankIndx(1:nsf_local))
        residuals(1:num_obs_points, 1:nsf_local) = residuals(1:num_obs_points,RankIndx(1:nsf_local))
        nsf_local = min(nsf_local,nsflim_local)

        write(*,'(A,'//trim(buf)//'F10.6)') 'function rms:', nlsf(1:nsf_local)%rms
        mu = sum(obs%field(1:num_obs_points)) / float(num_obs_points)
        rms0   = sqrt(sum((obs%field(1:num_obs_points) - mu  )**2) / float(num_obs_points))

        j = 1
        if (save_data) call write_csv(num_obs_points,nsf_local,residuals,'residuals0.csv',num_obs_points, .false.)
        do while( ( nlsf(j)%rms .lt. 0.9_dp * rms0 + 0.1_dp * nlsf(1)%rms ) .and. (j+1 .lt. nsf_local) )
            j = j+1
            write(*,'(A,5F10.6)')'delta rms:   ', nlsf(j)%rms, rms0, nlsf(1)%rms, 0.9_dp*rms0 + 0.1_dp*nlsf(1)%rms, &
            & sum((residuals(1:num_obs_points, j+1) - residuals(1:num_obs_points,j))**2 )/float(num_obs_points)
        enddo
        write(*,*) term_blk
        j = j - 1
        nsflim = j
        nsf = j
        write(*,*) term_blu, 'NLSF limit (nsf in use)=', term_blk, nsflim

    else
        nsf = nsf_local
        do j=1,nsf
            write(*,'(A,I3,A,F10.6)')'spatial function',j,'  postfit rms: ',rms(j)
        enddo
    endif

    deallocate(residual_vect, field_precond, residuals, rms, RankIndx)

endif ! nsf > 0
endsubroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Evaluates nonlinear spatial function with parameters x0 ad lambda at points in p. Values 
!> returned in vector f.
!>
!> inputs:
!> - p: Grid_Manager_Mod(see UniversalKriging.f90) Defines spatial point grid/field 
!> - nlsf: Nonlinear spatial function(see UniversalKriging.f90) Defines a nonlinear spatial function 
!>
!> outputs: 
!> - f: p%num_points length vector of values of nlsf at points defined in p.
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
function NLSF_Eval_Semivariance (p, nlsf)
type(Grid_Data_Class), intent(in):: p
type(NLSF_Class), intent(in)::nlsf
real(dp) :: NLSF_Eval_Semivariance(1:p%num_points)
real(dp), allocatable :: x(:)
integer num_points, j
num_points = p%num_points

allocate(x(1:num_points))

select case (trim(nlsf%axis))
case ('x')
    x(1:num_points) = p%x(1:num_points)
case ('y')
    x(1:num_points) = p%y(1:num_points)
case ('z')
    x(1:num_points) = p%z(1:num_points)
case ('x+y')
    x(1:num_points) = p%x(1:num_points)+p%y(1:num_points)
case default ! this should not occur as the variable values have already been checked
    write(*,*) 'Error unkown spatial dimension in nlsf(a):', trim(nlsf%axis)
    stop 1
end select

!Bound interpolation range to observed range 
if (is_truncate_range) then
    do j = 1, num_points
        if(x(j).lt.nlsf%f0_min) x(j) = nlsf%f0_min
        if(x(j).gt.nlsf%f0_max) x(j) = nlsf%f0_max
    enddo
endif

select case (trim(nlsf%form))
case ('Logistic')
    NLSF_Eval_Semivariance(:) = 1.D0 / ( 1.D0 + exp( - ( x(:) - nlsf%f0 ) / nlsf%lambda ) )

case ('Gaussian')
    NLSF_Eval_Semivariance(:) = exp( - ( (x(:) - nlsf%f0) / nlsf%lambda )**2 )

case ('SinExp')
    NLSF_Eval_Semivariance(:) = sin((x(:)-nlsf%f0)/nlsf%lambda ) * exp(-((x(:) - nlsf%f0)/nlsf%lambda )**2)

case ('CosExp')
    NLSF_Eval_Semivariance(:) = cos((x(:)-nlsf%f0)/nlsf%lambda ) * exp(-((x(:) - nlsf%f0)/(2.*nlsf%lambda))**2)

case default ! this should not occur as the variable values have already been checked
    write(*,*) 'Error unkown spatial form in nlsf(a):', trim(nlsf%form)
    stop 1
end select

deallocate(x)
endfunction NLSF_Eval_Semivariance

!----------------------------------------------------------------------------------------
!> Purpose: Calculate smoothing penalty for nonlinear spatial function fit.  Penalty is 
!> based on symbolic integral from -inf to inf of the functions second derivative squared.
!> i.e.: integral (d2 f /d x2)^2 from -inf to inf = smoothness penalty -> p
!> see: SageScriptSmoothing.s for derivation
!> input
!> - nlsf: Nonlinear spatial function(see UniversalKriging.f90)
!>
!> output
!> - p: smoothness penalty  
!>
!>
!> @author keston Smith (IBSS corp) 2022
!----------------------------------------------------------------------------------------
real(dp) function NLSF_Smooth_Penalty (nlsf)
type(NLSF_Class)::nlsf

select case (trim(nlsf%form))
case ('Logistic')
    NLSF_Smooth_Penalty = (1.D0/30.D0) / nlsf%lambda**3

case ('Gaussian')
    NLSF_Smooth_Penalty = 3.D0 * sqrt(pi/2.D0) / nlsf%lambda**3

case ('SinExp')
    NLSF_Smooth_Penalty = 0.25D0*sqrt(2.D0*pi)*(10.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3

case ('CosExp')
    NLSF_Smooth_Penalty = (1.D0/32.D0)*sqrt(2.D0*pi)*(43.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3

case default ! remove unintialized warning
    NLSF_Smooth_Penalty = (1.D0/30.D0) / nlsf%lambda**3
end select
endfunction NLSF_Smooth_Penalty

!--------------------------------------------------------------------------------------------------
!> Purpose: Fit nonlinear parameters nlsf%f0 and nlsf%lambda to observations at num_points points defined in 
!> obs with values y.  f is a function defined at the observation points. The penalty function is \n
!>           sum_i (a+b*nlsf%g_i(x0,lambda)*f_i - y_i)**2 \n
!> where a and b are estimated using simple linear regresion for each nonlinear parameter pair x_0, 
!> lambda.  The minimization is done with a brute force search over np=500 equally spaced values 
!> between (nlsf%f0_min and nlsf%f0_max) and (nlsf%f0_min and  nlsf%f0_max) respectivly.
!>
!> inputs:
!> - obs: Grid_Manager_Mod(see UniversalKriging.f90) Defines spatial observation points
!> - y: vector of observation values
!> - f: vector of function values (a preconditioning function) at obs.
!>
!> input/output
!> - nlsf: Nonlinear spatial function(see UniversalKriging.f90) Defines a nonlinear spatial 
!>             function. On exit optimal values of x_0 and lambda are specified 
!>
!> outputs: 
!> - residual: residual, r_i=y_i - nlsf(x_i |x0,lambda) where i=1..# of observations.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
function NLSF_Fit_Function(obs, nlsf, y, f)

type(Grid_Data_Class), intent(in) :: obs
type(NLSF_Class), intent(inout) ::nlsf
real(dp), intent(in):: y(*), f(*)
real(dp) :: NLSF_Fit_Function(1:obs%num_points)

real(dp)  err_min, err, x0hat, lambdahat, smoothness_penalty, spf, rms
integer j, k, num_points
real(dp), allocatable :: s(:), p(:), x0(:), lambda(:), lpr(:), pg(:)
integer, parameter :: np = 500

num_points = obs%num_points
allocate(s(1:num_points), lpr(1:num_points), p(1:np), x0(1:np), lambda(1:np), pg(1:num_points))

! linear interpolation positions???
do j = 1, np
    p(j) = float(j-1) / float(np-1)
enddo

!----------------------------------------------
! set smoothness to dominate below lambda_min
! spf: penalizes integral [d2 f /d x2 f(x)]^2
! set spf=0 for no roughness penalty
!----------------------------------------------
rms = sqrt( sum(y(1:num_points)**2) / float(num_points) )

nlsf%lambda = nlsf%lambda_min
smoothness_penalty = NLSF_Smooth_Penalty(nlsf)
spf = rms/smoothness_penalty
x0(1:np) = nlsf%f0_min + ( nlsf%f0_max - nlsf%f0_min ) * p(1:np)
lambda(1:np) = nlsf%lambda_min + (nlsf%lambda_max - nlsf%lambda_min) * p(1:np)
!----------------------------------------------

err_min = Huge(1.D0)
x0hat = x0(1)
lambdahat = lambda(1)
! index x0
do j = 1, np
    ! index lambda
    do k = 1, np
        nlsf%f0 = x0(j)
        nlsf%lambda = lambda(k)
        s(:) = NLSF_Eval_Semivariance(obs, nlsf)
        lpr = LSF_Simple_Linear_Regression(y(1:num_points), s(1:num_points) * f(1:num_points), num_points)
        rms = sqrt( sum((y(1:num_points) - lpr(1:num_points))**2) / float(num_points) )
        smoothness_penalty = NLSF_Smooth_Penalty(nlsf) ! penalize roughness analytic approximation
        err = rms + spf * smoothness_penalty
        if (err.lt.err_min)then
            ! write(*,*)nlsf%form, err, rms, spf*smoothness_penalty
            err_min = err
            x0hat = x0(j)
            lambdahat = lambda(k)
        endif
    enddo
enddo
nlsf%f0 = x0hat
nlsf%lambda = lambdahat
s(:) = NLSF_Eval_Semivariance(obs, nlsf)
lpr = LSF_Simple_Linear_Regression(y, s(1:num_points) * f(1:num_points), num_points)
NLSF_Fit_Function(1:num_points) = y(1:num_points) - lpr(1:num_points)

deallocate(s, p, x0, lambda, lpr)

endfunction NLSF_Fit_Function

endmodule