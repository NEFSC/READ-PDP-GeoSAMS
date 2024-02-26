!--------------------------------------------------------------------------------------------------
!> @page page2 Non Linear Spatial Functions
!>
!>
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS) 2024
!--------------------------------------------------------------------------------------------------

module NLSF_Mod
use globals
use GridManagerMod
use LSF_Mod
implicit none

type NLSF_Class
    real(dp) lambda, f0
    real(dp) lambda_min, lambda_max
    real(dp) f0_min, f0_max
    real(dp) f_range(2)
    real(dp) rms
    character(12) form
    character(3) axis
    !integer nsf, pre_cond_fcn_num, IsTruncateRange, UseGreedyFit, nsflim
    integer pre_cond_fcn_num
end type NLSF_Class

character(fname_len), PRIVATE :: config_file_name
integer, PRIVATE :: nsf
integer, PRIVATE :: nsflim
logical, PRIVATE :: is_truncate_range
logical, PRIVATE :: use_greedy_fit

CONTAINS

!-----------------------------------------------------------------------------------------------
!> Getter functions
!-----------------------------------------------------------------------------------------------
logical function Get_Use_Greedy_Fit()
    Get_Use_Greedy_Fit = use_greedy_fit
endfunction Get_Use_Greedy_Fit

logical function Get_Is_Truncate_Range()
    Get_Is_Truncate_Range = is_truncate_range
endfunction Get_Is_Truncate_Range

integer function Get_NSF_Limit()
    Get_NSF_Limit = nsflim
endfunction Get_NSF_Limit

integer function Get_NSF()
    Get_NSF = nsf
endfunction Get_NSF

!-----------------------------------------------------------------------------------------------
!> Determines which function to call to do a brute force least squares fit of nonlinear spatial 
!> function parameters to data points in obs.
!>
!> NOTE: Greedy function takes significantly longer to run
!-----------------------------------------------------------------------------------------------
subroutine NLSF_Select_Fit(obs, nlsf, proc_recruits)
type(Grid_Data_Class):: obs
type(NLSF_Class)::nlsf(*)
logical, intent(in) :: proc_recruits

if (use_greedy_fit) then
    call NLSF_Greedy_Least_Sq_Fit(obs, nlsf, proc_recruits)
else
    call NLSF_Least_Sq_Fit(obs, nlsf, proc_recruits)
endif

endsubroutine NLSF_Select_Fit
    

!-----------------------------------------------------------------------------------------------
!> Used during instantiation to set the name of the file to read to for configuration parameters
!> @brief Read Input File
!> 
!> Sets name of a configuration file, 'config_file_name.cfg'
!-----------------------------------------------------------------------------------------------
subroutine Set_Config_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    config_file_name = config_dir//fname
    inquire(file=config_file_name, exist=exists)

    if (exists) then
        PRINT *, term_blu, trim(config_file_name), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(config_file_name), ' NOT FOUND', term_blk
        stop
    endif
endsubroutine Set_Config_File_Name

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
integer function NLSF_Define_Functions(nlsf, p, InitialCallFlag)
type(NLSF_Class) :: nlsf(*)
type(Grid_Data_Class):: p
integer j, io, n, num_points
logical, intent(in):: InitialCallFlag
character(72) :: input_string

is_truncate_range = .true. !default
use_greedy_fit = .false.

open(69,file=config_file_name)
n=0

!-------------------------------------------------------------------------------------
if (InitialCallFlag) then
    ! only counting the number of defined 'Function n'
    do
        input_string = ""
        read(69,'(a)',iostat=io) input_string
        if (io.lt.0) exit
        if(input_string(1:1).eq.'F') n = n+1
    end do
    close(69)
    if (n .eq. 0) then
        write(*,*) term_red, 'No spatial functions are defined. Cannot proceed.', term_blk
        stop
    endif
    NLSF_Define_Functions = n
    return
endif

!-------------------------------------------------------------------------------------
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
                write(*,*) 'Unrecognized function Form in SpatialFcns.inp:', input_string
                stop
        end select
        j = index(input_string,"precon=",back=.true.)
        read( input_string(j+7:),* )nlsf(n)%pre_cond_fcn_num

    case ('G')
        j = index(input_string,"=",back=.true.)
        read( input_string(j+1:),* ) use_greedy_fit

    case ('I')
        j = index(input_string,"=",back=.true.)
        read( input_string(j+1:),* ) is_truncate_range
    endselect
 end do
close(69)

! set private variables to the number of detected spatial functions
nsf = n
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
            nlsf(n)%f0_max = maxval(p%z(1:num_points))
            nlsf(n)%lambda_min = 5.
        case ('x+y')
            nlsf(n)%f0_min = minval(p%x(1:num_points)+p%y(1:num_points))
            nlsf(n)%f0_max = maxval(p%x(1:num_points)+p%y(1:num_points))
            nlsf(n)%lambda_min = 5000.
        case default
            write(*,*) 'Error unkown spatial dimension in nlsf(b):', n, trim(nlsf(n)%axis)
            stop
    end select
    nlsf(n)%lambda_max = nlsf(n)%f0_max - nlsf(n)%f0_min
    nlsf(n)%f_range(1) = nlsf(n)%f0_min
    nlsf(n)%f_range(2) = nlsf(n)%f0_max
enddo
endfunction

!--------------------------------------------------------------------------------------------------
!> Purpose: Performs a brute force least squares fit of nonlinear spatial function parameters to data points in 
!> obs.
!>
!> inputs:
!> - obs: GridManagerMod(see UniversalKriging.f90) Defines observations to be fit. 
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
subroutine NLSF_Least_Sq_Fit(obs, nlsf, proc_recruits)
type(Grid_Data_Class), intent(in) :: obs
type(NLSF_Class), intent(inout) ::nlsf(*)
logical, intent(in) :: proc_recruits

integer j, num_obs_points, k
real(dp), allocatable :: residual_vect(:), field_precond(:), residuals(:, :), rms(:)
integer, allocatable:: RankIndx(:)

num_obs_points = obs%num_points

write(*,*)'NLSF_Least_Sq_Fit', num_obs_points, nsf
allocate(residual_vect(1:num_obs_points), field_precond(1:num_obs_points), residuals(1:num_obs_points, 1:nsf))
allocate(rms(1:nsf), RankIndx(1:nsf))

! initialize residual 0
residual_vect(:) = obs%field_psqm(1:num_obs_points)
write(*,'(A,F18.16)') 'residual  0: ', sqrt(sum(residual_vect(1:num_obs_points)**2)/float(num_obs_points))

do j = 1, nsf
    if (nlsf(j)%pre_cond_fcn_num.eq.0) then
        !no preconditioning
        field_precond(:) = 1.D0
    else
        k = nlsf(j)%pre_cond_fcn_num
        if (k.ge.j)then
            write(*,'(A, I2, I3)') 'Preconditiong function must be indexed first:', k, j
            stop
        endif
        write(*,'(A, I2, I3)') 'Preconditioning Function Number', k, j
        field_precond(:) = NLSF_Evaluate_Fcn (obs, nlsf(k))
    endif
    residual_vect = NLSF_Fit_Function(obs, nlsf(j), residual_vect, field_precond)

    nlsf(j)%rms = sqrt( sum( residual_vect(:)**2 )/float(num_obs_points) )
    rms(j) = nlsf(j)%rms
    residuals(:, j) = residual_vect(:)
    write(*,'(A,I2,A,F18.16)')'residual ', j, ': ', sqrt(sum(residual_vect(:)**2)/float(num_obs_points))
enddo

if (proc_recruits) then
    call write_csv(num_obs_points, nsf, residuals, 'residuals.csv', num_obs_points)
    open(63, file = 'NLSF_Class.csv')
    do j = 1, nsf
        write(63,*)nlsf(j)%axis, ', ', trim(nlsf(j)%form), ', ', nlsf(j)%f0, ', ', nlsf(j)%lambda
    enddo
    close(63)
endif

do j = 1, nsf
    write(*,*)'spatial function', j, 'postfit rms:', rms(j)
enddo

deallocate(residual_vect, field_precond, residuals, rms)
endsubroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Evaluates nonlinear spatial function with parameters x0 ad lambda at points in p. Values 
!> returned in vector f.
!>
!> inputs:
!> - p: GridManagerMod(see UniversalKriging.f90) Defines spatial point grid/field 
!> - nlsf: Nonlinear spatial function(see UniversalKriging.f90) Defines a nonlinear spatial function 
!>
!> outputs: 
!> - f: p%num_points length vector of values of nlsf at points defined in p.
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
function NLSF_Evaluate_Fcn (p, nlsf)
type(Grid_Data_Class), intent(in):: p
type(NLSF_Class), intent(in)::nlsf
real(dp) :: NLSF_Evaluate_Fcn(1:p%num_points)
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
    stop
end select

!Bound interpolation range to observed range 
if (is_truncate_range) then
    do j = 1, num_points
        if(x(j).lt.nlsf%f_range(1)) x(j) = nlsf%f_range(1)
        if(x(j).gt.nlsf%f_range(2)) x(j) = nlsf%f_range(2)
    enddo
endif

select case (trim(nlsf%form))
case ('Logistic')
    NLSF_Evaluate_Fcn(:) = 1.D0 / ( 1.D0 + exp( - ( x(:) - nlsf%f0 ) / nlsf%lambda ) )

case ('Gaussian')
    NLSF_Evaluate_Fcn(:) = exp( - ( (x(:) - nlsf%f0) / nlsf%lambda )**2 )

case ('SinExp')
    NLSF_Evaluate_Fcn(:) = sin((x(:)-nlsf%f0)/nlsf%lambda ) * exp(-((x(:) - nlsf%f0)/nlsf%lambda )**2)

case ('CosExp')
    NLSF_Evaluate_Fcn(:) = cos((x(:)-nlsf%f0)/nlsf%lambda ) * exp(-((x(:) - nlsf%f0)/(2.*nlsf%lambda))**2)

case default ! this should not occur as the variable values have already been checked
    write(*,*) 'Error unkown spatial form in nlsf(a):', trim(nlsf%form)
    stop
end select

deallocate(x)
endfunction NLSF_Evaluate_Fcn

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
!> - obs: GridManagerMod(see UniversalKriging.f90) Defines spatial observation points
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

real(dp)  ErrMin, Err, x0hat, lambdahat, smoothness_penalty, SPF, RMS
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
! SPF: penalizes integral [d2 f /d x2 f(x)]^2
! set SPF=0 for no roughness penalty
!----------------------------------------------
RMS = sqrt( sum( y(1:num_points)**2 )/float(num_points) )
nlsf%lambda = nlsf%lambda_min
smoothness_penalty = NLSF_Smooth_Penalty(nlsf)
SPF = RMS/smoothness_penalty
x0(1:np) = nlsf%f0_min + ( nlsf%f0_max - nlsf%f0_min ) * p(1:np)
lambda(1:np) = nlsf%lambda_min + (nlsf%lambda_max - nlsf%lambda_min) * p(1:np)
!----------------------------------------------

ErrMin = Huge(1.D0)
x0hat = x0(1)
lambdahat = lambda(1)
! index x0
do j = 1, np
    ! index lambda
    do k = 1, np
        nlsf%f0 = x0(j)
        nlsf%lambda = lambda(k)
        s(:) = NLSF_Evaluate_Fcn(obs, nlsf)
        lpr = LSF_Simple_Linear_Regression(y(1:num_points), s(1:num_points) * f(1:num_points), num_points)
        RMS = sqrt( sum( (y(1:num_points) - lpr(1:num_points))**2) / float(num_points) )
        smoothness_penalty = NLSF_Smooth_Penalty(nlsf) ! penalize roughness analytic approximation
        Err = RMS + SPF * smoothness_penalty
        if (Err.lt.ErrMin)then
            ! write(*,*)nlsf%form, Err, RMS, SPF*smoothness_penalty
            ErrMin = err
            x0hat = x0(j)
            lambdahat = lambda(k)
        endif
    enddo
enddo
nlsf%f0 = x0hat
nlsf%lambda = lambdahat
s(:) = NLSF_Evaluate_Fcn(obs, nlsf)
lpr = LSF_Simple_Linear_Regression(y, s(1:num_points) * f(1:num_points), num_points)
NLSF_Fit_Function(1:num_points) = y(1:num_points) - lpr(1:num_points)

deallocate(s, p, x0, lambda, lpr)

endfunction NLSF_Fit_Function

!--------------------------------------------------------------------------------------------------
!> Purpose: Performs a brute force least squares fit of nonlinear spatial function parameters to 
!> data points in obs.  Functions are fit based on which function has the lowest rms misfit when fit.
!> Functions are (nescerailly) fit to the remaining residual after fitting the preceeding functions.
!>
!> inputs:
!> - obs: GridManagerMod(see UniversalKriging.f90) Defines observations to be fit. 
!>
!> inputs/output:
!> - nlsf: Nonlinear spatial function(see UniversalKriging.f90). Defines a vector of nonlinear 
!>         spatial functions. On return nlsf(1:nsf)%f0 and nlsf(1:nsf)%lambda are specified.
!>
!> outputs: 
!> - f: p%num_points length vector of values of nlsf at points defined in p.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine NLSF_Greedy_Least_Sq_Fit(obs, nlsf, proc_recruits)
type(Grid_Data_Class), intent(in):: obs
type(NLSF_Class), intent(inout) ::nlsf(*)
logical, intent(in) :: proc_recruits

integer j, num_obs_points, num_points, k, jBest, notyet, nfi
real(dp) rmsMin
real(dp), allocatable :: residual_vect(:), field_precond(:), residuals(:, :), rms(:), res(:)
integer, allocatable :: isfit(:), RankIndx(:)
type(NLSF_Class), allocatable :: nlsfTmp(:)

num_obs_points = obs%num_points
write(*,*)'FNLSF', num_obs_points, num_points, nsf
allocate(residual_vect(1:num_obs_points), res(1:num_obs_points), field_precond(1:num_obs_points))
allocate(residuals(1:num_obs_points, 1:nsf), rms(1:nsf), isfit(1:nsf))
allocate(nlsfTmp(1:nsf), RankIndx(1:nsf))
residual_vect(1:num_obs_points) = obs%field_psqm(1:num_obs_points)
write(*,*)'res0:', sqrt(sum(residual_vect(1:num_obs_points)**2) / float(num_obs_points))
isfit(1:nsf) = 0
nfi = 0
do while(sum(isfit(1:nsf)).lt.nsflim)
    do j = 1, nsf
        notyet = 0
        if (isfit(j).eq.0)then
            if (nlsf(j)%pre_cond_fcn_num.eq.0)then
                !no preconditioning
                field_precond(1:num_obs_points) = 1.D0
            else
                k = nlsf(j)%pre_cond_fcn_num
                if(isfit(k).eq.1)then
                    field_precond(:) = NLSF_Evaluate_Fcn (obs, nlsf(k))
                else
                ! dont fit function if preconditioning function not set
                    rms(j) = huge(1.D0)
                    notyet = 1
                endif
            endif
            if(notyet.eq.0)then
                res = NLSF_Fit_Function(obs, nlsf(j), residual_vect, field_precond)
                nlsf(j)%rms = sqrt( sum( res(1:num_obs_points)**2) / float(num_obs_points) )
                residuals(1:num_obs_points, j) = res(1:num_obs_points)
                rms(j) = sqrt( sum( res(1:num_obs_points)**2) / float(num_obs_points) )
            endif
        else
            rms(j) = huge(1.D0)
        endif
    enddo

    rmsMin = huge(1.D0)
    do j = 1, nsf
     if (rms(j).lt.rmsMin)then
      Jbest = j
      rmsMin = rms(j)
     endif
    enddo
    isfit(Jbest) = 1
    
    res = NLSF_Fit_Function(obs, nlsf(Jbest), residual_vect, field_precond)
    nfi = nfi+1
    RankIndx(nfi) = Jbest
    residual_vect(1:num_obs_points) = res(1:num_obs_points)
    write(*,*)'Fit function number', Jbest, float(sum(isfit(1:nsf))) / float(nsf), &
             'Remaining RMS=', sqrt( sum( residual_vect(1:num_obs_points)**2) / float(num_obs_points) )
enddo

if (proc_recruits) then
    call write_csv(num_obs_points, nsf, residuals, 'residuals.csv', num_obs_points)
    open(63, file = 'NLSF_Class.csv')
    do j = 1, nsf
        write(63,*)nlsf(j)%axis, ', ', trim(nlsf(j)%form), ', ', nlsf(j)%f0, ', ', nlsf(j)%lambda
    enddo
    close(63)
endif
write(*,*)'function rank:', RankIndx(1:nsf)
nlsf(1:nsf) = nlsf(RankIndx(1:nsf))
deallocate(residual_vect, field_precond, residuals, rms, res, nlsfTmp)

endsubroutine

endmodule