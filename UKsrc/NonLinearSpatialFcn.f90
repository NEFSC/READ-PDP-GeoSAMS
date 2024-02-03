!--------------------------------------------------------------------------------------------------
!> @page page2 Non Linear Spatial Functions
!>
!>
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS) 2024
!--------------------------------------------------------------------------------------------------

module NonLinearSpatialFcnMod
use globals
use GridManagerMod
use LinearSpatialFcnMod
implicit none

type NLSFPar
    real(dp) lambda, x0
    real(dp) lambdaMin, lambdaMax
    real(dp) x0Min, x0Max
    real(dp) xRange(2)
    real(dp) rms
    character(12) form
    character(3) d
    integer nsf, PreCFnum, IsTruncateRange, UseGreedyFit, nsflim

end type NLSFPar

CONTAINS

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
integer function DefineNLSFunctions(nlsf, p, InitialCallFlag)
type(NLSFPar) :: nlsf(*)
type(Grid_Data_Class):: p
integer j, io, n, num_points
logical, intent(in):: InitialCallFlag
integer greedy_fit
character(72) :: input_string
integer is_truncate_range
logical exists

is_truncate_range = 1 !default
greedy_fit = 0

! Check if configuration file exists
input_string = 'Configuration\SpatialFcns.cfg'
inquire(file=input_string, exist=exists)

if (.NOT. exists) then
    PRINT *, term_red, input_string, ' NOT FOUND', term_blk
    stop
endif
open(69,file=input_string)
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
    DefineNLSFunctions = n
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
        nlsf(n)%d= trim( adjustl( input_string(j+4:j+4) ) )
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
        read( input_string(j+7:),* )nlsf(n)%PreCFnum

    case ('G')
        j = index(input_string,"=",back=.true.)
        read( input_string(j+1:),* ) greedy_fit

    case ('I')
        j = index(input_string,"=",back=.true.)
        read( input_string(j+1:),* ) is_truncate_range
    endselect
 end do
close(69)

nlsf(1:n)%nsf = n
DefineNLSFunctions = n

nlsf(1:n)%IsTruncateRange = is_truncate_range
nlsf(1:n)%UseGreedyFit = greedy_fit

num_points = p%num_points
do n = 1, nlsf(1)%nsf
    select case (trim(nlsf(n)%d))
        case ('x')
            nlsf(n)%x0Min = minval(p%x(1:num_points))
            nlsf(n)%x0Max = maxval(p%x(1:num_points))
            nlsf(n)%lambdaMin = 5000.
        case ('y')
            nlsf(n)%x0Min = minval(p%y(1:num_points))
            nlsf(n)%x0Max = maxval(p%y(1:num_points))
            nlsf(n)%lambdaMin = 5000.
        case ('z')
            nlsf(n)%x0Min = minval(p%z(1:num_points))
            nlsf(n)%x0Max = maxval(p%z(1:num_points))
            nlsf(n)%lambdaMin = 5.
        case ('x+y')
            nlsf(n)%x0Min = minval(p%x(1:num_points)+p%y(1:num_points))
            nlsf(n)%x0Max = maxval(p%x(1:num_points)+p%y(1:num_points))
            nlsf(n)%lambdaMin = 5000.
        case default
            write(*,*) 'Error unkown spatial dimension in nlsf(b):', n, trim(nlsf(n)%d)
            stop
    end select
    nlsf(n)%lambdaMax = nlsf(n)%x0Max - nlsf(n)%x0Min
    nlsf(n)%xRange(1) = nlsf(n)%x0Min
    nlsf(n)%xRange(2) = nlsf(n)%x0Max
enddo
return
endfunction

!--------------------------------------------------------------------------------------------------
!> Purpose: Performs a brute force least squares fit of nonlinear spatial function parameters to data points in 
!> obs.
!>
!> inputs:
!> - obs: GridManagerMod(see UniversalKriging.f90) Defines observations to be fit. 
!> - IsReset: (integer) if IsReset=0 the residual from the preceeding function is fit. 
!>   if IsReset=1 each function is fit to observations.
!>
!> inputs/output:
!>  - nlsf: Nonlinear spatial function(see UniversalKriging.f90). Defines a vector of nonlinear 
!>          spatial functions. On return nlsf(1:nsf)%x0 and nlsf(1:nsf)%lambda are specified.
!>
!> outputs: 
!>  - f: p%num_points length vector of values of nlsf at points defined in p.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine FitNLSFunctions(obs, nlsf, IsReset)

implicit none
integer j, num_obs_points, num_points, nsf, k, IsReset, nsflim
real(dp) mu, rms0
type(Grid_Data_Class):: obs
type(NLSFPar)::nlsf(*)
real(dp), allocatable :: r(:), fpc(:), residuals(:, :), rms(:)
integer, allocatable:: RankIndx(:)

num_obs_points = obs%num_points
nsf = nlsf(1)%nsf
nsflim = nlsf(1)%nsflim
write(*,*)'FNLSF', num_obs_points, num_points, nsf
allocate(r(1:num_obs_points), fpc(1:num_obs_points), residuals(1:num_obs_points, 1:nsf), rms(1:nsf), RankIndx(1:nsf))

r(1:num_obs_points) = obs%f_psqm(1:num_obs_points)
write(*,*)'res0:', sqrt(sum(r(1:num_obs_points)**2)/float(num_obs_points))
do j = 1, nsf
    if(IsReset.eq.1)r(1:num_obs_points) = obs%f_psqm(1:num_obs_points)
    if (nlsf(j)%PreCFnum.eq.0) then
        !no preconditioning
        fpc(1:num_obs_points) = 1.D0
    else
        k = nlsf(j)%PreCFnum
        if (k.ge.j)then
            write(*,*) 'Preconditiong function must be indexed first:', k, j
            stop
        endif
        write(*,*) 'Preconditiong', k, j
        call NLSFunc (obs, nlsf(k), fpc)
    endif
    call FitNLSFunc(obs, nlsf(j), r, fpc, r)
    nlsf(j)%rms = sqrt( sum( r(1:num_obs_points)**2 )/float(num_obs_points) )
    rms(j) = nlsf(j)%rms
    residuals(1:num_obs_points, j) = r(1:num_obs_points)
enddo
call write_csv(num_obs_points, nsf, residuals, 'residuals.csv', num_obs_points)
open(63, file = 'NLSFpar.csv')
do j = 1, nsf
    write(63,*)nlsf(j)%d, ', ', trim(nlsf(j)%form), ', ', nlsf(j)%x0, ', ', nlsf(j)%lambda
enddo
close(63)

if(IsReset.eq.1)then
    !sort functions into increasing rms
    write(*,*)'rms:', rms(1:nsf)
    do k = 1, nsf
        j = minloc(rms(1:nsf), 1)
        write(*,*)k, j, rms(j)
        RankIndx(k) = j
        rms(j) = huge(1.D0)
    enddo
    write(*,*)'function rank:', RankIndx(1:nsf)
    nlsf(1:nsf) = nlsf(RankIndx(1:nsf))
    residuals(1:num_obs_points, 1:nsf) = residuals(1:num_obs_points, RankIndx(1:nsf))
    nlsf(1:nsf)%nsf = min(nsf, nsflim)
    write(*,*)'function rms:', nlsf(1:nsf)%rms

    mu = sum(obs%f_psqm(1:num_obs_points)/float(num_obs_points))
    rms0 = sqrt(sum( ( obs%f_psqm(1:num_obs_points) - mu)**2) / float(num_obs_points))
    j = 1
    call write_csv(num_obs_points, nsf, residuals, 'residuals0.csv', num_obs_points)
    do while( ( nlsf(j)%rms .lt. .9*rms0 + .1*nlsf(1)%rms) .and. (j+1.lt.nsf) )
        j = j+1
        write(*,*)'delta rms:', nlsf(j)%rms, rms0, nlsf(1)%rms, .9*rms0 +.1*nlsf(1)%rms, &
        sum(  (residuals(1:num_obs_points, j+1) - residuals(1:num_obs_points, j))**2 )/float(num_obs_points) 
    enddo
    j = j-1
    nlsf(1:nsf)%nsflim = j
    nlsf(1:nsf)%nsf = nlsf(1:nsf)%nsflim
    write(*,*) 'NLSF limit = ', j
else
    nlsf(1:nsf)%nsflim = nsf
    nlsf(1:nsf)%nsf = nlsf(1:nsf)%nsflim
    do j = 1, nsf
        write(*,*)'spatial function', j, 'postfit rms:', rms(j)
    enddo
   
endif

deallocate(r, fpc, residuals, rms)
return
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
subroutine NLSFunc (p, nlsf, f)
type(Grid_Data_Class), intent(in):: p
type(NLSFPar), intent(in)::nlsf
real(dp), intent(out)::  f(*)
real(dp), allocatable :: x(:)
integer num_points, j
num_points = p%num_points

allocate(x(1:num_points))

select case (trim(nlsf%d))
    case ('x')
        x(1:num_points) = p%x(1:num_points)
    case ('y')
        x(1:num_points) = p%y(1:num_points)
    case ('z')
        x(1:num_points) = p%z(1:num_points)
    case ('x+y')
        x(1:num_points) = p%x(1:num_points)+p%y(1:num_points)
    case default
        write(*,*) 'Error unkown spatial dimension in nlsf(a):', trim(nlsf%d)
        stop
end select
!Bound interpolation range to observed range 
if (nlsf%IsTruncateRange.eq.1)then
    do j = 1, num_points
        if(x(j).lt.nlsf%xRange(1)) x(j) = nlsf%xRange(1)
        if(x(j).gt.nlsf%xRange(2)) x(j) = nlsf%xRange(2)
    enddo
endif
if(nlsf%form(1:8).eq.'Logistic')f(1:num_points) = 1.D0 / ( 1.D0 + exp( - ( x(1:num_points) - nlsf%x0 ) / nlsf%lambda ) )
if(nlsf%form(1:8).eq.'Gaussian')f(1:num_points) = exp( - ( (x(1:num_points) - nlsf%x0) / nlsf%lambda )**2 )
if(nlsf%form(1:6).eq.'SinExp')&
&         f(1:num_points) = sin((x(1:num_points)-nlsf%x0)/nlsf%lambda ) * exp(-((x(1:num_points) - nlsf%x0)/nlsf%lambda )**2)
if(nlsf%form(1:6).eq.'CosExp')&
&         f(1:num_points) = cos((x(1:num_points)-nlsf%x0)/nlsf%lambda ) * exp(-((x(1:num_points) - nlsf%x0)/(2.*nlsf%lambda))**2)

deallocate(x)

return
endsubroutine

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
subroutine NLSFuncPen (nlsf, p)
type(NLSFPar)::nlsf
real(dp), intent(out)::  p
if(nlsf%form(1:8).eq.'Gaussian')p = 3.D0 * sqrt(pi/2.D0) / nlsf%lambda**3
if(nlsf%form(1:8).eq.'Logistic')p = (1.D0/30.D0) / nlsf%lambda**3
if(nlsf%form(1:6).eq.'SinExp') p = 0.25D0*sqrt(2.D0*pi)*(10.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3
if(nlsf%form(1:6).eq.'CosExp') p = (1.D0/32.D0)*sqrt(2.D0*pi)*(43.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3
return
endsubroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Fit nonlinear parameters nlsf%x0 and nlsf%lambda to observations at num_points points defined in 
!> obs with values y.  f is a function defined at the observation points. The penalty function is \n
!>           sum_i (a+b*nlsf%g_i(x0,lambda)*f_i - y_i)**2 \n
!> where a and b are estimated using simple linear regresion for each nonlinear parameter pair x_0, 
!> lambda.  The minimization is done with a brute force search over np=500 equally spaced values 
!> between (nlsf%x0Min and nlsf%x0Max) and (nlsf%x0Min and  nlsf%x0Max) respectivly.
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
!> - r: residual, r_i=y_i - nlsf(x_i |x0,lambda) where i=1..# of observations.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine FitNLSFunc(obs, nlsf, y, f, r)

type(Grid_Data_Class):: obs
type(NLSFPar)::nlsf
real(dp), intent(in):: y(*), f(*)
real(dp), intent(inout):: r(*)

real(dp)  alpha, beta, ErrMin, Err, x0hat, lambdahat, smoothness_penalty, SPF, RMS
integer j, k, np, num_points, nx0, nlambda
real(dp), allocatable :: s(:), p(:), x0(:), lambda(:), lpr(:), pg(:)

num_points = obs%num_points
np = 500
allocate(s(1:num_points), lpr(1:num_points), p(1:np), x0(1:np), lambda(1:np), pg(1:num_points))

do j = 1, np
    p(j) = float(j-1) / float(np-1)
enddo

!----------------------------------------------
! set smoothness to dominate below lambdaMin
! SPF: penalizes integral [d2 f /d x2 f(x)]^2
! set SPF=0 for no roughness penalty
!----------------------------------------------
RMS = sqrt( sum( r(1:num_points)**2 )/float(num_points) )
nlsf%lambda = nlsf%lambdaMin
call NLSFuncPen(nlsf, smoothness_penalty)
SPF = RMS/smoothness_penalty
x0(1:np) = nlsf%x0Min + ( nlsf%x0Max - nlsf%x0Min ) * p(1:np)
lambda(1:np) = nlsf%lambdaMin + (nlsf%lambdaMax - nlsf%lambdaMin) * p(1:np)
!----------------------------------------------

nx0 = np
nlambda = np
ErrMin = Huge(1.D0)
x0hat = x0(1)
lambdahat = lambda(1)
do j = 1, nx0
    do k = 1, nlambda
        nlsf%x0 = x0(j)
        nlsf%lambda = lambda(k)
        call NLSFunc(obs, nlsf, s)
        call SLR(y, s(1:num_points)*f(1:num_points), num_points, alpha, beta, lpr)
        RMS = sqrt( sum( (y(1:num_points) - lpr(1:num_points))**2) / float(num_points) )
        call NLSFuncPen(nlsf, smoothness_penalty) ! penalize roguhness analytic approximation
        Err = RMS+SPF*smoothness_penalty
        if (Err.lt.ErrMin)then
            ! write(*,*)nlsf%form, Err, RMS, SPF*smoothness_penalty
            ErrMin = err
            x0hat = x0(j)
            lambdahat = lambda(k)
        endif
    enddo
enddo
nlsf%x0 = x0hat
nlsf%lambda = lambdahat
call NLSFunc(obs, nlsf, s)
call SLR(y, s(1:num_points)*f(1:num_points), num_points, alpha, beta, lpr)
r(1:num_points) = y(1:num_points) - lpr(1:num_points)

deallocate(s, p, x0, lambda, lpr)

return
endsubroutine

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
!>         spatial functions. On return nlsf(1:nsf)%x0 and nlsf(1:nsf)%lambda are specified.
!>
!> outputs: 
!> - f: p%num_points length vector of values of nlsf at points defined in p.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine FitNLSFunctionsGreedy(obs, nlsf)
integer j, num_obs_points, num_points, nsf, k, jBest, notyet, nfi, nsflim
real(dp) rmsMin
type(Grid_Data_Class), intent(in):: obs
type(NLSFPar), intent(inout) ::nlsf(*)
real(dp), allocatable :: r(:), fpc(:), residuals(:, :), rms(:), res(:)
integer, allocatable :: isfit(:), RankIndx(:)
type(NLSFPar), allocatable ::nlsfTmp(:)

num_obs_points = obs%num_points
nsf = nlsf(1)%nsf
nsflim = nlsf(1)%nsflim
write(*,*)'FNLSF', num_obs_points, num_points, nsf
allocate(r(1:num_obs_points), res(1:num_obs_points), fpc(1:num_obs_points))
allocate(residuals(1:num_obs_points, 1:nsf), rms(1:nsf), isfit(1:nsf))
allocate(nlsfTmp(1:nsf), RankIndx(1:nsf))
r(1:num_obs_points) = obs%f_psqm(1:num_obs_points)
write(*,*)'res0:', sqrt(sum(r(1:num_obs_points)**2) / float(num_obs_points))
isfit(1:nsf) = 0
nfi = 0
do while(sum(isfit(1:nsf)).lt.nsflim)
    do j = 1, nsf
        notyet = 0
        if (isfit(j).eq.0)then
            if (nlsf(j)%PreCFnum.eq.0)then
                !no preconditioning
                fpc(1:num_obs_points) = 1.D0
            else
                k = nlsf(j)%PreCFnum
                if(isfit(k).eq.1)then
                    call NLSFunc (obs, nlsf(k), fpc)
                else
                ! dont fit function if preconditioning function not set
                    rms(j) = huge(1.D0)
                    notyet = 1
                endif
            endif
            if(notyet.eq.0)then
                call FitNLSFunc(obs, nlsf(j), r, fpc, res)
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
    
    call FitNLSFunc(obs, nlsf(Jbest), r, fpc, res)
    nfi = nfi+1
    RankIndx(nfi) = Jbest
    r(1:num_obs_points) = res(1:num_obs_points)
    write(*,*)'Fit function number', Jbest, float(sum(isfit(1:nsf))) / float(nsf), &
             'Remaining RMS=', sqrt( sum( r(1:num_obs_points)**2) / float(num_obs_points) )
enddo

call write_csv(num_obs_points, nsf, residuals, 'residuals.csv', num_obs_points)
open(63, file = 'NLSFpar.csv')
do j = 1, nsf
    write(63,*)nlsf(j)%d, ', ', trim(nlsf(j)%form), ', ', nlsf(j)%x0, ', ', nlsf(j)%lambda
enddo
close(63)
write(*,*)'function rank:', RankIndx(1:nsf)
nlsf(1:nsf) = nlsf(RankIndx(1:nsf))
deallocate(r, fpc, residuals, rms, res, nlsfTmp)
return
endsubroutine

endmodule