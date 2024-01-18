!--------------------------------------------------------------------------------------------------
!> @page page2 Non Linear Spatial Functions
!>
!>
!> @authors Keston Smith, Tom Callaghan (IBSS) 2024
!--------------------------------------------------------------------------------------------------

module NonLinearSpatialFcnMod
use globals
use GridManagerMod
use LinearSpatialFcnMod
implicit none

type NLSFPar
    real(dp) lambda, x0
    real(dp) lambdaMin,lambdaMax
    real(dp) x0Min,x0Max
    real(dp) xRange(2)
    real(dp) rms
    character(12) form
    character(3) d
    integer nsf,PreCFnum,IsTrunkateRange,UseGreedyFit,nsflim

end type NLSFPar

CONTAINS

!--------------------------------------------------------------------------------------------------
!> Define non linear spatial functions(NLSF) and paramater search range.
!>
!> The file "UK.inp" contains a set of lines of the form
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
!>
!> InitiialCallFlag= T and the number of nonlinear spatial functions is returned.
!>
!> In the second call InitiialCallFlag= F and all of the functions are defined.
!> with precon=0 the nonlinear paraters function is fit in a least
!--------------------------------------------------------------------------------------------------
subroutine DefineNLSFunctions(nlsf,p,InitialCallFlag)
type(NLSFPar)::nlsf(*)
type(Grid_Data_Class):: p
integer j,io,n,nn
logical, intent(in):: InitialCallFlag
logical UseGreedyFit
character(72) :: InputStr

open(69,file='UK.inp')
n=0
if(InitialCallFlag)then
    do
        InputStr=""
        read(69,'(a)',iostat=io) InputStr
        if (io.lt.0) exit
        if(InputStr(1:1).eq.'F') n=n+1
    end do
    nlsf(1)%nsf=n
    write(*,*)'DefineNLSFunctions initial call nsf=',nlsf(1)%nsf
    close(69)
    return
endif

do
    InputStr=""
    read(69,'(a)',iostat=io) InputStr
    if (io.lt.0) exit
    if(InputStr(1:1).eq.'F')then
        n=n+1
        j = index(InputStr,'dim=',back=.true.)
        nlsf(n)%d= trim( adjustl( Inputstr(j+4:j+4) ) )
        j = index(InputStr,'shape=',back=.true.)
        select case (InputStr(j+6:j+6))
            case('G')
                nlsf(n)%form='Gaussian'
            case('L')
                nlsf(n)%form='Logistic'
            case('S')
                nlsf(n)%form='SinExp'
            case('C')
                nlsf(n)%form='CosExp'
            case default
                write(*,*) 'Unrecognized function Form in UK.inp:',InputStr
                stop
        end select
        j = index(InputStr,"precon=",back=.true.)
        read( Inputstr(j+7:),* )nlsf(n)%PreCFnum
    endif
    if(InputStr(1:1).eq.'G')then
        j = index(InputStr,"=",back=.true.)
        read( Inputstr(j+1:),* )UseGreedyFit
    endif
 end do
close(69)

nlsf(1:n)%nsf=n

!set to IsTrunkateRange to 0 to extrapolate beyond observation range
!set to IsTrunkateRange to 1 to restrict within observation range
nlsf(1:n)%IsTrunkateRange=1
nlsf(1:n)%UseGreedyFit=0
if(UseGreedyFit)nlsf(1:n)%UseGreedyFit=1

nn=p%n
do n=1,nlsf(1)%nsf
    select case (trim(nlsf(n)%d))
        case ('x')
            nlsf(n)%x0Min=minval(p%x(1:nn))
            nlsf(n)%x0Max=maxval(p%x(1:nn))
            nlsf(n)%lambdaMin=5000.
        case ('y')
            nlsf(n)%x0Min=minval(p%y(1:nn))
            nlsf(n)%x0Max=maxval(p%y(1:nn))
            nlsf(n)%lambdaMin=5000.
        case ('z')
            nlsf(n)%x0Min=minval(p%z(1:nn))
            nlsf(n)%x0Max=maxval(p%z(1:nn))
            nlsf(n)%lambdaMin=5.
        case ('x+y')
            nlsf(n)%x0Min=minval(p%x(1:nn)+p%y(1:nn))
            nlsf(n)%x0Max=maxval(p%x(1:nn)+p%y(1:nn))
            nlsf(n)%lambdaMin=5000.
        case default
            write(*,*) 'Error unkown spatial dimension in nlsf(b):',n,trim(nlsf(n)%d)
            stop
    end select
    nlsf(n)%lambdaMax=nlsf(n)%x0Max-nlsf(n)%x0Min
    nlsf(n)%xRange(1)=nlsf(n)%x0Min
    nlsf(n)%xRange(2)=nlsf(n)%x0Max
enddo
return
endsubroutine

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
!>  - f: p%nn length vector of values of nlsf at points defined in p.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine FitNLSFunctions(obs,nlsf,IsReset)

implicit none
integer j,no,nn,nsf,k,IsReset,nsflim
real(dp) mu,rms0
type(Grid_Data_Class):: obs
type(NLSFPar)::nlsf(*)
real(dp), allocatable :: r(:),fpc(:),residuals(:,:),rms(:)
integer, allocatable:: RankIndx(:)

no=obs%n
nsf=nlsf(1)%nsf
nsflim=nlsf(1)%nsflim
write(*,*)'FNLSF',no,nn,nsf
allocate(r(1:no),fpc(1:no),residuals(1:no,1:nsf),rms(1:nsf),RankIndx(1:nsf))

r(1:no)=obs%f(1:no)
write(*,*)'res0:',sqrt(sum(r(1:no)**2)/float(no))
do j=1,nsf
    if(IsReset.eq.1)r(1:no)=obs%f(1:no)
    if (nlsf(j)%PreCFnum.eq.0)then
        !no preconditioning
        fpc(1:no)=1.D0
    else
        k=nlsf(j)%PreCFnum
        if (k.ge.j)then
            write(*,*) 'Preconditiong function must be indexed first:',k,j
            stop
        endif
        write(*,*) 'Preconditiong',k,j
        call NLSFunc (obs,nlsf(k),fpc)
    endif
    call FitNLSFunc(obs,nlsf(j),r,fpc,r)
    nlsf(j)%rms=sqrt( sum( r(1:no)**2 )/float(no) )
    rms(j)=nlsf(j)%rms
    residuals(1:no,j)=r(1:no)
enddo
call write_csv(no,nsf,residuals,'residuals.csv',no)
open(63,file='NLSFpar.csv')
do j=1,nsf
    write(63,*)nlsf(j)%d,', ',trim(nlsf(j)%form),', ',nlsf(j)%x0,', ',nlsf(j)%lambda
enddo
close(63)

if(IsReset.eq.1)then
    !sort functions into increasing rms
    write(*,*)'rms:',rms(1:nsf)
    do k=1,nsf
        j=minloc(rms(1:nsf),1)
        write(*,*)k,j,rms(j)
        RankIndx(k)=j
        rms(j)=huge(1.D0)
    enddo
    write(*,*)'function rank:',RankIndx(1:nsf)
    nlsf(1:nsf)=nlsf(RankIndx(1:nsf))
    residuals(1:no,1:nsf)=residuals(1:no,RankIndx(1:nsf))
    nlsf(1:nsf)%nsf=min(nsf,nsflim)
    write(*,*)'function rms:',nlsf(1:nsf)%rms

    mu=sum(obs%f(1:no)/float(no))
    rms0=sqrt(sum( ( obs%f(1:no)-mu  )**2)/float(no))
    j=1
    call write_csv(no,nsf,residuals,'residuals0.csv',no)
    do while( ( nlsf(j)%rms .lt. .9*rms0 +.1*nlsf(1)%rms ) .and. (j+1.lt.nsf) )
        j=j+1
        write(*,*)'delta rms:',nlsf(j)%rms, rms0, nlsf(1)%rms, .9*rms0 +.1*nlsf(1)%rms,&
        sum(  (residuals(1:no,j+1)-residuals(1:no,j))**2 )/float(no) 
    enddo
    j=j-1
    nlsf(1:nsf)%nsflim=j
    nlsf(1:nsf)%nsf=nlsf(1:nsf)%nsflim
    write(*,*) 'NLSF limit=',j
else
    nlsf(1:nsf)%nsflim=nsf
    nlsf(1:nsf)%nsf=nlsf(1:nsf)%nsflim
    do j=1,nsf
        write(*,*)'spatial function',j,'postfit rms:',rms(j)
    enddo
   
endif

deallocate(r,fpc,residuals,rms)
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
!> - f: p%nn length vector of values of nlsf at points defined in p.
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine NLSFunc (p,nlsf,f)
type(Grid_Data_Class):: p
type(NLSFPar)::nlsf
real(dp), intent(out)::  f(*)
real(dp), allocatable :: x(:)
integer nn,j
nn=p%n

allocate(x(1:nn))

select case (trim(nlsf%d))
    case ('x')
        x(1:nn)=p%x(1:nn)
    case ('y')
        x(1:nn)=p%y(1:nn)
    case ('z')
        x(1:nn)=p%z(1:nn)
    case ('x+y')
        x(1:nn)=p%x(1:nn)+p%y(1:nn)
    case default
        write(*,*) 'Error unkown spatial dimension in nlsf(a):',trim(nlsf%d)
        stop
end select
!Bound interpolation range to observed range 
if (nlsf%IsTrunkateRange.eq.1)then
    do j=1,nn
        if(x(j).lt.nlsf%xRange(1)) x(j)=nlsf%xRange(1)
        if(x(j).gt.nlsf%xRange(2)) x(j)=nlsf%xRange(2)
    enddo
endif
if(nlsf%form(1:8).eq.'Logistic')f(1:nn)=1.D0 / ( 1.D0 + exp( - ( x(1:nn) - nlsf%x0 ) / nlsf%lambda ) )
if(nlsf%form(1:8).eq.'Gaussian')f(1:nn)=exp( - ( (x(1:nn) - nlsf%x0) / nlsf%lambda )**2 )
if(nlsf%form(1:6).eq.'SinExp')f(1:nn)=sin( (x(1:nn)-nlsf%x0)/nlsf%lambda ) * exp( -( (x(1:nn) - nlsf%x0)/nlsf%lambda )**2 )
if(nlsf%form(1:6).eq.'CosExp')f(1:nn)=cos( (x(1:nn)-nlsf%x0)/nlsf%lambda )*exp( -( (x(1:nn) - nlsf%x0)/(2.*nlsf%lambda) )**2 )

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
subroutine NLSFuncPen (nlsf,p)
type(NLSFPar)::nlsf
real(dp), intent(out)::  p
if(nlsf%form(1:8).eq.'Gaussian')p = 3.D0 * sqrt(pi/2.D0) / nlsf%lambda**3
if(nlsf%form(1:8).eq.'Logistic')p = (1.D0/30.D0) / nlsf%lambda**3
if(nlsf%form(1:6).eq.'SinExp') p = 0.25D0*sqrt(2.D0*pi)*(10.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3
if(nlsf%form(1:6).eq.'CosExp') p = (1.D0/32.D0)*sqrt(2.D0*pi)*(43.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3
return
endsubroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Fit nonlinear parameters nlsf%x0 and nlsf%lambda to observations at nn points defined in 
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
subroutine FitNLSFunc(obs,nlsf,y,f,r)

type(Grid_Data_Class):: obs
type(NLSFPar)::nlsf
real(dp), intent(in):: y(*),f(*)
real(dp), intent(inout):: r(*)

real(dp)  alpha,beta,ErrMin,Err,x0hat,lambdahat,smoothness_penalty,SPF,RMS
integer j,k,np,nn,nx0,nlambda
real(dp), allocatable :: s(:),p(:),x0(:),lambda(:),lpr(:),pg(:)

nn=obs%n
np=500
allocate(s(1:nn),lpr(1:nn),p(1:np),x0(1:np),lambda(1:np),pg(1:nn))

do j=1,np
    p(j)=float(j-1)/float(np-1)
enddo

!----------------------------------------------
! set smoothness to dominate below lambdaMin
! SPF: penalizes integral [d2 f /d x2 f(x)]^2
! set SPF=0 for no roughness penalty
!----------------------------------------------
RMS=sqrt( sum( r(1:nn)**2 )/float(nn) )
nlsf%lambda=nlsf%lambdaMin
call NLSFuncPen(nlsf,smoothness_penalty)
SPF=RMS/smoothness_penalty
x0(1:np)=nlsf%x0Min + ( nlsf%x0Max - nlsf%x0Min )*p(1:np)
lambda(1:np)=nlsf%lambdaMin + (nlsf%lambdaMax-nlsf%lambdaMin)*p(1:np)
!----------------------------------------------

nx0=np
nlambda=np
ErrMin=Huge(1.D0)
x0hat=x0(1)
lambdahat=lambda(1)
do j=1,nx0
    do k=1,nlambda
        nlsf%x0=x0(j)
        nlsf%lambda=lambda(k)
        call NLSFunc(obs,nlsf,s)
        call SLR(y,s(1:nn)*f(1:nn),nn,alpha,beta,lpr)
        RMS=sqrt( sum( (y(1:nn)-lpr(1:nn))**2 )/float(nn) )
        call NLSFuncPen(nlsf,smoothness_penalty) ! penalize roguhness analytic approximation
        Err=RMS+SPF*smoothness_penalty
        if (Err.lt.ErrMin)then
            ! write(*,*)nlsf%form,Err,RMS,SPF*smoothness_penalty
            ErrMin=err
            x0hat=x0(j)
            lambdahat=lambda(k)
        endif
    enddo
enddo
nlsf%x0=x0hat
nlsf%lambda=lambdahat
call NLSFunc(obs,nlsf,s)
call SLR(y,s(1:nn)*f(1:nn),nn,alpha,beta,lpr)
r(1:nn)=y(1:nn)-lpr(1:nn)

deallocate(s,p,x0,lambda,lpr)

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
!> - f: p%nn length vector of values of nlsf at points defined in p.
!>
!> @author keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
subroutine FitNLSFunctionsGreedy(obs,nlsf)
integer j,no,nn,nsf,k,jBest,notyet,nfi,nsflim
real(dp) rmsMin
type(Grid_Data_Class):: obs
type(NLSFPar)::nlsf(*)
real(dp), allocatable :: r(:),fpc(:),residuals(:,:),rms(:),res(:)
integer, allocatable :: isfit(:),RankIndx(:)
type(NLSFPar), allocatable ::nlsfTmp(:)

no=obs%n
nsf=nlsf(1)%nsf
nsflim=nlsf(1)%nsflim
write(*,*)'FNLSF',no,nn,nsf
allocate(r(1:no),res(1:no),fpc(1:no),residuals(1:no,1:nsf),rms(1:nsf),isfit(1:nsf))
allocate(nlsfTmp(1:nsf),RankIndx(1:nsf))
r(1:no)=obs%f(1:no)
write(*,*)'res0:',sqrt(sum(r(1:no)**2)/float(no))
isfit(1:nsf)=0
nfi=0
do while(sum(isfit(1:nsf)).lt.nsflim)
    do j=1,nsf
        notyet=0
        if (isfit(j).eq.0)then
            if (nlsf(j)%PreCFnum.eq.0)then
                !no preconditioning
                fpc(1:no)=1.D0
            else
                k=nlsf(j)%PreCFnum
                if(isfit(k).eq.1)then
                    call NLSFunc (obs,nlsf(k),fpc)
                else
                ! dont fit function if preconditioning function not set
                    rms(j)=huge(1.D0)
                    notyet=1
                endif
            endif
            if(notyet.eq.0)then
                call FitNLSFunc(obs,nlsf(j),r,fpc,res)
                nlsf(j)%rms=sqrt( sum( res(1:no)**2 )/float(no) )
                residuals(1:no,j)=res(1:no)
                rms(j)=sqrt( sum( res(1:no)**2  )/float(no) )
            endif
        else
            rms(j)=huge(1.D0)
        endif
    enddo

    rmsMin=huge(1.D0)
    do j=1,nsf
     if (rms(j).lt.rmsMin)then
      Jbest=j
      rmsMin=rms(j)
     endif
    enddo
    isfit(Jbest)=1
    
    call FitNLSFunc(obs,nlsf(Jbest),r,fpc,res)
    nfi=nfi+1
    RankIndx(nfi)=Jbest
    r(1:no)=res(1:no)
    write(*,*)'Fit function number',Jbest,float(sum(isfit(1:nsf)))/float(nsf),&
             'Remaining RMS=',sqrt( sum( r(1:no)**2  )/float(no) )
enddo

call write_csv(no,nsf,residuals,'residuals.csv',no)
open(63,file='NLSFpar.csv')
do j=1,nsf
    write(63,*)nlsf(j)%d,', ',trim(nlsf(j)%form),', ',nlsf(j)%x0,', ',nlsf(j)%lambda
enddo
close(63)
write(*,*)'function rank:',RankIndx(1:nsf)
nlsf(1:nsf)=nlsf(RankIndx(1:nsf))
deallocate(r,fpc,residuals,rms,res,nlsfTmp)
return
endsubroutine

endmodule