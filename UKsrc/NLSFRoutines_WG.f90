
subroutine DefineNLSFunctions(nlsf,p,InitialCallFlag)
!subroutine DefineNLSFunctions(nlsf,p,InitialCallFlag)
! Define non linear spatial functions(NLSF) and paramater search range.
! The file "UK.inp" contains a set of lines of the form
! "Function 1, dim=z, shape=Logistic, precon=0 "
! "Function 2, dim=z, shape=Gaussian, precon=0 "
! "Function 7, dim=x, shape=Logistic, precon=1 "
! these define spatial functions for setting the spatial trend in the universal kriging 
! algorithm. The precon=0 term 
! The function is called twice from the main program.  In the first call 
! InitiialCallFlag= T and the number of nonlinear spatial functions is returned.
! In the second call InitiialCallFlag= F and all of the functions are defined.
! with precon=0 the nonlinear paraters function is fit in a least
use NLSFMod
use DpointMod
implicit none
type(NLSFPar)::nlsf(*)
type(Dpoint):: p
integer j,io,k,n,nn
logical, intent(in):: InitialCallFlag
logical UseGreedyFit
character*72 :: InputStr


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
end

subroutine FitNLSFunctions(obs,grid,nlsf,IsReset)
! subroutine FitNLSFunctions(obs,grid,nlsf,IsReset)
! Purpose: Performs a brute force least squares fit of nonlinear spatial function parameters to data points in 
! obs.
!--------------------------------------------------------------------------------------------------
! Purpose: Evaluates nonlinear spatial function with parameters x0 ad lambda at points in p. Values 
! returned in vector f.
!
! inputs:
!       obs: DpointMod(see UniversalKriging.f90) Defines observations to be fit. 
! inputs/output:
!       nlsf: Nonlinear spatial function(see UniversalKriging.f90). Defines a vector of nonlinear 
!             spatial functions. On return nlsf(1:nsf)%x0 and nlsf(1:nsf)%lambda are specified.
!!
! outputs: 
!       f: p%nn length vector of values of nlsf at points defined in p.

! history:  Written by keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
use DpointMod
use NLSFMod

implicit none
integer j,n,no,nn,nsf,k,IsReset,nsflim
real*8 mu,rms0
type(Dpoint):: obs
type(Dpoint):: grid
type(NLSFPar)::nlsf(*)
real*8, allocatable :: r(:),fpc(:),residuals(:,:),rms(:)
integer, allocatable:: RankIndx(:)

no=obs%n
nn=grid%n
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
    call FitNLSFunc(obs,grid,nlsf(j),r,fpc,r)
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
    !do while(  ( nlsf(j+1)%rms-nlsf(j)%rms .gt. nlsf(1)%rms/(10.**4) ).and.(j.lt.nsf-1)  )
    !do while( ( nlsf(j)%rms .lt. .75*rms0 +.25*nlsf(1)%rms ).and.(j.lt.nsf)  )
    do while( ( nlsf(j)%rms .lt. .9*rms0 +.1*nlsf(1)%rms ) .and. (j+1.lt.nsf) )! & .and.     ( sum(  (residuals(1:no,j+1)-residuals(1:no,j))**2 )/float(no) .gt. 0.001     )   )
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
end
!----------------------------------------------------------------------------------------

subroutine NLSFunc (p,nlsf,f)
!subroutine NLSFunc (p,nlsf,f)
!Evaluates nonlinear spatial function at points in p. Values returned in f
!--------------------------------------------------------------------------------------------------
! Purpose: Evaluates nonlinear spatial function with parameters x0 ad lambda at points in p. Values 
! returned in vector f.
!
! inputs:
!       p: DpointMod(see UniversalKriging.f90) Defines spatial point grid/field 
!       nlsf: Nonlinear spatial function(see UniversalKriging.f90) Defines a nonlinear spatial 
!             function 
!!
! outputs: 
!       f: p%nn length vector of values of nlsf at points defined in p.

! history:  Written by keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
implicit none
!
use DpointMod
use NLSFMod
implicit none
type(Dpoint):: p
type(NLSFPar)::nlsf
real*8, intent(out)::  f(*)
real*8, allocatable :: x(:)
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
end
!----------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------

subroutine NLSFuncPen (nlsf,p)
!subroutine NLSFuncPen (nlsf,p)
! Purpose: Calculate smoothing penalty for nonlinear spatial function fit.  Penalty is 
! based on symbolic integral from -inf to inf of the functions second derivative squared.
! i.e.: integral (d2 f /d x2)^2 from -inf to inf = smoothness penalty
! see: SageScriptSmoothing.s for derivation
use DpointMod
use NLSFMod
implicit none
type(NLSFPar)::nlsf
real*8, intent(out)::  p
real pi
pi=4.*ATAN(1.0D0)
!if(nlsf%form(1:8).eq.'Gaussian')p = 3.D0 * sqrt(pi/2.D0) / nlsf%lambda**3
!if(nlsf%form(1:8).eq.'Logistic')p = (1.D0/30.D0) / nlsf%lambda**3
!if(nlsf%form(1:6).eq.'SinExp') p = 0.25D0*sqrt(2.D0*pi)*(10.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3
!if(nlsf%form(1:6).eq.'CosExp') p = (1.D0/32.D0)*sqrt(2.D0*pi)*(43.D0-3.D0/exp(-.5D0)) / nlsf%lambda**3
if(nlsf%form(1:8).eq.'Gaussian')p = 3.759942411946501 / nlsf%lambda**3
if(nlsf%form(1:8).eq.'Logistic')p = 0.033333333333333 / nlsf%lambda**3
if(nlsf%form(1:6).eq.'SinExp')  p = 3.167022170985631 / nlsf%lambda**3
if(nlsf%form(1:6).eq.'CosExp')  p = 2.980838179586422 / nlsf%lambda**3
return
end
!----------------------------------------------------------------------------------------
subroutine FitNLSFunc(obs,g,nlsf,y,f,r)
   ! call FitNLSFunc(obs,grid,nlsf(j),r,fpc,r)

use DpointMod
use NLSFMod
implicit none
type(Dpoint):: g
type(Dpoint):: obs
type(NLSFPar)::nlsf
real*8, intent(in):: y(*),f(*)
real*8, intent(inout):: r(*)

real*8  alpha,beta,ErrMin,Err,x0hat,lambdahat,SP,SPF,RMS
integer j,k,np,nn,ng,nx0,nlambda
real*8, allocatable :: s(:),p(:),x0(:),lambda(:),lpr(:),pg(:)

nn=obs%n
ng=g%n
np=500
allocate(s(1:nn),lpr(1:nn),p(1:np),x0(1:np),lambda(1:np),pg(1:nn))

do j=1,np
 p(j)=float(j-1)/float(np-1)
enddo

!----------------------------------------------
! set smoothness to dominate below lambdaMin
! SPF: penalizes integral [d2 f /d x2 f(x)]^2
! set SPF=0 for no roughness penalty
RMS=sqrt( sum( r(1:nn)**2 )/float(nn) )
nlsf%lambda=nlsf%lambdaMin
call NLSFuncPen(nlsf,SP)
SPF=RMS/SP
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
        call NLSFuncPen(nlsf,SP) ! penalize roguhness analytic approximation
        Err=RMS+SPF*SP
        if (Err.lt.ErrMin)then
!         write(*,*)nlsf%form,Err,RMS,SPF*SP
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
end


subroutine FitNLSFunctionsGreedy(obs,grid,nlsf)
use DpointMod
use NLSFMod

implicit none
integer j,n,no,nn,nsf,k,jBest,notyet,nfi,nsflim
real*8 rmsMin
type(Dpoint):: obs
type(Dpoint):: grid
type(NLSFPar)::nlsf(*)
real*8, allocatable :: r(:),fpc(:),residuals(:,:),rms(:),res(:)
integer, allocatable :: isfit(:),RankIndx(:)
type(NLSFPar), allocatable ::nlsfTmp(:)

no=obs%n
nn=grid%n
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
                call FitNLSFunc(obs,grid,nlsf(j),r,fpc,res)
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
    
    call FitNLSFunc(obs,grid,nlsf(Jbest),r,fpc,res)
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
end
!----------------------------------------------------------------------------------------
