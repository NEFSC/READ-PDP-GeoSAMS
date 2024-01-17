!--------------------------------------------------------------------------------------------------
!> @page page3 Linear Spatial Functions
!>
!> @authors Keston Smith, Tom Callaghan IBSS (2024)
!--------------------------------------------------------------------------------------------------
module LinearSpatialFcnMod
use globals
implicit none

CONTAINS

!--------------------------------------------------------------------------------------------------
!> Purpose:Generalized Least Squares solver. Solve for beta to fit y = F*beta +epsilon, for 
!> epsilon~N(0, C). Return optimal beta, covariance of Beta and residual
!> 
!>  Inputs:
!>  - y   (real(dp)) length n vector to fit
!>  - F    (real(dp)) size n x m matrix of j=1:m functions evaluated at points k=1:n    
!>  - C    (real(dp)) size n x n covariance matrix for epsilon
!>  - n   (integer) number of points to fit
!>  - m   (integer) number of functions in fit
!>
!>  Outputs:
!>  -   beta  (real(dp)) length m vector of optimal coefficients
!>  -   Cbeta (real(dp)) size m x m covariance matrix for beta
!>  -   r     (real(dp)) length nn vector of residuals r = y - F * beta
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine GLS(y, F, C, n, m, beta, Cbeta, r)
use globals
implicit none
integer,    intent(in):: n, m
real(dp),    intent(in):: y(*), F(n,*), C(n,*)
real(dp),    intent(out):: beta(*), Cbeta(m, m), r(*)

real(dp),    allocatable:: Cinv(:,:), CbetaInv(:,:), Vtmp(:), Vtmp2(:), Mtmp(:,:), ytr(:)
integer,    allocatable:: ipiv(:)

real(dp)     atmp, btmp
integer     j, info, error

allocate( Cinv(1:n, 1:n), CbetaInv(1:m, 1:m), ytr(1:n), Mtmp(1:n, 1:m), Vtmp(1:n), Vtmp2(1:m) ) 
allocate( ipiv(1:n), stat=error)

atmp=1.
btmp=0.

Cinv(1:n, 1:n)=0.
do j=1, n
    Cinv(j, j)=1.
enddo
call dgesv(n, n, C, n, IPIV, Cinv, n, info)
write(*,*)'GLS', info
!
! cov(beta)) =  inv (F' Cinv F )
!
call dgemm('N', 'N', n, m, n, atmp, Cinv, n, F,   n, btmp, Mtmp,    n )
call dgemm('T', 'N', m, m, n, atmp,  F,   n , Mtmp , n, btmp, CbetaInv, m )

CBeta(1:m, 1:m)=0.
do j=1, m
    CBeta(j, j)=1.
enddo
call dgesv(m, m, CBetaInv, m, IPIV, CBeta, m, info)
write(*,*)'GLS  dgesv info=', info
call write_csv(m, m, CbetaInv, 'CBeta0.csv', m)
call write_csv(n, m, F, 'Fglsa0.csv', n)
!
! beta = inv( F' * Cinv * F ) * F * Cinv * fo
!
call dgemv('N', n, n, atmp, Cinv, n, y,  1, btmp, Vtmp, 1)
call dgemv('T', n, m, atmp, F,  n, Vtmp,  1, btmp, Vtmp2, 1)
call dgemv('N', m, m, atmp, Cbeta, m, Vtmp2, 1, btmp, Beta, 1)
!
! compute residual
!
call dgemv('N', n, m, atmp, F, n, beta,  1, btmp, ytr, 1)
r(1:n)=y(1:n)-ytr(1:n)

call write_scalar_fields(n, 1, y, 'obs.txt', n)

deallocate( ipiv, stat=error)
deallocate(  Cinv, CbetaInv, ytr, Mtmp, Vtmp, Vtmp2, stat=error )

return
end subroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Simple linear regresion 
!> minimize sum_{j=1:n}  ( y(j) - alpha + beta * x(j) )**2 over alpha and beta
!>
!> Inputs:
!>  - y (real) [n]
!>  - x (real) [n]
!>  - n (integer)
!>
!> Outputs:
!>  - alpha (real)
!>  - beta (real)
!>  - p (real) [n] p(1:n) = alpha + beta * x(1:n) 
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
!Simple Least Squares
subroutine SLR(y, x, n, alpha, beta, p)
integer,    intent(in):: n
real(dp),    intent(in):: y(*), x(*)
real(dp),    intent(out):: p(*), alpha, beta
real(dp) Sxy, Sx, Sy, Sx2
Sxy=sum(x(1:n)*y(1:n))
Sx=sum(x(1:n))
Sy=sum(y(1:n))
Sx2=sum(x(1:n)*x(1:n))
beta=(n * Sxy - Sx * Sy) / ( n * Sx2 -Sx * Sx )
alpha=(Sy / n) - (beta*Sx / n)
p(1:n)=alpha + beta * x(1:n)
return
end subroutine

!----------------------------------------------------------------------------------------
!> Purpose: Restrict maximum recruit density based on historical relation ship between 
!> peak recruitment and bathymetry within a year.  The curves here are derived from scallop 
!> dredge survey data from 1979 to 2018.
!>
!> inputs: 
!>  - n      length of vectors f and z
!>  - z      [nn] bathymetric depth
!>  - fpeak  highest observed value of recruitment
!>  - Domain 'MA', Mid Atlantic or 'GB' Georges Bank
!>
!> input/output
!>  - f [n] recruitment observations from the same year.
!> @author Keston Smith 2022
!----------------------------------------------------------------------------------------
subroutine limitz(n, f, z, fpeak, Domain)

integer, intent(in) :: n
real(dp), intent(in) :: z(*), fpeak
real(dp), intent(inout) :: f(*)
character(2), intent(in):: Domain
real(dp) a, w, zc, fmax 
integer j, kappa

if(Domain(1:2).eq.'MA')then
    zc=60
    w=33
    a=.01
    kappa=10
endif

if(Domain(1:2).eq.'GB')then
    zc=70
    w=50
    a=.01
    kappa=20
endif

do j=1, n
    f(j)=max( f(j) , 0.D0 )
    fmax= fpeak*(a +   exp(- ( (z(j)-zc)/w )**kappa ))
    f(j)=min( f(j) , fmax )
enddo    
return
end subroutine

end module LinearSpatialFcnMod