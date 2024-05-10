!--------------------------------------------------------------------------------------------------
!> @page page3 Linear Spatial Functions
!>
!--------------------------------------------------------------------------------------------------
! Keston Smith, Tom Callaghan (IBSS) 2024
!--------------------------------------------------------------------------------------------------
module LSF_Mod
use globals
implicit none

CONTAINS

!--------------------------------------------------------------------------------------------------
!> Purpose:Generalized Least Squares solver. Solve for beta to fit y = F*beta +epsilon, for 
!> epsilon~N(0, C). Return optimal beta, covariance of beta and residual
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
!>  -   r     (real(dp)) length num_points vector of residuals r = y - F * beta
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
function LSF_Generalized_Least_Squares(y, F, C, n, m, beta, Cbeta, save_data)
use globals
implicit none
integer, intent(in):: n, m
real(dp), intent(in):: y(*), F(n,*), C(n,*)
real(dp) :: LSF_Generalized_Least_Squares(n)
real(dp), intent(out):: beta(*), Cbeta(m,m)
logical, intent(in) :: save_data

real(dp), allocatable:: Cinv(:,:), CbetaInv(:,:), Vtmp(:), Vtmp2(:), Mtmp(:,:), ytr(:)
integer, allocatable:: ipiv(:)

allocate( Cinv(1:n, 1:n), CbetaInv(1:m, 1:m), ytr(1:n), Mtmp(1:n, 1:m), Vtmp(1:n), Vtmp2(1:m) ) 
allocate( ipiv(1:n))

Cinv = matrixinv(C, n)

!
! cov(beta)) =  inv (F' Cinv F )
!
Mtmp(1:n, 1:m) = matmul(Cinv(1:n, 1:n), F(1:n, 1:m))
CbetaInv(1:m, 1:m) = matmul(transpose(F(1:n, 1:m)), Mtmp(1:n, 1:m))
if (save_data) then
    call Write_CSV(m, m, CbetaInv, 'CBeta0.csv', m, .false.)
    call Write_CSV(n, m, F, 'Fglsa0.csv', n, .false.)
endif

CBeta = matrixinv(CbetaInv, m)

!
! beta = inv( F' * Cinv * F ) * F * Cinv * fo
!
Vtmp(1:n) = matmul(Cinv(1:n,1:n), y(1:n))
Vtmp2(1:m) = matmul(transpose(F(1:n, 1:m)),Vtmp(1:n) )
beta(1:m) = matmul(CBeta(1:m, 1:m), Vtmp2(1:m))
!
! compute residual
!
! ytr is the "trend" as described by the spatial functions
ytr(1:n) = matmul(F(1:n, 1:m), beta(1:m))
LSF_Generalized_Least_Squares(1:n) = y(1:n) - ytr(1:n)

if (save_data) then
    call Write_Vector_Scalar_Field(n,  y(1:n), 'obs.txt')
    call Write_Vector_Scalar_Field(n,  ytr(1:n), 'obs_ytr.txt')
    call Write_Vector_Scalar_Field(n,  y(1:n) - ytr(1:n), 'obs_delta.txt')
endif

deallocate(ipiv)
deallocate(Cinv, CbetaInv, ytr, Mtmp, Vtmp, Vtmp2)

endfunction LSF_Generalized_Least_Squares

!--------------------------------------------------------------------------------------------------
!> Purpose: Simple linear regresion 
!> minimize sum_{j=1:n}  ( y(j) - alpha + beta * x(j) )**2 over alpha and beta
!>
!> Simple Least Squares
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
function LSF_Simple_Linear_Regression(y, x, n)
integer,  intent(in):: n
real(dp), intent(in):: y(*), x(*)
real(dp) :: LSF_Simple_Linear_Regression(1:n)

real(dp) alpha, beta
real(dp) Sxy, Sx, Sy, Sx2
Sxy = sum(x(1:n) * y(1:n))
Sx  = sum(x(1:n))
Sy  = sum(y(1:n))
Sx2 = sum(x(1:n) * x(1:n))

beta = (n * Sxy - Sx * Sy) / ( n * Sx2 - Sx * Sx)
alpha=(Sy / n) - (beta*Sx / n)
LSF_Simple_Linear_Regression(1:n) = alpha + beta * x(1:n)
end function LSF_Simple_Linear_Regression

!----------------------------------------------------------------------------------------
!> Purpose: Restrict maximum recruit density based on historical relation ship between 
!> peak recruitment and bathymetry within a year.  The curves here are derived from scallop 
!> dredge survey data from 1979 to 2018.
!>
!> inputs: 
!>  - n      length of vectors f and z
!>  - z      [num_points] bathymetric depth
!>  - fpeak  highest observed value of recruitment
!>  - Domain 'MA', Mid Atlantic or 'GB' Georges Bank
!>
!> input/output
!>  - f [n] recruitment observations from the same year.
!> @author Keston Smith 2022
!----------------------------------------------------------------------------------------
subroutine LSF_Limit_Z(n, f, z, fpeak, Domain)

integer, intent(in) :: n
real(dp), intent(in) :: z(*), fpeak
real(dp), intent(inout) :: f(*)
character(domain_len), intent(in):: Domain
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
end subroutine

end module LSF_Mod