!--------------------------------------------------------------------------------------------------
!> @page page1 Kriging Subroutines
!>
!> Define Kriging parameter structure with length scale alpha, 
!> sill value c and nugget value c0. Kringing form is 'spherical',
!> 'exponential', 'gaussian' or 'matern'
!--------------------------------------------------------------------------------------------------
! Keston Smith, Tom Callaghan (IBSS) 2024
!--------------------------------------------------------------------------------------------------
module KrigMod

use globals
use GridManagerMod
use NonLinearSpatialFcnMod
use RandomFieldMod
implicit none

type KrigPar
    real(dp) alpha
    real(dp) c0
    real(dp) c
    real(dp) Wz
    character(72) form
end type KrigPar
   
CONTAINS

!--------------------------------------------------------------------------------------------------
!>
!> Purpose: Computes distance between two vectors of points
!> Inputs:
!>    p: vector 1
!>    q: vector 2
!>   n_dim: allocated length of vectors
!> Outputs
!>   Dh: distance in x-y coordinates
!>   Dz: distance in depth
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine distance(p,q,Dh,Dz,n_dim)
    type(Grid_Data_Class), intent(in) :: p
    type(Grid_Data_Class), intent(in) :: q
    integer, intent(in)  :: n_dim
    real(dp), intent(out) :: Dh(n_dim,*),Dz(n_dim,*)
    integer    j

    do j=1,p%num_points
        Dh(j,1:q%num_points)= sqrt( (p%x(j) - q%x(1:q%num_points) )**2 + ( p%y(j)-q%y(1:q%num_points) )**2 )
        Dz(j,1:q%num_points)= sqrt( (p%z(j) - q%z(1:q%num_points) )**2 )
    enddo
end subroutine

!--------------------------------------------------------------------------------------------------
!>
!> Purpose: Computes a variogram (G) given distances between points (D).
!> 
!> @param[in] num_points    (integer) number of rows in D, Gamma
!> @param[in] num_cols    (integer) number of columns in D, Gamma
!> @param[in] D     (real(dp))  Size (num_points x num_cols) matrix distance between point pairs
!>
!> @param[out] G     (real(dp))  Size (num_points x num_cols) variogram matrix for point pairs represented in D
!>
!> Internal:\n
!>  - c     (real(dp))  Variogram parameter c+c0 = shelf\n
!>  - c0    (real(dp))  Variogram parameter nugget\n
!>  - alpha (real(dp))  Variogram length scale parameter\n
!>  - form  (charecter*72) functional form of variogram\n
!> The above should be read in an input file but are written as constants for now
!>    
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine variogram(num_points,num_cols,Dh,Dz,G,n_dim,par)
type(KrigPar):: par
integer, intent(in)    :: num_points,num_cols,n_dim
real(dp), intent(in)    :: Dh(n_dim,*),Dz(n_dim,*)
real(dp), intent(out)   :: G(n_dim,*)
integer j,k,error
real(dp) c,c0,alpha,Wz
real(dp) kap
character(72) form
real(dp), allocatable:: D(:,:)

allocate( D(1:num_points,1:num_cols) ) 

c0 = par%c0
c = par%c
alpha = par%alpha
Wz = par%Wz
form=par%form
D(1:num_points,1:num_cols)=sqrt( Dh(1:num_points,1:num_cols)**2 + ( Wz * Dz(1:num_points,1:num_cols) ) **2 )
if (trim(form).eq.'spherical')then
    do j=1,num_points
        do k=1,num_cols
            G(j,k)=c0+ c*(1.5*D(j,k)/alpha -((D(j,k)/alpha)**3)/2.)
            if(D(j,k).gt.alpha) G(j,k)=c0+c
            if(D(j,k).eq.0.) G(j,k)=0.
        enddo
    enddo
endif

if (trim(form).eq.'exponential')then
    do j=1,num_points
        do k=1,num_cols
            G(j,k)=c0+ c*(1 - exp(-D(j,k)/alpha)  )
            if(D(j,k).eq.0.) G(j,k)=0.
        enddo
    enddo
endif

if (trim(form).eq.'gaussian')then
    do j=1,num_points
        do k=1,num_cols
            G(j,k)=c0+ c*(1 - exp(-D(j,k)/alpha)**2  )
            if(D(j,k).eq.0.) G(j,k)=0.
        enddo
    enddo
endif
!BESSEL_JN(N, X)
if (trim(form).eq.'matern')then
    kap=.5
    do j=1,num_points
        do k=1,num_cols
            G(j,k)=c0+ c*(1. - (1./(gamma(kap) * 2.**(kap-1) ) ) * bessel_jn(2,D(j,k)/alpha) * (D(j,k)/alpha)**kap );
           if(D(j,k).eq.0.) G(j,k)=0.
        enddo
    enddo
endif

deallocate( D,stat=error) 

return
endsubroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Computes a variogram (G) given distances between points (D).
!>
!> @param[in]  num_points    (integer) number of rows in D, Gamma
!> @param[in]  n_dim    (integer) allocated number of rows D, Gamma
!> @param[in]  Dh, Dz     (real(dp))  Size (num_points x num_cols) matrix distance between point pairs
!> 
!> @param[out] G     (real(dp))  Size (num_points x num_cols) variogram matrix for point pairs represented in D
!> 
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine variogramF(num_points,Dh,Dz,G,n_dim,f,par)
type(KrigPar), intent(inout) :: par
type(KrigPar):: parTmp

integer  NIntVD,NPS
parameter(NIntVD=31,NPS=30)
integer, intent(in)  :: num_points,n_dim
real(dp), intent(in) :: Dh(n_dim,*),Dz(n_dim,*),f(*)
real(dp), intent(out):: G(n_dim,*)
real(dp) DintV(NIntVD),DintVm(NIntVD), SIntV(NIntVD)
real(dp) Pind(NPS),alphaR(NPS),c0R(NPS),cR(NPS),WzR
integer j,k,n,m,nc,NIntV
real(dp) dx,cost,costmin,Wz

parTmp%form=par%form
! Set up interval and average square of residual for variogram estimation
NintV=NIntVD
dx=5000.
DintV(1)=0.
do j=2,NIntV
    DintV(j)=DintV(j-1)+dx
enddo
do j=2,NIntV
    DintVm(j-1)=( DintV(j-1) + DintV(j) )/2. ! mid point
enddo
do k=1,NPS
    Pind(k)=float(k)/float(NPS)
enddo

costmin=huge(1.)
alphaR(1:NPS)=Pind(1:NPS)*100000.D0
! WzR,Wz -  These variables were used to allow anisotropy in the kriging covariance.
! they are not used here. WzR(1:NPS)=(Pind(1:NPS)-1./float(NPS))*10000.D0
WzR=0.D0

NIntV=NIntVD
Wz=WzR
do j=2,NIntV
    SIntV(j-1)=0.
    nc=0
    do n=1,num_points
        do m=1,num_points
            if (  ( Dh(n,m)+Wz*Dz(n,m).ge.DintV(j-1) ).and.( Dh(n,m)+Wz*Dz(n,m).le.DintV(j) )  )then
                SIntV(j-1)=SIntV(j-1)+(f(n)-f(m))**2
                nc=nc+1
            endif
        enddo
    enddo
    if (nc.gt.0)then
        SIntV(j-1)=SIntV(j-1)/(1.D0*nc)
    else
        SIntV(j-1)=0.
    endif
enddo
NIntV=NIntV-1
!set upper bounds for kriging parameter search
!if(nz.eq.1)then ! set bounds with isotropic distance
cR(1:NPS)=Pind(1:NPS)*maxval(SIntV(1:NIntV))
if(minval(SIntV(1:NIntV)).gt.0.)then
    c0R(1:NPS)=Pind(1:NPS)*minval(SIntV(1:NIntV))
else
    c0R(1:NPS)=Pind(1:NPS)*cR(1)
endif
!endif
do j=1,NPS
    do k=1,NPS
        do m=1,NPS
            parTmp%c0=c0R(j)
            parTmp%c=cR(k)
            parTmp%alpha=alphaR(m)
            parTmp%Wz=WzR
            call variogram(NintV,1,DintVm,0*DintVm,G,NintV,parTmp)
            cost=sum( (SIntV(1:NIntV)-G(1:NIntV,1))**2 )
            if( cost.lt.costmin )then
                costmin=cost
                par%c0=c0R(j)
                par%c=cR(k)
                par%alpha=alphaR(m)
                par%Wz=WzR
            endif
        enddo !m
    enddo !k
enddo !j

call variogram(NintV,1,DIntVm,0*DIntV,G,NintV,par)

call Write_2D_Scalar_Field(NintV,1,G,'GammaIntV.txt',NintV)
call Write_2D_Scalar_Field(NintV,1,DIntVm,'DIntV.txt',NintV)
call Write_2D_Scalar_Field(NintV,1,SIntV,'SIntV.txt',NintV)

write(*,*)'Bounds alpha:[', alphaR(1),par%alpha,alphaR(NPS),']'
write(*,*)'Bounds c0:[', c0R(1),par%c0,c0R(NPS),']'
write(*,*)'Bounds c:[',cR(1),par%c,cR(NPS),']'
call variogram(num_points, num_points, Dh,Dz, G, num_points, par)

return
endsubroutine
!--------------------------------------------------------------------------------------------------
!> Purpose: Computes value of spatial functions at x,y.
!> Inputs:
!> @param[in]  p      (Grid_Data_Class) - Spatial points to evaluate functions at
!> @param[in]  n_dim (integer) leading dimension of F
!> @param[in]  nlsf  (NLSF) vector of nonlinear spatial functions defined
!>
!> outputs:
!> @param[out] F    (real(dp))  Size (num_points x num_spat_fcns) matrix containg values of spatial functions at points (x,y)
!> @param[out] num_spat_fcns   (integer) number of spatial functions (be carefull allocating F)
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine spatial_function(p,F,num_spat_fcns,n_dim,nlsf)
type(Grid_Data_Class):: p
type(NLSFpar), intent(in):: nlsf(*)
integer, intent(in)    :: n_dim
integer, intent(in)   :: num_spat_fcns
real(dp), intent(out)   :: F(n_dim,*)
real(dp) s(n_dim), fpc(n_dim)
integer num_points,j,k

num_points=p%num_points
F(1:num_points,1)=1.
do j=1,nlsf(1)%nsf
    call NLSFunc (p,nlsf(j),s)
    if (nlsf(j)%PreCFnum.eq.0)then
        F(1:num_points,j+1)=s(1:num_points)
    else
        k=nlsf(j)%PreCFnum
        call NLSFunc (p,nlsf(k),fpc)
        F(1:num_points,j+1)=s(1:num_points)*fpc(1:num_points)
    endif
enddo

call write_csv(num_points,num_spat_fcns,F,'SpatialFunctions.csv',num_points)
end subroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Calculates Universal Kriging (UK) estimate at grid points given by coordinates (x, y). 
!> Additionally this returns the statistics needed to simulate from the posterior distribution, that 
!> is to simulate random fields consistent with the observations, spatial functions, and variogram.  
!> 
!>  Inputs:
!>   -  x    (real(dp)) length num_points vector of x-coordinates of grid  
!>   -  y    (real(dp)) length num_points vector of y-coordinates for grid  
!>   -  z    (real(dp)) length num_points vector of bathymetric depth at (x, y)  
!>   -  num_points    (integer) number of points in grid
!>   -  xo    (real(dp)) length num_obs_points vector of x-coordinates of data  
!>   -  yo    (real(dp)) length num_obs_points vector of y-coordinates for data  
!>   -  zo    (real(dp)) length num_obs_points vector of bathymetric depth at data point (xo, yo)  
!>   -  fo    (real(dp)) length num_obs_points vector of observations at (xo, yo)  
!>   -  n0    (integer) number of points in grid
!>
!>  Outputs:
!>   - f     (real(dp)) UK estimate (linear in observations, fo) of the field
!>   - beta  (real(dp)) length num_spat_fcns vector, best estimate of spatial function coefficients
!>   - Cbeta (real(dp)) (num_spat_fcns x num_spat_fcns) covariance matrix for the estimates of beta
!>   - eps   (real(dp)) length num_points vector, best estimate of residual process
!>   - CepsPost(real(dp)) (num_points x num_points) covariance matrix for residual esimate (the spatially coorelated random field component).
!>
!> Derivation for the UK algorithm and relation to generalized least squares estimation can be 
!> found in the book: N. Cressie (1993). Statistics for spatial data. Wiley, New York, 1993.
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine UK_GLS(g, obs, num_spat_fcns, par, beta, Cbeta, eps, CepsG, nlsf)
type(Grid_Data_Class):: g
type(Grid_Data_Class):: obs
type(KrigPar):: par
type(NLSFPar):: nlsf(*)

integer,    intent(in):: num_spat_fcns
real(dp),    intent(out):: beta(*), Cbeta(num_spat_fcns, num_spat_fcns), eps(*), CepsG(g%num_points, g%num_points)

real(dp),    allocatable:: Dh(:,:), Dz(:,:), W(:,:), gamma(:,:), Fs(:,:), FsT(:,:)
real(dp),    allocatable:: D0h(:,:), D0z(:,:), gamma0(:,:), Fs0(:,:), Fs0T(:,:)
real(dp),    allocatable:: R(:,:), V(:,:)
real(dp),    allocatable:: CbetaInv(:,:), Ceps(:,:), Cinv(:,:)
real(dp),    allocatable:: DGh(:,:), DGz(:,:), C0(:,:), gammaG(:,:)
real(dp),    allocatable:: Mtmp(:,:), Vtmp(:), Vtmp2(:), Gtmp(:,:), ftrnd(:)
integer,    allocatable:: ipiv(:)

real(dp)     Vinf(1, 1), Dinf(1, 1), atmp, btmp
integer     j, k, info, nopnf, error, num_points, num_obs_points
num_points=g%num_points
num_obs_points=obs%num_points
nopnf=num_obs_points+num_spat_fcns
allocate( Dh(1:num_obs_points, 1:num_obs_points), Dz(1:num_obs_points, 1:num_obs_points),  stat=error)
allocate( W(1:num_points, 1:num_obs_points), gamma(1:num_obs_points, 1:num_obs_points), stat=error)
allocate( Fs(1:num_obs_points, 1:num_spat_fcns), FsT(1:num_spat_fcns, 1:num_obs_points), stat=error)
allocate( D0h(1:num_obs_points, 1:num_points), D0z(1:num_obs_points, 1:num_points), stat=error)
allocate( gamma0(1:num_obs_points, 1:num_points), stat=error)
allocate( Fs0(1:num_points, 1:num_spat_fcns), Fs0T(1:num_spat_fcns, 1:num_points), stat=error)
allocate( R(1:nopnf, 1:nopnf), V(1:nopnf, 1:num_points), stat=error)
allocate( C0(1:num_obs_points, 1:num_points), DGh(1:num_points, 1:num_points), DGz(1:num_points, 1:num_points), stat=error)
allocate( gammaG(1:num_points, 1:num_points), stat=error )
allocate( CbetaInv(1:num_spat_fcns, 1:num_spat_fcns), Ceps(1:num_obs_points, 1:num_obs_points), stat=error)
allocate( Cinv(1:num_obs_points, 1:num_obs_points), stat=error)
allocate( Mtmp(1:num_obs_points, 1:num_spat_fcns), Vtmp(1:num_obs_points), Vtmp2(1:num_spat_fcns), stat=error)
allocate( Gtmp(1:num_obs_points, 1:num_points), stat=error)
allocate( ftrnd(1:num_points), ipiv(1:nopnf), stat=error)

!
! Compute variogram between observation points and spatial functions at
! observation points
!
call distance(obs, obs, Dh, Dz, num_obs_points)
call variogram(num_obs_points, num_obs_points, Dh, Dz, Gamma, num_obs_points, par)
call spatial_function(obs, Fs, num_spat_fcns, num_obs_points, nlsf)
!
! Ceps <- covariance between observation points (from variogram)
!

!
! Compute variogram between observation points and grid points and spatial
! functions at grid points
!
call distance(obs, g, D0h, D0z, num_obs_points)
call variogram(num_obs_points, num_points, D0h, D0z, gamma0, num_obs_points, par)
call spatial_function(g, Fs0, num_spat_fcns, num_points, nlsf)
!
!Compute the Univeral Kriging linear estimate of f following Cressie 1993
!pages 151-154
!
Fs0T=transpose(Fs0)
FsT=transpose(Fs)
R(1:num_obs_points+num_spat_fcns, 1:num_obs_points+num_spat_fcns)=0.
R(1:num_obs_points, 1:num_obs_points)=gamma(1:num_obs_points, 1:num_obs_points)
R(1:num_obs_points, num_obs_points+1:num_obs_points+num_spat_fcns)=Fs(1:num_obs_points, 1:num_spat_fcns)
R(num_obs_points+1:num_obs_points+num_spat_fcns, 1:num_obs_points)=FsT(1:num_spat_fcns, 1:num_obs_points)

V(1:num_obs_points, 1:num_points)=gamma0(1:num_obs_points, 1:num_points)
V(num_obs_points+1:num_obs_points+num_spat_fcns, 1:num_points)=Fs0T(1:num_spat_fcns, 1:num_points)

call dgesv(nopnf, num_points, R, nopnf, IPIV, V, nopnf, info)
write(*,*)'UK_GLS (a) dgesv info=', info
!
! compute best linear estimate of the field on the grid points x, y
! f = W f_obs
!
do j=1, num_points
    do k=1, num_obs_points
    W(j, k)=V(k, j)
    enddo
enddo
atmp=1.
btmp=0.
call dgemv('N', num_points, num_obs_points, atmp, W, num_points, obs%recr_psqm, 1,  btmp, g%recr_psqm, 1)
!
! compute posterior trend statistics
!
Dinf(1, 1)=10.**10
call variogram(1, 1, Dinf, Dinf, Vinf, 1, par)
!
! Ceps <- covariance between observation points (from variogram)
!
Ceps(1:num_obs_points, 1:num_obs_points)=Vinf(1, 1)-gamma(1:num_obs_points, 1:num_obs_points)
!
! Cinv = inv( Ceps )
!
Cinv(1:num_obs_points, 1:num_obs_points)=0.
do j=1, num_obs_points
    Cinv(j, j)=1.
enddo
call dgesv(num_obs_points, num_obs_points, Ceps, num_obs_points, IPIV, Cinv, num_obs_points, info)
!
! cov(beta_gls) = inv (F' Cinv F )
!
call dgemm('N', 'N', num_obs_points, num_spat_fcns, num_obs_points, atmp, Cinv, num_obs_points, Fs,   num_obs_points, &
&          btmp, Mtmp,     num_obs_points )
call dgemm('T', 'N', num_spat_fcns, num_spat_fcns, num_obs_points, atmp,  Fs, num_obs_points, Mtmp , num_obs_points, &
&          btmp, CbetaInv, num_spat_fcns )
CBeta(1:num_spat_fcns, 1:num_spat_fcns) = 0.
do j=1, num_spat_fcns
    CBeta(j, j)=1.
enddo
call dgesv(num_spat_fcns, num_spat_fcns, CBetaInv, num_spat_fcns, IPIV, CBeta, num_spat_fcns, info)
write(*,*)'UK_GLS (b) dgesv info=', info

!
! beta_gls = inv( F' * Cinv * F ) * F * Cinv * fo
!
Vtmp(1:num_obs_points)=matmul(Cinv(1:num_obs_points, 1:num_obs_points), obs%recr_psqm(1:num_obs_points))
Vtmp2(1:num_spat_fcns)=matmul(transpose(Fs(1:num_obs_points, 1:num_spat_fcns)), Vtmp(1:num_obs_points))
beta(1:num_spat_fcns)=matmul(Cbeta(1:num_spat_fcns, 1:num_spat_fcns), Vtmp2(1:num_spat_fcns))
!
! Posterior covariance for residual
!
call distance(g, g, DGh, DGz, num_points)
write(*,*)'UK_GLS (e) dgesv info=', info
call variogram(num_points, num_points, DGh, DGz, gammaG, num_points, par)
write(*,*)'UK_GLS (f) dgesv info=', info
CepsG(1:num_points, 1:num_points)=Vinf(1, 1)-gammaG(1:num_points, 1:num_points)
C0(1:num_obs_points, 1:num_points)=Vinf(1, 1)-gamma0(1:num_obs_points, 1:num_points)
!
! CepsG <== CepsPostG = CepsG - C0v' * inv(C_obs) * C0v
!
write(*,*)'UK_GLS (g) dgesv info=', info
call dgemm('N', 'N', num_obs_points, num_points, num_obs_points, atmp, Cinv, num_obs_points, C0, num_obs_points, &
&           btmp, Gtmp, num_obs_points )
write(*,*)'UK_GLS (h) dgesv info=', info
call dgemm('T', 'N', num_points, num_points, num_obs_points, atmp, C0, num_obs_points, Gtmp, num_obs_points, &
&           btmp, gammaG, num_points )
! note: gamma G<== C0' * Cinv * C0 no longer represents variogram for the grid!
write(*,*)'UK_GLS (i) dgesv info=', info

CepsG(1:num_points, 1:num_points) = CepsG(1:num_points, 1:num_points) - gammaG(1:num_points, 1:num_points)
!
! Compute posterior mean of epsilon.  This is the difference between unbiased estimate of f and
! the mean of the trend.  Thus the mean of the total distribution is the kriging estimate. 
!
write(*,*)'UK_GLS (j) dgesv info=', info
call dgemv('N', num_points, num_spat_fcns, atmp, Fs0, num_points, beta, 1, btmp, ftrnd, 1)
write(*,*)'UK_GLS (k) dgesv info=', info

eps(1:num_points)=g%recr_psqm(1:num_points)-ftrnd(1:num_points)

write(*,*)'UK_GLS end'
deallocate( Dh, Dz, W, gamma, Fs, FsT, stat=error )
deallocate( D0h, D0z, gamma0, Fs0, Fs0T , stat=error)
deallocate( R, V , stat=error)
deallocate( CbetaInv, Ceps, Cinv, stat=error )
deallocate( Mtmp, Vtmp, Vtmp2, Gtmp, ftrnd, stat=error )
deallocate( ipiv, stat=error)
deallocate( C0, DGh, DGz,  gammaG, ftrnd, stat=error )

return
end subroutine
    
!--------------------------------------------------------------------------------------------------
!> Purpose: Simulate independent random fields under the assumptions of a Universal Kriging model.
!> 
!>  Inputs:
!>   - x     (real(dp)) length num_points vector of x-coordinates of grid  
!>   - y     (real(dp)) length num_points vector of y-coordinates for grid  
!>   - z     (real(dp)) length num_points vector of bathymetric depth at (x, y)  
!>   - num_points    (integer) number of points in grid
!>   - beta  (real(dp)) length num_spat_fcns vector estimate of spatial function coefficients
!>   - Cbeta (real(dp)) (num_spat_fcns x num_spat_fcns) covariance matrix for estimate beta
!>   - eps   (real(dp)) length num_points vector estimate of noise field
!>   - Ceps  (real(dp)) (num_points x num_points) spatial covariance matrix of residual from linear fit of spatial functions
!>    Nsim    (integer) number of random fields to simulate
!>
!>  Outputs:
!>   - none    
!>  Currently random fields are written to files 'totalRF.csv', 'trendRF.csv', and noiseRF.csv' 
!>  as (num_points x Nsim) matricies in which each column represents an independent random field. '
!>  trend' is the component related to the spatial functions, 'noise' the component related 
!>  to the residual, and 'total' their sum.
!>
!> The independence of the trend component and spatially correlated noise component is assumed.
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine UK_RandomField(g, num_spat_fcns, beta, Cbeta, eps, Ceps, Nsim, nlsf, IsLogT, mu, A, SF, fmax, z)
implicit none
type(Grid_Data_Class):: g
type(NLSFpar):: nlsf(*)
integer,  intent(in) :: num_spat_fcns
real(dp), intent(in) :: beta(*), Cbeta(num_spat_fcns,*), eps(*), Ceps(g%num_points,*), mu(*), A, SF, fmax, z(*)
integer,  intent(in) :: Nsim
logical,  intent(in) :: IsLogT

real(dp),    allocatable    :: F(:,:)
real(dp),    allocatable    :: BetaRF(:,:), RFtrend(:,:), RFtotal(:,:), RFeps(:,:), EnsMu(:), &
trend(:), Ctrend(:,:)

real(dp)     atmp, btmp, adj
integer     k, n
n=g%num_points
allocate(F(1:n, 1:num_spat_fcns))
allocate(BetaRF(1:num_spat_fcns, 1:Nsim), RFtrend(1:n, 1:NSim), RFtotal(1:n, 1:NSim), RFeps(1:n, 1:NSim), &
        EnsMu(1:n), trend(1:n), Ctrend(1:n, 1:n) )

atmp=1.
btmp=0.

call RandomSampleF(n, n, Nsim, eps(1:n), Ceps, RFeps)

call spatial_function(g, F, num_spat_fcns, g%num_points, nlsf)
trend(1:n)=matmul(F(1:n, 1:num_spat_fcns), beta(1:num_spat_fcns))

call RandomSampleF(num_spat_fcns, num_spat_fcns, Nsim, beta(1:num_spat_fcns), Cbeta, BetaRF)
RFtrend(1:n, 1:Nsim)=matmul(F(1:n, 1:num_spat_fcns), BetaRF(1:num_spat_fcns, 1:Nsim))

RFtotal(1:n, 1:NSim)=RFtrend(1:n, 1:NSim)+RFeps(1:n, 1:NSim)

if(IsLogT)then
    RFtrend(1:n, 1:NSim) = SF * exp(RFtrend(1:n, 1:NSim)) - A  
    RFeps(1:n, 1:NSim) = SF * exp(RFeps(1:n, 1:NSim))   - A 
endif

!-----------------------------------------------------------
!enforce ensemble mean in trend
trend(1:n)=matmul(F(1:n, 1:num_spat_fcns), beta(1:num_spat_fcns))
if(IsLogT)trend(1:n) = SF * exp(trend(1:n)) - A  
!-----------------------------------------------------------

do k=1, n
    call limitz(Nsim, RFtrend(k, 1:Nsim), z(k)+0.*RFtrend(k, 1:Nsim), fmax, 'MA')
    call limitz(Nsim, RFeps(k, 1:Nsim), z(k)+0.*RFeps(k, 1:Nsim), fmax, 'MA')
enddo
if(IsLogT)then
    RFtotal(1:n, 1:NSim) = SF * exp(RFtotal(1:n, 1:NSim)) - A 
endif

open(69, FILE='RandomFieldSF.txt')
!adjust ensemble ensemble mean to match kriging estimate 
do k=1, n
    EnsMu(k)=sum( RFtotal(k, 1:Nsim) )/float(Nsim)
    adj=min( (mu(k)+A) / (EnsMu(k)+A), 1.D0)
    RFtotal(k, 1:Nsim) = RFtotal(k, 1:Nsim)*adj
    call limitz(Nsim, RFtotal(k, 1:Nsim), z(k)+0.*RFtotal(k, 1:Nsim), fmax, 'MA')
    write(69,*) mu(k)  - EnsMu(k)
enddo
close(69)

call write_csv(g%num_points, NSim, RFeps, 'noiseRF.csv', g%num_points)
call write_csv(g%num_points, NSim, RFtrend, 'trendRF.csv', g%num_points)
call write_csv(g%num_points, NSim, RFtotal, 'totalRF.csv', g%num_points)

deallocate(F)
deallocate(BetaRF, RFtrend, RFtotal, RFeps)

return
end subroutine

!--------------------------------------------------------------------------------------------------
!> Assign values needed to simulate random fields from a Universal Kriging 
!> model.
!>
!> Inputs: 
!>  - g   (real(dp)) Data point structure defining interpolation points 
!>  - num_spat_fcns  (integer) number of spatial functions
!>  - par variogram parameter structure
!>
!> Outputs:
!>  - beta(real(dp))  mean of spatial function coefficients
!>  - Cbeta(real(dp)) covariance of spatial function coefficients
!>  - eps(real(dp))   mean of residual
!>  - Ceps(real(dp))  covariance of residual
!> @author Keston Smith (IBSS corp) June-July 2021
!> @todo need to fix dimensioning (1/10/2022)!
!-----------------------------------------------------------------------
subroutine UK_prior(g, num_spat_fcns, par, beta, Cbeta, eps, Ceps)
type(Grid_Data_Class):: g
type(KrigPar):: par
integer,    intent(in):: num_spat_fcns
real(dp),    intent(out):: beta(*), Cbeta(num_spat_fcns,*), eps(*), Ceps(g%num_points,*) 

real(dp),    allocatable:: Dh(:,:), Dz(:,:), gamma(:,:)

real(dp)     Vinf(1, 1), Dinf(1, 1)
integer     error, num_points
num_points=g%num_points
allocate( Dh(1:num_points, 1:num_points), Dz(1:num_points, 1:num_points), gamma(1:num_points, 1:num_points), stat=error )
!
! Assign mean and Covariance for epsilon
!
call distance(g, g, Dh, Dz, num_points)
call variogram(num_points, num_points, Dh, Dz, gamma, num_points, par)
Dinf(1, 1)=10.**10
call variogram(1, 1, Dinf, Dinf, Vinf, 1, par)
Ceps(1:num_points, 1:num_points)=Vinf(1, 1)-gamma(1:num_points, 1:num_points)
eps(1:num_points)=0.
!
! Assign mean and Covariance for beta
!
beta(1:num_spat_fcns)=0.
Cbeta(1:num_spat_fcns, 1:num_spat_fcns)=0.
beta(1:4)= (/ 52.399839511692335, 1.0935428391808943E-004, -3.1615155809229245E-003, 1.2116283255694471E-002  /)
Cbeta(1, 1:4)= (/270.60028747602928, 1.5366756312328578E-003, 7.9881962338312195E-003, -6.0292972962538195E-002 /)
Cbeta(2, 1:4)= (/1.5366756312270658E-003, 1.7247639913097469E-005, 1.3324610875859451E-006, -2.3045855632454535E-007 /)
Cbeta(3, 1:4)= (/7.9881962338509589E-003, 1.3324610875862412E-006, 5.5464689237415081E-006, -2.4207894889311469E-006 /)
Cbeta(4, 1:4)= (/-6.0292972962540783E-002, -2.3045855632585232E-007, -2.4207894889268253E-006, 1.3517389449680424E-005 /)

deallocate( Dh, Dz, gamma, stat=error )

return
end subroutine

end module
