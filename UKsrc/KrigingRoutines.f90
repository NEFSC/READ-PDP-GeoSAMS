!--------------------------------------------------------------------------------------------------
!> @page page1 Kriging Subroutines
!>
!> Define Kriging parameter structure with length scale alpha, 
!> sill value c and nugget value c0. Kringing form is 'spherical',
!> 'exponential', 'gaussian' or 'matern'
!>
!> @authors Keston Smith, Tom Callaghan IBSS (2024)
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
!>   @param[in]  nn    (integer) number of spatial points in vectors x0,y0 
!>   @param[in] x0    (real(dp))  length nn vector of x coordinates (m)
!>   @param[in] y0    (real(dp))  length nn  vector of y coordinates (m)
!>   @param[in] z0    (real(dp))  length nn  vector of bathymetric depth at (x,y)
!>
!>   @param[in]  mm    (integer) number of spatial points in vectors x0,y0 
!>   @param[in] x1    (real(dp))  length mm  vector of x coordinates (m)
!>   @param[in] y1    (real(dp))  length mm vector of y coordinates (m)
!>   @param[in] z1    (real(dp))  length mm vector of bathymetric depth at (x,y)
!>
!>   @param[out] D    (real(dp))  Size (nn x mm) matrix of distances between point pairs (x0,y0),(x1,y1)
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine distance(p,q,Dh,Dz,n_dim)
    type(Grid_Data_Class):: p
    type(Grid_Data_Class):: q
    integer, intent(in)  :: n_dim
    real(dp), intent(out) :: Dh(n_dim,*),Dz(n_dim,*)
    integer    j

    do j=1,p%n
        Dh(j,1:q%n)= sqrt( (p%x(j) - q%x(1:q%n) )**2 + ( p%y(j)-q%y(1:q%n) )**2 )
        Dz(j,1:q%n)= sqrt( (p%z(j) - q%z(1:q%n) )**2 )
    enddo
end subroutine

!--------------------------------------------------------------------------------------------------
!>
!> Purpose: Computes a variogram (G) given distances between points (D).
!> 
!> @param[in] nn    (integer) number of rows in D, Gamma
!> @param[in] mm    (integer) number of columns in D, Gamma
!> @param[in] D     (real(dp))  Size (nn x mm) matrix distance between point pairs
!>
!> @param[out] G     (real(dp))  Size (nn x mm) variogram matrix for point pairs represented in D
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
subroutine variogram(nn,mm,Dh,Dz,G,n_dim,par)
type(KrigPar):: par
integer, intent(in)    :: nn,mm,n_dim
real(dp), intent(in)    :: Dh(n_dim,*),Dz(n_dim,*)
real(dp), intent(out)   :: G(n_dim,*)
integer j,k,error
real(dp) c,c0,alpha,Wz
real(dp) kap
character(72) form
real(dp), allocatable:: D(:,:)

allocate( D(1:nn,1:mm) ) 

c0 = par%c0
c = par%c
alpha = par%alpha
Wz = par%Wz
form=par%form
D(1:nn,1:mm)=sqrt( Dh(1:nn,1:mm)**2 + ( Wz * Dz(1:nn,1:mm) ) **2 )
if (trim(form).eq.'spherical')then
    do j=1,nn
        do k=1,mm
            G(j,k)=c0+ c*(1.5*D(j,k)/alpha -((D(j,k)/alpha)**3)/2.)
            if(D(j,k).gt.alpha) G(j,k)=c0+c
            if(D(j,k).eq.0.) G(j,k)=0.
        enddo
    enddo
endif

if (trim(form).eq.'exponential')then
    do j=1,nn
        do k=1,mm
            G(j,k)=c0+ c*(1 - exp(-D(j,k)/alpha)  )
            if(D(j,k).eq.0.) G(j,k)=0.
        enddo
    enddo
endif

if (trim(form).eq.'gaussian')then
    do j=1,nn
        do k=1,mm
            G(j,k)=c0+ c*(1 - exp(-D(j,k)/alpha)**2  )
            if(D(j,k).eq.0.) G(j,k)=0.
        enddo
    enddo
endif
!BESSEL_JN(N, X)
if (trim(form).eq.'matern')then
    kap=.5
    do j=1,nn
        do k=1,mm
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
!> @param[in]  nn    (integer) number of rows in D, Gamma
!> @param[in]  mm    (integer) number of columns in D, Gamma
!> @param[in]  Dh, Dz     (real(dp))  Size (nn x mm) matrix distance between point pairs
!> 
!> @param[out] G     (real(dp))  Size (nn x mm) variogram matrix for point pairs represented in D
!> 
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine variogramF(nn,Dh,Dz,G,n_dim,f,par)
type(KrigPar), intent(inout) :: par
type(KrigPar):: parTmp

integer  NIntVD,NPS
parameter(NIntVD=31,NPS=30)
integer, intent(in)  :: nn,n_dim
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
    do n=1,nn
        do m=1,nn
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

call write_scalar_fields(NintV,1,G,'GammaIntV.txt',NintV)
call write_scalar_fields(NintV,1,DIntVm,'DIntV.txt',NintV)
call write_scalar_fields(NintV,1,SIntV,'SIntV.txt',NintV)

write(*,*)'Bounds alpha:[', alphaR(1),par%alpha,alphaR(NPS),']'
write(*,*)'Bounds c0:[', c0R(1),par%c0,c0R(NPS),']'
write(*,*)'Bounds c:[',cR(1),par%c,cR(NPS),']'
call variogram(nn, nn, Dh,Dz, G, nn, par)

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
!> @param[out] F    (real(dp))  Size (nn x nf) matrix containg values of spatial functions at points (x,y)
!> @param[out] nf   (integer) number of spatial functions (be carefull allocating F)
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine spatial_function(p,F,nf,n_dim,nlsf)
type(Grid_Data_Class):: p
type(NLSFpar), intent(in):: nlsf(*)
integer, intent(in)    :: n_dim
integer, intent(out)   :: nf
real(dp), intent(out)   :: F(n_dim,*)
real(dp) s(n_dim), fpc(n_dim)
integer nn,j,k

nf=nlsf(1)%nsf+1
! flag for initial call
if (n_dim.eq.1) return
nn=p%n
F(1:nn,1)=1.
do j=1,nlsf(1)%nsf
    call NLSFunc (p,nlsf(j),s)
    if (nlsf(j)%PreCFnum.eq.0)then
        F(1:nn,j+1)=s(1:nn)
    else
        k=nlsf(j)%PreCFnum
        call NLSFunc (p,nlsf(k),fpc)
        F(1:nn,j+1)=s(1:nn)*fpc(1:nn)
    endif
enddo
call write_csv(nn,nf,F,'SpatialFunctions.csv',nn)
end subroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Computes value of spatial functions at p%x(1:p%n),p%y(1:p%n).  This is left as an example
!> subroutine for users to define their own spatial_function subroutine. 
!> 
!> @param[in] p     (Grid_Data_Class) - Spatial points at which to evaluate functions
!> @param[in] n_dim (integer) leading dimension of F  
!> 
!> @param[out] F    (real(dp))  Size (nn x nf) matrix containg values of spatial functions at points (x,y)
!> @param[out] nf   (integer) number of spatial functions (be carefull allocating F)
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine spatial_function_lin(p,F,nf,n_dim)
type(Grid_Data_Class):: p
integer, intent(in)    :: n_dim
integer, intent(out)   :: nf
real(dp), intent(out)   :: F(n_dim,*)
integer nn

nf=4
! flag for initial call
if (n_dim.eq.1) return
nn=p%n

F(1:nn,1)=1.
F(1:nn,2)=p%x(1:nn)/1000.
F(1:nn,3)=p%y(1:nn)/1000.
F(1:nn,4)=p%z(1:nn)

call write_csv(nn,nf,F,'SpatialFunctions.csv',nn)
end subroutine

!--------------------------------------------------------------------------------------------------
!> Purpose: Calculates Universal Kriging (UK) estimate at grid points given by coordinates (x, y). 
!> Additionally this returns the statistics needed to simulate from the posterior distribution, that 
!> is to simulate random fields consistent with the observations, spatial functions, and variogram.  
!> 
!>  Inputs:
!>   -  x    (real(dp)) length nn vector of x-coordinates of grid  
!>   -  y    (real(dp)) length nn vector of y-coordinates for grid  
!>   -  z    (real(dp)) length nn vector of bathymetric depth at (x, y)  
!>   -  nn    (integer) number of points in grid
!>   -  xo    (real(dp)) length no vector of x-coordinates of data  
!>   -  yo    (real(dp)) length no vector of y-coordinates for data  
!>   -  zo    (real(dp)) length no vector of bathymetric depth at data point (xo, yo)  
!>   -  fo    (real(dp)) length no vector of observations at (xo, yo)  
!>   -  n0    (integer) number of points in grid
!>
!>  Outputs:
!>   - f     (real(dp)) UK estimate (linear in observations, fo) of the field
!>   - beta  (real(dp)) length nf vector, best estimate of spatial function coefficients
!>   - Cbeta (real(dp)) (nf x nf) covariance matrix for the estimates of beta
!>   - eps   (real(dp)) length nn vector, best estimate of residual process
!>   - CepsPost(real(dp)) (nn x nn) covariance matrix for residual esimate (the spatially coorelated random field component).
!>
!> Derivation for the UK algorithm and relation to generalized least squares estimation can be 
!> found in the book: N. Cressie (1993). Statistics for spatial data. Wiley, New York, 1993.
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine UK_GLS(g, obs, nf, par, beta, Cbeta, eps, CepsG, nlsf)
type(Grid_Data_Class):: g
type(Grid_Data_Class):: obs
type(KrigPar):: par
type(NLSFPar):: nlsf(*)

integer,    intent(inout):: nf
real(dp),    intent(out):: beta(*), Cbeta(nf, nf), eps(*), CepsG(g%n, g%n)

real(dp),    allocatable:: Dh(:,:), Dz(:,:), W(:,:), gamma(:,:), Fs(:,:), FsT(:,:)
real(dp),    allocatable:: D0h(:,:), D0z(:,:), gamma0(:,:), Fs0(:,:), Fs0T(:,:)
real(dp),    allocatable:: R(:,:), V(:,:)
real(dp),    allocatable:: CbetaInv(:,:), Ceps(:,:), Cinv(:,:)
real(dp),    allocatable:: DGh(:,:), DGz(:,:), C0(:,:), gammaG(:,:)
real(dp),    allocatable:: Mtmp(:,:), Vtmp(:), Vtmp2(:), Gtmp(:,:), ftrnd(:)
integer,    allocatable:: ipiv(:)

real(dp)     Vinf(1, 1), Dinf(1, 1), atmp, btmp
integer     j, k, info, nopnf, error, nn, no
nn=g%n
no=obs%n
nopnf=no+nf
allocate( Dh(1:no, 1:no), Dz(1:no, 1:no), W(1:nn, 1:no), gamma(1:no, 1:no), Fs(1:no, 1:nf), FsT(1:nf, 1:no), stat=error )
allocate( D0h(1:no, 1:nn), D0z(1:no, 1:nn), gamma0(1:no, 1:nn), Fs0(1:nn, 1:nf), Fs0T(1:nf, 1:nn) , stat=error)
allocate( R(1:nopnf, 1:nopnf), V(1:nopnf, 1:nn) , stat=error)
allocate( C0(1:no, 1:nn), DGh(1:nn, 1:nn), DGz(1:nn, 1:nn), gammaG(1:nn, 1:nn), stat=error )
allocate( CbetaInv(1:nf, 1:nf), Ceps(1:no, 1:no), Cinv(1:no, 1:no), stat=error )
allocate( Mtmp(1:no, 1:nf), Vtmp(1:no), Vtmp2(1:nf), Gtmp(1:no, 1:nn), ftrnd(1:nn), stat=error )
allocate( ipiv(1:nopnf), stat=error)

!
! Compute variogram between observation points and spatial functions at
! observation points
!
call distance(obs, obs, Dh, Dz, no)
call variogram(no, no, Dh, Dz, Gamma, no, par)
call spatial_function(obs, Fs, nf, no, nlsf)
!
! Ceps <- covariance between observation points (from variogram)
!

!
! Compute variogram between observation points and grid points and spatial
! functions at grid points
!
call distance(obs, g, D0h, D0z, no)
call variogram(no, nn, D0h, D0z, gamma0, no, par)
call spatial_function(g, Fs0, nf, nn, nlsf)
!
!Compute the Univeral Kriging linear estimate of f following Cressie 1993
!pages 151-154
!
Fs0T=transpose(Fs0)
FsT=transpose(Fs)
R(1:no+nf, 1:no+nf)=0.
R(1:no, 1:no)=gamma(1:no, 1:no)
R(1:no, no+1:no+nf)=Fs(1:no, 1:nf)
R(no+1:no+nf, 1:no)=FsT(1:nf, 1:no)

V(1:no, 1:nn)=gamma0(1:no, 1:nn)
V(no+1:no+nf, 1:nn)=Fs0T(1:nf, 1:nn)

call dgesv(nopnf, nn, R, nopnf, IPIV, V, nopnf, info)
write(*,*)'UK_GLS (a) dgesv info=', info
!
! compute best linear estimate of the field on the grid points x, y
! f = W f_obs
!
do j=1, nn
    do k=1, no
    W(j, k)=V(k, j)
    enddo
enddo
atmp=1.
btmp=0.
call dgemv('N', nn, no, atmp, W, nn, obs%f, 1,  btmp, g%f, 1)
!
! compute posterior trend statistics
!
Dinf(1, 1)=10.**10
call variogram(1, 1, Dinf, Dinf, Vinf, 1, par)
!
! Ceps <- covariance between observation points (from variogram)
!
Ceps(1:no, 1:no)=Vinf(1, 1)-gamma(1:no, 1:no)
!
! Cinv = inv( Ceps )
!
Cinv(1:no, 1:no)=0.
do j=1, no
    Cinv(j, j)=1.
enddo
call dgesv(no, no, Ceps, no, IPIV, Cinv, no, info)
!
! cov(beta_gls) = inv (F' Cinv F )
!
call dgemm('N', 'N', no, nf, no, atmp, Cinv, no, Fs,   no, btmp, Mtmp,    no )
call dgemm('T', 'N', nf, nf, no, atmp,  Fs, no, Mtmp , no, btmp, CbetaInv, nf )
CBeta(1:nf, 1:nf)=0.
do j=1, nf
    CBeta(j, j)=1.
enddo
call dgesv(nf, nf, CBetaInv, nf, IPIV, CBeta, nf, info)
write(*,*)'UK_GLS (b) dgesv info=', info

!
! beta_gls = inv( F' * Cinv * F ) * F * Cinv * fo
!
Vtmp(1:no)=matmul(Cinv(1:no, 1:no), obs%f(1:no))
Vtmp2(1:nf)=matmul(transpose(Fs(1:no, 1:nf)), Vtmp(1:no))
beta(1:nf)=matmul(Cbeta(1:nf, 1:nf), Vtmp2(1:nf))
!
! Posterior covariance for residual
!
call distance(g, g, DGh, DGz, nn)
write(*,*)'UK_GLS (e) dgesv info=', info
call variogram(nn, nn, DGh, DGz, gammaG, nn, par)
write(*,*)'UK_GLS (f) dgesv info=', info
CepsG(1:nn, 1:nn)=Vinf(1, 1)-gammaG(1:nn, 1:nn)
C0(1:no, 1:nn)=Vinf(1, 1)-gamma0(1:no, 1:nn)
!
! CepsG <== CepsPostG = CepsG - C0v' * inv(C_obs) * C0v
!
write(*,*)'UK_GLS (g) dgesv info=', info
call dgemm('N', 'N', no, nn, no, atmp, Cinv, no, C0, no, btmp, Gtmp, no )
write(*,*)'UK_GLS (h) dgesv info=', info
call dgemm('T', 'N', nn, nn, no, atmp, C0, no, Gtmp, no, btmp, gammaG, nn )
! note: gamma G<== C0' * Cinv * C0 no longer represents variogram for the grid!
write(*,*)'UK_GLS (i) dgesv info=', info

CepsG(1:nn, 1:nn) = CepsG(1:nn, 1:nn) - gammaG(1:nn, 1:nn)
!
! Compute posterior mean of epsilon.  This is the difference between unbiased estimate of f and
! the mean of the trend.  Thus the mean of the total distribution is the kriging estimate. 
!
write(*,*)'UK_GLS (j) dgesv info=', info
call dgemv('N', nn, nf, atmp, Fs0, nn, beta, 1, btmp, ftrnd, 1)
write(*,*)'UK_GLS (k) dgesv info=', info

eps(1:nn)=g%f(1:nn)-ftrnd(1:nn)

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
!>   - x     (real(dp)) length nn vector of x-coordinates of grid  
!>   - y     (real(dp)) length nn vector of y-coordinates for grid  
!>   - z     (real(dp)) length nn vector of bathymetric depth at (x, y)  
!>   - nn    (integer) number of points in grid
!>   - beta  (real(dp)) length nf vector estimate of spatial function coefficients
!>   - Cbeta (real(dp)) (nf x nf) covariance matrix for estimate beta
!>   - eps   (real(dp)) length nn vector estimate of noise field
!>   - Ceps  (real(dp)) (nn x nn) spatial covariance matrix of residual from linear fit of spatial functions
!>    Nsim    (integer) number of random fields to simulate
!>
!>  Outputs:
!>   - none    
!>  Currently random fields are written to files 'totalRF.csv', 'trendRF.csv', and noiseRF.csv' 
!>  as (nn x Nsim) matricies in which each column represents an independent random field. '
!>  trend' is the component related to the spatial functions, 'noise' the component related 
!>  to the residual, and 'total' their sum.
!>
!> The independence of the trend component and spatially correlated noise component is assumed.
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine UK_RandomField(g, nf, beta, Cbeta, eps, Ceps, Nsim, nlsf, IsLogT, mu, A, SF, fmax, z)
implicit none
type(Grid_Data_Class):: g
type(NLSFpar):: nlsf(*)
integer,    intent(inout)    :: nf
real(dp),    intent(in)    :: beta(*), Cbeta(nf,*), eps(*), Ceps(g%n,*), mu(*), A, SF, fmax, z(*)
integer,    intent(in)    :: Nsim
logical,    intent(in)    :: IsLogT

real(dp),    allocatable    :: F(:,:)
real(dp),    allocatable    :: BetaRF(:,:), RFtrend(:,:), RFtotal(:,:), RFeps(:,:), EnsMu(:), &
trend(:), Ctrend(:,:)

real(dp)     atmp, btmp, adj
integer     k, n
n=g%n
allocate(F(1:n, 1:nf))
allocate(BetaRF(1:nf, 1:Nsim), RFtrend(1:n, 1:NSim), RFtotal(1:n, 1:NSim), RFeps(1:n, 1:NSim), &
        EnsMu(1:n), trend(1:n), Ctrend(1:n, 1:n) )

atmp=1.
btmp=0.

call RandomSampleF(n, n, Nsim, eps(1:n), Ceps, RFeps)

call spatial_function(g, F, nf, g%n, nlsf)
trend(1:n)=matmul(F(1:n, 1:nf), beta(1:nf))

call RandomSampleF(nf, nf, Nsim, beta(1:nf), Cbeta, BetaRF)
RFtrend(1:n, 1:Nsim)=matmul(F(1:n, 1:nf), BetaRF(1:nf, 1:Nsim))

RFtotal(1:n, 1:NSim)=RFtrend(1:n, 1:NSim)+RFeps(1:n, 1:NSim)

if(IsLogT)then
    RFtrend(1:n, 1:NSim) = SF * exp(RFtrend(1:n, 1:NSim)) - A  
    RFeps(1:n, 1:NSim) = SF * exp(RFeps(1:n, 1:NSim))   - A 
endif

!-----------------------------------------------------------
!enforce ensemble mean in trend
trend(1:n)=matmul(F(1:n, 1:nf), beta(1:nf))
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

call write_csv(g%n, NSim, RFeps, 'noiseRF.csv', g%n)
call write_csv(g%n, NSim, RFtrend, 'trendRF.csv', g%n)
call write_csv(g%n, NSim, RFtotal, 'totalRF.csv', g%n)

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
!>  - nf  (integer) number of spatial functions
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
subroutine UK_prior(g, nf, par, beta, Cbeta, eps, Ceps)
type(Grid_Data_Class):: g
type(KrigPar):: par
integer,    intent(in):: nf
real(dp),    intent(out):: beta(*), Cbeta(nf,*), eps(*), Ceps(g%n,*) 

real(dp),    allocatable:: Dh(:,:), Dz(:,:), gamma(:,:)

real(dp)     Vinf(1, 1), Dinf(1, 1)
integer     error, nn
nn=g%n
allocate( Dh(1:nn, 1:nn), Dz(1:nn, 1:nn), gamma(1:nn, 1:nn), stat=error )
!
! Assign mean and Covariance for epsilon
!
call distance(g, g, Dh, Dz, nn)
call variogram(nn, nn, Dh, Dz, gamma, nn, par)
Dinf(1, 1)=10.**10
call variogram(1, 1, Dinf, Dinf, Vinf, 1, par)
Ceps(1:nn, 1:nn)=Vinf(1, 1)-gamma(1:nn, 1:nn)
eps(1:nn)=0.
!
! Assign mean and Covariance for beta
!
beta(1:nf)=0.
Cbeta(1:nf, 1:nf)=0.
beta(1:4)= (/ 52.399839511692335, 1.0935428391808943E-004, -3.1615155809229245E-003, 1.2116283255694471E-002  /)
Cbeta(1, 1:4)= (/270.60028747602928, 1.5366756312328578E-003, 7.9881962338312195E-003, -6.0292972962538195E-002 /)
Cbeta(2, 1:4)= (/1.5366756312270658E-003, 1.7247639913097469E-005, 1.3324610875859451E-006, -2.3045855632454535E-007 /)
Cbeta(3, 1:4)= (/7.9881962338509589E-003, 1.3324610875862412E-006, 5.5464689237415081E-006, -2.4207894889311469E-006 /)
Cbeta(4, 1:4)= (/-6.0292972962540783E-002, -2.3045855632585232E-007, -2.4207894889268253E-006, 1.3517389449680424E-005 /)

deallocate( Dh, Dz, gamma, stat=error )

return
end subroutine

end module
