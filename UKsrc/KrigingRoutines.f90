!--------------------------------------------------------------------------------------------------
!> @page page1 Kriging Subroutines
!>
!> Define Kriging parameter structure with length scale alpha, 
!> sill value sill and nugget value nugget. Kringing form is 'spherical',
!> 'exponential', 'gaussian' or 'matern'
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS)
!--------------------------------------------------------------------------------------------------
module Krig_Mod

use globals
use GridManagerMod
use NLSF_Mod
use RandomFieldMod
implicit none

type Krig_Class
    real(dp) alpha
    real(dp) nugget
    real(dp) sill
    real(dp) Wz
    character(form_len) form
end type Krig_Class
   
CONTAINS

!--------------------------------------------------------------------------------------------------
!! @public @memberof Krig_Class
!>
!> Purpose: Computes distance between two vectors of points
!> Inputs:
!>    p: vector 1
!>    q: vector 2
!>   n_dim: allocated length of vectors
!> Outputs
!>   distance_horiz: distance in x-y coordinates
!>   distance_vert: distance in depth
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Krig_Compute_Distance(p, q, distance_horiz, distance_vert,n_dim)
    type(Grid_Data_Class), intent(in) :: p
    type(Grid_Data_Class), intent(in) :: q
    integer, intent(in)  :: n_dim
    real(dp), intent(out) :: distance_horiz(n_dim,*), distance_vert(n_dim,*)
    integer    j

    do j=1,p%num_points
        distance_horiz(j,1:q%num_points) = sqrt( (p%x(j) - q%x(1:q%num_points) )**2 + ( p%y(j)-q%y(1:q%num_points) )**2 )
        distance_vert(j,1:q%num_points) = sqrt( (p%z(j) - q%z(1:q%num_points) )**2 )
    enddo
end subroutine

!--------------------------------------------------------------------------------------------------
!! @public @memberof Krig_Class
!>
!> Purpose: Computes a variogram (variogram) given distances between points (D).
!> 
!> @param[in] num_points    (integer) number of rows in D, Gamma
!> @param[in] num_cols    (integer) number of columns in D, Gamma
!> @param[in] D     (real(dp))  Size (num_points x num_cols) matrix distance between point pairs
!>
!> @param[out] variogram     (real(dp))  Size (num_points x num_cols) variogram matrix for point pairs represented in D
!>
!> Internal:\n
!>  - sill     (real(dp))  Variogram parameter sill+nugget = shelf\n
!>  - nugget    (real(dp))  Variogram parameter nugget\n
!>  - alpha (real(dp))  Variogram length scale parameter\n
!>  - form  (charecter) functional form of variogram\n
!> The above should be read in an input file but are written as constants for now
!>    
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
function Krig_Compute_Variogram(num_points,num_cols,distance_horiz,distance_vert,n_dim,par)
type(Krig_Class), intent(in) :: par
integer, intent(in) :: num_points,num_cols,n_dim
real(dp), intent(in) :: distance_horiz(n_dim,*),distance_vert(n_dim,*)
real(dp) :: Krig_Compute_Variogram(n_dim,num_cols)
integer j,k,error
real(dp) sill,nugget,alpha,Wz
real(dp) kap
character(form_len) form
real(dp), allocatable:: D(:,:)

allocate( D(1:num_points,1:num_cols) ) 

nugget = par%nugget
sill = par%sill
alpha = par%alpha
Wz = par%Wz
form=par%form
D(1:num_points,1:num_cols) = sqrt( distance_horiz(1:num_points,1:num_cols)**2 &
&                                + (Wz * distance_vert(1:num_points,1:num_cols))**2)
if (trim(form).eq.'spherical')then
    do j=1,num_points
        do k=1,num_cols
            Krig_Compute_Variogram(j,k)=nugget + sill * (1.5_dp * D(j,k) / alpha - ((D(j,k) / alpha)**3) / 2._dp)
            if(D(j,k).gt.alpha) Krig_Compute_Variogram(j,k) = nugget + sill
            if(D(j,k).eq.0.) Krig_Compute_Variogram(j,k) = 0._dp
        enddo
    enddo
endif

if (trim(form).eq.'exponential')then
    do j=1,num_points
        do k=1,num_cols
            Krig_Compute_Variogram(j,k)=nugget + sill * (1._dp - exp(-D(j,k) / alpha)  )
            if(D(j,k).eq.0.) Krig_Compute_Variogram(j,k) = 0._dp
        enddo
    enddo
endif

if (trim(form).eq.'gaussian')then
    do j=1,num_points
        do k=1,num_cols
            Krig_Compute_Variogram(j,k)=nugget + sill * (1._dp - exp(-D(j,k)/alpha)**2  )
            if(D(j,k).eq.0.) Krig_Compute_Variogram(j,k) = 0._dp
        enddo
    enddo
endif
!BESSEL_JN(N, X)
if (trim(form).eq.'matern')then
    kap=.5
    do j=1,num_points
        do k=1,num_cols
            Krig_Compute_Variogram(j,k) = nugget + &
            & sill * (1._dp - (1._dp / (gamma(kap) * 2.**(kap - 1._dp))) * bessel_jn(2, D(j,k) / alpha) * (D(j,k)/alpha)**kap);
           if(D(j,k).eq.0.) Krig_Compute_Variogram(j,k)=0.
        enddo
    enddo
endif

deallocate( D,stat=error) 
endfunction Krig_Compute_Variogram

!--------------------------------------------------------------------------------------------------
!! @public @memberof Krig_Class
!> Purpose: Computes a variogram (variogram) given distances between points (D).
!>
!> @param[in]  num_points    (integer) number of rows in D, Gamma
!> @param[in]  n_dim    (integer) allocated number of rows D, Gamma
!> @param[in]  distance_horiz, distance_vert     (real(dp))  Size (num_points x num_cols) matrix distance between point pairs
!> 
!> @param[inout] par
!> 
!> @author Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Krig_Comp_Emp_Variogram(num_points,distance_horiz,distance_vert,n_dim,f,par, proc_recruits)
type(Krig_Class), intent(inout) :: par
type(Krig_Class):: parTmp

integer, intent(in)  :: num_points,n_dim
real(dp), intent(in) :: distance_horiz(n_dim,*),distance_vert(n_dim,*),f(*)
logical, intent(in) :: proc_recruits
real(dp) DintV(n_dim+1),DintVm(n_dim+1), SIntV(n_dim+1)
real(dp) variogram(n_dim,1)
real(dp) Pind(n_dim),alphaR(n_dim),c0R(n_dim),cR(n_dim),WzR
integer j,k,n,m,nc,NIntV, NPS
real(dp) dx,cost,costmin,Wz

NPS=n_dim
NintV = n_dim+1

parTmp%form=par%form
! Set up interval and average square of residual for variogram estimation
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
Wz=0.D0
do j=2,NIntV
    SIntV(j-1)=0.
    nc=0
    do n=1,num_points
        do m=1,num_points
            if ((distance_horiz(n,m) + Wz * distance_vert(n,m) .ge. DintV(j-1)) &
            &   .and. (distance_horiz(n,m) + Wz*distance_vert(n,m) .le. DintV(j))) then
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
            parTmp%nugget=c0R(j)
            parTmp%sill=cR(k)
            parTmp%alpha=alphaR(m)
            parTmp%Wz=WzR
            variogram = Krig_Compute_Variogram(NintV, 1, DintVm, 0*DintVm, n_dim, parTmp)
            cost = sum((SIntV(1:NIntV) - variogram(1:NIntV,1))**2)
            if( cost.lt.costmin )then
                costmin=cost
                par%nugget=c0R(j)
                par%sill=cR(k)
                par%alpha=alphaR(m)
                par%Wz=WzR
            endif
        enddo !m
    enddo !k
enddo !j

variogram = Krig_Compute_Variogram(NintV, 1, DIntVm, 0*DIntV, n_dim, par)

if (proc_recruits) then
    call Write_2D_Scalar_Field(NintV,1,variogram,'GammaIntV.txt',NintV)
    call Write_2D_Scalar_Field(NintV,1,DIntVm,'DIntV.txt',NintV)
    call Write_2D_Scalar_Field(NintV,1,SIntV,'SIntV.txt',NintV)
endif

write(*,*)'Bounds alpha:[', alphaR(1),par%alpha,alphaR(NPS),']'
write(*,*)'Bounds nugget:[', c0R(1),par%nugget,c0R(NPS),']'
write(*,*)'Bounds sill:[',cR(1),par%sill,cR(NPS),']'
endsubroutine Krig_Comp_Emp_Variogram

!--------------------------------------------------------------------------------------------------
!! @public @memberof Krig_Class
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
function Krig_Eval_Spatial_Function(p, num_spat_fcns, n_dim, nlsf, proc_recruits)
type(Grid_Data_Class), intent(in) :: p
type(NLSF_Class), intent(in):: nlsf(*)
integer, intent(in) :: n_dim
integer, intent(in) :: num_spat_fcns
real(dp) :: Krig_Eval_Spatial_Function(n_dim,num_spat_fcns)
logical, intent(in) :: proc_recruits
real(dp) s(n_dim), fpc(n_dim)
integer num_points,j,k
integer nsf

! num_spat_fcns is set to allow for N+1
nsf = num_spat_fcns - 1

num_points=p%num_points
Krig_Eval_Spatial_Function(1:num_points,1)=1.
do j=1,nsf
    s(:) = NLSF_Evaluate_Fcn(p, nlsf(j))
    if (nlsf(j)%pre_cond_fcn_num .eq. 0) then
        Krig_Eval_Spatial_Function(1:num_points,j+1) = s(1:num_points)
    else
        k = nlsf(j)%pre_cond_fcn_num
        fpc(:) = NLSF_Evaluate_Fcn(p, nlsf(k))
        Krig_Eval_Spatial_Function(1:num_points,j+1) = s(1:num_points) * fpc(1:num_points)
    endif
enddo

if (proc_recruits) call Write_CSV(num_points,num_spat_fcns,Krig_Eval_Spatial_Function,'SpatialFunctions.csv',num_points,.false.)
end function Krig_Eval_Spatial_Function

!--------------------------------------------------------------------------------------------------
!! @public @memberof Krig_Class
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
subroutine Krig_Generalized_Least_Sq(grid, obs, num_spat_fcns, par, beta, Cbeta, eps, CepsG, nlsf, proc_recruits)
type(Grid_Data_Class):: grid
type(Grid_Data_Class):: obs
type(Krig_Class):: par
type(NLSF_Class):: nlsf(*)

integer,  intent(in):: num_spat_fcns
real(dp), intent(out):: beta(*), Cbeta(num_spat_fcns, num_spat_fcns), eps(*), CepsG(grid%num_points, grid%num_points)
logical, intent(in) :: proc_recruits

real(dp), allocatable:: distance_horiz(:,:), distance_vert(:,:), W(:,:), gamma(:,:), Fs(:,:), FsT(:,:)
real(dp), allocatable:: D0h(:,:), D0z(:,:), gamma0(:,:), Fs0(:,:), Fs0T(:,:)
real(dp), allocatable:: R(:,:), V(:,:)
real(dp), allocatable:: CbetaInv(:,:), Ceps(:,:), Cinv(:,:)
real(dp), allocatable:: DGh(:,:), DGz(:,:), C0(:,:), gammaG(:,:)
real(dp), allocatable:: Mtmp(:,:), Vtmp(:), Vtmp2(:), Gtmp(:,:), ftrnd(:)
integer,  allocatable:: ipiv(:)

real(dp)     Vinf(1, 1), Dinf(1, 1), atmp, btmp
integer     j, k, info, nopnf, error, num_points, num_obs_points
num_points=grid%num_points
num_obs_points=obs%num_points
nopnf=num_obs_points+num_spat_fcns
allocate( distance_horiz(1:num_obs_points, 1:num_obs_points), distance_vert(1:num_obs_points, 1:num_obs_points),  stat=error)
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
call Krig_Compute_Distance(obs, obs, distance_horiz, distance_vert, num_obs_points)
Gamma = Krig_Compute_Variogram(num_obs_points, num_obs_points, distance_horiz, distance_vert, num_obs_points, par)
Fs = Krig_Eval_Spatial_Function(obs, num_spat_fcns, num_obs_points, nlsf, proc_recruits)
!
! Ceps <- covariance between observation points (from variogram)
!

!
! Compute variogram between observation points and grid points and spatial
! functions at grid points
!
call Krig_Compute_Distance(obs, grid, D0h, D0z, num_obs_points)
gamma0 = Krig_Compute_Variogram(num_obs_points, num_points, D0h, D0z, num_obs_points, par)
Fs0 = Krig_Eval_Spatial_Function(grid, num_spat_fcns, num_points, nlsf, proc_recruits)
!
!Compute the Univeral Kriging linear estimate of f following Cressie 1993
!pages 151-154
!
Fs0T=transpose(Fs0)
FsT=transpose(Fs)
R(1:num_obs_points+num_spat_fcns, 1:num_obs_points+num_spat_fcns) = 0._dp
R(1:num_obs_points, 1:num_obs_points) = gamma(1:num_obs_points, 1:num_obs_points)
R(1:num_obs_points, num_obs_points+1:num_obs_points+num_spat_fcns) = Fs(1:num_obs_points, 1:num_spat_fcns)
R(num_obs_points+1:num_obs_points+num_spat_fcns, 1:num_obs_points) = FsT(1:num_spat_fcns, 1:num_obs_points)

V(1:num_obs_points, 1:num_points) = gamma0(1:num_obs_points, 1:num_points)
V(num_obs_points+1:num_obs_points+num_spat_fcns, 1:num_points) = Fs0T(1:num_spat_fcns, 1:num_points)

call dgesv(nopnf, num_points, R, nopnf, IPIV, V, nopnf, info)
write(*,*)'Krig_Generalized_Least_Sq (a) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (a) dgesv solution could not be computed.', term_blk
    stop 1
endif
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
call dgemv('N', num_points, num_obs_points, atmp, W, num_points, obs%field_psqm, 1, btmp, grid%field_psqm, 1)
!
! compute posterior trend statistics
!
Dinf(1, 1)=10.**10
Vinf = Krig_Compute_Variogram(1, 1, Dinf, Dinf, 1, par)
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
write(*,*)'Krig_Generalized_Least_Sq (b) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (b) dgesv solution could not be computed.', term_blk
    stop 1
endif

!
! beta_gls = inv( F' * Cinv * F ) * F * Cinv * fo
!
Vtmp(1:num_obs_points)=matmul(Cinv(1:num_obs_points, 1:num_obs_points), obs%field_psqm(1:num_obs_points))
Vtmp2(1:num_spat_fcns)=matmul(transpose(Fs(1:num_obs_points, 1:num_spat_fcns)), Vtmp(1:num_obs_points))
beta(1:num_spat_fcns)=matmul(Cbeta(1:num_spat_fcns, 1:num_spat_fcns), Vtmp2(1:num_spat_fcns))
! call dgemv('T', num_obs_points, num_spat_fcns, atmp, Fs, num_obs_points, Vtmp, 1, btmp, Vtmp2,1) ! Vtmp2 = F^T C^{-1} y
! write(*,*)'Krig_Generalized_Least_Sq (c) dgesv info=', info
! call dgemv('N', num_spat_fcns, num_spat_fcns, atmp, Cbeta, num_spat_fcns, Vtmp2, 1, btmp, Beta, 1) ! beta = Cbeta F^t C^{-1} y
! write(*,*)'Krig_Generalized_Least_Sq (d) dgesv info=', info

!
! Posterior covariance for residual
!
call Krig_Compute_Distance(grid, grid, DGh, DGz, num_points)
write(*,*)'Krig_Generalized_Least_Sq (e) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (e) dgesv solution could not be computed.', term_blk
    stop 1
endif
gammaG = Krig_Compute_Variogram(num_points, num_points, DGh, DGz, num_points, par)
write(*,*)'Krig_Generalized_Least_Sq (f) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (f) dgesv solution could not be computed.', term_blk
    stop 1
endif
CepsG(1:num_points, 1:num_points)=Vinf(1, 1)-gammaG(1:num_points, 1:num_points)
C0(1:num_obs_points, 1:num_points)=Vinf(1, 1)-gamma0(1:num_obs_points, 1:num_points)
!
! CepsG <== CepsPostG = CepsG - C0v' * inv(C_obs) * C0v
!
write(*,*)'Krig_Generalized_Least_Sq (g) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (g) dgesv solution could not be computed.', term_blk
    stop 1
endif
call dgemm('N', 'N', num_obs_points, num_points, num_obs_points, atmp, Cinv, num_obs_points, C0, num_obs_points, &
&           btmp, Gtmp, num_obs_points )
write(*,*)'Krig_Generalized_Least_Sq (h) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (h) dgesv solution could not be computed.', term_blk
    stop 1
endif
call dgemm('T', 'N', num_points, num_points, num_obs_points, atmp, C0, num_obs_points, Gtmp, num_obs_points, &
&           btmp, gammaG, num_points )
! note: gamma variogram<== C0' * Cinv * C0 no longer represents variogram for the grid!
write(*,*)'Krig_Generalized_Least_Sq (i) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (i) dgesv solution could not be computed.', term_blk
    stop 1
endif

CepsG(1:num_points, 1:num_points) = CepsG(1:num_points, 1:num_points) - gammaG(1:num_points, 1:num_points)
!
! Compute posterior mean of epsilon.  This is the difference between unbiased estimate of f and
! the mean of the trend.  Thus the mean of the total distribution is the kriging estimate. 
!
write(*,*)'Krig_Generalized_Least_Sq (j) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (j) dgesv solution could not be computed.', term_blk
    stop 1
endif
call dgemv('N', num_points, num_spat_fcns, atmp, Fs0, num_points, beta, 1, btmp, ftrnd, 1)
write(*,*)'Krig_Generalized_Least_Sq (k) dgesv info=', info
if (info > 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (k) dgesv solution could not be computed.', term_blk
    stop 1
endif

eps(1:num_points)=grid%field_psqm(1:num_points)-ftrnd(1:num_points)

write(*,*)'Krig_Generalized_Least_Sq end'
deallocate( distance_horiz, distance_vert, W, gamma, Fs, FsT, stat=error )
deallocate( D0h, D0z, gamma0, Fs0, Fs0T , stat=error)
deallocate( R, V , stat=error)
deallocate( CbetaInv, Ceps, Cinv, stat=error )
deallocate( Mtmp, Vtmp, Vtmp2, Gtmp, ftrnd, stat=error )
deallocate( ipiv, stat=error)
deallocate( C0, DGh, DGz,  gammaG, ftrnd, stat=error )

end subroutine

!--------------------------------------------------------------------------------------------------
!! @public @memberof Krig_Class
!> Assign values needed to simulate random fields from a Universal Kriging 
!> model.
!>
!> Inputs: 
!>  - grid   (real(dp)) Data point structure defining interpolation points 
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
subroutine Krig_User_Estimates(grid, num_spat_fcns, par, beta, Cbeta, eps, Ceps)
type(Grid_Data_Class):: grid
type(Krig_Class):: par
integer,  intent(in):: num_spat_fcns
real(dp), intent(out):: beta(*), Cbeta(num_spat_fcns,*), eps(*), Ceps(grid%num_points,*) 

real(dp), allocatable:: distance_horiz(:,:), distance_vert(:,:), gamma(:,:)

real(dp) Vinf(1, 1), Dinf(1, 1)
integer  error, num_points
num_points=grid%num_points
allocate( distance_horiz(1:num_points, 1:num_points), stat=error)
allocate( distance_vert(1:num_points, 1:num_points), stat=error)
allocate( gamma(1:num_points, 1:num_points), stat=error )
!
! Assign mean and Covariance for epsilon
!
call Krig_Compute_Distance(grid, grid, distance_horiz, distance_vert, num_points)
gamma = Krig_Compute_Variogram(num_points, num_points, distance_horiz, distance_vert, num_points, par)
Dinf(1, 1)=10.**10
Vinf = Krig_Compute_Variogram(1, 1, Dinf, Dinf, 1, par)
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

deallocate( distance_horiz, distance_vert, gamma, stat=error )

end subroutine

end module
