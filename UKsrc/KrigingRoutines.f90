!--------------------------------------------------------------------------------------------------
!> @page page1 Kriging Routines
!>
!> @section m1p1 Implementation of Interpolation
!>
!> @subsection m1p1p1 Interpolation Algorithm
!> The interpolation of geospatial data is carried out via an Universal Kriging (UK) algorithm. 
!>
!> @subsubsection m1p1p1p1 Universal Kriging
!> Universal kriging (UK) is a generalization of ordinary kriging in which a set of spatial functions are used to model the
!> trend of a set of point observations.  The underlying model is:
!> @f[
!> f(x,y,H(x,y),\lambda)=\sum_{k=1}^{n_f} f_k(x,y,H(x,y),\lambda_k) +\epsilon(x,y) 
!> @f] 
!> where @f$f_k@f$ are the known spatial functions and @f$\epsilon(x,y)@f$ is a zero mean, spatially correlated,  stationary random 
!> process with semi-variogram @f$\gamma(s)@f$. For a summary of UK see @cite Cressie 1993, pages 151 -180.\n
!> The spatially variable @f$x@f$ here is taken to include latitude, longitude and, bathymetric depth(@f$x=[lat,lon,z(lat,lon)]@f$). 
!> 
!> @subsubsection m1p1p1p2 Spatial functions
!> The spatial functions (SF) used here are  a set of one dimensional, bounded, C-infinity functions with two parameters, 
!> 
!> Gaussian Bump:
!> @f[
!> f_a (s,\lambda,x_0) = \exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
!> @f]
!> Logistic curve:
!> @f[
!> f_b (s,\lambda,x_0) = \frac{1}{1+\exp( -\frac{s-x_0}{\lambda} ) }
!> @f]
!> Sin Exp curve:
!> @f[
!> f_c (s,\lambda,x_0) = \sin(\frac{s-x_0}{\lambda})\exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
!> @f]
!> Cos Exp curve:
!> @f[
!> f_c (s,\lambda,x_0) = \cos(\frac{s-x_0}{\lambda})\exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
!> @f]
!>
!> In all of the function form @f$\lambda@f$ controls the width of the transition and @f$x_0@f$ the transition point. 
!> 
!> After fitting these to the bathymetric variable (H) we can introduce interaction. Allowing interaction terms for the 
!> spatial functions depending on bathymetry only we can define, @f$g_j(x,H,\lambda^j,{x_0}^j,\lambda_k,{x_0}^k)=f_j(x)f_k ( H )@f$
!> @f[
!> f(x,y,H)=\sum_i f_i(H,\lambda^i,z_0^i) + \sum_j f_{j_x}(x,\lambda^{j_x},x_0^{j_x}) f_k(z,\lambda^k,x_0^k)+ \sum_j f_{j_y}(y,\lambda^{j_y},x_0^{j_y}) f_k(z,\lambda^k,x_0^k)
!> @f]
!>
!> Here @f$z@f$ is bathymetric depth. We start by fitting nonlinear parameters @f$\lambda^{c,s}@f$ and @f$x_0^{c,s}@f$ to log 
!> recruitment for "cross shelf" structure.  
!> @f[
!> f(x,y,z)=\beta_0+\sum_i \beta_i f_i(z) + \sum_j \beta_j g_j(x,z)+\sum_k \beta_k g_k(y,z)+ \epsilon
!> @f]
!> where @f$\beta_i@f$ are coefficients for the spatial functions and @f$\epsilon@f$ is the zero mean noise process associated with UK.
!> 
!> @subsubsection m1p1p1p3 Fitting non-linear parameters
!> A brute force approach is taken to fitting the nonlinear parameters @f$x_0@f$ and @f$\lambda@f$.  A search range is 
!> determined based on the geographic range of the observations.  The parameters are then fit to minimize the misfit to 
!> observations. 
!>
!> Subroutine @a NLSF_Fit_Function parameter np).  The nonlinear parameters are fit by minimizing RMS misfit to the simple least 
!> squares fit with a smoothness penalty,
!> @f[
!> J(x_0,\lambda)=\sqrt{ \frac{1}{n} \sum_i (d_i-a - b f(x_i|\lambda,x_0))^2 }+S(\lambda,x_0)
!> @f]
!> Where @f$S(\lambda,x_0)=\int_{-\infty}^\infty f''(x) ^2 d x= S(\lambda)@f$ is a roughness penalty, @f$a@f$ and @f$b@f$ are 
!> temporarily assigned (by least squares) constants fit to minimize @f$J@f$.  @f$S@f$ is proportional to @f$\lambda^{-3}@f$ 
!> for all examples used here (see subroutine @a NLSF_Smooth_Penalty).  Other one dimensional function forms can be added to the 
!> software in subroutine @a NLSF_Eval_Semivariance and @a NLSFFuncPen.
!> 
!> A smoothness penalty is imposed for each function based on the analytic 
!> 
!> @subsection m1p1p2 Residual process
!> After performing an ordinary least squares fit for the SF coeficients, @f$\beta@f$, we have an estimate of @f$\epsilon@f$. 
!> An empirical variogram is computed subroutine @a Krig_Comp_Emp_Variogram, and variogram parameters are fit (again by brute force).  
!> The variogram forms allowed are "spherical", "exponential", and "gaussian".  The form is defaults to 'spherical' if not specified 
!> by the UK configuration file.
!>
!> @subsubsection m1p1p2p1 Posterior sampling
!> With the fitting of the residual we have a covariance for @f$\epsilon@f$ and the estimation problem becomes one of 
!> Generalized Least Squares (LSF_Generalized_Least_Squares).  Posterior sampling is then conducted  achieved posterior sampling is Treating the TBD
!>
!> @section m1p2 Non Linear Spatial function fitting for UK
!> The universal kriging algorithm described above is used to build a distribution based on the historical recruitment data 
!> (1979-present).  Spatial function forms of one variable were selected for smoothness and boundedness. We have:\n
!> Gaussian bump
!> 
!> The nonlinear parameters are fit by minimizing RMS misfit to the simple least squares fit. 
!> @f[
!> J(x_0,\lambda)=\sqrt{ \frac{1}{n} \sum_i (d_i-a - b f(x_i|\lambda,x_0))^2 }+S(\lambda,x_0)
!> @f]
!> Where @f$S(\lambda,x_0)=\int_{-\infty}^\infty f''(x) ^2 d x@f$ is a roughness penalty, @f$a@f$ and @f$b@f$ are temporarily 
!> assigned constants fit to minimize @f$J@f$.  @f$S@f$ is proportional to @f$\lambda^{-3}@f$ for all examples used here.  
!> Other one dimensional function forms can be added to the software in subroutine @a NLSF_Eval_Semivariance and @a NLSFFuncPen.
!> 
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS)
!--------------------------------------------------------------------------------------------------
module Krig_Mod

use globals
use Grid_Manager_Mod
use NLSF_Mod
use Random_Field_Mod
implicit none

type Krig_Class
    real(dp) alpha
    real(dp) nugget
    real(dp) sill
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
function Krig_Compute_Variogram(num_points,num_cols,distance_horiz,n_dim,par)
type(Krig_Class), intent(in) :: par
integer, intent(in) :: num_points,num_cols,n_dim
real(dp), intent(in) :: distance_horiz(n_dim,*)
real(dp) :: Krig_Compute_Variogram(n_dim,num_cols)
integer j,k,error
real(dp) sill,nugget,alpha
real(dp) kap
character(form_len) form
real(dp), allocatable:: D(:,:)

allocate( D(1:num_points,1:num_cols) ) 

nugget = par%nugget
sill = par%sill
alpha = par%alpha
form=par%form
D(1:num_points,1:num_cols) = distance_horiz(1:num_points,1:num_cols)
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
subroutine Krig_Comp_Emp_Variogram(num_points,distance_horiz,n_dim,f,par)
type(Krig_Class), intent(inout) :: par
integer, intent(in)  :: num_points,n_dim
real(dp), intent(in) :: distance_horiz(n_dim,*),f(*)
type(Krig_Class):: parTmp

integer NIntVD,NPS
real(dp) variogram(n_dim,1)
real(dp), allocatable :: DintV(:), DintVm(:), SIntV(:)
real(dp), allocatable :: Pind(:),alphaR(:),c0R(:),cR(:)

integer j,k,n,m,nc,NIntV
real(dp) dx,cost,costmin
character(fname_len) fname

NPS = min( num_points-2, 30)
NIntVD = NPS + 1

allocate(DintV(NIntVD),DintVm(NIntVD), SIntV(NIntVD))
allocate(Pind(NPS),alphaR(NPS),c0R(NPS),cR(NPS))

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
do j=2,NIntV
    SIntV(j-1)=0.
    nc=0
    do n=1,num_points
        do m=1,num_points
            if ((distance_horiz(n,m) .ge. DintV(j-1)) &
            &   .and. (distance_horiz(n,m) .le. DintV(j))) then
                SIntV(j-1)=SIntV(j-1)+(f(n)-f(m))**2
                nc=nc+1
            endif
        enddo
    enddo
    if (nc.gt.0)then
        SIntV(j-1) = SIntV(j-1) / (1.D0*nc)
    else
        SIntV(j-1) = 0._dp
    endif
enddo
NIntV = NIntV - 1
!set upper bounds for kriging parameter search
cR(1:NPS) = Pind(1:NPS) * maxval(SIntV(1:NIntV))
if(minval(SIntV(1:NIntV)).gt.0.)then
    c0R(1:NPS) = Pind(1:NPS) * minval(SIntV(1:NIntV))
else
    c0R(1:NPS) = Pind(1:NPS) * cR(1)
endif

do j=1,NPS
    do k=1,NPS
        do m=1,NPS
            parTmp%nugget=c0R(j)
            parTmp%sill=cR(k)
            parTmp%alpha=alphaR(m)
            variogram = Krig_Compute_Variogram(NintV, 1, DintVm, n_dim, parTmp)
            cost = sum((SIntV(1:NIntV) - variogram(1:NIntV,1))**2)
            if( cost.lt.costmin )then
                costmin=cost
                par%nugget=c0R(j)
                par%sill=cR(k)
                par%alpha=alphaR(m)
            endif
        enddo !m
    enddo !k
enddo !j

variogram = Krig_Compute_Variogram(NintV, 1, DIntVm, n_dim, par)

fname = GridMgr_Get_Obs_Data_File_Name()
! Change output directory and file prefix
! /X_Y_...
! n12345
n = index(fname, '/') + 5
m = index(fname, '.') - 1
fname = fname(n:m)

call Write_2D_Scalar_Field(NintV,1,variogram, output_dir//trim(fname)//'_GammaIntV.txt',NintV)
call Write_2D_Scalar_Field(NintV,1,DIntVm, output_dir//trim(fname)//'_DIntV.txt',NintV)
call Write_2D_Scalar_Field(NintV,1,SIntV, output_dir//trim(fname)//'_SIntV.txt',NintV)

write(*,'(A,3F15.6,A)') 'Bounds alpha: [', alphaR(1), par%alpha, alphaR(NPS),']'
write(*,'(A,3F15.6,A)') 'Bounds nugget:[', c0R(1), par%nugget, c0R(NPS),']'
write(*,'(A,3F15.6,A)') 'Bounds sill:  [', cR(1), par%sill, cR(NPS),']'

deallocate(DintV, DintVm, SIntV)
deallocate(Pind, alphaR, c0R, cR)

endsubroutine Krig_Comp_Emp_Variogram

!--------------------------------------------------------------------------------------------------
!! @public @memberof Krig_Class
!> Purpose: Computes value of spatial functions at x,y.
!> 
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
function Krig_Eval_Spatial_Function(p, num_spat_fcns, n_dim, nlsf, save_data)
type(Grid_Data_Class), intent(in) :: p
type(NLSF_Class), intent(in):: nlsf(*)
integer, intent(in) :: n_dim
integer, intent(in) :: num_spat_fcns
real(dp) :: Krig_Eval_Spatial_Function(n_dim,num_spat_fcns)
logical, intent(in) :: save_data
real(dp) s(n_dim), fpc(n_dim)
integer num_points,j,k
integer nsf

nsf = NLSF_Get_NSF()

num_points=p%num_points
Krig_Eval_Spatial_Function(1:num_points,1) = 1._dp
do j=1,nsf
    s(:) = NLSF_Eval_Semivariance(p, nlsf(j))
    k = nlsf(j)%precon
    if (k .eq. 0) then
        Krig_Eval_Spatial_Function(1:num_points,j+1) = s(1:num_points)
    else
        fpc(:) = NLSF_Eval_Semivariance(p, nlsf(k))
        Krig_Eval_Spatial_Function(1:num_points,j+1) = s(1:num_points) * fpc(1:num_points)
    endif
enddo

if (save_data) call Write_CSV(num_points,num_spat_fcns,Krig_Eval_Spatial_Function,'SpatialFunctions.csv',num_points,.false.)
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
subroutine Krig_Generalized_Least_Sq(grid, obs, num_spat_fcns, par, beta, Cbeta, eps, CepsG, nlsf, save_data)
type(Grid_Data_Class):: grid
type(Grid_Data_Class):: obs
type(Krig_Class):: par
type(NLSF_Class):: nlsf(*)

integer,  intent(in):: num_spat_fcns
real(dp), intent(out):: beta(*), Cbeta(num_spat_fcns, num_spat_fcns), eps(*), CepsG(grid%num_points, grid%num_points)
logical, intent(in) :: save_data

real(dp), allocatable:: distance_horiz(:,:), distance_vert(:,:), W(:,:), gamma(:,:), Fs(:,:), FsT(:,:)
real(dp), allocatable:: D0h(:,:), D0z(:,:), gamma0(:,:), Fs0(:,:), Fs0T(:,:)
real(dp), allocatable:: R(:,:), V(:,:)
real(dp), allocatable:: CbetaInv(:,:), Ceps(:,:), Cinv(:,:)
real(dp), allocatable:: DGh(:,:), DGz(:,:), C0(:,:), gammaG(:,:)
real(dp), allocatable:: Mtmp(:,:), Vtmp(:), Vtmp2(:), Gtmp(:,:), ftrnd(:)
integer,  allocatable:: ipiv(:)

real(dp)     Vinf(1, 1), Dinf(1, 1), atmp, btmp
integer     j, k, info, nopnf, error, num_points, num_obs_points
integer n, p

! !=======================================================================================
! character(fname_len) fname, feps, fdata, ftrend
! character(80) fmtstr
! !=======================================================================================

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
Gamma = Krig_Compute_Variogram(num_obs_points, num_obs_points, distance_horiz, num_obs_points, par)
Fs = Krig_Eval_Spatial_Function(obs, num_spat_fcns, num_obs_points, nlsf, save_data)

!------------------------------------------------------------------------------
! Ceps <- covariance between observation points (from variogram)
!------------------------------------------------------------------------------

!
! Compute variogram between observation points and grid points and spatial
! functions at grid points
!
call Krig_Compute_Distance(obs, grid, D0h, D0z, num_obs_points)
gamma0 = Krig_Compute_Variogram(num_obs_points, num_points, D0h, num_obs_points, par)
!------------------------------------------------------------------------------
!Compute the Univeral Kriging linear estimate of f following Cressie 1993
!pages 151-154
!------------------------------------------------------------------------------
Fs0 = Krig_Eval_Spatial_Function(grid, num_spat_fcns, num_points, nlsf, save_data)
Fs0T = transpose(Fs0)
FsT  = transpose(Fs)
! R = 
! gamma (s_i - s_j ),  i = 1, ... , N;       j = 1,   ... , N
! f_j-1-N(s_i ),       i = 1, ... , N;       j = N+1, ... , N+p+1
! 0,                   i = N+1, ... , N+p+1; j = N+1, ... , N+p+1
n = num_obs_points
p = num_spat_fcns  ! = nsf+1
R(1:n,     1:n)     = gamma(1:n, 1:n)
R(1:n,     n+1:n+p) = Fs(1:n, 1:p)
R(n+1:n+p, 1:n)     = FsT(1:p, 1:n)
R(n+1:n+p, n+1:n+p)= 0._dp

V(1:n,     1:num_points) = gamma0(1:n, 1:num_points)
V(n+1:n+p, 1:num_points) = Fs0T(1:p,   1:num_points)

!------------------------------------------------------------------------------
! Solves the equation Rx = V for x
! m = n+p
!    V(1:m, 1:num_points) = x(1:m, 1:num_points)
!------------------------------------------------------------------------------
call dgesv(nopnf, num_points, R, nopnf, IPIV, V, nopnf, info)
if (info .NE. 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (a) dgesv solution could not be computed: info=', info , term_blk
    stop 1
endif

!------------------------------------------------------------------------------
! compute best linear estimate of the field on the grid points x, y
! f = W f_obs
!------------------------------------------------------------------------------
do j=1, num_points
    do k=1, num_obs_points
        W(j,k) = V(k,j)
    enddo
enddo
atmp = 1._dp
btmp = 0._dp
! Computes grid%field(1:num_points) = matmul(W(1:num_points, 1:num_obs_points), obs%field(1:num_obs_points))
call dgemv('N', num_points, num_obs_points, atmp, W, num_points, obs%field, 1, btmp, grid%field, 1)

!------------------------------------------------------------------------------
! compute posterior trend statistics
!------------------------------------------------------------------------------
Dinf(1, 1)=10._dp**10
Vinf = Krig_Compute_Variogram(1, 1, Dinf, 1, par)
!------------------------------------------------------------------------------
! Ceps <- covariance between observation points (from variogram)
!------------------------------------------------------------------------------
Ceps(1:num_obs_points, 1:num_obs_points) = Vinf(1, 1) - gamma(1:num_obs_points, 1:num_obs_points)
!------------------------------------------------------------------------------
! Cinv = inv( Ceps )
!------------------------------------------------------------------------------
Cinv(1:num_obs_points, 1:num_obs_points) = 0._dp
do j=1, num_obs_points
    Cinv(j, j)=1._dp
enddo
!------------------------------------------------------------------------------
! Solves the equation Ceps * x = Cinv for x
!    Cinv(1:n, 1:n) = x(1:n, 1:n)
!------------------------------------------------------------------------------
call dgesv(num_obs_points, num_obs_points, Ceps, num_obs_points, IPIV, Cinv, num_obs_points, info)
if (info .NE. 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (b) dgesv solution could not be computed. info=', info, term_blk
    stop 1
endif

!------------------------------------------------------------------------------
! cov(beta_gls) = inv (F' Cinv F )
!------------------------------------------------------------------------------
! dgemm( transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
! 'N', 'N', computes   C = alpha * A(m, k) * B(k , n)   + beta*C(m, n), where we set alpha = 1.0 and beta = 0.0 
! Mtmp(num_obs_points, num_spat_fcns) = Cinv(num_obs_points, num_obs_points) * Fs(num_obs_points, num_spat_fcns) 
call dgemm('N', 'N', num_obs_points, num_spat_fcns, num_obs_points, atmp, Cinv, num_obs_points, Fs, num_obs_points, &
&          btmp, Mtmp, num_obs_points )

! 'T', 'N', computes   C = alpha * A**T(m, k) * B(k , n)   + beta*C(m, n), where we set alpha = 1.0 and beta = 0.0
! CbetaInv(num_spat_fcns, num_spat_fcns) = Fs**T(num_spat_fcns, num_obs_points) * Mtmp(num_obs_points, num_spat_fcns)
call dgemm('T', 'N', num_spat_fcns, num_spat_fcns, num_obs_points, atmp,  Fs, num_obs_points, Mtmp , num_obs_points, &
&          btmp, CbetaInv, num_spat_fcns )

CBeta(1:num_spat_fcns, 1:num_spat_fcns) = 0._dp
do j=1, num_spat_fcns
    CBeta(j, j)=1._dp
enddo
!------------------------------------------------------------------------------
! Solves the equation CBetaInv * x = CBeta for x
!    CBeta(1:n, 1:n) = x(1:n, 1:n)
!------------------------------------------------------------------------------
call dgesv(num_spat_fcns, num_spat_fcns, CBetaInv, num_spat_fcns, IPIV, CBeta, num_spat_fcns, info)
if (info .NE. 0) then
    write(*,*) term_red, 'Krig_Generalized_Least_Sq (c) dgesv solution could not be computed. info=', info, term_blk
    stop 1
endif

!------------------------------------------------------------------------------
! beta_gls = inv( F' * Cinv * F ) * F * Cinv * fo
!------------------------------------------------------------------------------
Vtmp(1:num_obs_points) = matmul(Cinv(1:num_obs_points, 1:num_obs_points), obs%field(1:num_obs_points))
Vtmp2(1:num_spat_fcns) = matmul(transpose(Fs(1:num_obs_points, 1:num_spat_fcns)), Vtmp(1:num_obs_points))
beta(1:num_spat_fcns)  = matmul(Cbeta(1:num_spat_fcns, 1:num_spat_fcns), Vtmp2(1:num_spat_fcns))

!------------------------------------------------------------------------------
! Posterior covariance for residual
!------------------------------------------------------------------------------
call Krig_Compute_Distance(grid, grid, DGh, DGz, num_points)
gammaG = Krig_Compute_Variogram(num_points, num_points, DGh, num_points, par)
CepsG(1:num_points, 1:num_points)  = Vinf(1, 1) - gammaG(1:num_points, 1:num_points)
C0(1:num_obs_points, 1:num_points) = Vinf(1, 1) - gamma0(1:num_obs_points, 1:num_points)
!------------------------------------------------------------------------------
! CepsG <== CepsPostG = CepsG - C0v' * inv(C_obs) * C0v
!------------------------------------------------------------------------------
! dgemm( transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
! 'N', 'N', computes   C = alpha * A(m, k) * B(k , n)   + beta*C(m, n), where we set alpha = 1.0 and beta = 0.0 
! Gtmp(num_obs_points, num_points) = Cinv(num_obs_points, num_obs_points) * C0(num_obs_points, num_points) 
call dgemm('N', 'N', num_obs_points, num_points, num_obs_points, atmp, Cinv, num_obs_points, C0, num_obs_points, &
&           btmp, Gtmp, num_obs_points )
! 'T', 'N', computes   C = alpha * A**T(m, k) * B(k , n)   + beta*C(m, n), where we set alpha = 1.0 and beta = 0.0
! gammaG(num_points, num_points) = C0**T(num_points, num_obs_points) * Gtmp(num_obs_points, num_points)
call dgemm('T', 'N', num_points, num_points, num_obs_points, atmp, C0, num_obs_points, Gtmp, num_obs_points, &
&           btmp, gammaG, num_points )
! note: gamma variogram<== C0' * Cinv * C0 no longer represents variogram for the grid!

CepsG(1:num_points, 1:num_points) = CepsG(1:num_points, 1:num_points) - gammaG(1:num_points, 1:num_points)
!------------------------------------------------------------------------------
! Compute posterior mean of epsilon.  This is the difference between unbiased estimate of f and
! the mean of the trend.  Thus the mean of the total distribution is the kriging estimate. 
!
! Computes ftrnd(1:num_points) = matmul( Fs0(1:num_points, 1:num_spat_fcns), beta(1:num_spat_fcns))
!------------------------------------------------------------------------------
call dgemv('N', num_points, num_spat_fcns, atmp, Fs0, num_points, beta, 1, btmp, ftrnd, 1)
eps(1:num_points) = grid%field(1:num_points) - ftrnd(1:num_points)

! !=======================================================================================
! fname = GridMgr_Get_Obs_Data_File_Name()
! ! Change output directory and file prefix
! ! /X_Y_...
! ! n12345
! j = index(fname, '/') + 5
! feps = 'Temp/Lat_Lon_Grid_Eps-'//fname(j:)
! fdata = 'Temp/Lat_Lon_Grid_'//fname(j:)
! ftrend = 'Temp/Lat_Lon_Grid_Trend-'//fname(j:)
! fmtstr='(2(ES14.7 : ", ") (ES14.7 : ))'

! open(63,file=trim(feps))
! open(64,file=trim(fdata))
! open(65,file=trim(ftrend))

! do j=1, num_points
!     write(63, fmtstr) grid%lat(j), grid%lon(j), exp(eps(j))
!     write(64, fmtstr) grid%lat(j), grid%lon(j), exp(grid%field(j))
!     write(65, fmtstr) grid%lat(j), grid%lon(j), exp(ftrnd(j))
! enddo
! close(65)
! close(64)
! close(63)
! !=======================================================================================

deallocate( distance_horiz, distance_vert, W, gamma, Fs, FsT, stat=error )
deallocate( D0h, D0z, gamma0, Fs0, Fs0T , stat=error)
deallocate( R, V , stat=error)
deallocate( CbetaInv, Ceps, Cinv, stat=error )
deallocate( Mtmp, Vtmp, Vtmp2, Gtmp, ftrnd, stat=error )
deallocate( ipiv, stat=error)
deallocate( C0, DGh, DGz,  gammaG)!, ftrnd, stat=error )

end subroutine

end module
