!--------------------------------------------------------------------------------------------------
! Purpose: This program is designed to simulate random fields under the assumptions of Universal 
! Kriging (UK).  It is assumed that the underlying spatial field is a linear combination of known 
! spatial functions and a stationary zero mean random field with known variogram parameters.  The 
! program can be configured in two ways:
!
! (1) To simulate random fields conditioned on the assumed UK model and a set of observations.
!     (postrior simulation).
!
! (0) To simulate random fields based on specified mean spatial coefficients, their covariance
!     and known variogram with known parameters (prior simulation).
!
! I still need to flesh the program header out, clean up some variables etc.
!
!
!
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------

! Define a data point with position (x,y), bathymetric depth z(x,y) and scalar field value f(x,y)
module DpointMod              
integer NDim
parameter(nDim=12000)
 type Dpoint
    real*8 x(NDim)
    real*8 y(NDim)
    real*8 z(nDim)
    real*8 f(nDim)
    real*8 lat(NDim)
    real*8 lon(NDim)
    integer n,ne
    integer E(4,nDim)
    integer ManagementRegion(nDim)
    character*2 region(nDim)
 end type Dpoint
end module

! Define Kriging parameter structure with length scale alpha, 
! sill value c and nugget value c0. Kringing form is 'spherical',
! 'exponential', 'gaussian' or 'matern'

module KrigMod              
 type KrigPar
    real*8 alpha
    real*8 c0
    real*8 c
     real*8 Wz
      character*72 form
 end type KrigPar
end module


! Non Linear Spatial Function parameters


module NLSFMod             
 type NLSFPar
    real*8 lambda, x0
    real*8 lambdaMin,lambdaMax
    real*8 x0Min,x0Max
    real*8 xRange(2)
    real*8 rms
    character*12 form
    character*3 d
    integer nsf,PreCFnum,IsTrunkateRange,UseGreedyFit,nsflim

end type NLSFPar
end module

program UKsimulation
!
! Performs Universal Kriging with posterior sampling.  The functions are 
!
use DpointMod
use KrigMod
use NLSFMod
implicit none

real*8     atmp,btmp,muY,muZ,NRandomField
real*8,    allocatable :: beta(:),Cbeta(:,:),eps(:),Ceps(:,:),Cr(:,:),F(:,:),r(:),trndOBS(:),resOBS(:)
real*8,    allocatable :: Dh(:,:),Dz(:,:),gamma(:,:),Veps(:),VSpFn(:),CbetaF(:,:),Fg(:,:)
real*8,    allocatable :: ClimEst(:),ClimEstObs(:),trend(:),Vtotal(:),fRaw(:),logmu(:)
real*8, allocatable :: RandomField(:,:)
integer      nn,nf,no,SimType,j,nits
real*8     tmp(1,1),fmax,A,DomainAverage,SF,EnsMu,EnsSTD,adj
! variables for nonlinear fitting
integer np,nsf,n,nsflim,NRand,k,ncla
logical IsClimEst,IsHiLimit,IsMatchMean,IsLogT
character*72 gridfile, obsfile,climfile,flnm
character*2 DomainName
type(Dpoint):: grid
type(Dpoint):: obs
type(KrigPar):: par
type(NLSFPar), allocatable ::nlsf(:)
real*8 alpha
par%form='spherical'
IsLogT=.true.
IsHiLimit=.true.
IsClimEst=.false.
IsMatchMean=.true.
alpha=1.D0

write (*,*) "PROGRAM STARTING"

call ReadInput(DomainName,obsfile,climfile,Nrand,IsLogT,IsHiLimit,fmax,IsMatchMean,IsClimEst,par,alpha)
!overrude UK.inp with command line arguments if present
ncla=command_argument_count()
if(ncla.ge.1)call get_command_argument(1, DomainName)
if(ncla.ge.2)call get_command_argument(2, obsfile)
if(ncla.ge.3)call get_command_argument(3, climfile)
if(ncla.ge.3)IsClimEst=.true.

write (*,*) "Reading ", DomainName

write(*,*)'observation file:  ',trim(obsfile)
if(IsClimEst) write(*,*)'clim file:  ',trim(climfile)
write(*,*)'Logtransorm',IsLogT
write(*,*)'Match stratified sampling estimate',IsMatchMean
write(*,*)'high limit fmax',fmax
write(*,*)par%form
call random_seed( )
!SimType=0 !priori simulation
SimType=1 !posterior simulation
! Initalize number of spatial functions nf with dummy call to spatial_function
allocate(nlsf(1:1))
call DefineNLSFunctions(nlsf,obs,.true.)
nsf=nlsf(1)%nsf
deallocate(nlsf)
allocate(nlsf(1:nsf))
nlsf(1:nsf)%nsf=nsf
call spatial_function(grid,tmp,nf,1,nlsf)
!-----------------------------------------------------------------------
!
! Initalize grid point coordinates and bathymetry - initialize nn
!
call loadgrid(grid%x,grid%y,grid%z,grid%lat,grid%lon,grid%n,&
              grid%E,grid%ne,grid%ManagementRegion,DomainName)
nn=grid%n
!
! Initalize number of spatial functions nf with dummy call to spatial_function
!
allocate( beta(1:nf), Cbeta(1:nf,1:nf), eps(1:nn), Ceps(1:nn,1:nn),CbetaF(1:nf,1:nn))
allocate( Veps(1:nn),VSpFn(1:nn),Fg(1:nn,1:nf),trend(1:nn),Vtotal(1:nn),logmu(1:nn))
allocate( RandomField(1:nn,1:Nrand))
if (SimType.eq.1) then
!
! Initalize data point coordinates, bathymetry and data - initialize no
!
call loaddata(obs%x,obs%y,obs%z,obs%f,obs%n,trim(obsfile))
no=obs%n
nsf=nlsf(1)%nsf
nlsf(1:nsf)%nsflim=nsf
obs%f(1:no)=obs%f(1:no)**alpha

write(*,*)'no=',no,'nsf limit=',nlsf(1)%nsflim,'nsf=',nlsf(1)%nsf
!-------------------------------------------------------------------------  
! nonlinear curve fitting for spatial functions
!-------------------------------------------------------------------------  
call DefineNLSFunctions(nlsf,grid,.false.)
!nlsf(1:nsf)%nsf=NSpatFunLim
nsf=nlsf(1)%nsf
 
allocate( Cr(1:no,1:no), F(1:no,1:nf), r(1:no),trndOBS(1:no),resOBS(1:no))
allocate( Dh(1:no,1:no),Dz(1:no,1:no), gamma(1:no,1:no) )
!fmax=1.5*maxval(obs%f(1:no))
fmax=fmax*maxval(obs%f(1:no))
call GetDomainAverage(obs,grid,DomainAverage)

 if(IsLogT) then
    A=1.D0/1852.*(.3048*8.) ! 1 scallop per tow 
    SF=sum(obs%f(1:no))/float(no)
    SF=SF/5.D0  ! mean / 5 ~ median
    obs%f(1:no)=log(( A+obs%f(1:no) )/SF)
 endif
 
 if(IsClimEst)then
    allocate(ClimEst(1:nn),ClimEstObs(1:no))
    write(*,*)'ClimFile=',climfile
    call readsf(climfile, ClimEst, j)
    ClimEst(1:nn)=ClimEst(1:nn)**alpha
    call WriteScalarField(nn,ClimEst(1:nn),'ClimEstField.txt')
    write(*,*)'climatology: read nn=',j,'values from ',trim(ClimFile)
    if (IsLogT)ClimEst(1:nn)=log(A+ClimEst(1:nn))
    call InterpFromGrid(grid,ClimEst,obs,ClimEstObs)
    call WriteScalarField(no,ClimEstObs(1:no),'ClimEstObs.txt')
    call WriteScalarField(no,obs%f(1:no),'RawObs.txt')
    obs%f(1:no)=obs%f(1:no)-ClimEstObs(1:no)
    call WriteScalarField(no,obs%f(1:no),'AdjObs.txt')
endif

if (nlsf(1)%UseGreedyFit.eq.1) call FitNLSFunctionsGreedy(obs,nlsf)
if (nlsf(1)%UseGreedyFit.eq.0) call FitNLSFunctions(obs,nlsf,0)
nsf=nlsf(1)%nsf
do j=1,nlsf(1)%nsf
    write(*,*)nlsf(j)%d,' ',nlsf(j)%form,nlsf(j)%x0,nlsf(j)%lambda
enddo

!-------------------------------------------------------------------------  
!
! OLS fit with spatial functions
!
 Cr(1:no,1:no)=0.D0
 do j=1,no
  Cr(j,j)=1.D0
 enddo
 call spatial_function(obs,F,nf,no,nlsf)
!subroutine GLS(y,F,C,n,m,beta,Cbeta,r)
 call GLS(obs%f,F,Cr,no,nf,beta,Cbeta,r)
 write(*,*)'OLSres:',sqrt(sum(r(1:no)**2)/float(no))
!-------------------------------------------------------------------------  
!
! Fit variogram parameters to OLS residual
!
 call WriteScalarField(no,r,'OLSresidual.txt')
 call WriteScalarField(no,obs%f,'data.txt')
 call distance(obs,obs,Dh,Dz,no)
 nits=1
 do j=1,nits
  call variogramF(no,no,Dh,Dz,Gamma,no,r,par)
  open(63,file='KRIGpar.txt')
  write(63,*)par%c, par%c0, par%alpha, par%Wz
  close(63)
!-------------------------------------------------------------------------  
!-----------------------------------------------------------------------
!
! Compute Universal Kriging estimate of field on grid (fest) given
! observations xo,yo,zo,fo. Also returns the estimate of spatial function
! coeficients, beta, and posterior covariance of beta(Cbeta).
!
  call UK_GLS(grid,obs,nf,par,beta,Cbeta,eps,Ceps,nlsf)
  call spatial_function(obs,F,nf,no,nlsf)
  atmp=1.D0
  btmp=0.D0
  call dgemv('N',no,nf,atmp,F,no,beta,1,btmp,trndOBS,1)
  resOBS(1:no)=obs%f(1:no)-trndOBS(1:no)
  write(*,*)'GLSres:',sqrt(sum(resOBS(1:no)**2)/float(no))
enddo
else 
 ! simulate from user supplied prior estimate of beta,Cbeta,eps,Ceps
 call UK_prior(grid,nf,par,beta,Cbeta,eps,Ceps)
endif

call OutputUK(nn,nf,Nrand,grid,nlsf,beta,eps,Ceps,Cbeta,ClimEst,fmax,SF,A,DomainAverage,&
                    IsLogT,IsClimEst,IsMatchMean,IsHiLimit,DomainName,alpha)

call InterpFromGrid(grid,grid%f,obs,r)
r(1:no)=r(1:no)**(1./alpha)
call WriteScalarField(no,r,'KrigAtObs.txt')

deallocate( Dh,Dz, beta, Cbeta, Ceps,r)

stop
end

!---------------------------------------------------------------------------------------------------
!123456789!123456789!123456789!123456789!123456789!123456789!123456789!123456789!123456789!123456789

subroutine OutputUK(nn,nf,Nrand,grid,nlsf,beta,eps,Ceps,Cbeta,ClimEst,fmax,SF,A,DomainAverage,&
                    IsLogT,IsClimEst,IsMatchMean,IsHiLimit,DomainName,alpha)
!---------------------------------------------------------------------------------------------------
! Purpose: This subroutine writes files for output.  This includes a central prediction: 
! "KrigingEstimate.txt" and random fields generated from the posterior distribution:
! "RandomFieldN.txt", where N =1:Nrand. Predictor standard deviation  is output to "KrigSTD.txt".
! Function coefficient 
!---------------------------------------------------------------------------------------------------

use DpointMod
use NLSFMod
implicit none

type(Dpoint),intent(inout) :: grid
type(NLSFPar),intent(in)::nlsf(*)
                                    
character*2 DomainName
integer, intent(in) :: nn,nf,Nrand
real*8, intent(in) :: eps(*),beta(*),Cbeta(nf,*),ClimEst(*),fmax,SF,A,DomainAverage,alpha
real*8, intent(inout) :: Ceps(nn,*)
logical,intent(in) ::IsLogT,IsClimEst,IsMatchMean,IsHiLimit
integer j,k,n
real*8 trend(nn),V(nn),Fg(nn,nf),MuY,EnsMu,EnsSTD,logf(nn),adj,RandomField(nn,Nrand)
character*72 buf
write(*,*)'output fmax,SF,A=',fmax,SF,A
do n=1,nn
 V(n)=Ceps(n,n)
enddo
grid%f(1:nn)=grid%f(1:nn)**(1./alpha)
if(IsClimEst)grid%f(1:nn)=grid%f(1:nn)+ClimEst(1:nn)**(1./alpha)
logf(1:nn)=grid%f(1:nn)
if(IsLogT) grid%f(1:nn)=SF*exp( grid%f(1:nn) + V(1:nn)/2. ) - A  ! adjusted inverse log(A+f)
if(IsHiLimit)call limitz(nn,grid%f,grid%z,fmax,DomainName)
MuY=sum( grid%f(1:nn) )/float(nn)
if(IsMatchMean)grid%f(1:nn)=DomainAverage*grid%f(1:nn)/MuY
if(IsHiLimit)call limitz(nn,grid%f,grid%z,fmax,DomainName)
call WriteScalarField(nn,grid%f,'KrigingEstimate.txt')

call spatial_function(grid,Fg,nf,nn,nlsf)
trend(1:nn)=matmul( Fg(1:nn,1:nf),beta(1:nf)) 
if(IsClimEst)trend(1:nn)=trend(1:nn)+ClimEst(1:nn)
if(IsLogT)trend(1:nn)=SF*exp(trend(1:nn))-A
if(IsMatchMean)trend(1:nn)=DomainAverage*trend(1:nn)/MuY
call WriteScalarField(nn,trend,'SpatialTrend.txt')

call WriteScalarField(nn,eps,'epsilon.txt')
call WriteScalarField(nf,beta,'beta.txt')

call write_csv(nf,nf,Cbeta,'CovBeta.csv',nf)

call spatial_function(grid,Fg,nf,nn,nlsf)
Ceps(1:nn,1:nn)=Ceps(1:nn,1:nn)+matmul( Fg(1:nn,1:nf) ,matmul(Cbeta(1:nf,1:nf),transpose(Fg(1:nn,1:nf)) ) )
do n=1,nn
 V(n)=Ceps(n,n)
enddo
if(IsLogT)call WriteScalarField(nn,SF*exp(sqrt(V(1:nn)))-A,'KrigSTD.txt')
if(.not.IsLogT)call WriteScalarField(nn,sqrt(V(1:nn)),'KrigSTD.txt')

if(IsLogT)call RandomSampleF(nn,nn,NRand,logf(1:n),Ceps,RandomField)
if(.not.IsLogT)call RandomSampleF(nn,nn,NRand,grid%f(1:n)**alpha,Ceps,RandomField)
RandomField(1:nn,1:Nrand)=RandomField(1:nn,1:Nrand)**(1./alpha)
do k=1,nn
    if(IsLogT)adj=SF*exp(sqrt(V(k)))-A
    if(.not.IsLogT)adj=sqrt(V(k))
    EnsMu=sum(  RandomField(k,1:Nrand) )/float(Nrand)
    EnsSTD=sqrt( sum( ( RandomField(k,1:Nrand)-EnsMu )**2 )/float(Nrand) )
    if(EnsSTD.gt.0.)then
        RandomField(k,1:Nrand) = grid%f(k) +( RandomField(k,1:Nrand) - EnsMu )*adj/EnsSTD
    else
        RandomField(k,1:Nrand) = grid%f(k) + RandomField(k,1:Nrand) - EnsMu 
    endif
    if(IsHiLimit)call limitz(Nrand,RandomField(k,1:NRand),grid%z(k)+0.*RandomField(k,1:NRand),&
                            fmax,DomainName)
enddo

!call write_csv(nn,NRand,RandomField,'RandomField.csv',nn)
do j=1,Nrand
    write(buf,'(I6)')j
    call WriteScalarField(nn,RandomField(1:nn,j),'RandomField'//trim(adjustl(buf))//'.txt')
enddo

return
end


subroutine limitz(n,f,z,fpeak,Domain)
!----------------------------------------------------------------------------------------
!subroutine limitz(n,f,z,fpeak,Domain)
!
! Purpose: Restrict maximum recruit density based on historical relation ship between 
! peak recruitment and bathymetry within a year.  The curves here are derived from scallop 
! dredge survey data from 1979 to 2018.
!
! inputs: 
!   n - length of vectors f and z
!   z - [nn] bathymetric depth
!   fpeak - highest observed value of recruitment
!   Domain - 'MA', Mid Atlantic or 'GB' Georges Bank
!
! input/output
!   f - [n] recruitment observations from the same year.
! Keston Smith 2022
!----------------------------------------------------------------------------------------

integer, intent(in) :: n
real*8, intent(in) :: z(*),fpeak
real*8, intent(inout) :: f(*)
character*2, intent(in):: Domain
real*8 a,w,zc,fmax 
integer j,kappa

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

do j=1,n
    f(j)=max( f(j) , 0. )
    fmax= fpeak*(a +   exp(- ( (z(j)-zc)/w )**kappa ))
    f(j)=min( f(j) , fmax )
enddo    
return
end
!----------------------------------------------------------------------------------------

subroutine InterpFromGrid(g,f,obs,fInterp)
!----------------------------------------------------------------------------------------
!subroutine InterpFromGrid(g,f,obs,fInterp)
! Purpose:  Localized linear interpolate data from a field, f, on the nodes of grid g with square elements, g%E to 
! points described in obs.
! inputs: 
!   g (Dpoint)      - grid with square elements - see Main Program
!   obs(dpoint)     - observation points to be interpolated to
!   f [g%n]         - vector of values on nodes of grid
!
! output:
!   fInterp [obs%n] - vector of values interpolated from field onto onto points obs%x, obs%y
!----------------------------------------------------------------------------------------
! Keston Smith 2022
!----------------------------------------------------------------------------------------
use DpointMod
type(Dpoint):: g
type(Dpoint):: obs
real*8, intent(out) :: fInterp(*)
real*8, intent(in) :: f(*)
integer nn,no,n,ne,j,k
real*8,    allocatable:: Dist(:),xe(:),ye(:)
real*8 W(4),De(4),SmallDist
integer*4 ke(4)
nn=g%n
ne=g%ne
no=obs%n
SmallDist=1.D0 !meters
write(*,*)'nn,ne,no',nn,ne,no
allocate(Dist(1:ne),xe(1:ne),ye(1:ne))

do k=1,ne
    ke(1:4)=g%E(1:4,k)
    xe(k)=sum(g%x(ke(1:4)))/4.D0
    ye(k)=sum(g%y(ke(1:4)))/4.D0
enddo

do j=1,no
    Dist(1:ne)=(xe(1:ne)-obs%x(j))**2 + (ye(1:ne)-obs%y(j))**2
    k=minloc(Dist(1:ne),1)
    ke(1:4)=g%E(1:4,k)
    De(1:4)=sqrt( (g%x(ke(1:4))-obs%x(j))**2 + (g%y(ke(1:4))-obs%y(j))**2 )
    if (minval(De(1:4)).gt.SmallDist)then
        W(1:4)=1.D0/De(1:4)
        fInterp(j)=sum( W(1:4)*f( ke(1:4) )  ) / sum( W(1:4) )
    else !avoid division by 0, probably not nescesary
        k=minloc(De,1)
        fInterp(j)=f( ke(k) )
    endif
enddo
deallocate(Dist,xe,ye)
return
end

!----------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------

subroutine GetDomainAverage(obs,g,DomainAverage)
!----------------------------------------------------------------------------------------
! subroutine GetDomainAverage(obs,g,DomainAverage)
! compute spatial average of values in obs%f, by averaging across regions in g%region.
! This is here to match stratified sampling in the CASA model etc.
!
! inputs:
!   obs (dpoint) -  obs%f - values for which spatial average is computed
!   g (dpoint) -    grid with nodes assigned to regions g%ManagementRegion to compute 
!                   local averages within
! output:
!   DomainAverage (real) - regional area weighted average of obs%f across grid 
!----------------------------------------------------------------------------------------
! Keston Smith 2022
!----------------------------------------------------------------------------------------
use DpointMod
type(Dpoint):: g
type(Dpoint):: obs
real*8, intent(out) :: DomainAverage
integer nn,no,Nregion,k,rn,n
real*8 dx,dy
real*8,    allocatable:: RegionalAverage(:),D(:),RegionArea(:)
integer, allocatable:: ObsInRegion(:)
nn=g%n
no=obs%n
Nregion=maxval(g%ManagementRegion(1:nn))
allocate(ObsInRegion(1:Nregion),RegionalAverage(1:Nregion),D(1:nn),RegionArea(1:Nregion))
RegionalAverage(1:Nregion)=0.
ObsInRegion(1:Nregion)=0
do j=1,no
    D(1:nn)=(g%x(1:nn)-obs%x(j))**2 + (g%y(1:nn)-obs%y(j))**2
    k=minloc(D,1)
    rn=g%ManagementRegion(k)
    RegionalAverage(rn)=RegionalAverage(rn)+obs%f(j)
    ObsInRegion(rn)=ObsInRegion(rn)+1
enddo

do k=1,Nregion
    if(ObsInRegion(k).gt.0)RegionalAverage(k)=RegionalAverage(k)/float(ObsInRegion(k))
enddo

dx=1852.
dy=1852.
do k=1,nn
    RegionArea(g%ManagementRegion(k))=RegionArea(g%ManagementRegion(k))+dx*dy
enddo
DomainAverage=sum(RegionalAverage(1:Nregion)*RegionArea(1:Nregion))/sum(RegionArea(1:Nregion))
write(*,*)'Regional Averages',RegionalAverage(1:Nregion)
write(*,*)
write(*,*)'Regional Areas',RegionArea(1:Nregion)
write(*,*)
write(*,*)'Nobs in region',ObsInRegion(1:Nregion)
write(*,*)'Obs Average',sum(obs%f(1:no))/float(no),no
deallocate(ObsInRegion,RegionalAverage,D,RegionArea)
return
end
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------Universal Kriging Subroutines------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
subroutine UK_GLS(g,obs,nf,par,beta,Cbeta,eps,CepsG,nlsf)
! Purpose: Calculates Universal Kriging (UK) estimate at grid points given by coordinates (x,y). 
! Additionally this returns the statistics needed to simulate from the posterior distribution, that 
! is to simulate random fields consistent with the observations, spatial functions, and variogram.  
! 
!  Inputs:
!     x    (real*8) length nn vector of x-coordinates of grid  
!     y    (real*8) length nn vector of y-coordinates for grid  
!     z    (real*8) length nn vector of bathymetric depth at (x,y)  
!     nn    (integer) number of points in grid
!     xo    (real*8) length no vector of x-coordinates of data  
!     yo    (real*8) length no vector of y-coordinates for data  
!     zo    (real*8) length no vector of bathymetric depth at data point (xo,yo)  
!     fo    (real*8) length no vector of observations at (xo,yo)  
!     n0    (integer) number of points in grid
!
!  Outputs:
!    f    (real*8) UK estimate (linear in observations, fo) of the field
!    beta    (real*8) length nf vector, best estimate of spatial function coefficients
!    Cbeta    (real*8) (nf x nf) covariance matrix for the estimates of beta
!    eps    (real*8) length nn vector, best estimate of residual process
!    CepsPost(real*8) (nn x nn) covariance matrix for residual esimate (the spatially 
!               coorelated random field component).
!
! Derivation for the UK algorithm and relation to generalized least squares estimation can be 
! found in the book: N. Cressie (1993). Statistics for spatial data. Wiley, New York, 1993.
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
use DpointMod
use KrigMod
use NLSFMod

implicit none
type(Dpoint):: g
type(Dpoint):: obs
type(KrigPar):: par
type(NLSFPar):: nlsf(*)

integer,    intent(inout):: nf
real*8,    intent(out):: beta(*),Cbeta(nf,nf),eps(*),CepsG(g%n,g%n)

real*8,    allocatable:: Dh(:,:),Dz(:,:),W(:,:),gamma(:,:),Fs(:,:),FsT(:,:)
real*8,    allocatable:: D0h(:,:),D0z(:,:),gamma0(:,:),Fs0(:,:),Fs0T(:,:)
real*8,    allocatable:: R(:,:),V(:,:)
real*8,    allocatable:: CbetaInv(:,:),Ceps(:,:),Cinv(:,:)
real*8,    allocatable:: DGh(:,:),DGz(:,:), C0(:,:), gammaG(:,:)
real*8,    allocatable:: Mtmp(:,:),Vtmp(:),Vtmp2(:),Gtmp(:,:),ftrnd(:)
integer,    allocatable:: ipiv(:)

real*8     Vinf(1,1),Dinf(1,1),atmp,btmp
integer     j,k,info,nopnf,error,nn,no
nn=g%n
no=obs%n
nopnf=no+nf
allocate( Dh(1:no,1:no), Dz(1:no,1:no), W(1:nn,1:no),gamma(1:no,1:no), Fs(1:no,1:nf), FsT(1:nf,1:no),stat=error )
allocate( D0h(1:no,1:nn),D0z(1:no,1:nn),gamma0(1:no,1:nn),Fs0(1:nn,1:nf),Fs0T(1:nf,1:nn) ,stat=error)
allocate( R(1:nopnf,1:nopnf),V(1:nopnf,1:nn) ,stat=error)
allocate( C0(1:no,1:nn), DGh(1:nn,1:nn),DGz(1:nn,1:nn), gammaG(1:nn,1:nn), stat=error )
allocate( CbetaInv(1:nf,1:nf), Ceps(1:no,1:no), Cinv(1:no,1:no),stat=error )
allocate( Mtmp(1:no,1:nf),Vtmp(1:no),Vtmp2(1:nf),Gtmp(1:no,1:nn),ftrnd(1:nn),stat=error )
allocate( ipiv(1:nopnf),stat=error)


!
! Compute variogram between observation points and spatial functions at
! observation points
!
call distance(obs,obs,Dh,Dz,no)
call variogram(no,no,Dh,Dz,Gamma,no,par)
call spatial_function(obs,Fs,nf,no,nlsf)
!
! Ceps <- covariance between observation points (from variogram)
!

!
! Compute variogram between observation points and grid points and spatial
! functions at grid points
!
call distance(obs,g,D0h,D0z,no)
call variogram(no,nn,D0h,D0z,gamma0,no,par)
call spatial_function(g,Fs0,nf,nn,nlsf)
!
!Compute the Univeral Kriging linear estimate of f following Cressie 1993
!pages 151-154
!
Fs0T=transpose(Fs0)
FsT=transpose(Fs)
R(1:no+nf,1:no+nf)=0.
R(1:no,1:no)=gamma(1:no,1:no)
R(1:no,no+1:no+nf)=Fs(1:no,1:nf)
R(no+1:no+nf,1:no)=FsT(1:nf,1:no)

V(1:no,1:nn)=gamma0(1:no,1:nn)
V(no+1:no+nf,1:nn)=Fs0T(1:nf,1:nn)

call dgesv(nopnf,nn,R,nopnf,IPIV,V,nopnf,info)
write(*,*)'UK_GLS (a) dgesv info=', info
!
! compute best linear estimate of the field on the grid points x,y
! f = W f_obs
!
do j=1,nn
 do k=1,no
  W(j,k)=V(k,j)
 enddo
enddo
atmp=1.
btmp=0.
call dgemv('N',nn,no,atmp,W, nn,obs%f,1,  btmp, g%f,1)
!
! compute posterior trend statistics
!
Dinf(1,1)=10.**10
call variogram(1,1,Dinf,Dinf,Vinf,1,par)
!
! Ceps <- covariance between observation points (from variogram)
!
Ceps(1:no,1:no)=Vinf(1,1)-gamma(1:no,1:no)
!
! Cinv = inv( Ceps )
!
Cinv(1:no,1:no)=0.
do j=1,no
 Cinv(j,j)=1.
enddo
call dgesv(no,no,Ceps,no,IPIV,Cinv,no,info)
!
! cov(beta_gls) =  inv (F' Cinv F )
!
call dgemm('N','N', no, nf, no, atmp, Cinv, no, Fs,   no,btmp, Mtmp,    no )
call dgemm('T','N', nf, nf, no, atmp,  Fs, no, Mtmp , no,btmp, CbetaInv, nf )
CBeta(1:nf,1:nf)=0.
do j=1,nf
 CBeta(j,j)=1.
enddo
call dgesv(nf,nf,CBetaInv,nf,IPIV,CBeta,nf,info)
write(*,*)'UK_GLS (b) dgesv info=', info

!
! beta_gls = inv( F' * Cinv * F ) * F * Cinv * fo
!
Vtmp(1:no)=matmul(Cinv(1:no,1:no),obs%f(1:no))
Vtmp2(1:nf)=matmul(transpose(Fs(1:no,1:nf)),Vtmp(1:no))
beta(1:nf)=matmul(Cbeta(1:nf,1:nf),Vtmp2(1:nf))
!call dgemv('N',no,no,atmp,Cinv, no,obs%f,1,  btmp, Vtmp,1)!Vtmp=C^{-1} y
!write(*,*)'UK_GLS (b) dgesv info=', info
!call dgemv('T',no,nf,atmp, Fs,  no,Vtmp,1,btmp, Vtmp2,1)!Vtmp2= F^T C^{-1} y
!write(*,*)'UK_GLS (c) dgesv info=', info
!call dgemv('N',nf,nf,atmp,Cbeta,nf,Vtmp2,1,btmp,Beta,1)!beta=Cbeta F^t C^{-1} y
!write(*,*)'UK_GLS (d) dgesv info=', info
!
! Posterior covariance for residual
!
call distance(g,g,DGh,DGz,nn)
write(*,*)'UK_GLS (e) dgesv info=', info
call variogram(nn,nn,DGh,DGz,gammaG,nn,par)
write(*,*)'UK_GLS (f) dgesv info=', info
CepsG(1:nn,1:nn)=Vinf(1,1)-gammaG(1:nn,1:nn)
C0(1:no,1:nn)=Vinf(1,1)-gamma0(1:no,1:nn)
!
! CepsG <== CepsPostG = CepsG - C0v' * inv(C_obs) * C0v
!
write(*,*)'UK_GLS (g) dgesv info=', info
call dgemm('N','N', no, nn, no, atmp, Cinv, no, C0, no, btmp, Gtmp, no )
write(*,*)'UK_GLS (h) dgesv info=', info
call dgemm('T','N', nn, nn, no, atmp, C0, no, Gtmp, no, btmp, gammaG, nn )
! note: gamma G<== C0' * Cinv * C0 no longer represents variogram for the grid!
write(*,*)'UK_GLS (i) dgesv info=', info

CepsG(1:nn,1:nn) = CepsG(1:nn,1:nn) - gammaG(1:nn,1:nn)
!
! Compute posterior mean of epsilon.  This is the difference between unbiased estimate of f and
! the mean of the trend.  Thus the mean of the total distribution is the kriging estimate. 
!
write(*,*)'UK_GLS (j) dgesv info=', info
call dgemv('N',nn,nf,atmp,Fs0,nn,beta,1,btmp,ftrnd,1)
write(*,*)'UK_GLS (k) dgesv info=', info

eps(1:nn)=g%f(1:nn)-ftrnd(1:nn)

write(*,*)'UK_GLS end'
deallocate( Dh,Dz, W,gamma, Fs, FsT,stat=error )
deallocate( D0h,D0z,gamma0,Fs0,Fs0T ,stat=error)
deallocate( R,V ,stat=error)
deallocate( CbetaInv, Ceps, Cinv,stat=error )
deallocate( Mtmp,Vtmp,Vtmp2,Gtmp, ftrnd,stat=error )
deallocate( ipiv,stat=error)
deallocate( C0, DGh,DGz,  gammaG, ftrnd, stat=error )

return
end
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
!subroutine UK_RandomField(x,y,z,nn,nf,beta,Cbeta,eps,Ceps,Nsim)
subroutine UK_RandomField(g,nf,beta,Cbeta,eps,Ceps,Nsim,nlsf,IsLogT,mu,A,SF,fmin,fmax,z,logmu)
! Purpose: Simulate independent random fields under the assumptions of a Universal Kriging model.
! 
!  Inputs:
!     x    (real*8) length nn vector of x-coordinates of grid  
!     y    (real*8) length nn vector of y-coordinates for grid  
!     z    (real*8) length nn vector of bathymetric depth at (x,y)  
!     nn    (integer) number of points in grid
!    beta    (real*8) length nf vector estimate of spatial function coefficients
!    Cbeta   (real*8) (nf x nf) covariance matrix for estimate beta
!    eps    (real*8) length nn vector estimate of noise field
!    Ceps    (real*8) (nn x nn) spatial covariance matrix of residual from linear fit of spatial 
!                   functions
!    Nsim    (integer) number of random fields to simulate
!
!  Outputs:
!    none    currently random fields are written to files 'totalRF.csv','trendRF.csv', and
!        'noiseRF.csv' as (nn x Nsim) matricies in which each column represents an 
!        independent random field. 'trend' is the component related to the spatial functions,
!        'noise' the component related to the residual, and 'total' their sum.
!
! The independence of the trend component and spatially correlated noise component is assumed.
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
use DpointMod
use NLSFMod
implicit none
type(Dpoint):: g
type(NLSFpar):: nlsf(*)
real*8,    intent(in)    :: beta(*),Cbeta(nf,*),eps(*),Ceps(g%n,*),mu(*),A,SF,fmin,fmax,z(*),logmu(*)
integer,    intent(in)    :: Nsim
logical,    intent(in)    :: IsLogT
integer,    intent(inout)    :: nf

real*8,    allocatable    :: F(:,:)
real*8,    allocatable    :: BetaRF(:,:),RFtrend(:,:),RFtotal(:,:),RFeps(:,:),EnsMu(:),&
trend(:),Ctrend(:,:)

real*8     atmp,btmp,adj
character*72     flnm
integer     j,k,n
n=g%n
allocate(F(1:n,1:nf))
allocate(BetaRF(1:nf,1:Nsim),RFtrend(1:n,1:NSim),RFtotal(1:n,1:NSim),RFeps(1:n,1:NSim),&
        EnsMu(1:n),trend(1:n),Ctrend(1:n,1:n) )

atmp=1.
btmp=0.

call RandomSampleF(n,n,Nsim,eps(1:n),Ceps,RFeps)

call spatial_function(g,F,nf,g%n,nlsf)
!Ctrend(1:n,1:n)=matmul( F(1:n,1:nf) ,matmul(Cbeta(1:nf,1:nf),transpose(F(1:n,1:nf)) ) )
trend(1:n)=matmul(F(1:n,1:nf),beta(1:nf))
!call RandomSampleF(n,n,Nsim,trend,Ctrend,RFtrend)

call RandomSampleF(nf,nf,Nsim,beta(1:nf),Cbeta,BetaRF)
!call dgemm('N','N',g%n,Nsim,nf,atmp,F,g%n,BetaRF,nf,btmp,RFtrend,g%n)
RFtrend(1:n,1:Nsim)=matmul(F(1:n,1:nf),BetaRF(1:nf,1:Nsim))

RFtotal(1:n,1:NSim)=RFtrend(1:n,1:NSim)+RFeps(1:n,1:NSim)
!do k=1,n
!    RFtotal(k,1:NSim)=logmu(k)+RFtrend(k,1:NSim)+RFeps(k,1:NSim)
!enddo

if(IsLogT)then
    RFtrend(1:n,1:NSim) = SF * exp(RFtrend(1:n,1:NSim)) - A  
    RFeps(1:n,1:NSim) =   SF * exp(RFeps(1:n,1:NSim))   - A 
endif

!-----------------------------------------------------------
!enforce ensemble mean in trend
trend(1:n)=matmul(F(1:n,1:nf),beta(1:nf))
if(IsLogT)trend(1:n) = SF * exp(trend(1:n)) - A  
!do k=1,n
!    EnsMu(k)=sum( RFtrend(k,1:Nsim) )/float(Nsim)
!    RFtrend(k,1:Nsim)=trend(k)*RFtrend(k,1:Nsim)/EnsMu(k)
!enddo
!-----------------------------------------------------------

do k=1,n
    call limitz(Nsim,RFtrend(k,1:Nsim),z(k)+0.*RFtrend(k,1:Nsim),fmax,'MA')
    call limitz(Nsim,RFeps(k,1:Nsim),z(k)+0.*RFeps(k,1:Nsim),fmax,'MA')
enddo
if(IsLogT)then
    RFtotal(1:n,1:NSim) = SF * exp(RFtotal(1:n,1:NSim)) - A 
endif

open(69,FILE='RandomFieldSF.txt')
!adjust ensemble ensemble mean to match kriging estimate 
do k=1,n
    EnsMu(k)=sum( RFtotal(k,1:Nsim) )/float(Nsim)
!    RFtotal(k,1:Nsim) = RFtotal(k,1:Nsim) + mu(k)  - EnsMu(k)
!    RFtotal(k,1:Nsim) = RFtotal(k,1:Nsim)*(mu(k)+A) / (EnsMu(k)+A)
    adj=min( (mu(k)+A) / (EnsMu(k)+A), 1.D0)
    RFtotal(k,1:Nsim) = RFtotal(k,1:Nsim)*adj
    call limitz(Nsim,RFtotal(k,1:Nsim),z(k)+0.*RFtotal(k,1:Nsim),fmax,'MA')
    write(69,*) mu(k)  - EnsMu(k)
enddo
close(69)

call write_csv(g%n,NSim,RFeps,'noiseRF.csv',g%n)
call write_csv(g%n,NSim,RFtrend,'trendRF.csv',g%n)
call write_csv(g%n,NSim,RFtotal,'totalRF.csv',g%n)

deallocate(F)
deallocate(BetaRF,RFtrend,RFtotal,RFeps)

return
end

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-------------------------------Random Field Subroutines------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

!--------------------------------------------------------------------------------------------------
subroutine IndStdNRV(n,m,R)
! Purpose: Generate independent standard normal samples in a (n x m) matrix 
! (R)
!  Inputs:
!     n    (integer) number of rows in R
!     m       (integer) number of columns in R
!  Outputs:
!    R    (real*8)  Size (nn x mm) matrix of independent standard 
!                normal random variables
!
! The algorithm uses the Box Muller transformation to compute independent 
! standard normal numbers from uniform random numbers generated by the 
! intrinsic subroutine random_number( ).  For refrence see:
!
! Box, G. E. P.; Muller, Mervin E. (1958). "A Note on the Generation of 
! Random Normal Deviates". The Annals of Mathematical Statistics. 29 
! (2): 610–611. doi:10.1214/aoms/1177706645. JSTOR 2237361
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------

implicit none
real*8,    intent(out)::R(n,*)
integer,    intent(in)::n,m    
real*8,    allocatable:: U1(:,:),U2(:,:)
real*8     pi
integer     k,j
allocate( U1(1:n,1:m), U2(1:n,1:m) )
pi=4.*ATAN(1.0D0)
call random_number(U1)
call random_number(U2)
R(1:n,1:m) = sqrt ( - 2.0D0* log ( U1(1:n,1:m) ) ) * cos ( 2.0D0 * pi * U2(1:n,1:m) )
deallocate(U1,U2)
return
end
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
subroutine RandomSampleF(nndim,nn,Nsample,mu,C,X)
!subroutine RandomSampleF(nndim,nn,Nsample,mu,C,X)
! Generate random sample from a multivariate gaussian distribution N(mu,C)
! where mu is the distribution mean C is the covariance. The number of
! independent samples drawn is Nsample which are stored in the columns of X.
!
! Inputs:
!     nndim    (integer) Leading dimentsion of C and X
!    nn    (integer) Length of mu / dimension of random variable
!    Nsample (integer) Number of Samples to draw from N(mu,C)
!    mu    (real*8)  Length nn vector, mean of the distribution
!    C    (real*8)  Size (nn x nn) Covariance matrix of the distribution
!
! Outputs:
!    X       (real*8)  Size (nn x Nsample) independent random samples drawn from N(mu,C)
!
! Requires: LAPACK and BLAS libraries for Cholesky factorization of C and matrix
! vector multiplication.
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------

implicit none
integer,     intent(in) :: nndim,nn,Nsample
real*8,     intent(in) :: mu(*),C(nndim,*)
real*8,     intent(out)::X(nn,*)
real*8,    allocatable::R(:,:),S(:,:)
real*8     atmp,btmp
integer     j,k,info
allocate( R(1:nn,1:Nsample), S(1:nn,1:nn))
!
! S<-C for cholesky decomposition
!
 S(1:nn,1:nn)=C(1:nn,1:nn)
!
! S<-Cholesky(C) so that C = S * S^T
!
 call dpotrf('L',nn,S,nn,info )
 write(*,*)'dpotrf in RandomSample, info=',info
!
! set upper triangular portion of S to 0.
!
 do k=1,nn-1
  S(k,k+1:nn)=0.
 enddo

atmp=1.
btmp=0.
!
! R <- (nn x Nsample) independent standard normal random numbers R ~ N(0,I(nn))
!
call IndStdNRV(nn,Nsample,R)
!
! X <- S * R, X ~ N(0,C)
!
call dgemm('N','N', nn, Nsample, nn, atmp, S, nn, R, nn, btmp, X, nn )
!
! X <- mu + X, X ~ N(mu,C)
!
do j=1,Nsample
 X(1:nn,j)=mu(1:nn)+X(1:nn,j)
enddo

deallocate(R,S)

return
end
!--------------------------------------------------------------------------------------------------


!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------- End Random Field Subroutines----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
subroutine UK_prior(g,nf,par,beta,Cbeta,eps,Ceps)
! subroutine UK_prior(g,nf,par,beta,Cbeta,eps,Ceps)
! Assign values needed to simulate random fields from a Universal Kriging 
! model.
!
! Inputs: 
!    g    (real*8) Data point structure defining interpolation points 
!    nf    (integer) number of spatial functions
!   par variogram parameter structure
!
! Outputs:
!   beta(real*8) mean of spatial function coefficients
!   Cbeta(real*8) covariance of spatial function coefficients
!   eps(real*8) mean of residual
!   Ceps(real*8) covariance of residual
! Keston Smith (IBSS corp) June-July 2021
! need to fix dimensioning (1/10/2022)!
!-----------------------------------------------------------------------
use DpointMod
use KrigMod
implicit none
type(Dpoint):: g
type(KrigPar):: par
integer,    intent(in):: nf
real*8,    intent(out):: beta(*), Cbeta(nf,*), eps(*), Ceps(nn,*) 

real*8,    allocatable:: Dh(:,:), Dz(:,:),gamma(:,:)

real*8     Vinf(1,1),Dinf(1,1),atmp,btmp
integer     j,k,error,nn
nn=g%n
allocate( Dh(1:nn,1:nn), Dz(1:nn,1:nn),gamma(1:nn,1:nn), stat=error )
!
! Assign mean and Covariance for epsilon
!
call distance(g,g,Dh,Dz,nn)
call variogram(nn,nn,Dh,Dz,gamma,nn,par)
Dinf(1,1)=10.**10
call variogram(1,1,Dinf,Dinf,Vinf,1,par)
Ceps(1:nn,1:nn)=Vinf(1,1)-gamma(1:nn,1:nn)
eps(1:nn)=0.
!
! Assign mean and Covariance for beta
!
beta(1:nf)=0.
Cbeta(1:nf,1:nf)=0.
beta(1:4)=[ 52.399839511692335, 1.0935428391808943E-004,-3.1615155809229245E-003, 1.2116283255694471E-002 ]
Cbeta(1,1:4)=[270.60028747602928, 1.5366756312328578E-003, 7.9881962338312195E-003,-6.0292972962538195E-002]
Cbeta(2,1:4)=[1.5366756312270658E-003, 1.7247639913097469E-005, 1.3324610875859451E-006,-2.3045855632454535E-007]
Cbeta(3,1:4)=[7.9881962338509589E-003, 1.3324610875862412E-006, 5.5464689237415081E-006,-2.4207894889311469E-006]
Cbeta(4,1:4)=[-6.0292972962540783E-002,-2.3045855632585232E-007,-2.4207894889268253E-006, 1.3517389449680424E-005]

deallocate( Dh, Dz, gamma,stat=error )

return
end
!--------------------------------------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------End User Subroutines--------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
subroutine GLS(y,F,C,n,m,beta,Cbeta,r)
! Purpose:Generalized Least Squares solver. Solve for beta to fit y = F*beta +epsilon, for 
! epsilon~N(0,C). Return optimal beta, covariance of Beta and residual
! 
!  Inputs:
!   y   (real*8) length n vector to fit
!     F    (real*8) size n x m matrix of j=1:m functions evaluated at points k=1:n    
!     C    (real*8) size n x n covariance matrix for epsilon
!   n   (integer) number of points to fit
!   m   (integer) number of functions in fit
!  Outputs:
!     beta(real*8) length m vector of optimal coefficients
!     Cbeta(real*8) size m x m covariance matrix for beta
!     r    (real*8) length nn vector of residuals r = y - F * beta
!
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
implicit none
integer,    intent(in):: n,m
real*8,    intent(in):: y(*),F(n,*),C(n,*)
real*8,    intent(out):: beta(*),Cbeta(m,m),r(*)

real*8,    allocatable:: Cinv(:,:),CbetaInv(:,:),Vtmp(:),Vtmp2(:),Mtmp(:,:),ytr(:)
integer,    allocatable:: ipiv(:)

real*8     atmp,btmp
integer     j,k,info,nopnf,error
character*72 flnm

allocate( Cinv(1:n,1:n),CbetaInv(1:m,1:m),ytr(1:n),Mtmp(1:n,1:m),Vtmp(1:n),Vtmp2(1:m) ) 
allocate( ipiv(1:n),stat=error)

atmp=1.
btmp=0.

Cinv(1:n,1:n)=0.
do j=1,n
 Cinv(j,j)=1.
enddo
call dgesv(n,n,C,n,IPIV,Cinv,n,info)
write(*,*)'GLS',info
!
! cov(beta)) =  inv (F' Cinv F )
!
call dgemm('N','N', n, m, n, atmp, Cinv, n, F,   n,btmp, Mtmp,    n )
call dgemm('T','N', m, m, n, atmp,  F,   n , Mtmp , n,btmp, CbetaInv, m )

CBeta(1:m,1:m)=0.
do j=1,m
 CBeta(j,j)=1.
enddo
call dgesv(m,m,CBetaInv,m,IPIV,CBeta,m,info)
write(*,*)'GLS  dgesv info=', info
call write_csv(m,m,CbetaInv,'CBeta0.csv',m)
call write_csv(n,m,F,'Fglsa0.csv',n)
!
! beta = inv( F' * Cinv * F ) * F * Cinv * fo
!
call dgemv('N',n,n,atmp,Cinv, n, y,  1,btmp, Vtmp,1)
call dgemv('T',n,m,atmp, F,  n,Vtmp,  1,btmp, Vtmp2,1)
call dgemv('N',m,m,atmp,Cbeta,m,Vtmp2, 1,btmp,Beta,1)
!
! compute residual
!
call dgemv('N',n,m,atmp,F, n, beta,  1,btmp, ytr,1)
r(1:n)=y(1:n)-ytr(1:n)

call write_scalar_fields(n,1,y,'obs.txt',n)
!call write_scalar_fields(n,1,ytr,'ytr.txt',n)

deallocate( ipiv,stat=error)
deallocate(  Cinv, CbetaInv, ytr, Mtmp, Vtmp, Vtmp2, stat=error )

return
end
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
subroutine SLR(y,x,n,alpha,beta,p)
!--------------------------------------------------------------------------------------------------
!subroutine SLR(y,x,n,alpha,beta,p)
!Purpose: Simple linear regresion 
! minimize sum_{j=1:n}  ( y(j) - alpha + beta * x(j) )**2 over alpha and beta
!
! Inputs:
!   y (real) [n]
!   x (real) [n]
!   n (integer)
!
! Outputs:
!   alpha (real)
!   beta (real)
!   p (real) [n] p(1:n) = alpha + beta * x(1:n) 
!   
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
!Simple Least Squares
implicit none
integer,    intent(in):: n
real*8,    intent(in):: y(*),x(*)
real*8,    intent(out):: p(*),alpha,beta
real*8 Sxy,Sx,Sy,Sx2
Sxy=sum(x(1:n)*y(1:n))
Sx=sum(x(1:n))
Sy=sum(y(1:n))
Sx2=sum(x(1:n)*x(1:n))
beta=(n * Sxy - Sx * Sy) / ( n * Sx2 -Sx * Sx )
alpha=(Sy / n) - (beta*Sx / n)
p(1:n)=alpha + beta * x(1:n)
return
end

