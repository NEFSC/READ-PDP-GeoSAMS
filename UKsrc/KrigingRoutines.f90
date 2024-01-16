!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------Kriging Subroutines--------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


!--------------------------------------------------------------------------------------------------
subroutine distance(p,q,Dh,Dz,nndim)
!subroutine distance(nn,mm,x0,y0,z0,x1,y1,z1,D,ndim)
!
! Purpose: Computes distance between two vectors of points
! Inputs:
!     nn    (integer) number of spatial points in vectors x0,y0 
!    x0    (real(kind(1.0D0)))  length nn vector of x coordinates (m)
!    y0    (real(kind(1.0D0)))  length nn  vector of y coordinates (m)
!    z0    (real(kind(1.0D0)))  length nn  vector of bathymetric depth at (x,y)
!
!     mm    (integer) number of spatial points in vectors x0,y0 
!    x1    (real(kind(1.0D0)))  length mm  vector of x coordinates (m)
!    y1    (real(kind(1.0D0)))  length mm vector of y coordinates (m)
!    z1    (real(kind(1.0D0)))  length mm vector of bathymetric depth at (x,y)
!
! outputs:
!    D    (real(kind(1.0D0)))  Size (nn x mm) matrix of distances between point pairs (x0,y0),(x1,y1)
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
use DpointMod
implicit none
type(Dpoint):: p
type(Dpoint):: q
integer, intent(in)    :: nndim
real(kind(1.0D0)), intent(out)   :: Dh(nndim,*),Dz(nndim,*)
integer    j

do j=1,p%n
  Dh(j,1:q%n)= sqrt( (p%x(j) - q%x(1:q%n) )**2 + ( p%y(j)-q%y(1:q%n) )**2 )
  Dz(j,1:q%n)= sqrt( (p%z(j) - q%z(1:q%n) )**2 )
enddo
end subroutine
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
subroutine variogram(nn,mm,Dh,Dz,G,ndim,par)
!subroutine variogram(nn,mm,D,G,ndim)
!
!Purpose: Computes a variogram (G) given distances between points (D).
! Inputs:
!     nn    (integer) number of rows in D, Gamma
!     mm      (integer) number of columns in D, Gamma
!    D    (real(kind(1.0D0)))  Size (nn x mm) matrix distance between point pairs
!
! Outputs:
!    G    (real(kind(1.0D0)))  Size (nn x mm) variogram matrix for point pairs represented in D
!
! Internal:
!    c    (real(kind(1.0D0)))  Variogram parameter c+c0 = shelf
!    c0    (real(kind(1.0D0)))  Variogram parameter nugget
!    alpha    (real(kind(1.0D0)))  Variogram length scale parameter
!    form    (charecter*72) functional form of variogram
!The above should be read in an input file but are written as constants for now
!    
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
use KrigMod
implicit none
type(KrigPar):: par
integer, intent(in)    :: nn,mm,ndim
real(kind(1.0D0)), intent(in)    :: Dh(ndim,*),Dz(ndim,*)
real(kind(1.0D0)), intent(out)   :: G(ndim,*)
integer j,k,error
real(kind(1.0D0)) c,c0,alpha,Wz
real(kind(1.0D0)) kap
character(72) form
real(kind(1.0D0)), allocatable:: D(:,:)

allocate( D(1:nn,1:mm) ) 

c0 =  par%c0
c =  par%c
alpha =  par%alpha
Wz =  par%Wz
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
end
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
subroutine variogramF(nn,Dh,Dz,G,ndim,f,par)
!subroutine variogram(nn,mm,D,G,ndim)
!
!Purpose: Computes a variogram (G) given distances between points (D).
! Inputs:
!     nn    (integer) number of rows in D, Gamma
!     mm      (integer) number of columns in D, Gamma
!    D    (real(kind(1.0D0)))  Size (nn x mm) matrix distance between point pairs
!
! Outputs:
!    G    (real(kind(1.0D0)))  Size (nn x mm) variogram matrix for point pairs represented in D
!
! Internal:
!    c    (real(kind(1.0D0)))  Variogram parameter c+c0 = shelf
!    c0    (real(kind(1.0D0)))  Variogram parameter nugget
!    alpha    (real(kind(1.0D0)))  Variogram length scale parameter
!    form    (charecter*72) functional form of variogram
!The above should be read in an input file but are written as constants for now
!    
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
use KrigMod
implicit none
type(KrigPar):: par
type(KrigPar):: parTmp

integer  NIntVD,NPS
parameter(NIntVD=31,NPS=30)
integer, intent(in)    :: nn,ndim
real(kind(1.0D0)), intent(in)    :: Dh(ndim,*),Dz(ndim,*),f(*)
real(kind(1.0D0)), intent(out)   :: G(ndim,*)
real(kind(1.0D0)) DintV(NIntVD),DintVm(NIntVD), SIntV(NIntVD)
real(kind(1.0D0)) Pind(NPS),alphaR(NPS),c0R(NPS),cR(NPS),WzR
integer j,k,n,m,nc,NIntV
real(kind(1.0D0)) dx,cost,costmin,Wz

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
!alphaR(1:NPS)=Pind(1:NPS)*100000.D0
alphaR(1:NPS)=Pind(1:NPS)*100000.D0
! WzR,Wz -  These variables were used to allow anisotropy in the kriging covariance.
! they are not used here.
!WzR(1:NPS)=(Pind(1:NPS)-1./float(NPS))*10000.D0
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
! c0R(1:NPS)=Pind(1:NPS)*minval(SIntV(1:NIntV))
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
!write(*,*)'Bounds Wz:[',WzR(1),par%Wz,WzR(NPS),']'
write(*,*)'Bounds c0:[', c0R(1),par%c0,c0R(NPS),']'
write(*,*)'Bounds c:[',cR(1),par%c,cR(NPS),']'
call variogram(nn, nn, Dh,Dz, G, nn, par)

return
end
!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
subroutine spatial_function(p,F,nf,nndim,nlsf)
!subroutine spatial_function(nn,x,y,z,F,nf,ndim)
!
! Purpose: Computes value of spatial functions at x,y.
! Inputs:
!   p (Dpoint) - Spatial points to evaluate functions at
!     nndim    (integer) leading dimension of F
!   nlsf (NLSF) vector of nonlinear spatial functions defined
!
! outputs:
!    F    (real(kind(1.0D0)))  Size (nn x nf) matrix containg values of spatial functions at points (x,y)
!    nf    (integer) number of spatial functions (be carefull allocating F)
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
use DpointMod
use NLSFMod
implicit none
type(Dpoint):: p
type(NLSFpar), intent(in):: nlsf(*)
integer, intent(in)    :: nndim
integer, intent(out)   :: nf
real(kind(1.0D0)), intent(out)   :: F(nndim,*)
real(kind(1.0D0)) s(nndim), fpc(nndim)
integer nn,j,k

nf=nlsf(1)%nsf+1
! flag for initial call
if (nndim.eq.1) return
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

!--------------------------------------------------------------------------------------------------
subroutine spatial_function_lin(p,F,nf,nndim)
!subroutine spatial_function_lin(p,F,nf,ndim,nlsf)
!
! Purpose: Computes value of spatial functions at p%x(1:p%n),p%y(1:p%n).  This is left as an example
! subroutine for users to define their own spatial_function subroutine. 
! Inputs:
!   p (Dpoint) - Spatial points to evaluate functions at
!     nndim    (integer) leading dimension of F
!   nlsf (NLSF) Dummy variable.
!
! outputs:
!    F    (real(kind(1.0D0)))  Size (nn x nf) matrix containg values of spatial functions at points (x,y)
!    nf    (integer) number of spatial functions (be carefull allocating F)
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
use DpointMod
use NLSFMod
implicit none
type(Dpoint):: p
integer, intent(in)    :: nndim
integer, intent(out)   :: nf
real(kind(1.0D0)), intent(out)   :: F(nndim,*)
integer nn

nf=4
! flag for initial call
if (nndim.eq.1) return
nn=p%n

F(1:nn,1)=1.
F(1:nn,2)=p%x(1:nn)/1000.
F(1:nn,3)=p%y(1:nn)/1000.
F(1:nn,4)=p%z(1:nn)

call write_csv(nn,nf,F,'SpatialFunctions.csv',nn)


end subroutine
!--------------------------------------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------End Kriging Subroutines------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

