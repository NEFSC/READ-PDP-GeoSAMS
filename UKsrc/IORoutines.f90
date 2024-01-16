
subroutine ReadInput(DomainName,obsfile,climfile,NRand,IsLogT,IsHiLimit,fmax,IsMatchMean,IsClimEst,par,alpha)
!subroutine ReadInput(DomainName,obsfile,climfile,NRand,IsLogT,IsHiLimit,fmax,IsMatchMean,IsClimEst,par)
!--------------------------------------------------------------------------------------------------
! Purpose: Read parameter values, flags, etc. from a ascii text input file:"UK.inp".  Parameters etc. 
! to be read from UK.inp are identified by the first letter of the line.  Values are read from the 
! line to the right of an "=" character. Logical variables are read from 'T','F'.
!
! outputs: 
!       all variables
!
! history:  Written by keston Smith (IBSS corp) 2022
!--------------------------------------------------------------------------------------------------
use KrigMod
implicit none
type(KrigPar), intent(out):: par
integer j,io
integer, intent(out):: NRand
character(72),intent(out):: obsfile,climfile
character(2),intent(out):: DomainName
!logical, intent(out) :: IsLogT,IsHiLimit,IsMatchMean
logical IsLogT,IsHiLimit,IsMatchMean,IsClimEst
character(72) :: InputStr
real(kind(1.0D0)),intent(out)::  fmax,alpha
IsClimEst=.false.
IsHiLimit=.false.
open(69,file='UK.inp')
do
    InputStr=""
    read(69,'(a)',iostat=io) InputStr
    if (io.lt.0) exit

    select case (InputStr(1:1))
        case('#')
            write(*,*)'Comment echo:',InputStr
        case('D')
            j = scan(InputStr,"=",back=.true.)
            DomainName=trim(adjustl(Inputstr(j+1:)))
            write(*,*)'Domain is ',DomainName
        case('I')
            j = scan(InputStr,"=",back=.true.)
            obsfile=trim(adjustl(InputStr(j+1:)))
            write(*,*)'taking point obs from ',trim(obsfile),' format should be decimal year, x, y, z, f'
        case('L')
            j = scan(InputStr,"=",back=.true.)
            read(Inputstr(j+1:),*)IsLogT
            write(*,*)'Is log transform?',IsLogT
        case('H')
            j = scan(InputStr,"=",back=.true.)
            read(Inputstr(j+1:),*)fmax
        case('P')
            j = scan(InputStr,"=",back=.true.)
            read(Inputstr(j+1:),*)alpha
            IsHiLimit = .true.
            write(*,*)'Is this the fmax you were searching for?', fmax
        case('C')
            j = scan(InputStr,"=",back=.true.)
            climfile=trim(adjustl(InputStr(j+1:)))
            write(*,*)'taking climatological background field from ',trim(climfile)
            IsClimEst=.true.
        case('N')
            j = scan(InputStr,"=",back=.true.)
            read( InputStr(j+1:),* )NRand
        case('K')
            j = scan(InputStr,"=",back=.true.)
            read( InputStr(j+1:),* )par%form
        case('M')
            j = scan(InputStr,"=",back=.true.)
            read( InputStr(j+1:),* )IsMatchMean
            
            !Match stratified mean = F

        case default
            write(*,*) 'ReadInput: Unrecognized line in UK.inp'
            write(*,*) 'Unrecognized Line->',InputStr
            write(*,*) 'This is probably not a problem'
            !stop
    end select

end do
close(69)
return
end

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------User Subroutines-----------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

subroutine loadgrid(x,y,z,lat,lon,nn,E,ne,ManagementRegion,DomainName)
! subroutine loadgrid(x,y,z,f,nn)
! load grid coordinates and bathymetric depth from CSV file with 5 columns
! representing an x coordinate, y, bathymetric depth (z), latitude, and
! longitude.
!
! Inputs: 
!    none
!
! Outputs:
!    x    (real(kind(1.0D0))) x-coordinate of data
!    y    (real(kind(1.0D0))) y-coordinate of data
!    z    (real(kind(1.0D0))) bathymetric depth at (x,y)
!    lat    (real(kind(1.0D0))) latitude at (x,y)
!    lon    (real(kind(1.0D0))) longitude at (x,y)
!    nn    (integer)length of x,y,z,lat,lon (number of data points)
!
! Note: At present lat and lon variabels are not used. 
!-----------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------

implicit none
real(kind(1.0D0)) lat(*),lon(*),x(*),y(*),z(*)
integer n,nn,ne,io,ManagementRegion(*),E(4,*)
character(72) input_str,flnm
character(2) DomainName
flnm='Grids/'//DomainName//'xyzLatLon.csv'
write(*,*)'grid file: ',trim(flnm)
open(63,file=trim(flnm),status='old')
n=0
do
 read(63,'(a)',iostat=io) input_str
 if (io.lt.0) exit
 n=n+1
 read(input_str,*) x(n),y(n),z(n),lat(n),lon(n)
end do
close(63)
nn=n

flnm='Grids/ManagementArea'//DomainName//'.txt'
open(63,file=trim(flnm),status='old')
n=0
do n=1,nn
 read(63,'(a)',iostat=io) input_str
 if (io.lt.0) exit
 read(input_str,*) ManagementRegion(n)
end do
close(63)

flnm='Grids/'//DomainName//'squares.csv'
open(63,file=trim(flnm),status='old')
n=0
do
 read(63,'(a)',iostat=io) input_str
 if (io.lt.0) exit
 n=n+1
 read(input_str,*) E(1,n),E(2,n),E(3,n),E(4,n)
end do
close(63)
ne=n

close(63)

return
end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine loaddata(x,y,z,f,nn,flnm)
! subroutine loaddata(x,y,z,f,nn)
! Load data from CSV file with 4 columns representing an x coordinate, y
! coordinate, bathymetric depth (z), and a scaller field f.
!
! Inputs: 
!    none
!
! Outputs:
!    x    (real(kind(1.0D0))) x-coordinate of data
!    y    (real(kind(1.0D0))) y-coordinate of data
!    z    (real(kind(1.0D0))) bathymetric depth at (x,y)
!    f    (real(kind(1.0D0))) scalar data at (x,y)
!    nn    (integer)length of x,y, and z (number of data points)
!
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------


implicit none
real(kind(1.0D0)), intent(out):: x(*),y(*),z(*),f(*)
integer, intent(out):: nn
character (*), intent(in)::  flnm
real(kind(1.0D0)) year
integer    n,io
character(72) input_str

!flnm='recruitsOBS.csv'
open(63,file=flnm,status='old')
n=0
read(63,*) input_str
do
 read(63,'(a)',iostat=io) input_str
 if (io.lt.0) exit
 n=n+1
 read(input_str,*) year, x(n),y(n),z(n),f(n)
! write(*,*)n,year, x(n),y(n),z(n),f(n)
! read(input_str,*)  x(n),y(n),z(n),f(n)
end do
close(63)
nn=n
return
end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------

subroutine readsf(flnm, M, nn)
!subroutine readsf(flnm, M, nn)
!Purpose: Read real valued scaler field, M, from file flnm.
! Inputs:
!     flnm (charecter*72)    number of rows in D, Gamma
! Outputs:
!     M    (real(kind(1.0D0)))       length nn vector of values
!    nn   (integer)        length of M
!-----------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------

implicit none
integer, intent(inout):: nn
real(kind(1.0D0)), intent(out):: M(*)
character (*), intent(in)::flnm
integer n,io
character(72) input_str
write(*,*)'readsf flnm=',flnm
open(63,file=trim(flnm),status='old')
n=0
do
 read(63,'(a)',iostat=io) input_str
 if (io.lt.0) exit
 n=n+1
 read(input_str,*) M(n)
end do
close(63)
nn=n

return
end

!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
subroutine write_scalar_fields(nn,nsim,f,flnm,ndim)
!subroutine scalar_fields(nn,nsim,f,flnm,ndim)
! Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
! Inputs:
!     nn    (integer) number of rows in f 
!     nsim(integer) number of columns in f
!    f    (real(kind(1.0D0))) values to write to text file
!    flnm(character(72)) filename to write f to in csv format
!   ndim(integer) leading dimension of f
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
implicit none
integer, intent(in):: nn,nsim,ndim
real(kind(1.0D0)), intent(in):: f(ndim,*)
character (*), intent(in):: flnm
integer k,n
character(72) buf,flnm2
!--------------------------------------------------------------------------------------------------
if (nsim.gt.1)then
    do k=1,nsim
        write(buf,'(I6)')k
        flnm2=trim(flnm)//trim(adjustl(buf))//'.txt'
        open(63,file=flnm2)
        do n=1,nn
            write(63,*) f(n,k)
        end do
        close(63)
    enddo
else
    open(63,file=flnm)
    do n=1,nn
        write(63,*) f(n,1)
    end do
    close(63)
endif
end subroutine
!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
subroutine WriteScalarField(nn,f,flnm)
!subroutine WriteScalarField(nn,f,flnm)
! Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
! Inputs:
!     nn    (integer) number of rows in f 
!    f    (real(kind(1.0D0))) values to write to text file
!    flnm(character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
implicit none
integer, intent(in):: nn
real(kind(1.0D0)), intent(in):: f(*)
character (*), intent(in):: flnm
integer n
!--------------------------------------------------------------------------------------------------
open(63,file=trim(flnm))
do n=1,nn
    write(63,*) f(n)
end do
close(63)
end subroutine
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
subroutine write_csv(n,m,f,flnm,ndim)
!subroutine write_csv(n,m,f,flnm,ndim)
! Purpose: Write values of a matrrix (f) to a csv file in exponential format.
! Inputs:
!     n    (integer) number of rows in f 
!     m    (integer) number of columns in f
!    f    (real(kind(1.0D0))) values to write to csv file
!    flnm    (character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------

implicit none
integer, intent(in):: n,m,ndim
real(kind(1.0D0)), intent(in):: f(ndim,*)
character(*), intent(in)::flnm
integer k
character(100) buf,fmtstr

k=m-1
write(buf,'(I6)')k
!
! format for exponential format
!
fmtstr='('//trim(adjustl(buf))//'(ES14.7 : ", ") (ES14.7 : ))'
!fmtstr='('//trim(adjustl(buf))//'(E : ", ") (E : ))'
open(63,file=flnm)
do k=1,n
 !write(63,trim(fmtstr)) f(k,1:m)
 write(63,fmtstr) f(k,1:m)
enddo
close(63)

end subroutine

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------End Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
