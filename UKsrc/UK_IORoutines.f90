
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
use globals
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
real(dp),intent(out)::  fmax,alpha
IsClimEst=.false.
IsHiLimit=.false.
open(69,file='UK.inp')
do
    InputStr=""
    read(69,'(a)',iostat=io) InputStr
    if (io.lt.0) exit

    select case (InputStr(1:1))
        case('#')
            !write(*,*)'Comment echo:',InputStr
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

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------
!subroutine readsf(flnm, M, nn)
!Purpose: Read real valued scaler field, M, from file flnm.
! Inputs:
!     flnm (charecter*72)    number of rows in D, Gamma
! Outputs:
!     M    (real(dp))       length nn vector of values
!    nn   (integer)        length of M
!-----------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine readsf(flnm, M, nn)
use globals
implicit none

    integer, intent(inout):: nn
    real(dp), intent(out):: M(*)
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
!subroutine scalar_fields(nn,nsim,f,flnm,nndim)
! Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
! Inputs:
!     nn    (integer) number of rows in f 
!     nsim(integer) number of columns in f
!    f    (real(dp)) values to write to text file
!    flnm(character(72)) filename to write f to in csv format
!   nndim(integer) leading dimension of f
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine write_scalar_fields(nn,nsim,f,flnm,nndim)
use globals
implicit none
    
    integer, intent(in):: nn,nsim,nndim
    real(dp), intent(in):: f(nndim,*)
    character (*), intent(in):: flnm
    integer k,n
    character(72) buf,flnm2
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
!subroutine WriteScalarField(nn,f,flnm)
! Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
! Inputs:
!     nn    (integer) number of rows in f 
!    f    (real(dp)) values to write to text file
!    flnm(character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine WriteScalarField(nn,f,flnm)
    use globals
    implicit none

    integer, intent(in):: nn
    real(dp), intent(in):: f(*)
    character (*), intent(in):: flnm
    integer n
    open(63,file=trim(flnm))
    do n=1,nn
        write(63,*) f(n)
    end do
    close(63)
end subroutine

!--------------------------------------------------------------------------------------------------
!subroutine write_csv(n,m,f,flnm,nndim)
! Purpose: Write values of a matrrix (f) to a csv file in exponential format.
! Inputs:
!     n    (integer) number of rows in f 
!     m    (integer) number of columns in f
!    f    (real(dp)) values to write to csv file
!    flnm    (character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine write_csv(n,m,f,flnm,nndim)
use globals
implicit none

    integer, intent(in):: n,m,nndim
    real(dp), intent(in):: f(nndim,*)
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
