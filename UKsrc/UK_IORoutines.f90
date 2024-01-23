
!-----------------------------------------------------------------------
!> Purpose: Read real valued scaler field, M, from file flnm.
!> Inputs:
!> -    flnm (charecter*72)    number of rows in D, Gamma
!> Outputs:
!> -    M    (real(dp))       length nn vector of values
!. -   nn   (integer)        length of M
!>
!> @author  Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine Read_CSV(flnm, M, nn)
use globals
implicit none

    integer, intent(inout):: nn
    real(dp), intent(out):: M(*)
    character (*), intent(in)::flnm
    integer n,io
    character(72) input_str
    write(*,*)'Read_CSV flnm=',flnm
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
!> Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
!> Inputs:
!> -    nn    (integer) number of rows in f 
!> -    nsim(integer) number of columns in f
!> -   f    (real(dp)) values to write to text file
!> -   flnm(character(72)) filename to write f to in csv format
!> -  nndim(integer) leading dimension of f
!>
!> @author  Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Write_2D_Scalar_Field(nn,nsim,f,flnm,nndim)
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
!> Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
!> Inputs:
!> -  nn    (integer) number of rows in f 
!> -   f    (real(dp)) values to write to text file
!> -  flnm  (character(72)) filename to write f to in csv format
!>
!> @author  Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Write_Vector_Scalar_Field(nn,f,flnm)
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
!> Purpose: Write values of a matrix (f) to a csv file in exponential format.
!> Inputs:
!> -    n    (integer) number of rows in f 
!> -    m    (integer) number of columns in f
!> -   f    (real(dp)) values to write to csv file
!> -   flnm    (character(72)) filename to write f to in csv format
!>
!> @author Keston Smith (IBSS corp) June-July 2021
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
