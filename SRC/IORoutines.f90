!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------
!subroutine Read_Scalar_Field(file_name, M, vector_len)
!Purpose: Read real valued scaler field, M, from file file_name.
! The number of data points may be greater than desired, so limits number to vector_len.
! Inputs:
!  file_name (charecter*72) number of rows in D, Gamma
! Outputs:
!  M    (real(dp))       length vector_len vector of values
! vector_len   (integer)     length of M
!-----------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine Read_Scalar_Field(file_name, M, vector_len)
    use globals
    implicit none
    integer, intent(inout):: vector_len
    real(dp), intent(out):: M(*)
    character(*), intent(in)::file_name
    integer n,io
    character(input_str_len) input_str

    PRINT '(A,A,A,A)', term_blu, ' READING FILE: ', file_name, term_blk

    open(read_dev,file=trim(file_name),status='old')
    n=0
    do while (n < vector_len)
        read(read_dev,'(a)',iostat=io) input_str
        if (io.lt.0) exit
        n=n+1
        read(input_str,*) M(n)
    end do
    close(read_dev)
    vector_len=n

    return
endsubroutine Read_Scalar_Field

!--------------------------------------------------------------------------------------------------
!> Purpose: Write columns of a matrix (f) to a series of text files in exponential format.
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
endsubroutine Write_2D_Scalar_Field
    
!--------------------------------------------------------------------------------------------------
!subroutine Write_Vector_Scalar_Field(vector_len,f,file_name)
! Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
! Inputs:
!  vector_len (integer) number of rows in f 
! f (real(dp)) values to write to text file
! file_name(character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Write_Vector_Scalar_Field(vector_len,f,file_name)
use globals
implicit none
integer, intent(in):: vector_len
real(dp), intent(in):: f(*)
character (*), intent(in):: file_name
integer  n

open(write_dev,file=trim(file_name))
do n=1,vector_len
    write(write_dev,*) f(n)
end do
close(write_dev)
endsubroutine Write_Vector_Scalar_Field

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
subroutine Write_CSV(n,m,f,file_name,nndim, append)
use globals
implicit none
integer, intent(in):: n,m,nndim
real(dp), intent(in):: f(nndim,*)
character(*), intent(in)::file_name
logical, intent(in) :: append
integer k
character(fname_len) buf,fmtstr
character(1) cr

k=m-1
write(buf,'(I6)')k
!
! format for exponential format
!
if (append) then
    if (k.LE.0) then
        fmtstr='(ES14.7 : )'//NEW_LINE(cr)
    else
        fmtstr='('//trim(adjustl(buf))//'(ES14.7 : ", ") (ES14.7 : ))'//NEW_LINE(cr)
    endif
    open(69,file=file_name, status="old", position="append", action="write")
else
    if (k.LE.0) then
        fmtstr='(ES14.7 : )'
    else
        fmtstr='('//trim(adjustl(buf))//'(ES14.7 : ", ") (ES14.7 : ))'
    endif
    open(69,file=file_name)
endif
do k=1,n
    write(69,fmtstr) f(k,1:m)
enddo
close(69)
endsubroutine Write_CSV
!===========================================================================================================
! Only used for debugging special access settings
!-----------------------------------------------------------------------------------------------------------
! subroutine Write_CSV_Logical(n,m,f,file_name,nndim, append)
! use globals
! implicit none
! integer, intent(in):: n,m,nndim
! logical, intent(in):: f(nndim,*)
! character(*), intent(in)::file_name
! logical, intent(in) :: append
! integer k
! character(fname_len) buf,fmtstr
! character(1) cr
    
! k=m-1
! write(buf,'(I6)')k
! !
! ! format for exponential format
! !
! if (append) then
!     if (k.LE.0) then
!         fmtstr='(L1 : )'//NEW_LINE(cr)
!     else
!         fmtstr='('//trim(adjustl(buf))//'(L1 : ", ") (L1 : ))'//NEW_LINE(cr)
!     endif
!     open(69,file=file_name, status="old", position="append", action="write")
! else
!     if (k.LE.0) then
!         fmtstr='(L1 : )'
!     else
!         fmtstr='('//trim(adjustl(buf))//'(L1 : ", ") (L1 : ))'
!     endif
!     open(69,file=file_name)
! endif
! do k=1,n
!     write(69,fmtstr) f(k,1:m)
! enddo
! close(69)
! endsubroutine Write_CSV_Logical

! subroutine Write_CSV_Int(n,m,f,file_name,nndim, append)
! use globals
! implicit none
! integer, intent(in):: n,m,nndim
! integer, intent(in):: f(nndim,*)
! character(*), intent(in)::file_name
! logical, intent(in) :: append
! integer k
! character(fname_len) buf,fmtstr
! character(1) cr
    
! k=m-1
! write(buf,'(I6)')k
! !
! ! format for exponential format
! !
! if (append) then
!     if (k.LE.0) then
!         fmtstr='(I2 : )'//NEW_LINE(cr)
!     else
!         fmtstr='('//trim(adjustl(buf))//'(I2 : ", ") (I2 : ))'//NEW_LINE(cr)
!     endif
!     open(69,file=file_name, status="old", position="append", action="write")
! else
!     if (k.LE.0) then
!         fmtstr='(I2 : )'
!     else
!         fmtstr='('//trim(adjustl(buf))//'(I2 : ", ") (I2 : ))'
!     endif
!     open(69,file=file_name)
! endif
! do k=1,n
!     write(69,fmtstr) f(k,1:m)
! enddo
! close(69)
! endsubroutine Write_CSV_Int
!===========================================================================================================

!--------------------------------------------------------------------------------------------------
!subroutine Write_Column_CSV(n,m,f,file_name,nndim)
! Purpose: Write values of a matrix (f) to a csv file in exponential format by column rather than row
! Inputs:
!  n (integer) number of rows in f 
!  m (integer) number of columns in f
!  header string to write as a column header
!  f (real(dp)) values to write to csv file
! file_name (character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Tom Callaghan
!--------------------------------------------------------------------------------------------------
subroutine Write_Column_CSV(n,f,header,file_name,append)
use globals
implicit none
integer, intent(in):: n
real(dp), intent(in):: f(*)
character(*), intent(in) :: header
character(*), intent(in) :: file_name
logical, intent(in) :: append
integer k, io
character(fname_len) fmtstr
character(1) cr
character(5000) input_str
character(5000) output_str

if (append) then
    ! create a temp file to write output then move temp to file_name
    open(read_dev, file=file_name, status='old')

    open(unit=write_dev, iostat=io, file=output_dir//'TEMP', status='replace')
    ! read and write header row
    read(read_dev,'(A)',iostat=io) input_str
    write(output_str,'(A,A,A)') trim(input_str),',',header
    write(write_dev, '(A)'//NEW_LINE(cr)) trim(output_str)
    do k=1,n
        read(read_dev,'(A)',iostat=io) input_str
        ! sometimes f(k) takes on the value of E-311, which can not be read back in
        ! seems the minimum value is E-300, even with using format ES15.7E3 
        if (((f(k) > 0.D0) .AND. (f(k) < zero_threshold)) .OR. (f(k) .GT. 1.D0/zero_threshold)) then
            write(output_str,'(A,A,(ES14.7 : ))') trim(input_str),',',0.D0
        else
            write(output_str,'(A,A,(ES14.7 : ))') trim(input_str),',',f(k)
        endif
        write(write_dev, '(A)'//NEW_LINE(cr)) trim(output_str)
    enddo
    close(write_dev)
    close(read_dev)

    open(read_dev, file=output_dir//'TEMP', status='old')
    open(unit=write_dev, iostat=io, file=file_name, status='replace')
    do k=1,n+1 ! including header row
        read(read_dev,'(A)',iostat=io) input_str
        write(write_dev,'(A)') trim(input_str)
    enddo
    close(write_dev)
    close(read_dev, status='delete')

else
    fmtstr='(ES14.7 : )'//NEW_LINE(cr)
    open(write_dev,file=file_name)
    write(write_dev, '(A)') header
    do k=1,n
        ! sometimes f(k) takes on the value of E-311, which can not be read back in
        ! seems the minimum value is E-300, even with using format ES15.7E3 
        if (((f(k) > 0.D0) .AND. (f(k) < zero_threshold)) .OR. (f(k) .GT. 1.D0/zero_threshold)) then
            write(write_dev,fmtstr) 0.D0
        else
            write(write_dev,fmtstr) f(k)
        endif
    enddo
    close(write_dev)
endif

endsubroutine Write_Column_CSV
    

!--------------------------------------------------------------------------------------------------
! subroutine Read_CSV
! read a generic csv file.
!   
! file_name  (character(72)) filename to write f read from csv format
!  nndim    (integer) max dimension of M 
!  num_cols      (integer) number of columns in f
!   Output:
!   M (real(dp))    values read from csv file
!  num_rows (integer) number of rows in M 
!--------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine Read_CSV(num_rows, num_cols, file_name, M, nndim)
    use globals
    implicit none
    integer, intent(in):: num_cols,nndim
    integer, intent(out):: num_rows
    real(dp), intent(out):: M(nndim,*)
    real(dp) tmp(num_cols)
    character (*), intent(in):: file_name
    integer n,io
    
    open(read_dev, file=file_name, status='old')
    n=0
    do
        n = n + 1
        read(read_dev,*,iostat=io) tmp(1:num_cols)
        if (io.lt.0) then
            if (n .EQ. 1) PRINT '(A,A,I7,A,A,A)', term_red,' *** FILE IO ERROR', io, ' reading file: ', term_blk, file_name
            n = n - 1
            exit
        endif
        if (n > nndim) then
            n = n - 1
            PRINT '(A,A,I7,A,A,A)', term_yel,' *** WARNING:  Stopped at', n, ' rows ',term_blk, file_name
            exit
        endif
        M(n,1:num_cols) = tmp(1:num_cols)
    end do
    close(read_dev)
    num_rows = n
    return
endsubroutine Read_CSV
!-----------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------End Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
