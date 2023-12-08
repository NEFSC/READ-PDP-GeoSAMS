!-----------------------------------------------------------------------
!> Read_Input
!! @brief Read Input File
!! 
!! Reads a configuration file, 'Scallop.inp', to set data parameters for simulation
!!
!! 
!! @param[out] domain_name can be either 
!!             MA MidAtlantic or 
!!             GB GeorgesBank
!! @param[out] init_cond_file_name File name that contains intial simulation conditions
!! @param[out] start_year Starting year for simulation read from config file
!! @param[out] stop_year  End year for simulation read from config file
!! @param[out] fishing_type Fishing can be USD, BMS, or, CAS
!! @param[out] num_time_steps Number of times steps to evaluate growth
!! @param[out] num_monte_carlo_iter Number of iterations for Monte Carlo simulation
!-----------------------------------------------------------------------
subroutine Read_Input(domain_name, init_cond_file_name, start_year, stop_year, fishing_type,num_time_steps,num_monte_carlo_iter)
    use globals
    implicit none
    integer, intent(out) :: start_year, stop_year, num_time_steps, num_monte_carlo_iter
    integer j,io  !,k
    character(72),intent(out):: init_cond_file_name
    character(2),intent(out):: domain_name
    character(3),intent(out):: fishing_type
    character(72) :: input_string

    open(write_dev,file=error_file_name)

    open(read_dev,file=sim_input_fname)
    do
        input_string=""
        read(read_dev,'(a)',iostat=io) input_string
        if (io.lt.0) exit

        ! TODO
        ! if (input_string(1:6).eq.'Output')then
        !     ! call Read_Output_Flags()
        
        ! else
  
            select case (input_string(1:1))
                case('#')
                !     write(*,*)'Comment echo:',input_string
                case('D')
                    j = scan(input_string,"=",back=.true.)
                    domain_name=trim(adjustl(input_string(j+1:)))
                    if (.not. ( any ((/ domain_name.eq.'MA', domain_name.eq.'GB'/)) )) then
                        write(*,*) term_red, ' **** INVALID DOMAIN NAME: ', domain_name, term_blk
                        stop
                    endif
                case('I')
                    j = scan(input_string,"=",back=.true.)
                    init_cond_file_name=trim(adjustl(input_string(j+1:)))
                    write(*,*)'Initial Conditions File Name =',init_cond_file_name
                case('B')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )start_year
                case('E')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )stop_year
                case('T')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )num_time_steps
                case('N')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )num_monte_carlo_iter
                case('F')
                    j = scan(input_string,"=",back=.true.)
                    fishing_type=trim(adjustl(input_string(j+1:)))
                    if (.not. ( any ((/ fishing_type.eq.'USD', fishing_type.eq.'BMS', fishing_type.eq.'CAS'/)) )) then
                        write(*,*) term_red, ' **** INVALID FISHING TYPE: ', fishing_type, term_blk
                        stop
                    endif
                case default
                    write(write_dev,*) 'Unrecognized line in ',sim_input_fname
                    write(write_dev,*) 'Unknown Line->',input_string
                    stop
            end select

        ! endif

    end do
    close(read_dev)
    close(write_dev)
    return
end subroutine Read_Input

subroutine Read_Recruit_Input(RecStartYD,RecStopYD,RandomStartYear,NRecruitFields)
    use globals
    implicit none
    real(dp), intent(out) :: RecStartYD,RecStopYD
    integer, intent(out) :: RandomStartYear,NRecruitFields
    character(72) :: input_string
    integer io,j

    open(read_dev,file=sim_input_fname)
    do
        input_string=""
        read(read_dev,'(a)',iostat=io) input_string
        if (io.lt.0) exit
        if(input_string(1:1).eq.'R') then
            select case (input_string(13:13))
                case('S')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )RecStartYD
                    write(*,*)'Recruit settlement start year day=',RecStartYD
                case('F')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )RecStopYD
                    write(*,*)'Recruit settlement stop year day=',RecStopYD
                case('R')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )RandomStartYear
                    write(*,*)'Begin Random Recruitment=',RandomStartYear
                case('N')
                    j = scan(input_string,"=",back=.true.)
                    read( input_string(j+1:),* )NRecruitFields
                    write(*,*)'Number of Random Recruitment fields per year=',NRecruitFields
                case default
                    !write(*,*) 'Unrecognized line in ',sim_input_fname
                    !write(*,*) 'Unkown Line->',input_string
                end select
        endif
    end do
    close(read_dev)
    return
endsubroutine Read_Recruit_Input


subroutine Read_Output_Flags(nperc,P,IsStateOut,IsAbundanceOut,IsFOut,IsBMSOut,IsExplBMSOut,IsCashOut,IsRecOut)
    use globals
    implicit none
    integer j,io,k
    real(dp),intent(out):: P(*)
    integer,intent(out):: nperc
    character(72) :: input_string
    logical,intent(out) ::  IsStateOut,IsAbundanceOut,IsFOut,IsBMSOut,IsExplBMSOut,IsCashOut,IsRecOut

    k=0;
    open(read_dev,file=sim_input_fname)
    do
        input_string=""
        read(read_dev,'(a)',iostat=io) input_string
        if (io.lt.0) exit
          write(*,*)input_string
        if (input_string(1:6).eq.'Output')then
          write(*,*)'B',input_string
            select case (input_string(8:8))
            case('#')
                write(*,*)'Comment echo:',input_string
            case('P')
                j = scan(input_string,"=",back=.true.)
                k=k+1
                read( input_string(j+1:),* )P(k)
                write(*,*)'Percentiles for output',P(1:k)
            case('S')
                j = scan(input_string,"=",back=.true.)
                read( input_string(j+1:),* )IsStateOut
                write(*,*)'IsStateOut:',IsStateOut
            case('A')
                j = scan(input_string,"=",back=.true.)
                read( input_string(j+1:),* )IsAbundanceOut
                write(*,*)'IsAbundanceOut:',IsAbundanceOut
            case('F')
                j = scan(input_string,"=",back=.true.)
                read( input_string(j+1:),* )IsFOut
                write(*,*)'IsFOut:',IsFOut
            case('B')
                j = scan(input_string,"=",back=.true.)
                read( input_string(j+1:),* )IsBMSOut
                write(*,*)'IsBMSOut:',IsBMSOut
            case('E')
                j = scan(input_string,"=",back=.true.)
                read( input_string(j+1:),* )IsExplBMSOut
                write(*,*)'IsExplBMSOut:',IsExplBMSOut
            case('C')
                j = scan(input_string,"=",back=.true.)
                read( input_string(j+1:),* )IsCashOut
                write(*,*)'IsCashOut:',IsCashOut
            case('R')
                j = scan(input_string,"=",back=.true.)
                read( input_string(j+1:),* )IsRecOut
                write(*,*)'IsRecOut:',IsRecOut
            case default
                write(*,*) 'Unkown Output Line->',input_string
            end select
        endif
    end do
    close(read_dev)
    nperc=k
    return
endsubroutine Read_Output_Flags

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------User Subroutines-----------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

!> 
!! function Load_Grid()
!! load grid coordinates and bathymetric depth from CSV file with 5 columns
!! representing an x coordinate, y, bathymetric depth (z), latitude, and
!! longitude.
!! @author Keston Smith (IBSS corp) June-July 2021
!!
!! @param[in,out] grid Map of how management area is subdivided
!! @param[in] domain_name Area being managed
!!
!! TODO: At present lon variables are not used. 
integer function Load_Grid(grid, domain_name)
    use globals
    use Data_Point_Mod
    implicit none
    type(Data_Vector_Class), intent(inout) :: grid
    character(2), intent(in) :: domain_name
    integer n, io, num_grids
    character(72) input_str,file_name
    write(*,*) 'Domain Name: ', domain_name
    file_name='Grids/'//domain_name//'xyzLatLon.csv'
    write(*,*)'reading grid file: ',trim(file_name)
    open(63,file=trim(file_name),status='old')
    n=0
    do
     read(63,'(a)',iostat=io) input_str
     if (io.lt.0) exit
     n=n+1
     ! merged <domain_name>xyzLatLon and ManagementArea<domain_name>
     !read(input_str,*) x(n),y(n),z(n),lat(n),lon(n),management_region(n)
     read(input_str,*) grid%posn(n)%x, grid%posn(n)%y, &
     &                 grid%posn(n)%z, grid%posn(n)%lat, grid%posn(n)%lon, grid%posn(n)%mgmt_area_index
    end do
    close(63)
    num_grids=n
    write(*,*) 'READ ', n, 'LINE(S)'

    if (domain_name .eq. 'GB')then
        grid%posn(1:num_grids)%is_closed = .TRUE.
        do n=1,num_grids
            if( any ((/grid%posn(n)%mgmt_area_index.eq.8, grid%posn(n)%mgmt_area_index.eq.5,&
            &           grid%posn(n)%mgmt_area_index.eq.10 /)))then
                grid%posn(n)%is_closed = .FALSE.
            endif
        enddo
    else
        grid%posn(1:num_grids)%is_closed = .FALSE.
        do n=1,num_grids
            if( any ((/ grid%posn(n)%mgmt_area_index.eq.3, grid%posn(n)%mgmt_area_index.eq.4,&
            &           grid%posn(n)%mgmt_area_index.eq.5, grid%posn(n)%mgmt_area_index.eq.7 /)))then
                grid%posn(n)%is_closed = .TRUE.
            endif
        enddo
    endif
    grid%len = num_grids
    Load_Grid = num_grids

    return
endfunction Load_Grid
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! subroutine Load_Data(x,y,z,f,vector_len)
! Load data from CSV file with 4 columns representing an x coordinate, y
! coordinate, bathymetric depth (z), and a scaller field f.
!
! Inputs: 
! none
!
! Outputs:
! x (real(dp)) x-coordinate of data
! y (real(dp)) y-coordinate of data
! z (real(dp)) bathymetric depth at (x,y)
! f (real(dp)) scalar data at (x,y)
! vector_len (integer)length of x,y, and z (number of data poinumTimeSteps)
!
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine Load_Data(x,y,z,f,vector_len,file_name)
    use globals
    implicit none
    real(dp), intent(out):: x(*),y(*),z(*),f(*)
    integer, intent(out):: vector_len
    character (*), intent(in)::  file_name
    real(dp) year
    integer n,io
    character(72) input_str

    !file_name='recruitsOBS.csv'
    open(read_dev,file=file_name,status='old')
    n=0
    read(read_dev,*) input_str
    do
     read(read_dev,'(a)',iostat=io) input_str
     if (io.lt.0) exit
     n=n+1
     read(input_str,*) year, x(n),y(n),z(n),f(n)
    ! read(input_str,*)  x(n),y(n),z(n),f(n)
    end do
    close(read_dev)
    vector_len=n
    return
endsubroutine Load_Data
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

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
    character(72) input_str

    !PRINT '(A,A,A,A)', term_blu, ' READING FILE: ', file_name, term_blk

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

!--------------------------------------------------------------------------------------------------
!subroutine Write_Scalar_Field(vector_len,f,file_name)
! Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
! Inputs:
!  vector_len (integer) number of rows in f 
! f (real(dp)) values to write to text file
! file_name(character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Write_Scalar_Field(vector_len,f,file_name)
    use globals
    implicit none
    integer, intent(in):: vector_len
    real(dp), intent(in):: f(*)
    character (*), intent(in):: file_name
    integer  n

    !PRINT '(A,A,A,A)', term_blu, ' WRITING FILE: ', file_name, term_blk

    open(write_dev,file=trim(file_name))
    do n=1,vector_len
        write(write_dev,*) f(n)
    end do
    close(write_dev)
endsubroutine Write_Scalar_Field
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
!subroutine Write_CSV(n,m,f,file_name,ndim)
! Purpose: Write values of a matrix (f) to a csv file in exponential format.
! Inputs:
!  n (integer) number of rows in f 
!  m (integer) number of columns in f
! f (real(dp)) values to write to csv file
! file_name (character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Write_CSV(n,m,f,file_name,ndim)
    use globals
    implicit none
    integer, intent(in):: n,m,ndim
    real(dp), intent(in):: f(ndim,*)
    character(*), intent(in)::file_name
    integer  k
    character(100) buf,fmtstr
    k=m-1
    write(buf,'(I6)')k
    !
    ! format for exponential format
    !
    fmtstr='('//trim(adjustl(buf))//'(ES14.7 : ", ") (ES14.7 : ))'
    !fmtstr='('//trim(adjustl(buf))//'(E : ", ") (E : ))'
    open(write_dev,file=file_name)
    do k=1,n
     !write(write_dev,trim(fmtstr)) f(k,1:m)
     write(write_dev,fmtstr) f(k,1:m)
         enddo
    close(write_dev)
endsubroutine Write_CSV

!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
!subroutine Write_CSV_H(n,m,f,file_name,ndim)
! Purpose: Write values of a matrrix (f) to a csv file in exponential format.
! Inputs:
!  n (integer) number of rows in f 
!  m (integer) number of columns in f
! f (real(dp)) values to write to csv file
! file_name (character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Write_CSV_H(n,m,f,file_name,ndim,header)
    use globals
    implicit none
    integer, intent(in):: n,m,ndim
    real(dp), intent(in):: f(ndim,*)
    character(*), intent(in)::file_name,header
    integer  k
    character(100) buf,fmtstr
    k=m-1
    write(buf,'(I6)')k
    !
    ! format for exponential format
    !
    fmtstr='('//trim(adjustl(buf))//'(ES14.7 : ", ") (ES14.7 : ))'
    !fmtstr='('//trim(adjustl(buf))//'(E : ", ") (E : ))'
    open(write_dev,file=file_name)
    write(write_dev,'(A)') trim(adjustl( header))
    do k=1,n
    !write(write_dev,trim(fmtstr)) f(k,1:m)
    write(write_dev,fmtstr) f(k,1:m)
    enddo
    close(write_dev)
endsubroutine Write_CSV_H
!--------------------------------------------------------------------------------------------------


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
        if (n > nndim) then
            n = n - 1
            PRINT '(A,A,I7,A,A,A)', term_yel,' *** WARNING:  Stopped at', n, ' rows ',term_blk, file_name
            exit
        endif
        read(read_dev,*,iostat=io) tmp(1:num_cols)
        if (io.lt.0) then
            if (n .EQ. 1) PRINT '(A,A,I7,A,A,A)', term_red,' *** FILE IO ERROR', io, ' reading file: ', term_blk, file_name
            n = n - 1
            exit
        endif
        M(n,1:num_cols) = tmp(1:num_cols)
    end do
    close(read_dev)
    num_rows = n
    ! if (num_rows <= nndim) then
    !     PRINT '(A,A,I7,A,A,A)', term_blu,' Read ', num_rows, ' rows from file: ', term_blk, file_name
    ! endif
    return
endsubroutine Read_CSV
!-----------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------End Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
