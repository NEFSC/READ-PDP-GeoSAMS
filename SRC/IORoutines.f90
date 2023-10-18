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
    integer j,io
    character(72),intent(out):: init_cond_file_name
    character(2),intent(out):: domain_name
    character(3),intent(out):: fishing_type
    character(72) :: input_string, file_name, errFileName
    file_name='Scallop.inp'
    errFileName = 'InputDataError.txt'
    open(write_dev,file=errFileName)

    open(read_dev,file=file_name)
    do
        input_string=""
        read(read_dev,'(a)',iostat=io) input_string
        if (io.lt.0) exit

        select case (input_string(1:1))
            case('#')
                write(*,*)'Comment echo:',input_string
            case('D')
                j = scan(input_string,"=",back=.true.)
                domain_name=trim(adjustl(input_string(j+1:)))
                write(*,*)'Domain is ',domain_name
            case('I')
                j = scan(input_string,"=",back=.true.)
                init_cond_file_name=trim(adjustl(input_string(j+1:)))
                write(*,*)'Initial Conditions File Name =',init_cond_file_name
            case('B')
                j = scan(input_string,"=",back=.true.)
                write(*,* )input_string(j+1:)
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
                write(*,*)'Fishing Type = ',fishing_type
            case default
                write(write_dev,*) 'Unrecognized line in ',file_name
                write(write_dev,*) 'Unknown Line->',input_string
                !   stop
        end select

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
    character(72) :: input_string,file_name
    integer io,j
    file_name='Scallop.inp'

    open(read_dev,file=file_name)
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
                    !write(*,*) 'Unrecognized line in ',file_name
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
    character(72) :: input_string,file_name
    logical,intent(out) ::  IsStateOut,IsAbundanceOut,IsFOut,IsBMSOut,IsExplBMSOut,IsCashOut,IsRecOut
    file_name='Scallop.inp'

    k=0;
    open(read_dev,file=file_name)
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
!! subroutine Load_Grid(x,y,z,f,num_time_steps)
!! load grid coordinates and bathymetric depth from CSV file with 5 columns
!! representing an x coordinate, y, bathymetric depth (z), latitude, and
!! longitude.
!! @author Keston Smith (IBSS corp) June-July 2021
!!
!! @param[out] x (float double) x-coordinate of data
!! @param[out] y (float double) y-coordinate of data
!! @param[out] z (float double) bathymetric depth at (x,y)
!! @param[out] lat (float double) latitude at (x,y)
!! @param[out] lon (float double) longitude at (x,y)
!! @param[out] num_time_steps (integer)length of x,y,z,lat,lon (number of data point Time Steps)
!!
!! Note: At present lat and lon variabels are not used. 
subroutine Load_Grid(x, y, z, lat, lon, num_time_steps, E, ne, managementRegion, Clop, domain_name)
    use globals

    implicit none
    real(dp) lat(*),lon(*),x(*),y(*),z(*)
    integer n, num_time_steps, ne, io, managementRegion(*), E(4,*), Clop(*)
    character(72) input_str,file_name
    character(2) domain_name
    write(*,*) 'Domain Name: ', domain_name
    file_name='Grids/'//domain_name//'xyzLatLon.csv'
    write(*,*)'reading grid file: ',trim(file_name)
    open(63,file=trim(file_name),status='old')
    n=0
    do
     read(63,'(a)',iostat=io) input_str
     if (io.lt.0) exit
     n=n+1
     read(input_str,*) x(n),y(n),z(n),lat(n),lon(n)
    end do
    close(63)
    num_time_steps=n

    file_name='Grids/ManagementArea'//domain_name//'.txt'
    write(*,*)'reading grid file: ',trim(file_name)
    open(63,file=trim(file_name),status='old')
    n=0
    do n=1,num_time_steps
     read(63,'(a)',iostat=io) input_str
     if (io.lt.0) exit
     read(input_str,*) managementRegion(n)
    end do
    close(63)

    file_name='Grids/'//domain_name//'squares.csv'
    write(*,*)'reading grid file: ',trim(file_name)
    open(63,file=trim(file_name),status='old')
    n=0
    do
     read(63,'(a)',iostat=io) input_str
     if (io.lt.0) exit
     n=n+1
     read(input_str,*) E(1,n),E(2,n),E(3,n),E(4,n)
    end do
    close(63)
    ne=n

    if (domain_name(1:2).eq.'GB')then
        Clop(1:num_time_steps)=1
        do n=1,num_time_steps
            if( any ((/ managementRegion(n).eq.8, managementRegion(n).eq.5,&
                        managementRegion(n).eq.10 /)))then
                Clop(n)=0
            endif
        enddo
    endif
    if (domain_name(1:2).eq.'MA')then
        Clop(1:num_time_steps)=0
        do n=1,num_time_steps
            if( any ((/ managementRegion(n).eq.3, managementRegion(n).eq.4,&
                        managementRegion(n).eq.5, managementRegion(n).eq.7 /)))then
                Clop(n)=1
            endif
        enddo
    endif

    return
endsubroutine Load_Grid
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! subroutine Load_Data(x,y,z,f,num_time_steps)
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
! num_time_steps (integer)length of x,y, and z (number of data poinumTimeSteps)
!
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine Load_Data(x,y,z,f,num_time_steps,file_name)
    use globals
    implicit none
    real(dp), intent(out):: x(*),y(*),z(*),f(*)
    integer, intent(out):: num_time_steps
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
    num_time_steps=n
    return
endsubroutine Load_Data
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------

!subroutine Read_Scalar_Field(file_name, M, num_time_steps)
!Purpose: Read real valued scaler field, M, from file file_name.
! Inputs:
!  file_name (charecter*72) number of rows in D, Gamma
! Outputs:
!  M    (real(dp))       length num_time_steps vector of values
! num_time_steps   (integer)     length of M
!-----------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine Read_Scalar_Field(file_name, M, num_time_steps)
    use globals
    implicit none
    integer, intent(inout):: num_time_steps
    real(dp), intent(out):: M(*)
    character(*), intent(in)::file_name
    integer n,io
    character(72) input_str

    open(read_dev,file=trim(file_name),status='old')
    n=0
    do
     read(read_dev,'(a)',iostat=io) input_str
     if (io.lt.0) exit
     n=n+1
     read(input_str,*) M(n)
    end do
    close(read_dev)
    num_time_steps=n

    return
endsubroutine Read_Scalar_Field

!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
!subroutine Write_Scalar_Field(num_time_steps,f,file_name)
! Purpose: Write columns of a matrrix (f) to a series of text files in exponential format.
! Inputs:
!  num_time_steps (integer) number of rows in f 
! f (real(dp)) values to write to text file
! file_name(character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!--------------------------------------------------------------------------------------------------
subroutine Write_Scalar_Field(num_time_steps,f,file_name)
    use globals
    implicit none
    integer, intent(in):: num_time_steps
    real(dp), intent(in):: f(*)
    character (*), intent(in):: file_name
    integer  n
    open(write_dev,file=trim(file_name))
    do n=1,num_time_steps
        write(write_dev,*) f(n)
    end do
    close(write_dev)
endsubroutine Write_Scalar_Field
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
!subroutine Write_CSV(n,m,f,file_name,ndim)
! Purpose: Write values of a matrrix (f) to a csv file in exponential format.
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
!  nndim    (integer) leading dimension of M 
!  nf      (integer) number of columns in f
!   Output:
!   M (real(dp))    values read from csv file
!  num_time_steps (integer) number of rows in M 
!--------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------
! Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
subroutine Read_CSV(num_time_steps, nf, file_name, M, nndim)
    use globals
    implicit none
    integer, intent(in):: nf,nndim
    integer, intent(out):: num_time_steps
    real(dp), intent(out):: M(nndim,*)
    real(dp) tmp(nf)
    character (*), intent(in):: file_name
    integer n,io

    open(read_dev,file=file_name,status='old')
    n=0
    do
        read(read_dev,*,iostat=io) tmp(1:nf)
        if (io.lt.0) exit
        n=n+1
        M(n,1:nf)=tmp(1:nf)
    end do
    close(read_dev)
    num_time_steps=n
    return
endsubroutine Read_CSV
!-----------------------------------------------------------------------

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------End Input/Output Subroutines-----------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
