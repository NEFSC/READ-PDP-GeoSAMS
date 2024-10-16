!--------------------------------------------------------------------------------------------------
!> @page page6 Common Parameters
!>
!>
!--------------------------------------------------------------------------------------------------
! Tom Callaghan (IBSS) 2024
!--------------------------------------------------------------------------------------------------
module globals
implicit none

integer, parameter :: sp = selected_real_kind(6, 37)
integer, parameter :: dp = selected_real_kind(15, 307)
integer, parameter :: qp = selected_real_kind(33, 4931)

integer, parameter :: nDim = 12000
integer, parameter :: shell_len_max = 150
integer, parameter :: shell_len_min = 30
integer, parameter :: shell_len_delta = 5
integer, parameter :: num_size_classes = (shell_len_max - shell_len_min) / shell_len_delta + 1

integer, parameter :: max_num_years  = 50
integer, parameter :: max_num_areas = 25
integer, parameter :: max_sides = 8
integer, parameter :: region_none=0
integer, parameter :: region_N=1
integer, parameter :: region_S=2
integer, parameter :: region_SW=3
integer, parameter :: region_W=4
integer, parameter :: region_MA=5
integer, parameter :: region_GBK = 1
integer, parameter :: region_MAB = 5

! used by configuration files
integer, parameter :: tag_len = 40
integer, parameter :: value_len = 30
integer, parameter :: comment_len = 80
integer, parameter :: line_len = tag_len+value_len+comment_len
integer, parameter :: fname_len = 100
integer, parameter :: form_len = 20
integer, parameter :: input_str_len = 100
integer, parameter :: csv_line_len = 2000
integer, parameter :: domain_len = 2

integer, parameter :: read_dev = 69
integer, parameter :: write_dev = 63

! arbritrary value chosen as smallest with 2 digit exponent
real(dp), parameter :: zero_threshold = 1.0D-99 

! ASIN incorrectly produces error
! Error: Fortran 2003: Elemental function as initialization expression with non-integer/non-character arguments
real(dp), parameter :: pi = 3.14159265358979311599796346854D0 ! 2._dp * ASIN(1._dp)

! metric equivalents
real(dp), parameter :: grams_per_pound = 453.592_dp
real(dp), parameter :: meters_per_naut_mile = 1852.D0
real(dp), Parameter :: feet_per_naut_mile = 6076.12
real(dp), parameter :: grams_per_metric_ton = 1000000._dp
real(dp), parameter :: grid_area_sqm = meters_per_naut_mile**2

real(dp), parameter :: tow_area_sqm = 4516._dp
real(dp), parameter :: one_scallop_per_tow = 1.D0 / tow_area_sqm ! 1 scallop per tow, prevents log(0)
real(dp), parameter :: ma_gb_border = -70.5
! Adding a leap year every 4 years adds 1/4 to the 365-day year 
!     or increases the average year-length by 0.25 days.
! Omitting a leap year every 100 years subtracts 1/100 = 0.01 days from the 365-day year.
! Adding a leap year every 400 years adds 1/400 = 0.0025 days to the 365-day year.
!
real(dp), parameter :: days_in_year=365+0.25-0.01+0.0025

! colors taken from https://i.stack.imgur.com/9UVnC.png
character(*), parameter :: term_red = ''//achar(27)//'[31m'
character(*), parameter :: term_yel = ''//achar(27)//'[33m'
character(*), parameter :: term_grn = ''//achar(27)//'[92m'
character(*), parameter :: term_blu = ''//achar(27)//'[94m'
character(*), parameter :: term_blk = ''//achar(27)//'[0m'


character(*), parameter :: init_cond_dir = 'InitialCondition/'
character(*), parameter :: growth_out_dir = 'GrowthOutput/'
character(*), parameter :: rec_input_dir = 'RecruitEstimates/'
character(*), parameter :: rec_output_dir = 'RecruitField/'
character(*), parameter :: output_dir = 'Results/'
character(*), parameter :: config_dir_sim = 'Configuration/Simulation/'
character(*), parameter :: config_dir_interp = 'Configuration/Interpolation/'
character(*), parameter :: config_dir_special = 'Configuration/SpecialAccess/'
character(*), parameter :: grid_dir = 'Grids/'
character(*), parameter :: data_dir = 'Data/'

integer, parameter :: num_regions = 2
!character(3) :: rgn(num_regions) = (/ '_N ', '_S ', '_SW', '_W ', '_MA'/)
character(3) :: rgn(num_regions) = (/ '_GB', '_MA'/)

CONTAINS

elemental real(dp) function Logic_To_Double (value)
    logical, intent(in) :: value
    if (value) then
        Logic_To_Double = 1._dp
    else
        Logic_To_Double = 0._dp
    endif
endfunction Logic_To_Double

!=====================================
! Returns the inverse matrix of a(n,n)
!=====================================
function matrixinv(x,n)
    integer, intent(in) :: n
    real(dp), intent(in) :: x(n,n)
    real(dp) :: matrixinv(n,n)
    ! https://www.webpages.uidaho.edu/~gabrielp/ME549-CE546/matrix-inverse.pdf
    ! function to calculate the inverse of a matrix using Gauss-Jordan elimination
    ! the inverse of matrix a(n,n) is calculated and returned
    integer :: i,j,k,l,irow
    real(dp) :: a(n,n)
    real(dp):: big,dum
    
    irow=1
    ! preserve contents of x
    a = x
    
    !build the identity matrix
    matrixinv(1:n, 1:n) = 0._dp
    do j=1, n
        matrixinv(j, j)=1._dp
    enddo
    
    do i = 1,n ! this is the big loop over all the columns of a(n,n)
        ! in case the entry a(i,i) is zero, we need to find a good pivot; this pivot
        ! is chosen as the largest value on the column i from a(j,i) with j = 1,n
        big = a(i,i)
        do j = i,n
            if (a(j,i).gt.big) then
                big = a(j,i)
                irow = j
            endif
        enddo
     
        ! interchange lines i with irow for both a() and matrixinv() matrices
        if (big.gt.a(i,i)) then
            do k = 1,n
                dum = a(i,k) ! matrix a()
                a(i,k) = a(irow,k)
                a(irow,k) = dum
                dum = matrixinv(i,k) ! matrix matrixinv()
                matrixinv(i,k) = matrixinv(irow,k)
                matrixinv(irow,k) = dum
            enddo
        endif
        ! divide all entries in line i from a(i,j) by the value a(i,i);
        ! same operation for the identity matrix
        dum = a(i,i)
        do j = 1,n
            a(i,j) = a(i,j)/dum
            matrixinv(i,j) = matrixinv(i,j)/dum
        enddo
        ! make zero all entries in the column a(j,i); same operation for indent()
        do j = i+1,n
            dum = a(j,i)
            do k = 1,n
                a(j,k) = a(j,k) - dum*a(i,k)
                matrixinv(j,k) = matrixinv(j,k) - dum*matrixinv(i,k)
            enddo
        enddo
    enddo
    
    ! substract appropiate multiple of row j from row j-1
    do i = 1,n-1
        do j = i+1,n
            dum = a(i,j)
            do l = 1,n
                a(i,l) = a(i,l)-dum*a(j,l)
                matrixinv(i,l) = matrixinv(i,l)-dum*matrixinv(j,l)
            enddo
        enddo
    enddo   

    ! Test for NaN
    if (matrixinv(1,1)/=matrixinv(1,1)) then
        write(*,*) term_red, 'matrixinv FAILED', term_blk
        STOP 99
    endif 
endfunction matrixinv

!=============================================================================================
! Adding a leap year every 4 years adds 1/4 to the 365-day year 
!      or increases the average year-length by 0.25 days.
! Omitting a leap year every 100 years subtracts 1/100 = 0.01 days from the 365-day year.
! Adding a leap year every 400 years adds 1/400 = 0.0025 days to the 365-day year.
! When these are combined, the corrected average year-length is 
!     365 + 0.25 - 0.01 + 0.0025 = 365.2425 days/year.
!=============================================================================================
logical function Leap_Year(year)
    integer year
    if ((DivBy(year,400) .OR. .NOT. DivBy(year,100)) .AND. DivBy(year,4)) then
        Leap_Year = .TRUE.
    else
        Leap_Year = .FALSE.
    endif
endfunction Leap_Year

!=============================================================================================
! Instead of MOD explicity state if Divisible By
!=============================================================================================
logical function DivBy(y,val)
    integer y, val
    DivBy = mod(y,val) == 0
endfunction

!=============================================================================================
! Computes day of year given month, day, if year is a Leap Year
! Converts day of year to Growth year starting May 31 at 2400
! Leap year is handled in Main Loop as determined by current year
!=============================================================================================
integer function DayOfYear(m, d)
    integer, intent(in) :: m, d
    integer day
    integer, parameter :: dayInYear(12) = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/)

    day = dayInYear(m) + d
    ! 
    ! Apply offset for Growth Year starting May 31 @ 2400
    if (day - dayInYear(6) > 0) then 
        day = day - dayInYear(6)
    else
        day = day - dayInYear(6) + 365
    endif
    DayOfYear = day

endfunction DayOfYear

!===================================================================
! Test for float not a number
! None of these tests worked
! - (tmp /= tmp), 
! - isnan(tmp)
! - (.not.tmp<1.and..not.tmp>1) 
!===================================================================
logical function Is_NAN(x)
    real(dp), intent(in) :: x
    character(3) buf

    write(buf, '(F3.0)') x
    Is_NAN = (buf(1:3) == 'NaN')
endfunction

end module globals