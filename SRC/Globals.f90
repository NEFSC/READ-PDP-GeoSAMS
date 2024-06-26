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
integer, parameter :: num_regions = 5
integer, parameter :: region_none=0
integer, parameter :: region_N=1
integer, parameter :: region_S=2
integer, parameter :: region_SW=3
integer, parameter :: region_W=4
integer, parameter :: region_MA=5

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



CONTAINS

elemental real(dp) function Logic_To_Double (value)
    logical, intent(in) :: value
    if (value) then
        Logic_To_Double = 1._dp
    else
        Logic_To_Double = 0._dp
    endif
endfunction Logic_To_Double

logical function Is_Leap_Year (yr)
    integer, intent(in) :: yr
    if (mod(yr,100) .ne. 0 .AND. mod(yr,4) .eq. 0) then
        Is_Leap_Year = .true.
    elseif (mod(yr,400) .eq. 0) then
        Is_Leap_Year = .true.
    else
        Is_Leap_Year = .false.
    endif
endfunction

!---------------------------------------------------------------------------------------------------
!> Purpose: Computes SQRT( SUM( X(1:N)^2 / N) )
!> @param[in] x a real array 
!> @param[in] n the number of elements in x
!---------------------------------------------------------------------------------------------------
real(dp) function Compute_RMS(x, n)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(in) :: x(:)
    real(dp) result 

    result = sqrt(sum(x(1:n)**2) / float(n))
    if (is_nan(result)) then
        write(*,*) term_red, 'Compute_RMS FAILED; NaN', term_blk
        STOP 99
    endif
    Compute_RMS = result
endfunction Compute_RMS

!---------------------------------------------------------------------------------------------------
!> isnan does not work nor does (x/=x)
!>
!---------------------------------------------------------------------------------------------------
logical function is_nan(x)
    real(dp), intent(in) :: x
    character(10) buf

    write(buf,'(F10.6)') x
    is_nan = (buf(8:10) .EQ. 'NaN')
endfunction is_nan

!---------------------------------------------------------------------------------------------------
!> Purpose: Computes Artithmetic Mean
!> @param[in] x a real array 
!> @param[in] n the number of elements in x
!---------------------------------------------------------------------------------------------------
real(dp) function Compute_MEAN(x, n)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(in) :: x(:)
    Compute_MEAN = sum(x(1:n) / float(n))
endfunction Compute_MEAN

!==============================
! Changes a string to upper case
!==============================
Pure Function to_upper (str) Result (string)
    Implicit None
    Character(*), Intent(In) :: str
    Character(LEN(str))      :: string

    Integer :: ic, i

    Character(26), Parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    Character(26), Parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

    !   Capitalize each letter if it is lowecase
    string = str
    do i = 1, LEN_TRIM(str)
        ic = INDEX(low, str(i:i))
        if (ic > 0) string(i:i) = cap(ic:ic)
    end do
endfunction to_upper

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
endfunction matrixinv

end module globals