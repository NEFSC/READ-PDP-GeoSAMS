! common parameters
module globals
    implicit none
    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)

    integer, parameter :: num_dimensions = 12000
    integer, parameter :: shell_len_max = 150
    integer, parameter :: shell_len_min = 30
    integer, parameter :: shell_len_delta = 5
    integer, parameter :: num_size_classes = (shell_len_max - shell_len_min) / shell_len_delta + 1

    integer, parameter :: max_num_years  = 50

    integer, parameter :: read_dev = 69
    integer, parameter :: write_dev = 63

    ! ASIN incorrectly produces error
    ! Error: Fortran 2003: Elemental function as initialization expression with non-integer/non-character arguments
    real(dp), parameter :: pi = 3.14159265358979311599796346854D0 ! 2._dp * ASIN(1._dp)

    ! metric equivalents
    real(dp), parameter :: grams_per_pound = 453.592_dp
    real(dp), parameter :: meters_per_naut_mile = 1852.D0
    real(dp), parameter :: grams_per_metric_ton = 1000000._dp
    real(dp), parameter :: grid_area_sqm = meters_per_naut_mile**2


    ! colors taken from https://i.stack.imgur.com/9UVnC.png
    character(*), parameter :: term_red = ''//achar(27)//'[31m'
    character(*), parameter :: term_yel = ''//achar(27)//'[33m'
    character(*), parameter :: term_grn = ''//achar(27)//'[92m'
    character(*), parameter :: term_blu = ''//achar(27)//'[94m'
    character(*), parameter :: term_blk = ''//achar(27)//'[0m'


    character(*), parameter :: init_cond_dir = 'InitialCondition/'
    character(*), parameter :: growth_out_dir = 'GrowthOutput/'
    character(*), parameter :: rec_input_dir = 'KrigingEstimates/'
    character(*), parameter :: rec_output_dir = 'RecruitField/'
    character(*), parameter :: output_dir = 'Results/'
    character(*), parameter :: config_dir = 'Configuration/'

    character(*), parameter :: sim_input_fname = 'Scallop.inp'

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

!   ==============================
!   Changes a string to upper case
!   ==============================
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


end module globals