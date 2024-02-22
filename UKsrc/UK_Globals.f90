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


    ! used by configuration files
    integer, parameter :: tag_len = 40
    integer, parameter :: value_len = 30
    integer, parameter :: comment_len = 80
    integer, parameter :: line_len = tag_len+value_len+comment_len
    integer, parameter :: fname_len = 100
    integer, parameter :: form_len = 20
    integer, parameter :: input_str_len = 100
    integer, parameter :: csv_line_len = 2000

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
    real(dp), parameter :: tow_area_sqm = 4516.D0 ! nautical mmile x 8 feet
    real(dp), parameter :: one_scallop_per_tow = 1.D0 / tow_area_sqm ! 1 scallop per tow, prevents log(0)

    ! colors taken from https://i.stack.imgur.com/9UVnC.png
    character(*), parameter :: term_red = ''//achar(27)//'[31m'
    character(*), parameter :: term_yel = ''//achar(27)//'[33m'
    character(*), parameter :: term_grn = ''//achar(27)//'[92m'
    character(*), parameter :: term_blu = ''//achar(27)//'[94m'
    character(*), parameter :: term_blk = ''//achar(27)//'[0m'

    character(*), parameter :: config_dir = 'Configuration/'
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

end module globals