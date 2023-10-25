! common parameters
module globals
    implicit none
    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)
    integer, parameter :: max_size_mm = 150
    integer, parameter :: min_size_mm = 30
    integer, parameter :: max_size_class = (max_size_mm - min_size_mm) / 5 + 1
    integer, parameter :: max_num_years  = 50

    integer, parameter :: read_dev = 69
    integer, parameter :: write_dev = 63

    ! colors taken from https://i.stack.imgur.com/9UVnC.png
    character(*), parameter :: term_red = ''//achar(27)//'[31m'
    character(*), parameter :: term_blu = ''//achar(27)//'[94m'
    character(*), parameter :: term_yel = ''//achar(27)//'[33m'
    character(*), parameter :: term_blk = ''//achar(27)//'[0m'

    real(dp), parameter :: meters_per_naut_mile = 1852.D0

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