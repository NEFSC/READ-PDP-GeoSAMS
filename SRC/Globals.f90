! common parameters
module globals
    implicit none
    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)
    integer, parameter :: read_dev = 69
    integer, parameter :: write_dev = 63
end module globals