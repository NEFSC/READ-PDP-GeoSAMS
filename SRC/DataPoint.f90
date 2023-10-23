! Data_Point_Mod
module Data_Point_Mod
    use globals
    implicit none
    integer, parameter :: num_dimensions = 12000
    type Data_Point
        real(dp) x(num_dimensions)
        real(dp) y(num_dimensions)
        real(dp) lat(num_dimensions)
        real(dp) lon(num_dimensions)
        real(dp) z(num_dimensions)
        !!!real(dp) f(num_dimensions)  !  NOT USED???
        integer E(4,num_dimensions) ! NOT USED???
        logical is_closed(num_dimensions)
        integer management_area(num_dimensions)
        character(2) region(num_dimensions)
        integer len   !>< Size of management area, i.e. number of grids
        integer num_elements  !>< Number of squares, number of elements NOT USED
                              !! Determined by MAsquares.csv
    end type Data_Point
end module Data_Point_Mod
    