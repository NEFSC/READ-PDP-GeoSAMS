! Data_Point_Mod
module Data_Point_Mod
    use globals
    integer, parameter :: num_dimensions = 12000
    type Data_Point
        real(dp) x(num_dimensions)
        real(dp) y(num_dimensions)
        real(dp) lat(num_dimensions)
        real(dp) lon(num_dimensions)
        real(dp) z(num_dimensions)
        real(dp) f(num_dimensions)
        integer E(4,num_dimensions)
        logical is_closed(num_dimensions)
        integer management_area(num_dimensions)
        character(2) region(num_dimensions)
        integer len, ne
    end type Data_Point
end module Data_Point_Mod
    