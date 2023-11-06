!> @class Data_Point_Mod
! Data_Point_Mod
module Data_Point_Mod
    use globals
    implicit none
    integer, parameter :: num_dimensions = 12000
    type Data_Point_Class
        !> @public @memberof Data_Point_Mod
        !! UTM Northing
        real(dp) x(num_dimensions)
        !> @public @memberof Data_Point_Mod
        !! UTM Easting
        real(dp) y(num_dimensions)
        !> @public @memberof Data_Point_Mod
        !! Latitude
        real(dp) lat(num_dimensions)
        !> @public @memberof Data_Point_Mod
        !! Longitude
        real(dp) lon(num_dimensions)
        !> @public @memberof Data_Point_Mod
        !! Depth, i.e. altitude
        real(dp) z(num_dimensions)
        !!!real(dp) f(num_dimensions)  !  NOT USED???
        integer E(4,num_dimensions) ! NOT USED???
        !> @public @memberof Data_Point_Mod
        !! Indicates if grid is closed for fishing
        logical is_closed(num_dimensions)
        !> @public @memberof Data_Point_Mod
        !! Indexed management area
        integer mgmt_area_index(num_dimensions)
        !> @public @memberof Data_Point_Mod
        !! Size of management area, i.e. number of grids
        integer len
        !> @public @memberof Data_Point_Mod
        !! Number of squares, number of elements NOT USED
        !! Determined by MAsquares.csv
        integer num_elements
                              
    end type Data_Point_Class
end module Data_Point_Mod
    