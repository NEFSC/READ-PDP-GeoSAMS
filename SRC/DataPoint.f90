module Data_Point_Mod
    use globals
    implicit none
    integer, parameter :: num_dimensions = 12000
    !> @class Data_Point_Class
    type Data_Point_Class
        !> @public @memberof Data_Point_Class
        !! UTM Northing
        real(dp) x(num_dimensions)
        !> @public @memberof Data_Point_Class
        !! UTM Easting
        real(dp) y(num_dimensions)
        !> @public @memberof Data_Point_Class
        !! Latitude
        real(dp) lat(num_dimensions)
        !> @public @memberof Data_Point_Class
        !! Longitude
        real(dp) lon(num_dimensions)
        !> @public @memberof Data_Point_Class
        !! Depth, i.e. altitude
        real(dp) z(num_dimensions)
        integer E(4,num_dimensions) ! NOT USED???
        !> @public @memberof Data_Point_Class
        !! Indicates if grid is closed for fishing
        logical is_closed(num_dimensions)
        !> @public @memberof Data_Point_Class
        !! Indexed management area
        integer mgmt_area_index(num_dimensions)
        !> @public @memberof Data_Point_Class
        !! Size of management area, i.e. number of grids
        integer len
        !> @public @memberof Data_Point_Class
        !! Number of squares, number of elements NOT USED
        !! Determined by MAsquares.csv
        integer num_elements
                              
    end type Data_Point_Class
end module Data_Point_Mod
    