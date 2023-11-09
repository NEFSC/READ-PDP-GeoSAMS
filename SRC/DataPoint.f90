module Data_Point_Mod
    use globals
    implicit none
    integer, parameter :: num_dimensions = 12000
    !> @class Data_Point_Class
    type Data_Point_Class
        !> @public @memberof Data_Point_Class
        !! UTM Northing
        real(dp) x
        !> @public @memberof Data_Point_Class
        !! UTM Easting
        real(dp) y
        !> @public @memberof Data_Point_Class
        !! Latitude
        real(dp) lat
        !> @public @memberof Data_Point_Class
        !! Longitude
        real(dp) lon
        !> @public @memberof Data_Point_Class
        !! bathymetric depth at (x,y)
        real(dp) z
        !> @public @memberof Data_Point_Class
        !! Indicates if grid is closed for fishing
        logical is_closed
        !> @public @memberof Data_Point_Class
        !! Indexed management area
        integer mgmt_area_index
    end type Data_Point_Class

    !> @class Data_Vector_Class
    type Data_Vector_Class
        !> @public @memberof Data_Vector_Class
        type(Data_Point_Class) :: posn(num_dimensions)
        !> @public @memberof Data_Vector_Class
        !! Size of management area, i.e. number of grids
        integer len
    end type Data_Vector_Class
end module Data_Point_Mod
    