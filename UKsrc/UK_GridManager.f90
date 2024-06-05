!--------------------------------------------------------------------------------------------------
!> @page page5 Grid Manager
!>
!>
!--------------------------------------------------------------------------------------------------
! Keston Smith, Tom Callaghan (IBSS) 2024
!--------------------------------------------------------------------------------------------------
module Grid_Manager_Mod
use globals
implicit none

! Define a data point with position (x, y), bathymetric depth z(x, y) and scalar field value f(x, y)
type Grid_Data_Class
    real(dp) x(NDim)
    real(dp) y(NDim)
    real(dp) z(nDim)
    real(dp) field(nDim)
    real(dp) lat(NDim)
    real(dp) lon(NDim)
    integer num_points
end type Grid_Data_Class

character(fname_len), PRIVATE :: grid_data_file_name
character(fname_len), PRIVATE :: obs_data_file_name

CONTAINS

!-----------------------------------------------------------------------------------------------
!! @public @memberof Grid_Data_Class
!> Initialize survey and grid location data
!> 
!-----------------------------------------------------------------------------------------------
subroutine GridMgr_Set_Grid_Manager(obs, grid, alpha_obs, nobs, ngrid) !, fmax_multiplier, fmax)
type(Grid_Data_Class), intent(out):: grid
type(Grid_Data_Class), intent(out):: obs
real(dp), intent(in) :: alpha_obs
integer, intent(out) :: nobs, ngrid
! real(dp), intent(in) :: fmax_multiplier
! real(dp), intent(inout) :: fmax

!
! Initalize data point coordinates, bathymetry and data - initialize no
!
obs%num_points = GridMgr_Load_Observation_Data(obs%x, obs%y, obs%z, obs%field)
nobs = obs%num_points
obs%field(1:nobs) = obs%field(1:nobs)**alpha_obs

!
! Initalize grid point coordinates and bathymetry - initialize num_points
!
ngrid = GridMgr_Load_Grid(grid%x, grid%y, grid%z, grid%lat, grid%lon)
grid%num_points = ngrid

! ! Used if IsHiLimit is true
! fmax = fmax_multiplier * maxval(obs%field(1:nobs))

endsubroutine

!-----------------------------------------------------------------------------------------------
!! @public @memberof Grid_Data_Class
!> Used during instantiation to set the name of the file to read to for main grid data points
!> @brief Read Input File
!> 
!> Sets file names for main grid parameters, x, y, lat, lon, depth
!-----------------------------------------------------------------------------------------------
subroutine GridMgr_Set_Grid_Data_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    grid_data_file_name = grid_dir//fname
    inquire(file=grid_data_file_name, exist=exists)

    if (exists) then
        PRINT *, term_blu, trim(grid_data_file_name), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(grid_data_file_name), ' NOT FOUND', term_blk
        stop 1
    endif
endsubroutine GridMgr_Set_Grid_Data_File_Name

!-----------------------------------------------------------------------------------------------
!! @public @memberof Grid_Data_Class
!> Used during instantiation to set the name of the file to read to for observation data points
!> @brief Read Input File
!> 
!> Sets file names for initial state data
!-----------------------------------------------------------------------------------------------
subroutine GridMgr_Set_Obs_Data_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    obs_data_file_name = data_dir//fname
    inquire(file=obs_data_file_name, exist=exists)

    if (exists) then
        PRINT *, term_blu, trim(obs_data_file_name), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(obs_data_file_name), ' NOT FOUND', term_blk
        stop 1
    endif
endsubroutine GridMgr_Set_Obs_Data_File_Name

character(fname_len) function GridMgr_Get_Obs_Data_File_Name()
    GridMgr_Get_Obs_Data_File_Name = obs_data_file_name
endfunction GridMgr_Get_Obs_Data_File_Name

!-----------------------------------------------------------------------
!! @public @memberof Grid_Data_Class
!> load grid coordinates and bathymetric depth from CSV file with 5 columns
!> representing an x coordinate, y, bathymetric depth (z), latitude, and
!> longitude.
!>
!> Inputs: 
!> - none
!>
!> Outputs:
!> -   x    (real(dp)) x-coordinate of data
!> -   y    (real(dp)) y-coordinate of data
!> -   z    (real(dp)) bathymetric depth at (x, y)
!> -   lat    (real(dp)) latitude at (x, y)
!> -   lon    (real(dp)) longitude at (x, y)
!> -   num_points    (integer)length of x, y, z, lat, lon (number of data points)
!>
!> Note: At present lat and lon variabels are not used. 
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!>-----------------------------------------------------------------------
integer function GridMgr_Load_Grid(x, y, z, lat, lon)
real(dp) lat(*), lon(*), x(*), y(*), z(*)
integer n, io
character(csv_line_len) input_str

write(*,*)'grid file: ', trim(grid_data_file_name)
open(63, file = trim(grid_data_file_name), status = 'old')
n = 0
do
    read(63, '(a)', iostat = io) input_str
    if (io.lt.0) exit
    n = n + 1
    read(input_str,*) x(n), y(n), z(n), lat(n), lon(n)
end do
close(63)
GridMgr_Load_Grid = n

end function

!-----------------------------------------------------------------------
!! @public @memberof Grid_Data_Class
!> Load data from CSV file with 4 columns representing an x coordinate, y
!> coordinate, bathymetric depth (z), and a scaller field f.
!>
!> Inputs: 
!> -   none
!>
!> Outputs:
!> -   x    (real(dp)) x-coordinate of data
!> -   y    (real(dp)) y-coordinate of data
!> -   z    (real(dp)) bathymetric depth at (x, y)
!> -   f    (real(dp)) scalar data at (x, y)
!> -   num_points    (integer)length of x, y, and z (number of data points)
!>
!> @author Keston Smith (IBSS corp) June-July 2021
!-----------------------------------------------------------------------
integer function GridMgr_Load_Observation_Data(x, y, z, f)

real(dp), intent(out):: x(*), y(*), z(*), f(*)
real(dp) year
integer    n, io
character(line_len) input_str

open(63, file = obs_data_file_name, status = 'old')
n = 0
! read header text
read(63,*) input_str
do
    read(63, '(a)', iostat = io) input_str
    if (io.lt.0) exit
    n = n + 1
    read(input_str,*) year, x(n), y(n), z(n), f(n)
end do
close(63)
GridMgr_Load_Observation_Data = n
end function

end module Grid_Manager_Mod
