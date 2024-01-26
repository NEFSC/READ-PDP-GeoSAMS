!--------------------------------------------------------------------------------------------------
!> @page page5 Grid Manager
!>
!>
!--------------------------------------------------------------------------------------------------
! Keston Smith, Tom Callaghan (IBSS) 2024
!--------------------------------------------------------------------------------------------------
module GridManagerMod
use globals
implicit none

! Define a data point with position (x, y), bathymetric depth z(x, y) and scalar field value f(x, y)
type Grid_Data_Class
    real(dp) x(NDim)
    real(dp) y(NDim)
    real(dp) z(nDim)
    real(dp) f_psqm(nDim)
    real(dp) lat(NDim)
    real(dp) lon(NDim)
    integer num_points, num_squares
    integer E(4, nDim)
    integer ManagementRegion(nDim)
    character(2) region(nDim)
end type Grid_Data_Class

CONTAINS

!-----------------------------------------------------------------------
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
integer function Load_Grid(x, y, z, lat, lon, ManagementRegion, domain_name)
real(dp) lat(*), lon(*), x(*), y(*), z(*)
integer n, io, ManagementRegion(*)
character(72) input_str, flnm
character(2) domain_name

flnm = 'Grids/'//domain_name//'xyzLatLonRgn.csv'
!flnm = 'Grids/'//domain_name//'xyzLatLon.csv'
!flnm = 'Grids/2005MAxyzLatLon.csv'
write(*,*)'grid file: ', trim(flnm)
open(63, file = trim(flnm), status = 'old')
n = 0
do
    read(63, '(a)', iostat = io) input_str
    if (io.lt.0) exit
    n = n + 1
    read(input_str,*) x(n), y(n), z(n), lat(n), lon(n), ManagementRegion(n)
end do
close(63)
Load_Grid = n

! flnm = 'Grids/ManagementArea'//domain_name//'.txt'
! open(63, file = trim(flnm), status = 'old')

! do n = 1, num_points
!     read(63, '(a)', iostat = io) input_str
!     if (io.lt.0) exit
!     read(input_str,*) ManagementRegion(n)
! end do
! n = n-1
! if (n .NE. num_points) then
!     write(*,'(A,A,A,A,I6,A,A,A,I6)') term_red, trim(flnm), ' incorrect size, expected ', &
!     &  term_blk, num_points, term_red, ' read ', term_blk, n
!     stop
! endif
! close(63)

! Load_Grid = num_points

return
end function

!-----------------------------------------------------------------------
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
integer function Load_Data(x, y, z, f, flnm)

real(dp), intent(out):: x(*), y(*), z(*), f(*)
character (*), intent(in)::  flnm
real(dp) year
integer    n, io
character(72) input_str

open(63, file = flnm, status = 'old')
n = 0
read(63,*) input_str
do
    read(63, '(a)', iostat = io) input_str
    if (io.lt.0) exit
    n = n + 1
    read(input_str,*) year, x(n), y(n), z(n), f(n)
end do
close(63)
Load_Data = n
return
end function

!----------------------------------------------------------------------------------------
!> compute spatial average of values in obs%recr_psqm, by averaging across regions in g%region.
!> This is here to match stratified sampling in the CASA model etc.
!>
!> inputs:
!> -  obs (Grid_Data_Class) -  obs%recr_psqm - values for which spatial average is computed
!> -  g (Grid_Data_Class) -    grid with nodes assigned to regions g%ManagementRegion to compute 
!>                   local averages within
!> output:
!> -  domain_avg (real) - regional area weighted average of obs%recr_psqm across grid 
!>
!> @author Keston Smith 2022
!----------------------------------------------------------------------------------------
real(dp) function Get_Domain_Average(obs, g)

type(Grid_Data_Class), intent(in):: g
type(Grid_Data_Class), intent(in):: obs
integer num_points, num_survey, Nregion, k, j, rn
real(dp) dx, dy
real(dp), allocatable:: regional_average(:), D(:), region_area(:)
integer, allocatable:: obs_in_region(:)
num_points = g%num_points
num_survey = obs%num_points
Nregion = maxval(g%ManagementRegion(1:num_points))
allocate(obs_in_region(1:Nregion), regional_average(1:Nregion), D(1:num_points), region_area(1:Nregion))
regional_average(1:Nregion) = 0.
obs_in_region(1:Nregion) = 0
do j = 1, num_survey
    D(1:num_points) = (g%x(1:num_points) - obs%x(j))**2 + (g%y(1:num_points) - obs%y(j))**2
    k = minloc(D, 1)
    rn = g%ManagementRegion(k)
    regional_average(rn) = regional_average(rn) + obs%f_psqm(j)
    obs_in_region(rn) = obs_in_region(rn) + 1
enddo

do k = 1, Nregion
    if(obs_in_region(k).gt.0) regional_average(k) = regional_average(k) / float(obs_in_region(k))
enddo

dx = meters_per_naut_mile
dy = meters_per_naut_mile
do k = 1, num_points
    region_area(g%ManagementRegion(k)) = region_area(g%ManagementRegion(k)) + dx * dy
enddo
Get_Domain_Average = sum(regional_average(1:Nregion) * region_area(1:Nregion)) / sum(region_area(1:Nregion))
write(*,*)'Regional Averages', regional_average(1:Nregion)
write(*,*)
write(*,*)'Regional Areas', region_area(1:Nregion)
write(*,*)
write(*,*)'Nobs in region', obs_in_region(1:Nregion)
write(*,*)'Obs Average', sum(obs%f_psqm(1:num_survey)) / float(num_survey), num_survey
deallocate(obs_in_region, regional_average, D, region_area)
return
end function 

end module GridManagerMod
