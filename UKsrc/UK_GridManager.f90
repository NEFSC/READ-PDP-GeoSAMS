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
    real(dp) recr_psqm(nDim)
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
subroutine Load_Grid(x, y, z, lat, lon, num_points, E, num_squares, ManagementRegion, DomainName)
real(dp) lat(*), lon(*), x(*), y(*), z(*)
integer n, num_points, num_squares, io, ManagementRegion(*), E(4,*)
character(72) input_str, flnm
character(2) DomainName

flnm = 'Grids/'//DomainName//'xyzLatLon.csv'
write(*,*)'grid file: ', trim(flnm)
open(63, file = trim(flnm), status = 'old')
n = 0
do
    read(63, '(a)', iostat = io) input_str
    if (io.lt.0) exit
    n = n + 1
    read(input_str,*) x(n), y(n), z(n), lat(n), lon(n)
end do
close(63)
num_points = n

flnm = 'Grids/ManagementArea'//DomainName//'.txt'
open(63, file = trim(flnm), status = 'old')

do n = 1, num_points
    read(63, '(a)', iostat = io) input_str
    if (io.lt.0) exit
    read(input_str,*) ManagementRegion(n)
end do
n = n-1
if (n .NE. num_points) then
    write(*,'(A,A,A,A,I6,A,A,A,I6)') term_red, trim(flnm), ' incorrect size, expected ', &
    &  term_blk, num_points, term_red, ' read ', term_blk, n
    stop
endif
close(63)

flnm = 'Grids/'//DomainName//'squares.csv'
open(63, file = trim(flnm), status = 'old')
n = 0
do
    read(63, '(a)', iostat = io) input_str
    if (io.lt.0) exit
    n = n + 1
    read(input_str,*) E(1,n), E(2,n), E(3,n), E(4,n)
end do
close(63)
num_squares = n

return
end subroutine

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
subroutine Load_Data(x, y, z, f, num_survey, flnm)

real(dp), intent(out):: x(*), y(*), z(*), f(*)
integer, intent(out):: num_survey
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
num_survey = n
return
end subroutine

!----------------------------------------------------------------------------------------
!> Purpose:  Localized linear interpolate data from a field, f, on the nodes of grid g 
!> with square elements, g%E to points described in obs.
!>
!> inputs: 
!> -  g (Grid_Data_Class)      - grid with square elements - see Main Program
!> -  obs(Grid_Data_Class)     - observation points to be interpolated to
!> -  f [g%num_points]         - vector of values on nodes of grid
!>
!> output:
!> -  fInterp [obs%num_points] - vector of values interpolated from field onto onto points obs%x, obs%y
!>
!> @author Keston Smith 2022
!>
!----------------------------------------------------------------------------------------
subroutine Interpret_From_Grid(g, f, obs, fInterp)

type(Grid_Data_Class):: g
type(Grid_Data_Class):: obs
real(dp), intent(out) :: fInterp(*)
real(dp), intent(in) :: f(*)
integer num_points, num_survey, num_squares, j, k
real(dp), allocatable:: Dist(:), xe(:), ye(:)
real(dp) W(4), De(4), SmallDist
integer ke(4)
num_points = g%num_points
num_squares = g%num_squares
num_survey = obs%num_points
SmallDist = 1.D0 !meters
write(*,*)'num_points, num_squares, num_survey', num_points, num_squares, num_survey
allocate(Dist(1:num_squares), xe(1:num_squares), ye(1:num_squares))

do k = 1, num_squares
    ke(1:4) = g%E(1:4, k)
    xe(k) = sum(g%x(ke(1:4))) / 4.D0
    ye(k) = sum(g%y(ke(1:4))) / 4.D0
enddo

do j = 1, num_survey
    Dist(1:num_squares) = (xe(1:num_squares) - obs%x(j))**2 + (ye(1:num_squares) - obs%y(j))**2
    k = minloc(Dist(1:num_squares), 1)
    ke(1:4) = g%E(1:4, k)
    De(1:4) = sqrt( (g%x(ke(1:4)) - obs%x(j))**2 + (g%y(ke(1:4)) - obs%y(j))**2 )
    if (minval(De(1:4)).gt.SmallDist)then
        W(1:4) = 1.D0 / De(1:4)
        fInterp(j) = sum( W(1:4) * f( ke(1:4) )  ) / sum( W(1:4) )
    else !avoid division by 0, probably not nescesary
        k = minloc(De, 1)
        fInterp(j) = f( ke(k) )
    endif
enddo
deallocate(Dist, xe, ye)
return
end subroutine

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
subroutine Get_Domain_Average(obs, g, domain_avg)

type(Grid_Data_Class):: g
type(Grid_Data_Class):: obs
real(dp), intent(out) :: domain_avg
integer num_points, num_survey, Nregion, k, j, rn
real(dp) dx, dy
real(dp), allocatable:: RegionalAverage(:), D(:), RegionArea(:)
integer, allocatable:: ObsInRegion(:)
num_points = g%num_points
num_survey = obs%num_points
Nregion = maxval(g%ManagementRegion(1:num_points))
allocate(ObsInRegion(1:Nregion), RegionalAverage(1:Nregion), D(1:num_points), RegionArea(1:Nregion))
RegionalAverage(1:Nregion) = 0.
ObsInRegion(1:Nregion) = 0
do j = 1, num_survey
    D(1:num_points) = (g%x(1:num_points) - obs%x(j))**2 + (g%y(1:num_points) - obs%y(j))**2
    k = minloc(D, 1)
    rn = g%ManagementRegion(k)
    RegionalAverage(rn) = RegionalAverage(rn) + obs%recr_psqm(j)
    ObsInRegion(rn) = ObsInRegion(rn) + 1
enddo

do k = 1, Nregion
    if(ObsInRegion(k).gt.0)RegionalAverage(k) = RegionalAverage(k) / float(ObsInRegion(k))
enddo

dx = meters_per_naut_mile
dy = meters_per_naut_mile
do k = 1, num_points
    RegionArea(g%ManagementRegion(k)) = RegionArea(g%ManagementRegion(k)) + dx * dy
enddo
domain_avg = sum(RegionalAverage(1:Nregion) * RegionArea(1:Nregion)) / sum(RegionArea(1:Nregion))
write(*,*)'Regional Averages', RegionalAverage(1:Nregion)
write(*,*)
write(*,*)'Regional Areas', RegionArea(1:Nregion)
write(*,*)
write(*,*)'Nobs in region', ObsInRegion(1:Nregion)
write(*,*)'Obs Average', sum(obs%recr_psqm(1:num_survey)) / float(num_survey), num_survey
deallocate(ObsInRegion, RegionalAverage, D, RegionArea)
return
end subroutine

end module GridManagerMod
