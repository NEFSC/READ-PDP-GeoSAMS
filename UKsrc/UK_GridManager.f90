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
    real(dp) x(nDim)
    real(dp) y(nDim)
    real(dp) z(nDim)
    real(dp) field(nDim)
    real(dp) lat(nDim)
    real(dp) lon(nDim)
#ifdef USE_DOM_AVG
    integer mgmt_region(nDim)
#endif
    integer num_points
end type Grid_Data_Class

character(fname_len), PRIVATE :: grid_data_file_name
character(fname_len), PRIVATE :: obs_data_file_name
#ifdef USE_DOM_AVG
character(fname_len), PRIVATE :: mgmt_region_fname
#endif

CONTAINS

!-----------------------------------------------------------------------------------------------
!! @public @memberof Grid_Data_Class
!> Initialize survey and grid location data
!> 
!-----------------------------------------------------------------------------------------------
#ifdef USE_DOM_AVG
subroutine GridMgr_Set_Grid_Manager(obs, grid, alpha_obs, nobs, ngrid, fmax, domain_avg) !, fmax_multiplier, fmax)
real(dp), intent(inout) :: domain_avg
#else
subroutine GridMgr_Set_Grid_Manager(obs, grid, alpha_obs, nobs, ngrid, fmax) !, fmax_multiplier, fmax)
#endif
type(Grid_Data_Class), intent(out):: grid
type(Grid_Data_Class), intent(out):: obs
real(dp), intent(in) :: alpha_obs
integer, intent(out) :: nobs, ngrid
! real(dp), intent(in) :: fmax_multiplier
real(dp), intent(inout) :: fmax

!
! Initalize data point coordinates, bathymetry and data - initialize no
!
obs%num_points = GridMgr_Load_Observation_Data(obs%x, obs%y, obs%z, obs%field)
nobs = obs%num_points
obs%field(1:nobs) = obs%field(1:nobs)**alpha_obs

!
! Initalize grid point coordinates and bathymetry - initialize num_points
!
#ifdef USE_DOM_AVG
ngrid = GridMgr_Load_Grid(grid%x, grid%y, grid%z, grid%lat, grid%lon, grid%mgmt_region)
#else
ngrid = GridMgr_Load_Grid(grid%x, grid%y, grid%z, grid%lat, grid%lon)
#endif
grid%num_points = ngrid

! ! Used if IsHiLimit is true
fmax = maxval(obs%field(1:nobs))
#ifdef USE_DOM_AVG
domain_avg = GetDomainAverage(obs, grid)
#endif

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
#ifdef USE_DOM_AVG
integer function GridMgr_Load_Grid(x, y, z, lat, lon, mgmtRegion)
#else
integer function GridMgr_Load_Grid(x, y, z, lat, lon)
#endif
real(dp) lat(*), lon(*), x(*), y(*), z(*)
#ifdef USE_DOM_AVG
integer, intent(out) :: mgmtRegion(*)
character(domain_len) domain_name ! for now get domain name from grid
integer m, j
logical exists
#endif
integer n, io
character(csv_line_len) input_str

!write(*,*)'grid file: ', trim(grid_data_file_name)
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

#ifdef USE_DOM_AVG
j = index(grid_data_file_name, '/') + 1
domain_name = grid_data_file_name(j:j+1)
mgmt_region_fname = 'Grids/ManagementArea'//domain_name//'.txt'
inquire(file=mgmt_region_fname, exist=exists)

if (exists) then
    PRINT *, term_blu, trim(mgmt_region_fname), ' FOUND', term_blk
else
    PRINT *, term_red, trim(mgmt_region_fname), ' NOT FOUND', term_blk
    stop 1
endif

open(63,file=trim(mgmt_region_fname),status='old')
m=0
do
    read(63,'(a)',iostat=io) input_str
    if (io.lt.0) exit
    m = m + 1
    read(input_str,*) mgmtRegion(m)
end do
close(63)

if (n .NE. m) then
    PRINT *, term_red, 'OOPS something went wrong. Grid: ', term_blk, n, term_red, ' does not match Region Grid: ', term_blk, m
    STOP 1
endif
#endif

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

#ifdef USE_DOM_AVG
!----------------------------------------------------------------------------------------
! function GetDomainAverage(obs,g)
! compute spatial average of values in obs%f, by averaging across regions in g%region.
! This is here to match stratified sampling in the CASA model etc.
!
! inputs:
!   obs (dpoint) -  obs%f - values for which spatial average is computed
!   g (dpoint) -    grid with nodes assigned to regions g%ManagementRegion to compute 
!                   local averages within
! output:
!   DomainAverage (real) - regional area weighted average of obs%f across grid 
!----------------------------------------------------------------------------------------
! Keston Smith 2022
!----------------------------------------------------------------------------------------
real(dp) function GetDomainAverage(obs,grid)
    type(Grid_Data_Class), intent(out):: grid
    type(Grid_Data_Class), intent(out):: obs
    integer nn,no,num_mgmt_regs,k,reg_number,j
    real(dp) dx,dy
    real(dp), allocatable:: reg_avg(:),distance(:), reg_area(:)
    integer, allocatable:: obs_in_region(:)
    character(20) FMT
    nn = grid%num_points
    no = obs%num_points
    num_mgmt_regs = maxval(grid%mgmt_region(1:nn))
    allocate(obs_in_region(1:num_mgmt_regs), reg_avg(1:num_mgmt_regs), distance(1:nn), reg_area(1:num_mgmt_regs))
    reg_avg(1:num_mgmt_regs) = 0._dp
    obs_in_region(1:num_mgmt_regs) = 0
    do j=1,no
        distance(1:nn) = (grid%x(1:nn) - obs%x(j))**2 + (grid%y(1:nn) - obs%y(j))**2
        k = minloc(distance,1)
        reg_number = grid%mgmt_region(k)
        reg_avg(reg_number) = reg_avg(reg_number) + obs%field(j)
        obs_in_region(reg_number) = obs_in_region(reg_number) + 1
    enddo
    
    do k=1,num_mgmt_regs
        if(obs_in_region(k).gt.0) reg_avg(k) = reg_avg(k) / float(obs_in_region(k))
    enddo
    
    dx=1852.
    dy=1852.
    do k=1,nn
        reg_area(grid%mgmt_region(k)) = reg_area(grid%mgmt_region(k)) + dx*dy
    enddo

    GetDomainAverage = sum(reg_avg(1:num_mgmt_regs) * reg_area(1:num_mgmt_regs)) / sum(reg_area(1:num_mgmt_regs))
    
    write(FMT,*) num_mgmt_regs
    write(*,'(A,'//adjustl(FMT)//'ES11.4)') 'Regional Averages: ',reg_avg(1:num_mgmt_regs)
    write(*,'(A,'//adjustl(FMT)//'ES11.4)') 'Regional Areas:    ',reg_area(1:num_mgmt_regs)
    write(*,'(A,'//adjustl(FMT)//'I11)')    'Nobs in region:    ',obs_in_region(1:num_mgmt_regs)
    write(*,'(A,ES11.4,A,I5)')'Obs Average: ', sum(obs%field(1:no)) / float(no), '  Number of Obs: ', no
    deallocate(obs_in_region, reg_avg, distance, reg_area)
    return
endfunction
#endif

end module Grid_Manager_Mod
