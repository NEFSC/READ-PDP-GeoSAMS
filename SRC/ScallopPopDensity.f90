PROGRAM ScallopPopDensity
!> @mainpage Scallop Popupation Density
!! This program is used to compute Scallop Density after a given growth period

    use Growth_Mod
    use Data_Point_Mod

    implicit none

    integer j  !> loop count

    character(2) domain_name
    integer start_year, stop_year
    logical is_rand_recr
    character(3) fishing_type      !> Fishing can be USD,  BMS,  or,  CAS
    integer num_time_steps
    integer num_monte_carlo_iter
    character(72) file_name
    real(dp) delta_time

    type(Data_Point):: grid
    real(dp) element_area, domain_area
    integer data_len

    type(Growth_Struct), allocatable :: growth(:)
    !type(MortalityStruct), allocatable :: mortality(:)
    !type(RecruitStruct), allocatable :: recruit(:)

    integer, parameter :: num_size_classes = 31 ! not 25, steps from 30 to 180 in steps of 5

    real(dp), allocatable :: shell_height(:)


    call Read_Input(domain_name, file_name, start_year, stop_year, fishing_type, num_time_steps, num_monte_carlo_iter)

    delta_time = 1._dp / float(num_time_steps)
    
    is_rand_recr = .FALSE.
    
    write(*,*) '========================================================'
    write(*,'(A,A6)') ' Domain:         ', domain_name
    write(*,'(A,I6)') ' Start Year:     ', start_year
    write(*,'(A,I6)') ' Stop Year:      ', stop_year
    write(*,'(A,L6)') ' Random''d Recr:  ', is_rand_recr
    write(*,'(A,A6)') ' Fishing Type:   ', fishing_type
    write(*,'(A,I6,A,F7.4)') ' Time steps/year:', num_time_steps, ' delta ', delta_time
    write(*,*) '========================================================'
    
    call Load_Grid(grid%x, grid%y, grid%z, grid%lat, grid%lon, grid%len, grid%E, grid%ne, &
    &          grid%management_area, grid%is_closed, domain_name)
    write(*,*)'grid length=',grid%len,' ', domain_name
    data_len = grid%len
    element_area = 1852.D0**2
    domain_area = float(data_len) * element_area
    write(*,*) 'Domain Area: ', domain_area

    write(*,*) '========================================================'

    !!!allocate(growth(1:data_len) ,mortality(1:data_len) ,recruit(1:data_len) )
    allocate(growth(1:data_len))

    do j=1, data_len
       growth(j)%region = domain_name
       !!!mortality(j)%region = domain_name
       grid%region(j) = domain_name
       !!!mortality(j)%ManagementArea = grid%ManagementArea(j)
    enddo
    
    allocate(shell_height(1:num_size_classes))
    call Set_Shell_Height_Intervals(shell_height, num_size_classes, 30.D0, 5.D0)
    
    !
    ! Assign recruitment by year and node
    !
    !!!call SetRecruitment(recruit, data_len, domain_name, is_rand_recr)
    call Set_Growth(growth, grid, shell_height, delta_time, data_len, num_size_classes, domain_name)
    !!!call SetMortality(mortality, grid, shell_height, data_len, num_size_classes, domain_name)

END PROGRAM ScallopPopDensity