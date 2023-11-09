PROGRAM ScallopPopDensity
    !> @mainpage Scallop Popupation Density
    !! This program is used to compute Scallop Density after a given growth period
    !!
    !! Steps
    !!  - I. Read Configuration file 'Scallop.inp'
    !!      - A. Get Domain Name
    !!      - B. Get Start Year
    !!      - C. Get End Year
    !!      - D. Time steps per Year
    !!      - E. Inintial Conditions as read from Input/SimMA2000/InitialCondition.csv
    !!      - F. Fishing Type: can be USD, BMS, or CAS
    !!      - G. Parameters for Recruitment 
    !!          - i.   Start Day
    !!          - ii.  Finish Day
    !!          - iii. Random recruit start year
    !!          - iv.  Number of random fields
    !!      - H. Parameters for Monte Carlo
    !!          - i.   Length
    !!          - ii.  Percentile values
    !!      - I. Expected Output Parameters
    !!      .
    !!  - II. Load Grid. 
    !!        Load grid coordinates and bathymetric depth from CSV file with 5 columns
    !!        representing an x coordinate, y coordinate, bathymetric depth (z), latitude, and longitude.
    !!
    !!  - III. Set Growth parameters and initial conditions
    !!      - A. Set shell height intervals to define classes
    !!      - B. Compute state transition matrices
    !!          - i.   Growth
    !!          - ii.  Recruitment
    !!          - iii. Mortality
    !!
    !! ReadParameters
    !!      - RunCount
    !!      - SizeBinWidth (typically 5mm)
    !!      - NumberofSizeClasses
    !!      - NumberYears
    !!      - StepsPerYear
    !!      - GridParameters
    !!          - grid size
    !!          - spatial limits)
    !!      - NumBootstraps
    !!      - RandomNumberSeed
    !!      - GrowthParameters 
    !!          - K
    !!          - L_infinty)
    !!      - Shell height to weight parameters
    !!      - Dredge effeciency parameters
    !!      - Natural Mortality (may depend on space, density)
    !!      - Fishing selectivity parameters (may depend on space)
    !!      - Catch Rate (LPUE) parameters
    !! ReadInitialConditions (Number at shell height bin vectors at each grid location, potentially with uncertanty)
    !! ReadManagementParameters (which areas are closed, open and access), levels of fishing by area
    !! Make Growth Matrices
    !! Selectivity Vectors
    !! Meat and Gonad Weight Vectors etc
    !!
    !! MAIN LOOP
    !!
    !!      Loop over simulation number
    !!
    !!          Loop over years/time steps
    !!
    !!              Loop over Grid Points (maybe double loop)
    !! 
    !!  
    !!
    !! @f{eqnarray*}{
    !!    \vec{\mathbf{Size}}[Grid] &=& | \mathbf{GrowthMatrix}[Grid] | * | \vec{\mathbf{Size}}[Grid] | \\
    !!                         &*& e^{-(Mort_{nat}[Grid,Height_{shell}] + Mort_{fish}[Grid,Height_{shell}]) * timestep}
    !! @f}
    !!
    !!
    !! Calculate Harvest
    !!
    !! WriteOutput (harvest,biomass and numbers by grid cell, maybe only every year)
    !!
    !! Geostatistics, plots etc then likely done in R, matlab etc.
    !!
    !! @section Growth_Mod
    !!
    !! @ref sec1
    !!
    !! @section Recruit_Mod
    !!
    !! @section Mortallity_Mod

    use globals
    use Growth_Mod
    use Data_Point_Mod
    use Recruit_Mod
    use Mortality_Mod
    use Output_Mod

    implicit none

    integer n, j  !> loop count

    character(2) domain_name
    integer start_year, stop_year, year
    logical is_rand_rec
    character(3) fishing_type      !> Fishing can be USD,  BMS,  or,  CAS
    integer num_time_steps
    integer num_monte_carlo_iter
    character(72) file_name
    real(dp) delta_time

    type(Data_Vector_Class):: grid
    real(dp) element_area, domain_area
    integer num_grids

    type(Growth_Class), allocatable :: growth(:)
    type(Mortality_Class), allocatable :: mortality(:)
    type(Recruit_Class), allocatable :: recruit(:)

    integer, parameter :: shell_len_min = 30
    integer, parameter :: shell_len_delta = 5
    integer, parameter :: num_size_classes = (150 - shell_len_min) / shell_len_delta + 1

    real(dp), allocatable :: state(:, :)
    real(dp), allocatable :: state_time_steps(:,:)
    real(dp), allocatable :: shell_height_mm(:)
    real(dp), allocatable :: weight_grams(:,:)
    real(dp), allocatable :: fishing_effort(:)
    real(dp), allocatable :: samp(:,:)


    call Read_Input(domain_name, file_name, start_year, stop_year, fishing_type, num_time_steps, num_monte_carlo_iter)

    delta_time = 1._dp / float(num_time_steps)
    
    is_rand_rec = .FALSE.
    
    write(*,*) '========================================================'
    write(*,'(A,A6)') ' Domain:         ', domain_name
    write(*,'(A,I6)') ' Start Year:     ', start_year
    write(*,'(A,I6)') ' Stop Year:      ', stop_year
    write(*,'(A,L6)') ' Random''d Recruit:  ', is_rand_rec
    write(*,'(A,A6)') ' Fishing Type:   ', fishing_type
    write(*,'(A,I6,A,F7.4)') ' Time steps/year:', num_time_steps, ' delta ', delta_time
    write(*,*) '========================================================'
    
    call Load_Grid(grid, domain_name)
    num_grids = grid%len
    element_area = meters_per_naut_mile**2 ! convert 1 sq nm to sq m
    domain_area = float(num_grids) * element_area
    write(*,'(A,I7)') ' Number of Grids: ',grid%len
    write(*,*)        ' Domain Area: ', domain_area, ' square meters'
    
    write(*,*) '========================================================'

    allocate(growth(1:num_grids), mortality(1:num_grids), recruit(1:num_grids) )

    do j=1, num_grids
       mortality(j)%mgmt_area_index = grid%posn(j)%mgmt_area_index
    enddo
    
    allocate(shell_height_mm(1:num_size_classes))
    allocate(state(1:num_grids,1:num_size_classes), &
    &        weight_grams(1:num_grids,1:num_size_classes), &
    &        fishing_effort(1:num_grids),&
    &        samp(1:num_grids, 1:num_size_classes), &
    &        state_time_steps(1:num_time_steps,1:num_size_classes))
    !
    ! Assign recruitment by year and node, initialize shell_height_mm
    !
    call Set_Growth(growth, grid, shell_height_mm, delta_time, num_size_classes, domain_name, &
    &              shell_len_min, shell_len_delta, file_name, start_year, state, weight_grams)
    call Set_Recruitment(recruit, num_grids, domain_name, is_rand_rec, num_size_classes, &
                         & growth(1:num_grids)%L_inf_mu, growth(1:num_grids)%K_mu, shell_height_mm)
    call SetMortality(mortality, grid, shell_height_mm, num_size_classes, domain_name)

    ! Start simulation
    do year = start_year, stop_year
        do j = 1, num_grids
            mortality(j)%select = Ring_Size_Selectivity(year, shell_height_mm, mortality(j)%is_closed)
        enddo
        call Set_Fishing(fishing_type, year, num_grids, state, weight_grams, mortality, fishing_effort, element_area)
        do n = 1, num_grids
            call Time_To_Grow(growth(n), mortality(n), recruit(n), state(n, 1:num_size_classes), &
            &           fishing_effort(n), delta_time, num_time_steps, state_time_steps, domain_area, year)
            call Scallop_Output_Region(year, start_year, stop_year, num_size_classes, num_grids, state_time_steps, &
            &                        num_time_steps, grid, n)
            samp(n, 1:num_size_classes) = state_time_steps(floor(float(num_time_steps)/2.), 1:num_size_classes)! sample mid calander year for output
    !        samp(n, 1:num_size_classes) = state_time_steps(171, 1:num_size_classes)
        enddo  
        call Scallop_Output(year, num_size_classes, num_grids, samp, fishing_effort, weight_grams, mortality, recruit, &
        &                  start_year, stop_year, element_area, domain_area)
    enddo
    


    deallocate(growth, mortality, recruit, shell_height_mm, state, weight_grams, fishing_effort, samp)

END PROGRAM ScallopPopDensity

