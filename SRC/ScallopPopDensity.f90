PROGRAM ScallopPopDensity
    !> @mainpage Scallop Population Density
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
    !!          - i.   Start year
    !!          - ii.  Finish year
    !!          - iii. Random recruit start year
    !!          - iv.  Number of random fields
    !!      - H. Parameters for Monte Carlo TODO
    !!          - i.   Length
    !!          - ii.  Percentile values
    !!      - I. Expected Output Parameters
    !!      .
    !!  - II. Load Grid. 
    !!        Load grid coordinates and bathymetric depth from CSV file with 6 columns
    !!        - x coordinate
    !!        - y coordinate
    !!        - z - bathymetric depth
    !!        - latitude
    !!        - longitude
    !!        - management area index
    !!
    !!  - III. Set Growth parameters and initial conditions
    !!      - A. Set shell height intervals to define classes
    !!      - B. Compute state transition matrices
    !!          - i.   Growth
    !!          - ii.  Recruitment
    !!          - iii. Mortality
    !!
    !!  - IV.  Make Growth Matrices
    !!  - V.   Selectivity Vectors
    !!  - VI.  Meat and Gonad Weight Vectors etc
    !!
    !!  - VII. MAIN LOOP
    !!
    !!      Loop over simulation number
    !!
    !!          Loop over years/time steps
    !!
    !!              Loop over Grid Points (maybe double loop)
    !! 
    !!      @f{eqnarray*}{
    !!          \vec{\mathbf{Size}}[Grid] &=& | \mathbf{GrowthMatrix}[Grid] | * | \vec{\mathbf{Size}}[Grid] | \\
    !!                                    &*& e^{-(Mort_{nat}[Grid,Height_{shell}] + Mort_{fish}[Grid,Height_{shell}]) * timestep}
    !!      @f}
    !!
    !!  - VIII. Calculate Harvest
    !!
    !!  - IX.  WriteOutput (harvest,biomass and numbers by grid cell, maybe only every year)
    !!
    !!  - X.   Geostatistics, plots etc then likely done in R, matlab etc.
    !!
    !! @section Growth_Mod
    !!
    !! @ref sec1
    !!
    !! @section Recruit_Mod
    !!
    !! @ref sec2
    !!
    !! @section Mortallity_Mod
    !!
    !----------------------------------------------------------------------------

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
    type(Recruitment_Class), allocatable :: recruit(:)

    integer, parameter :: shell_len_min = 30
    integer, parameter :: shell_len_delta = 5
    integer, parameter :: num_size_classes = (150 - shell_len_min) / shell_len_delta + 1

    real(dp), allocatable :: state(:, :)
    real(dp), allocatable :: state_time_steps(:,:)
    real(dp), allocatable :: shell_height_mm(:)
    real(dp), allocatable :: weight_grams(:,:)
    real(dp), allocatable :: fishing_effort(:)
    real(dp), allocatable :: samp(:,:)

    !==================================================================================================================
    !  - I. Read Configuration file 'Scallop.inp'
    !==================================================================================================================
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

    !==================================================================================================================
    !  - II. Load Grid. 
    !==================================================================================================================
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

    !==================================================================================================================
    !  - III. Set Growth parameters and initial conditions
    !
    ! Assign recruitment by year and node, initialize shell_height_mm
    !
    !==================================================================================================================
    call Set_Growth(growth, grid, shell_height_mm, delta_time, num_size_classes, domain_name, domain_area, element_area, &
    &              shell_len_min, shell_len_delta, file_name, start_year, state, weight_grams)
    call Set_Recruitment(recruit, num_grids, domain_name, domain_area, element_area,  is_rand_rec, num_size_classes, &
                         & growth(1:num_grids)%L_inf_mu, growth(1:num_grids)%K_mu, shell_height_mm)
    call SetMortality(mortality, grid, shell_height_mm, num_size_classes, domain_name, domain_area, element_area)

    !==================================================================================================================
    !  - VII. MAIN LOOP
    ! Start simulation
    !==================================================================================================================
    do year = start_year, stop_year
        do j = 1, num_grids
            mortality(j)%select = Ring_Size_Selectivity(year, shell_height_mm, mortality(j)%is_closed)
        enddo
        call Set_Fishing(fishing_type, year, state, weight_grams, mortality, fishing_effort)
        do n = 1, num_grids
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            !  - VIII. Calculate Harvest
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            call Time_To_Grow(growth(n), mortality(n), recruit(n), state(n, 1:num_size_classes), &
            &           fishing_effort(n), delta_time, num_time_steps, state_time_steps, year)
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            !  - IX.  WriteOutput by region
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            call Scallop_Output_Region(year, start_year, stop_year, num_size_classes, num_grids, state_time_steps, &
            &                        num_time_steps, grid, n)
            ! sample mid calander year for output
            samp(n, 1:num_size_classes) = state_time_steps(floor(float(num_time_steps)/2.), 1:num_size_classes)
    !        samp(n, 1:num_size_classes) = state_time_steps(171, 1:num_size_classes)
        enddo  
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !  - IX.  WriteOutput (harvest,biomass and numbers by grid cell, maybe only every year)
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        call Scallop_Output(year, num_size_classes, num_grids, samp, fishing_effort, weight_grams, mortality, recruit, &
        &                  start_year, stop_year, element_area)
    enddo
    
    deallocate(growth, mortality, recruit, shell_height_mm, state, weight_grams, fishing_effort, samp)

END PROGRAM ScallopPopDensity

