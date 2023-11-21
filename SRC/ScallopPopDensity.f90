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
    !!      - E. Initial Conditions as read from Input/SimMA2000/InitialCondition.csv
    !!      - F. Fishing Type: can be USD, BMS, or CAS
    !!          - USD: fishing proportional to value of stock
    !!          - BMS: fishing proportional to biomass
    !!          - CAS: fishing spatially constant with region, CASA
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
    !! @f{equation}{
    !! \begin{split}
    !!   \vec{\mathbf{State}}[Grid] =& \left| \mathbf{GrowthMatrix}[Grid] \right| \times \vec{\mathbf{State}}[Grid]  \notag\\
    !!                              &\times \left|e^{-(Mort_{nat}[Grid,Height_{shell}] + Mort_{fish}[Grid,Height_{shell}]) * timestep}\right|
    !! \end{split}
    !! @f}
    !!
    !!      - A. Loop years
    !!
    !!          - i.   Determine fishing effort
    !!          - ii.  For each grid
    !!              - a. Compute new state
    !!              - b. Compute regional average
    !!          - iii. ouput annual data
    !!
    !! 
    !! @section Growth_Mod
    !!
    !! @ref Gsec1
    !!
    !! @section Recruit_Mod
    !!
    !! @ref Rsec1
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

    interface
        integer function Load_Grid(g, d)
            use Data_Point_Mod
            type(Data_Vector_Class), intent(inout) :: g
            character(2), intent(in) :: d
        endfunction
    endinterface

    integer n,k !> loop count

    character(2) domain_name
    integer start_year, stop_year, year
    logical is_rand_rec
    character(3) fishing_type      !> Fishing can be USD,  BMS,  or,  CAS
    integer num_time_steps
    integer num_monte_carlo_iter
    character(72) file_name

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
    real(dp), allocatable :: state_at_time_step(:,:)
    real(dp), allocatable :: shell_height_mm(:)
    real(dp), allocatable :: weight_grams(:,:)
    real(dp), allocatable :: fishing_effort(:) ! rate of fishing mortality TODO units?
    real(dp), allocatable :: mid_year_sample(:,:)

    !==================================================================================================================
    !  - I. Read Configuration file 'Scallop.inp'
    !==================================================================================================================
    call Read_Input(domain_name, file_name, start_year, stop_year, fishing_type, num_time_steps, num_monte_carlo_iter)

    is_rand_rec = .FALSE.

    write(*,*) '========================================================'
    write(*,'(A,A6)') ' Domain:         ', domain_name
    write(*,'(A,I6)') ' Start Year:     ', start_year
    write(*,'(A,I6)') ' Stop Year:      ', stop_year
    write(*,'(A,L6)') ' Random''d Recruit:  ', is_rand_rec
    write(*,'(A,A6)') ' Fishing Type:   ', fishing_type
    write(*,'(A,I6,A,F7.4)') ' Time steps/year:', num_time_steps, ' delta ', 1._dp / dfloat(num_time_steps)
    write(*,*) '========================================================'

    !==================================================================================================================
    !  - II. Load Grid. 
    !==================================================================================================================
    num_grids = Load_Grid(grid, domain_name)
    element_area = meters_per_naut_mile**2 ! convert 1 sq nm to sq m
    domain_area = float(num_grids) * element_area
    write(*,'(A,I7)') ' Number of Grids: ', num_grids
    write(*,*)        ' Domain Area: ', domain_area, ' square meters'
    
    write(*,*) '========================================================'

    allocate(growth(1:num_grids), mortality(1:num_grids), recruit(1:num_grids) )

    do n=1, num_grids
       mortality(n)%mgmt_area_index = grid%posn(n)%mgmt_area_index
    enddo
    
    allocate(shell_height_mm(1:num_size_classes))
    allocate(state(1:num_grids,1:num_size_classes), &
    &        weight_grams(1:num_grids,1:num_size_classes), &
    &        fishing_effort(1:num_grids),&
    &        mid_year_sample(1:num_grids, 1:num_size_classes), &
    &        state_at_time_step(1:num_time_steps,1:num_size_classes))

    !==================================================================================================================
    !  - III. Set Growth parameters and initial conditions
    !
    ! Assign recruitment by year and node, initialize shell_height_mm
    !
    !==================================================================================================================
    call Set_Growth(growth, grid, shell_height_mm, num_time_steps, num_size_classes, domain_name, domain_area, element_area, &
    &              shell_len_min, shell_len_delta, file_name, state, weight_grams)
    call Set_Recruitment(recruit, num_grids, domain_name, domain_area, element_area,  is_rand_rec, num_size_classes, &
                         & growth(1:num_grids)%L_inf_mu, growth(1:num_grids)%K_mu, shell_height_mm)
    call Set_Mortality(mortality, grid, shell_height_mm, num_size_classes, domain_name, domain_area, element_area)
    call Set_Scallop_Output( num_grids, num_size_classes, domain_name, domain_area, element_area, start_year, stop_year,&
    &                        num_time_steps)

    !==================================================================================================================
    !  - VII. MAIN LOOP
    ! Start simulation
    !==================================================================================================================
    do year = start_year, stop_year
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        !  i. Determine fishing effort
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        do n = 1, num_grids
            !+++++++++++++++++++++++++++++++++++++++
            ! Moved from ScallopMortality
            !+++++++++++++++++++++++++++++++++++++++
            mortality(n)%select = Ring_Size_Selectivity(year, shell_height_mm, mortality(n)%is_closed)
            ! These equations included here as they are dependent on %select
            mortality(n)%discard = 0.2 * mortality(n)%select

            do k = 1, num_size_classes
                if(shell_height_mm(k).gt.90.) mortality(n)%discard(k) = 0.D0
            !+++++++++++++++++++++++++++++++++++++++
            enddo
        enddo
        fishing_effort = Set_Fishing(fishing_type, year, state, weight_grams, mortality)

        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        !  ii. For each grid
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        do n = 1, num_grids
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            !  a. Compute new state
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            state_at_time_step(1:num_time_steps, 1:num_size_classes) = Time_To_Grow(growth(n), mortality(n), recruit(n), &
            &                         state(n, 1:num_size_classes), fishing_effort(n), year)
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            !  b. Compute regional average
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            call Scallop_Output_Regional_Avg(year, state_at_time_step, grid, n, mid_year_sample(n,:))
        enddo  
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !  iii. ouput annual data
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        call Scallop_Output_Annual(year, mid_year_sample, fishing_effort, weight_grams, mortality, recruit)
    enddo
    
    deallocate(growth, mortality, recruit, shell_height_mm, state, weight_grams, fishing_effort, mid_year_sample)

END PROGRAM ScallopPopDensity

