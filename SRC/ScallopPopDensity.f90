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

    interface

    subroutine Set_Initial_Conditions(filename, numgrids, nsc, length, yr, grow, st)
        use globals
        use Growth_Mod
        implicit none
        character(*), intent(in) :: filename
        type(Growth_Class), intent(in):: grow(*)
        integer, intent(in):: numgrids, nsc, yr
        real(dp), intent(out):: st(numgrids, *)
        real(dp), intent(in):: length(*)
    end subroutine Set_Initial_Conditions
    
    real(dp) function Shell_to_Weight(shell_height, is_closed, depth, latitude, domain, ispp)
        use globals
        implicit none
        real(dp) , intent(in) :: shell_height, depth, latitude
        logical , intent(in) :: is_closed
        character (*), intent(in) :: domain
        logical, intent(in) ::  ispp
    end function Shell_to_Weight
    

    end interface

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

    real(dp) L30mm

    character(*), parameter :: init_cond_dir = 'InitialCondition/'


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
    !
    ! Assign recruitment by year and node, initialize shell_height_mm
    !
    call Set_Growth(growth, grid, shell_height_mm, delta_time, num_size_classes, &
    &               domain_name, shell_len_min, shell_len_delta)
    call Set_Recruitment(recruit, num_grids, domain_name, is_rand_rec, num_size_classes)
    call SetMortality(mortality, grid, shell_height_mm, num_size_classes, domain_name)

    allocate(state(1:num_grids,1:num_size_classes), &
    &        weight_grams(1:num_grids,1:num_size_classes), &
    &        fishing_effort(1:num_grids),&
    &        samp(1:num_grids, 1:num_size_classes), &
    &        state_time_steps(1:num_time_steps,1:num_size_classes))

    call Set_Initial_Conditions(file_name, num_grids, num_size_classes, shell_height_mm, start_year, growth, state)
    open(write_dev,file = init_cond_dir//'RecIndx.txt')
    do n = 1, num_grids
        L30mm = (growth(n) % L_inf_mu -30.D0) * exp(-growth(n) % K_mu)
        do j=1, num_size_classes 
            if (shell_height_mm(j) .le. L30mm) recruit(n)%max_rec_ind = j
        enddo
        write(write_dev,*) n, L30mm, recruit(n)%max_rec_ind
    enddo
    close(write_dev)

    ! Compute Shell Height (mm) to Meat Weight (g)
    do n = 1, num_grids
        do j = 1, num_size_classes
            weight_grams(n,j) =  Shell_to_Weight(shell_height_mm(j), grid%posn(n)%is_closed, &
            &                         grid%posn(n)%z, grid%posn(n)%lat, domain_name, .false.)
        enddo
        if ( (domain_name .eq. 'GB') .and. (grid%posn(n)%mgmt_area_index .eq. 11) ) &    ! Peter Pan 
        &    weight_grams(n,j) = Shell_to_Weight(shell_height_mm(j), grid%posn(n)%is_closed, &
        &                               grid%posn(n)%z, grid%posn(n)%lat, domain_name, .true.)
    enddo

    call Write_CSV(num_grids, num_size_classes, weight_grams, growth_out_dir//'WeightShellHeight.csv', num_grids)

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

!-----------------------------------------------------------------------
!> Sets initial conditions based on data in file_name
!! 
!! @param[in] file_name The name of the file to read input conditions
!! @param[in] num_grids The number of grids that the simulation is considering
!! @param[in] num_size_classes The number of size classes the simulation is evaluating for growth
!!            The size of the scallop is placed into bins of 5mm increments, typically
!!            - 30 to 35
!!            - 35+ to 40
!!            - 40+ to 45
!!            - ...
!!            - 150+
!! @param[in] length The length, or size, of the shell, typically in mm
!! @param[in] start_year The year that the simulation starts estimating scallop growth
!! @param[in] growth Expected??? scallop growth
!! @param[in] state Current scallop state, in scallops per square meter
!-----------------------------------------------------------------------
subroutine Set_Initial_Conditions(file_name, num_grids, num_size_classes, length, start_year, growth, state)
    use globals
    use Growth_Mod
    
    implicit none
    character(*), intent(in) :: file_name
    type(Growth_Class), intent(in):: growth(*)
    integer, intent(in):: num_grids, num_size_classes, start_year
    real(dp), intent(out):: state(num_grids, *)
    real(dp), intent(in):: length(*)
    integer n, j, num_rows, num_cols
    character(72) buffer
    
    write(buffer,'(I6)')start_year

    num_rows = num_grids
    num_cols = num_size_classes
    
    call Read_CSV(num_rows, num_cols, file_name, state, num_grids)
    PRINT *, term_blu
    PRINT '(A,A)', ' READ FROM FILE: ', file_name
    PRINT '(A,I6)', ' LENGTH = ', num_rows
    PRINT '(A,I5)', ' NUMBER OF SCALLOP SIZE CLASSES: ', num_size_classes
    PRINT *, term_blk

    if (num_rows > num_grids) num_rows = num_grids
    do n=1,num_rows
        do j=num_size_classes,2,-1
            if( (length(j) .gt. growth(n)%L_inf_mu) .and. (state(n,j) .gt. 0.D0) )then
                state(n,j-1) = state(n,j-1)+state(n,j)! lump scallops into smaller class
                state(n,j) = 0.D0
            endif
        enddo
    enddo
    
    return
endsubroutine Set_Initial_Conditions

!-----------------------------------------------------------------------
!> Computes weight given a shell height
!!
!! For Mid-Atlantic
!!
!! @f{eqnarray*}{
!!    exp &=& -9.713394 + 2.51 * log(height_{mm}) \\
!!        &+& (-0.0033 * depth) + 0.021 * latitude \\
!!        &+& (-0.031 * isClosed) + 0.00525 * (log(height_{mm}) * 21.0) \\
!!        &+& (-0.000065 * (21.0 * depth))
!! @f}
!! @f[
!! weight = e^{exp}
!! @f]
!!
!! @param[in] shell_height_mm The shell height, or length, in millimeters
!! @param[in] is_closed Logic to indicate if grid is open (F) or closed (T) to fishing
!! @param[in] depth The depth of the grid in meters
!! @param[in] latitude Geographic coordinate
!! @param[in] domain
!!            - MA for Mid-Atlantic
!!            - GB for Georges Bank
!! @param[in] ispp Logic to indiate is Peter Pan???
!! @returns weight in grams
!-----------------------------------------------------------------------
real(dp) function Shell_to_Weight(shell_height_mm, is_closed, depth, latitude, domain, ispp)
    !Get Scallop meat weight shell height
    use globals
    implicit none
    real(dp) , intent(in) :: shell_height_mm, depth, latitude
    logical , intent(in) :: is_closed
    character (*), intent(in) :: domain
    logical, intent(in) ::  ispp

    Shell_to_Weight = 0._dp ! default

    !mm to grams
    if(domain(1:2).eq.'GB')then
        Shell_to_Weight = exp(-6.69 + 2.878 * log(shell_height_mm) + (-0.0073 * depth) + (-0.073 * latitude) &
        &                + 1.28 * Logic_To_Double(is_closed) &
        &                + (-0.25 * (log(shell_height_mm) * Logic_To_Double(is_closed))))
        !for GB scallops, open covariate is 1 in the former groundfish closed areas or access areas and 0 in the open areas (includes NLS-EXT and CAII-EXT)
        if(ispp) Shell_to_Weight = exp(-11.84 + 3.167 * log(shell_height_mm)) !for peterpan 
    endif
    if(domain(1:2).eq.'MA')then
        Shell_to_Weight = exp(-9.48 + 2.51 * log(shell_height_mm) - 0.1743 - 0.059094 + (-0.0033 * depth) &
        &      + 0.021 * latitude + (-0.031 * Logic_To_Double(is_closed)) + 0.00525 * (log(shell_height_mm) * 21.) &
        &      + (-0.000065 * (21. * depth)))
    endif

    return

endfunction Shell_to_Weight
    
    