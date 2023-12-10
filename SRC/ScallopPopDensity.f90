PROGRAM ScallopPopDensity
    !> @mainpage Scallop Population Density
    !! This program is used to compute Scallop Density after a given growth period
    !!
    !! @section ms1 Initialize Simulation Parameters
    !! @subsection ms1p1 Read Input
    !!  - Domain Name = MA or GB
    !!  - Beging Year = YYYY
    !!  - Ending Year = YYYY
    !!  - Fishing type = USD, BMS, or CAS
    !!  - Time steps per Year = 52
    !!  - Initial Conditions File, population density in #/m^2 umber of grids by number of size classes
    !!    File Name = Data/bin5mm2000MA.csv this data is pulled from dredge survey data in OriginalData/dredgetowbysize7917.csv
    !!    or InitialCondition/InitialCondition.txt this data has been interpolated to lay over existing grid positions
    !!
    !! @subsection ms1p2 Instantiate Growth Module
    !! The simulation then instantiates parameters that define how growth occurs
    !!
    !! @subsubsection ms1p2p1 Load Grid and Initial State
    !! The data in each file, Data/bin5mmYYYY[MA|GB].csv has grid information of where each grid is located and its depth. 
    !! Data in the same row is used for the initial state, in units of scallop count per square for each size classs.
    !!
    !! @subsubsection ms1p2p2 For each class: Define shell_lengths weight conversion
    !! @paragraph ms1p2p2p1 Shell Length
    !! Starting at 30mm to 150mm inclusive, in 5 mm steps.\n
    !! That is (150 – 30) / 5 + 1, or 25 size classes
    !!
    !! @paragraph ms1p2p2p2 Weigth in grams
    !! GB
    !! @f{eqnarray*}{
    !! ShellToWeight = exp( &-& 6.69 + 2.878 * log(shellLengthmm) \\
    !!                      &-& 0.0073 * depth - 0.073 * latitude \\
    !!                      &+& (1.28 - 0.25 * log(shellLengthmm)) * isClosed )
    !! @f}
    !!
    !!
    !! MA\n
    !! @f{eqnarray*}{
    !! ShellToWeight = exp( &-& 9.713394 + 2.62025 * log(shell_length_mm) \\
    !!                      &-& 0.004665 * depth + 0.021 * latitude \\
    !!                      &-& 0.031 * isClosed)
    !! @f}
    !!
    !! where @a isClosed is 1 if closed or 0 if open
    !!
    !! @subsubsection ms1p2p3 Computes Growth Parameters, given depth, latitude, and isClosed
    !! - @f$L_{\infty_\mu}@f$
    !! - @f$L_{\infty_\sigma}@f$
    !! - @f$K_\mu@f$
    !! - @f$K_\sigma@f$
    !!
    !! @subsubsection ms1p2p4 Computes G matrix for given growth parameters
    !!
    !> From MN18 p. 1312, 1313
    !! @f[
    !! c = 1.0 - e^{-K_{\mu} * \delta_t}
    !! @f]
    !! @f[
    !! \eta = c * L_{{\infty}_\mu}
    !! @f]
    !! For each size class, @a k
    !! @f[
    !! \omega_k = l_k - l_{k-1}
    !! @f]
    !! @f[
    !! \omega_{k_{avg}} = \frac{l_k + l_{k-1}}{2}
    !! @f]
    !! @f[
    !! \Omega = (1 - c) \omega_k
    !! @f]
    !! @f[
    !! X(y,k) = l_y - \eta - (1-c)l_{k}
    !! @f]
    !! @f[
    !! \Phi(x,\mu,\sigma) = \frac{1}{2}(1+Erf(\frac{x-\mu}{\sigma\sqrt{2}}))
    !! @f]
    !! @f[
    !! \phi(x,\mu,\sigma) = \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{(x-\mu)^2}{2\sigma^2}}
    !! @f]
    !! @f[
    !! H_{MN18}(x, \sigma, \omega) = \frac{1}{\omega}\left[x\Phi_N(x,0,\sigma^2) + \sigma^2\phi_N(x,0,\sigma^2)\right]
    !! @f]
    !!
    !! @f[
    !! G(y, k, \sigma, \omega_k) = H_{MN18}(X(y,k-1), \sigma, \Omega)\\
    !!                           - H_{MN18}(X(y,k),   \sigma, \Omega)
    !! @f]
    !! 
    !! @subsection ms1p3 Instantiate Recruitment
    !!
    !! The simulation next instantiates how recruitment will be handled.
    !! @subsubsection ms1p3p1 Recruitment data
    !! @paragraph ms1p3p1p1 For years 1979, 2018
    !! Data is read in from KrigingEstimates/Sim[MA|GB]YYYY/KrigingEstimate.txt
    !! @paragraph ms1p3p1p2 For years 2019, 2025
    !! Data is read in from KrigingEstimates/Sim[MA|GB]Clim//KrigingEstimate.txt
    !!
    !! @subsubsection ms1p3p2 This method is effectively setting
    !! For all years\n
    !!  - Year_index = year - 1978
    !!  - for year_index in [1..max]
    !!      -recruitment(year_index) = KrigingEstimate
    !!      - year(year_index) = year
    !!      - rec_start = 1/365, or January 1st
    !!      - rec_stop = 100/365, or April 10
    !!
    !! @subsubsection ms1p3p3 It then quantizes recruitment,
    !! For each grid, n
    !!  - L30mm = (@f$L_{\infty_\mu}@f$(n) – 30) * exp(-@f$K_\mu@f$(n))
    !!  - For each class, j
    !!      - If (length(n) <= L30mm) recruit(n).max_rec_ind = j
    !!
    !! @subsection ms1p4 Instantiate Mortality
    !! @section ms2 Main Loop
    !! @subsection ms2p1 Determine Selectivity
    !! @subsection ms2p2 Set Fishing Effort
    !! @subsection ms2p3 For Each Grid
    !! @subsubsection ms2p3p1 Compute natural mortality based on current state
    !! @subsubsection ms2p3p2 Adjust population state based on von Bertalanffy growth
    !! @subsubsection ms2p3p3 Compute increase in population due to recruitment, R
    !! @subsubsection ms2p3p4 Compute Overall Mortality
    !! @subsubsection ms2p3p5 Compute effect of mortality to arrive at new state

    !----------------------------------------------------------------------------

    use globals
    use Growth_Mod
    use Data_Point_Mod
    use Recruit_Mod
    use Mortality_Mod
    use Output_Mod

    implicit none

    integer n,k !> loop count

    character(2) domain_name
    integer start_year, stop_year, year
    logical is_rand_rec
    logical use_interp
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
    real(dp), allocatable :: shell_length_mm(:)
    real(dp), allocatable :: weight_grams(:,:)
    real(dp), allocatable :: fishing_effort(:) ! rate of fishing mortality TODO units?
    real(dp), allocatable :: mid_year_sample(:,:)

    character(130) :: stateFileName

    !==================================================================================================================
    !  - I. Read Configuration file 'Scallop.inp'
    !==================================================================================================================
    call Read_Input(domain_name, file_name, start_year, stop_year, fishing_type, num_time_steps, num_monte_carlo_iter)
    n = index(file_name, '/') - 1
    use_interp = (file_name(1:n) == 'InitialCondition')

    is_rand_rec = .FALSE.

    write(*,*) '========================================================'
    write(*,'(A,A6)') ' Domain:         ', domain_name
    write(*,'(A,I6)') ' Start Year:     ', start_year
    write(*,'(A,I6)') ' Stop Year:      ', stop_year
    write(*,'(A,L6)') ' Random''d Recruit:  ', is_rand_rec
    write(*,'(A,A6)') ' Fishing Type:   ', fishing_type
    write(*,'(A,I6,A,F7.4)') ' Time steps/year:', num_time_steps, ' delta ', 1._dp / dfloat(num_time_steps)
    write(*,*) '========================================================'

    ! allow for maximum expected number of grids
    allocate(growth(1:num_dimensions), mortality(1:num_dimensions), recruit(1:num_dimensions) )
    allocate(shell_length_mm(1:num_size_classes))
    allocate(state(1:num_dimensions,1:num_size_classes), &
    &        weight_grams(1:num_dimensions,1:num_size_classes))
    allocate(fishing_effort(1:num_dimensions),&
    &        mid_year_sample(1:num_dimensions, 1:num_size_classes), &
    &        state_at_time_step(1:num_time_steps,1:num_size_classes))

    !==================================================================================================================
    !  - II. Instantiate modes, that is objects
    !==================================================================================================================
    call Set_Growth(use_interp, growth, grid, shell_length_mm, num_time_steps, num_size_classes, domain_name, domain_area,&
    &              element_area, shell_len_min, shell_len_delta, file_name, state, weight_grams)
    num_grids = grid%len

    call Set_Recruitment(recruit, num_grids, domain_name, domain_area, element_area,  is_rand_rec, num_size_classes, &
                         & growth(1:num_grids)%L_inf_mu, growth(1:num_grids)%K_mu, shell_length_mm)
    call Set_Mortality(mortality, grid, shell_length_mm, num_size_classes, domain_name, domain_area, element_area)
    call Set_Scallop_Output( num_grids, num_size_classes, domain_name, domain_area, element_area, start_year, stop_year,&
    &                        num_time_steps)

    !==================================================================================================================
    !  - III. MAIN LOOP
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
            mortality(n)%select = Ring_Size_Selectivity(year, shell_length_mm, mortality(n)%is_closed)

            ! These equations included here as they are dependent on %select
            do k = 1, num_size_classes
                if (domain_name .eq. 'MA') then
                    if(shell_length_mm(k) .gt. 90.) then
                        mortality(n)%discard(k) = 0.D0
                    else
                        mortality(n)%discard(k) = 0.2 * mortality(n)%select(k)
                    endif
                else
                    if ((shell_length_mm(k) .gt. 100.) .and. (mortality(n)%is_closed)) then
                        mortality(n)%discard(k) = 0.D0
                    else
                        mortality(n)%discard(k) = 0.2 * mortality(n)%select(k)
                    endif
                endif
            enddo
            !+++++++++++++++++++++++++++++++++++++++
        enddo
        fishing_effort = Set_Fishing(fishing_type, year, state, weight_grams, mortality)

        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        !  ii. For each grid
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        write( stateFileName,"(A,I4,A)") growth_out_dir//'State', year, '_main.csv'
        do n = 1, num_grids
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            !  a. Compute new state
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            state_at_time_step(1:num_time_steps, 1:num_size_classes) = Time_To_Grow(growth(n), mortality(n), recruit(n), &
            &                         state(n, 1:num_size_classes), fishing_effort(n), year)
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            !  b. Compute regional average
            !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            !!!call Scallop_Output_Regional_Avg(year, state_at_time_step, grid, n, mid_year_sample(n,:))
            if (n .eq. 22) &
            &   call Write_CSV(num_time_steps, num_size_classes, state_at_time_step, stateFileName, size(state_at_time_step,1))
        enddo

        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !  iii. ouput annual data
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !!!call Scallop_Output_Annual(year, mid_year_sample, fishing_effort, weight_grams, mortality, recruit)
    enddo
    
    deallocate(growth, mortality, recruit, shell_length_mm, state, weight_grams, fishing_effort, mid_year_sample)

END PROGRAM ScallopPopDensity

