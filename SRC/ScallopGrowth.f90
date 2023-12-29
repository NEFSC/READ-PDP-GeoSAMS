!>----------------------------------------------------------------------------------------------------------------
!> @page page1 Growth_Mod
!>
!> @section Gsec1 Growth Class
!>
!> The scallop state at each node in the domain is a vector of length @f$N_{sc} = (150 - 30) / 5 + 1 = 25@f$ 
!> representing the abundance of scallops in size classes @f$[30-35mm, 35-40mm, ...145-150mm, 150mm+]@f$.  
!> Size class transition matrices are generated for each node based on the work of  Millar and Nottingham 2018 
!> Appendix C, henceforth MN18 \ref mn "[1]", although other methods are present in the code including direct Monte Carlo simulation.
!> including( see subroutine@f${\it GenTransMat}@f$).
!>
!> Growth in GeoSAMS is based off of von Bertanlffy growth. 
!>
!> @f[
!> \delta(u) = (L_{\infty}-u)(1-e^{-K})
!> @f]
!>
!> Or from HC2009 \ref hc "[2]", equation (1)
!> @f[
!> L_{t} = L_{t-1} e^{-K} + L_{\infty}(1-e^{-K})
!> @f]
!>
!> We assume normal distribution on @f${L_\infty}@f$ and @f$K@f$ with all distribution parameters independent. 
!>
!> The shell height of the ith individual at time @f$t+1@f$, @f$L_{t+1,i}@f$ depends on the random effects 
!> (@f$\alpha_i@f$ and @f$\beta_i@f$) as well as the mean slope and intercept:
!> @f{equation}{
!> \begin{split}
!> &L_{t+1,i} = (m + \alpha_i)L_{t,i}+(b+\beta_i)+\epsilon,\notag\\
!> &\mbox{where }\epsilon\mbox{ is a random error with expected value zero.}
!> \end{split}
!> @f}
!>
!> The values of the distribution means (@f$\mu_{L_\infty}@f$ and @f$\mu_K@f$) are taken from previous work of 
!> Hart, HC2009. The distribution of increments by size class as in MN18). Growth increment is given by the 
!> von Bertlanaffy growth curve
!> 
!> We begin by determining the scallop time to grow for a given year: 
!> Computes the overall growth of the scallop population over a time period of (num_time_steps * delta_time) 
!> in units of years, typically one year with delta_time as a decimal year, e.g. one day = 1/365 = 0.00274
!>
!> For each time step, @f$\delta_t@f$
!>  - Computes mortality based on current state.
!>  - Computes increase in population due to recruitment, @f$\vec{R}@f$,if within recruitment months, i.e. Jan to April 10th
!> @f[
!> \vec{S} = \vec{S} + \delta_t\frac{\vec{R}}{RecruitDuration}
!> @f]
!>  - Adjusts population based on von Bertalanffy growth
!> @f[
!> \vec{S} = \left| G \right| \times \vec{S} 
!> @f]
!> where G is the transition matrix
!>  - Compute overall mortality, <b>M</b>
!> @f[
!> \vec{M} = \vec{M}_{nat} + Fishing * \vec{M}_{selectivity} + \vec{M}_{incidental} + \vec{M}_{discard}
!> @f]
!>
!>  - Compute new state
!> @f[
!> \vec{S_{t+1}} = \vec{S_t} * (1- \delta_t * \vec{M})
!> @f]
!>
!> \anchor mn 1. MN18 refers to Miller, R. B. and Nottingham, 2018, "Improved approximations for estimation of size-transition 
!> probabilities within size-structured models"
!>
!> \anchor hc 2. HC2009 refers to Hart, D. R. and Chute, A. S. 2009, "Estimating von Bertalanffy growth parameters from growth
!> increment data using a linear mixed-effects model, with an application to the sea scallop Placopecten magellanicus."
!>
!> @subsection Gsubsec1 Transition Matrix
!> A transition matrix, 25 by 25, is computed under the  assumption of von  Bertlanaffy growth.
!> It is assumed that the parameters of von BernBertlanaffy growth K and L_inf have normal distributions.
!>
!> From MN18 p. 1312, 1313
!> @f[
!> c = 1.0 - e^{-K_{\mu} * \delta_t}
!> @f]
!> @f[
!> \eta = c * L_{{\infty}_\mu}
!> @f]
!> @f[
!> \omega_k = l_k - l_{k-1}
!> @f]
!> @f[
!> \omega_{k_{avg}} = \frac{l_k + l_{k-1}}{2}
!> @f]
!> @f[
!> \Omega = (1 - c) \omega_k
!> @f]
!> @f[
!> X(y,k) = l_y - \eta - (1-c)l_{k}
!> @f]
!> @f[
!> G(y, k, \sigma, \omega_k) = H_{MN18}(X(y,k-1), \sigma, \Omega)\\
!>                           - H_{MN18}(X(y,k),   \sigma, \Omega)
!> @f]
!>
!> @subsubsection Gsubsubsec1 Function H(x, sigma, omega)
!> Given (MN18 Appendix B)
!>
!> @f$\Phi_N@f$ denotes the normal <b>cumulative</b> distribution function.
!>
!> @f$\phi_N@f$ denotes the normal <b>density</b> function.
!>
!> @f[
!> H_{MN18}(x, \sigma, \omega) = \frac{1}{\omega}\left[x\Phi_N(x,0,\sigma^2) + \sigma^2\phi_N(x,0,\sigma^2)\right]
!> @f]
!>
!> WAS
!> @f[
!> H_{MN18}(x, \sigma, \omega) = \frac{1}{\omega}(x*f+\sigma^2 * f)
!> @f]
!> where @f$f = \Phi_N@f$ 
!>
!> @subsubsection Gssubsubsec2 Normal Cumulative Distribution Function
!> @f[
!> \Phi(x,\mu,\sigma) = \frac{1}{2}(1+Erf(\frac{x-\mu}{\sigma\sqrt{2}}))
!> @f]
!>
!> @subsubsection Gssubsubsec3 Normal Density Function
!> @f[
!> \phi(x,\mu,\sigma) = \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{(x-\mu)^2}{2\sigma^2}}
!> @f]


!>----------------------------------------------------------------------------------------------------------------
MODULE Growth_Mod
    use globals
    use Data_Point_Mod
    implicit none

    interface
        integer function Load_Grid(g, d)
            use Data_Point_Mod
            type(Data_Point_Class), intent(inout) :: g(*)
            character(2), intent(in) :: d
        endfunction
    endinterface

    integer, parameter :: growth_param_size = 4

    !> @class Growth_Class
    !> 
    !> Subroutines that determine expected growth of scallops
        type Growth_Class
        !> @public @memberof Growth_Class
        !> Asymptotic size mean
        real(dp) L_inf_mu 
        !> @public @memberof Growth_Class
        !> Growth coefficient mean
        real(dp) K_mu  ! mean 
        !> @public @memberof Growth_Class
        !> Asymptotic size standard deviation
        real(dp) L_inf_sd
        !> @public @memberof Growth_Class
        !> Growth coefficient standard deviation
        real(dp) K_sd  ! 
        !> @public @memberof Growth_Class
        !> Growth matrix
        real(dp) G(num_size_classes, num_size_classes)
    end type Growth_Class

    ! @private @memberof Growth_Mod
    integer, PRIVATE :: num_grids
    character(2), PRIVATE :: domain_name
    real(dp), PRIVATE :: domain_area_sqm
    integer, PRIVATE :: num_time_steps
    integer, PRIVATE :: time_steps_year
    real(dp), PRIVATE :: delta_time

    CONTAINS

    !==================================================================================================================
    !> @public @memberof Growth_Class
    !>
    !> Initializes growth for startup
    !>
    !> @param[in,out] growth Parameters that identify how the scallop should grow
    !> @param[in] grid Vector that identifies the geospatial locations under simulation
    !> @param[in,out] shell_lengths Vector of the size, length, of scallops
    !> @param[in] num_ts number of time steps per year for simulation
    !> @param[in] num_sz_classes Number of size classes to set private member
    !> @param[in] domain_name Name of domain being simulate, 'MA' or 'GB'
    !> @param[out] domain_area, Size of domain under consideration in square meters
    !> @param[in] file_name The name of the file with initial state, i.e. scallops per sq meter
    !> @param[in] start_year Year in which to start simulation
    !> @param[out] state Initial state as set by initial conditions
    !> @param[in,out] weight_grams Computed combined scallop weight
    !> 
    !==================================================================================================================
    subroutine Set_Growth(use_interp, growth, grid, shell_lengths, num_ts, ts_per_year, dom_name, dom_area, &
        &                 file_name, state, weight_grams, ngrids)
        logical, intent(in) :: use_interp
        type(Growth_Class), intent(inout) :: growth(*)
        type(Data_Point_Class), intent(inout) :: grid(*)
        real(dp), intent(inout) :: shell_lengths(*)
        integer, intent(in) :: num_ts
        integer, intent(in) :: ts_per_year
        character(2), intent(in) :: dom_name
        real(dp), intent(out) :: dom_area
        character(*), intent(in) :: file_name
        ! state actual first dimension. Recall that fortran stores by column first.
        ! second dimension as this is constant
        real(dp), intent(out):: state(1:num_dimensions, 1:num_size_classes)
        real(dp), intent(inout) :: weight_grams(1:num_dimensions, 1:num_size_classes)
        integer, intent(out) :: ngrids
    
        integer n, j
        real(dp), allocatable :: Gpar(:,:)

        ! initalize private members
        domain_name = dom_name
        num_time_steps = num_ts
        delta_time = 1._dp / dfloat(ts_per_year)
        time_steps_year = ts_per_year

        ! Load Grid. 
        if (use_interp) then
            ! Loads grid data based on TBD, use to interpolate the survey data by overlaying onto this grid
            ! Grid is defined by: Grids/[MA|GB]xyzLatLon.csv
            ngrids = Load_Grid(grid, domain_name)
        else
            ! read in grid and state from file_name
            PRINT *, term_yel,'NO INTERP: using ', file_name,term_blk
            ngrids = Load_Grid_State(grid, state, file_name)
        endif
        ! set private variable
        num_grids = ngrids

        dom_area = float(num_grids) * grid_area_sqm
        domain_area_sqm = dom_area
        write(*,'(A,I7)') ' Number of Grids: ', num_grids
        write(*,*)        ' Domain Area: ', dom_area, ' square meters'
        
        write(*,*) '========================================================'

        ! Compute Shell Lengths (mm) and conversion to Meat Weight (g)
        shell_lengths(1:num_size_classes) = Set_Shell_Lengths(dfloat(shell_len_min), dfloat(shell_len_delta))
        do n = 1, num_grids
            weight_grams(n,1:num_size_classes) =  Shell_to_Weight(shell_lengths(1:num_size_classes), &
            &                grid(n)%is_closed, grid(n)%z, grid(n)%lat, grid(n)%mgmt_area_index )
        enddo
        call Write_CSV(num_grids, num_size_classes, weight_grams, growth_out_dir//'WeightShellHeight.csv', size(weight_grams,1))

        allocate(Gpar(1:num_grids, 1:growth_param_size))
        ! Compute Growth Parameters, L_inf_mu, K_mu, L_inf_sd, K_sd based on depth, latitude, and if grid is closed
        if(domain_name .eq. 'GB')then
            do n=1,num_grids
                call Get_Growth_GB(grid(n)%z, grid(n)%lat, grid(n)%is_closed, &
                &                  growth(n)%L_inf_mu, growth(n)%K_mu,&
                &                  growth(n)%L_inf_sd, growth(n)%K_sd, grid(n)%mgmt_area_index)
                ! Compute Growth Transition Matrix
                growth(n)%G = Gen_Size_Trans_Matrix(growth(n)%L_inf_mu, growth(n)%L_inf_sd, &
                &                              growth(n)%K_mu, growth(n)%K_sd, shell_lengths, 'AppxC')
            enddo
        else
            do n=1,num_grids
                call Get_Growth_MA(grid(n)%z, grid(n)%lat, grid(n)%is_closed, &
                &                  growth(n)%L_inf_mu, growth(n)%K_mu,&
                &                  growth(n)%L_inf_sd, growth(n)%K_sd)
                ! Compute Growth Transition Matrix
                growth(n)%G = Gen_Size_Trans_Matrix(growth(n)%L_inf_mu, growth(n)%L_inf_sd, &
                &                              growth(n)%K_mu, growth(n)%K_sd, shell_lengths, 'AppxC')
            enddo
        endif
        ! save grid paramaters
        Gpar(1:num_grids,1) = growth(1:num_grids)%L_inf_mu
        Gpar(1:num_grids,2) = growth(1:num_grids)%L_inf_sd
        Gpar(1:num_grids,3) = growth(1:num_grids)%K_mu
        Gpar(1:num_grids,4) = growth(1:num_grids)%K_sd
        call Write_CSV(num_grids, growth_param_size, Gpar, growth_out_dir//'GrowthParOut.csv', size(Gpar,1))
        deallocate(Gpar)

        if (use_interp) then
            ! Reads data from file_name
            ! Establishes state from which growth simulation begins as a f(growth(n)%L_inf_mu, length)
            call Set_Current_State(file_name, state, shell_lengths, growth)
        else
            ! already read in state via Load_Grid_State, truncate larger size classes.
            do n=1,num_grids
                do j=num_size_classes,2,-1
                    if( (shell_lengths(j) .gt. growth(n)%L_inf_mu) .and. (state(n,j) .gt. 0.D0) )then
                        ! lump scallops into smaller class
                        state(n,j-1) = state(n,j-1) + state(n,j)
                        state(n,j) = 0.D0
                    endif
                enddo
            enddo
        endif

        return 
    end subroutine Set_Growth

    !==================================================================================================================
    !> @fn Set_Current_State
    !> @public @memberof Growth_Class
    !>
    !> Sets the current state of scallop density based on data in file_name
    !> 
    !> @param[in] file_name The name of the file to read input conditions
    !> @param[in] length The length, or size, of the shell, in mm
    !> @param[in] start_year The year that the simulation starts estimating scallop growth
    !> @param[in] growth Expected??? scallop growth
    !> @returns Current scallop state, in scallops per square meter
    !==================================================================================================================
    subroutine Set_Current_State(file_name, state, length, growth)
        use globals
        
        implicit none
        character(*), intent(in) :: file_name
        real(dp), intent(inout) :: state(1:num_dimensions, 1:num_size_classes) 
        type(Growth_Class), intent(in):: growth(*)
        real(dp), intent(in):: length(*)
        integer n, j, num_rows, num_cols

        num_rows = num_grids
        num_cols = num_size_classes
        
        call Read_CSV(num_rows, num_cols, file_name, state, size(state,1))
        PRINT *, term_blu
        PRINT '(A,A)', ' READ FROM FILE: ', file_name
        PRINT '(A,I6)', ' LENGTH = ', num_rows
        PRINT '(A,I5)', ' NUMBER OF SCALLOP SIZE CLASSES: ', num_size_classes
        PRINT *, term_blk

        if (num_rows > num_grids) num_rows = num_grids
        do n=1,num_rows
            do j=num_size_classes,2,-1
                if( (length(j) .gt. growth(n)%L_inf_mu) .and. (state(n,j) .gt. 0.D0) )then
                    ! lump scallops into smaller class
                    state(n,j-1) = state(n,j-1) + state(n,j)
                    state(n,j) = 0.D0
                endif
            enddo
        enddo

        return
    endsubroutine Set_Current_State

    !==================================================================================================================
    !> @fn Gen_Size_Trans_Matrix
    !> @public @memberof Growth_Class
    !>
    !> Transition matrix used to determine the growth of the scallop. The equations are based on MN18, Appendix C
    !>
    !> @f{eqnarray*}{
    !>    \vec{\mathbf{Size}}[Grid] &=& \left| \mathbf{GrowthMatrix}[Grid] \right| \times \vec{\mathbf{Size}}[Grid] \\
    !>                         &\times& \left|e^{-(Mort_{nat}[Grid,Height_{shell}] + Mort_{fish}[Grid,Height_{shell}]) * timestep}\right|
    !> @f}
    !>
    !> @param[in] L_inf_mu    [real 1x1] = mean of von Bertlanaffy asymptotic growth parameter L_inf(see HC09     eqn 1)
    !> @param[in] L_inf_std   [real 1x1] =  standard deviation of von Bertlanaffy asymptotic growth parameter  L_inf(see HC09 eqn 1)
    !> @param[in] K_mu        [real 1x1] =  mean of  mean of von Bertlanaffy asymptotic growth parameter K(see HC09 eqn 1)
    !> @param[in] K_sd        [real 1x1] =  standard deviation of von Bertlanaffy growth parameter K(see HC09 eqn 1)
    !> @param[in] shell_lengths for each size class
    !> @returns Transition Matrix
    !> @author Keston Smith (IBSS corp) June-July 2021
    !==================================================================================================================
    function Gen_Size_Trans_Matrix(L_inf_mu, L_inf_sd, K_mu, K_sd, shell_lengths, method)
        implicit none
        real(dp), intent(in):: shell_lengths(*)
        real(dp), intent(in) :: K_mu, K_sd, L_inf_mu, L_inf_sd
        character(*), intent(in) :: method
        character(72) file_name
        real(dp)  :: Gen_Size_Trans_Matrix(1:num_size_classes, 1:num_size_classes)

        select case (method)
        case ('AppxC')
            Gen_Size_Trans_Matrix = MN18_AppxC_Trans_Matrix(L_inf_mu, K_mu, L_inf_sd, K_sd,shell_lengths)
        case default
            Gen_Size_Trans_Matrix = MN18_AppxC_Trans_Matrix(L_inf_mu, K_mu, L_inf_sd, K_sd,shell_lengths)
            write(*,*) term_red, ' Unknown Matrix Method', method, ' using C', term_blk
        endselect

       ! output transition matrix
        file_name = growth_out_dir//'Growth.csv'
        call Write_CSV(num_size_classes, num_size_classes, Gen_Size_Trans_Matrix, file_name, size(Gen_Size_Trans_Matrix,1))

        return
    end function Gen_Size_Trans_Matrix

    !==================================================================================================================
    !> @fn Set_Shell_Lengths
    !> @public @memberof Growth_Class
    !>
    !> setup shell shell_lengths intervals
    !> - length_min
    !> - length_min + length_delta
    !> - @f$length_{shell}(n) = length_{min} + (n-1) * length_{delta}@f$
    !>
    !> @param[in] length_min Size of smallest size class
    !> @param[in] length_delta amount between size classes
    !> @returns shell length in millimeters
    !==================================================================================================================
    function Set_Shell_Lengths(length_min, length_delta)
        real(dp) :: Set_Shell_Lengths(num_size_classes)
        real(dp), intent(in) :: length_min, length_delta
        integer n  ! loop counter

        Set_Shell_Lengths(1) = length_min
        do n=2, num_size_classes
            Set_Shell_Lengths(n) = Set_Shell_Lengths(n-1) + length_delta
        enddo
        return
    endfunction Set_Shell_Lengths

    !==================================================================================================================
    !>
    !> @public @memberof Growth_Class
    !>
    !> Provides growth parameters L and K parameters for Georges Bank.
    !> From R code sent by Dvora July 28th 2021
    !>
    !> @param[in] depth in meters
    !> @param[in] lat Geospatial coordinate, Latitude
    !> @param[in] is_closed Logical that indicates if grid is closed for fishing
    !> @param[out] L_inf_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] L_inf_sd  standard deviation von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_sd standard deviation von Bertlanaffy asymptotic growth parameter
    !> @param[in] area_index index to indicate management area
    !==================================================================================================================
    subroutine Get_Growth_GB(depth, lat, is_closed, L_inf_mu, K_mu, L_inf_sd, K_sd, area_index)
        real(dp), intent(in) :: depth, lat
        logical, intent(in) :: is_closed
        real(dp), intent(out) :: L_inf_mu, K_mu
        real(dp), intent(out) :: L_inf_sd, K_sd
        integer, intent(in) :: area_index

        real(dp) fixed_coef(6), random_eff(3)
        real(dp) depth_norm, latitude, intercept, slope, random_intercept, random_slope,randomCov
        real(dp) mean_start_shell_length, mean_depth, mean_lat, mean_ring2

        parameter(mean_start_shell_length = 94.73,  mean_depth = 72.89,  mean_lat = 41.04,  mean_ring2 = 109.576)
        if (area_index .eq. 11) then
            !Peter Pan region Linfity = 110.3 K = 0.423
            L_inf_mu = 110.3D0
            K_mu = 0.423D0
        else
            fixed_coef = (/ -34.96, 57.415, -8.51, -17.65, 0.9076, 3.741/)  !fixed effects coef
            ! intercept, slope, coef of depth, latitude, closed/open for intercept term
            random_eff = (/ 81.05, 51.38, -60.53 /)  !random effects, intercept, slope, cov
            depth_norm = depth / mean_depth
            latitude = lat/mean_lat
            intercept = fixed_coef(1) + fixed_coef(3) * depth_norm + fixed_coef(4)*latitude + fixed_coef(5) &
            &    * Logic_To_Double(is_closed) + mean_ring2
            slope = ( fixed_coef(2) + fixed_coef(6)* depth_norm ) / mean_start_shell_length
            random_intercept = random_eff(1)
            random_slope = random_eff(2) / mean_start_shell_length**2
            randomCov = random_eff(3) / mean_start_shell_length

            L_inf_mu = (intercept/(1.-slope)) + (intercept * random_slope / (1. - slope) + randomCov) / (1. - slope**2)
            K_mu = -log(slope) + random_slope/2. / slope**2
        endif

        L_inf_sd = 10.8D0
        K_sd = .045D0
        return
    end subroutine Get_Growth_GB
        
    !==================================================================================================================
    !>
    !> @public @memberof Growth_Class
    !>
    !> Provides growth parameters L and K parameters for Mid Atlantic
    !> From R code sent by Dvora July 28th 2021
    !>
    !> @param[in] depth in meters
    !> @param[in] lat Geospatial coordinate, Latitude
    !> @param[in] is_closed Logical that indicates if grid is closed for fishing
    !> @param[out] L_inf_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] L_inf_sd  standard deviation von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_sd standard deviation von Bertlanaffy asymptotic growth parameter
    !==================================================================================================================
    subroutine Get_Growth_MA(depth, lat, is_closed, L_inf_mu, K_mu, L_inf_sd, K_sd)
        real(dp), intent(in)::depth, lat
        logical, intent(in)::is_closed
        real(dp), intent(out)::L_inf_mu,K_mu
        real(dp), intent(out) :: L_inf_sd, K_sd

        real(dp) fixed_coef(5),random_eff(3)
        real(dp) depth_norm, latitude, intercept, slope, random_intercept, random_slope, randomCov
        real(dp) mean_start_shell_length, mean_depth, mean_lat, mean_ring2

        parameter(mean_start_shell_length = 85.74,  mean_depth = 52.79,  mean_lat = 38.99,  mean_ring2 = 106.17 )
                fixed_coef = (/0.951,48.333,-9.53,-37.51,-2.31 /)  !fixed effects coef
        ! intercept, slope, coef of depth, latitude, closed/open for intercept term
        random_eff = (/38.35,13.27,-20.77 /)  !random effects, intercept, slope, cov 
        depth_norm = depth / mean_depth
        latitude = lat/mean_lat
        intercept = fixed_coef(1)+fixed_coef(3) * depth_norm + fixed_coef(4) *latitude &
        &         + fixed_coef(5) * Logic_To_Double(is_closed) + mean_ring2
        slope = fixed_coef(2) / mean_start_shell_length
        random_intercept = random_eff(1)
        random_slope = random_eff(2) / mean_start_shell_length**2
        randomCov = random_eff(3) / mean_start_shell_length
        L_inf_mu = (intercept / (1.-slope)) + (intercept*random_slope / (1.-slope) + randomCov) / (1.-slope**2)
        K_mu = -log(slope) + random_slope/2./slope**2

        L_inf_sd = 10.8D0
        K_sd = .045D0
        return
    end subroutine Get_Growth_MA

        
    !==================================================================================================================
    !> @fn MN18_AppxC_Trans_Matrix
    !> @public @memberof Growth_Class
    !> Purpose: This subroutine computes a sizeclass transition matrix under the  assumption of von  Bertlanaffy growth.
    !> It is assumed that the parameters of von BernBertlanaffy growth K and L_inf have normal distributions.
    !>
    !> From MN18 p. 1312, 1313
    !> @f[
    !> c = 1.0 - e^{-K_{\mu} * \delta_t}
    !> @f]
    !> @f[
    !> \eta = c * L_{{\infty}_\mu}
    !> @f]
    !> @f[
    !> \omega_k = l_k - l_{k-1}
    !> @f]
    !> @f[
    !> \omega_{k_{avg}} = \frac{l_k + l_{k-1}}{2}
    !> @f]
    !> @f[
    !> \Omega = (1 - c) \omega_k
    !> @f]
    !> @f[
    !> X(y,k) = l_y - \eta - (1-c)l_{k}
    !> @f]
    !> @f[
    !> G(y, k, \sigma, \omega_k) = H_{MN18}(X(y,k-1), \sigma, \Omega)\\
    !>                           - H_{MN18}(X(y,k),   \sigma, \Omega)
    !> @f]
    !>
    !> @param[in]        L_inf_mu [real 1x1] = mean of von Bertlanaffy asymptotic growth parameter L_inf(see HC09     eqn 1)
    !> @param[in]        K_mu [real 1x1] =  mean of  mean of von Bertlanaffy asymptotic growth parameter K(see HC09 eqn 1)
    !> @param[in]        L_inf_std [real 1x1] =  standard deviation of von Bertlanaffy asymptotic growth parameter  L_inf(see HC09 eqn 1)
    !> @param[in]        K_std [real 1x1] =  standard deviation of von Bertlanaffy growth parameter K(see HC09 eqn 1)
    !> @param[in]        shell_lengths [real nx1] = shell_lengths for each size class 
    !>
    !> @returns G [real n x n] = size transition matrix estimated under the assumption of uniform size 
    !>                  distribution within size interval and growth distribution evaluated at 
    !>                  mid point of size interval. Derivation is from MN18 appendix C.  
    !>                  Derivation of formula for growth increment mean and variance is in MN18eq7.pdf
    !>
    !> history:  Written by keston Smith (IBSS corp) May 2021
    ! -----------------------------------------------------------------------
    function MN18_AppxC_Trans_Matrix(L_inf_mu, K_mu, L_inf_sd, K_sd, shell_lengths)
        real(dp), INTENT(IN) :: L_inf_mu, K_mu, L_inf_sd, K_sd, shell_lengths(num_size_classes)
        real(dp) :: MN18_AppxC_Trans_Matrix(num_size_classes, num_size_classes)
        real(dp), allocatable :: G(:,:)
    
        ! local variables
    
        real(dp) omega, mu, sigma, omega_avg
        integer j, k
        real(dp) Fya, Fyb, c, eta, Ha, Hb, x0, x1, w

        allocate(G(1:num_size_classes,1:num_size_classes) )

        G = 0._dp
        c = 1._dp - exp(-K_mu * delta_time)
        eta = c * L_inf_mu
    
        do k = 2, num_size_classes
            ! calculate interval omega and midpoint
            omega = shell_lengths(k) - shell_lengths(k-1) 
            ! average size
            omega_avg = (shell_lengths(k) + shell_lengths(k-1)) / 2._dp
    
            ! calculate increment mean,mu->evaluated at midpoint and increment standard deviation sigma->evaluated at midpoint   
            call increment_mean_std(L_inf_mu, K_mu, L_inf_sd, K_sd, omega_avg, mu, sigma)
            do j = k, num_size_classes
                x0 = shell_lengths(j) - eta - (1._dp - c) * shell_lengths(k-1)
                x1 = shell_lengths(j) - eta - (1._dp - c) * shell_lengths(k)
                w = (1._dp - c) * omega
                Ha = H_MN18(x0, sigma, w)
                Hb = H_MN18(x1, sigma, w)
                Fya = Ha - Hb
    
                x0 = shell_lengths(j-1) - eta - (1._dp - c) * shell_lengths(k-1)
                x1 = shell_lengths(j-1) - eta - (1._dp - c) * shell_lengths(k)
                Ha = H_MN18(x0, sigma, w)
                Hb = H_MN18(x1, sigma, w)
                Fyb = Ha - Hb
                G(j-1, k-1) = Fya - Fyb
            enddo
        enddo

        ! If growth increments are assumed normally distributed the left hand tail of the distribution 
        ! can lead to unrealistic "shrinking" transitions
        call enforce_non_negative_growth(G)
        do j = 1,num_size_classes
            if (shell_lengths(j) > L_inf_mu) then
                G(j,1:num_size_classes) = 0._dp
                G(j,j) = 1._dp
            endif
        enddo

        do k = 1, num_size_classes
            do j = 1, num_size_classes
                if (G(k,j) < 1.0D-14) G(k,j) = 0._dp
            enddo
        enddo

        MN18_AppxC_Trans_Matrix = G

        deallocate( G )
        return
    end function MN18_AppxC_Trans_Matrix

    !==================================================================================================================
    !>
    !> Purpose: This subroutine computes a growth increment distribution parameters under 
    !> the  assumption of von Bertlanaffy growth and normally distributed growth increments. 
    !> It is assumed that the parameters of von BernBertlanaffy growth K and L_inf have
    !> normal distributions.
    !>
    !> @public @memberof Growth_Class
    !>
    !> @param[in] L_inf_mu [real 1x1] = mean of von Bertlanaffy asymptotic growth parameterL_inf(see HC09 eqn 1)
    !> @param[in] K_mu [real 1x1] =  mean of von Bertlanaffy growth parameter K(see HC09 eqn 1)
    !> @param[in] L_inf_sd [real 1x1] =  standard deviation of von Bertlanaffy asymptoticgrowth parameter L_inf(see  HC09 eqn 1)
    !> @param[in] K_sd [real 1x1] =  standard deviation of von Bertlanaffy growth parameter (see HC09 eqn 1)
    !> @param[in] size [real 1x1] = size to estimate increment stats
    !>
    !>
    !> @param[out] mu [1x1] = mean of increment at size
    !> @param[out] sigma [1x1]  = standard deviation of increment at size
    !>
    !> history:  Written by keston Smith (IBSS corp) May 2021
    !==================================================================================================================
    subroutine increment_mean_std(L_inf_mu, K_mu, L_inf_sd, K_sd, size, mu, sigma)
        real(dp), INTENT(IN) :: L_inf_mu, K_mu, L_inf_sd, K_sd, size
        
        real(dp), INTENT(OUT) :: mu, sigma
        
        real(dp) sigma2
        real(dp) prod_mu, prod_sd
        real(dp) H, B, y, gammainc, TL, TY, Q, a
        integer n_data
        
        ! change notation and remove dimension from K for convenience
        prod_mu = K_mu * delta_time 
        prod_sd = K_sd * delta_time
        
        y = (size - L_inf_mu) / (L_inf_sd * sqrt(2._dp))
        
        !H = (L_inf_mu / 2._dp) * (1._dp + erf(y)) - (L_inf_sd / sqrt(2._dp * pi)) * exp(-1._dp * y * y)
        H = (size / 2._dp) * (1._dp + erf(y)) + (L_inf_sd / sqrt(2._dp * pi)) * exp(-y * y)
        B = (1._dp / 2._dp) * (erf(-y) + 1._dp)
        n_data = 1
        a = 3._dp / 2._dp
        call gamma_inc_values ( n_data, a, y**2, gammainc )
        mu = (H / B - size) * (1._dp - exp(.5_dp * prod_sd**2 - prod_mu))
        mu = max(mu, 0._dp)

        Q = sqrt(pi) * (1._dp-erf(y))
        TL = (L_inf_mu - size)**2 + ( 2._dp * sqrt(2._dp) * L_inf_sd * (L_inf_mu - size) * exp(-y**2) ) / Q &
        &    + 2._dp * (L_inf_sd**2) * gammainc / Q
        TY = exp(2._dp * prod_sd**2 - 2._dp * prod_mu) - 2._dp * exp(.5_dp * prod_sd**2 - prod_mu)  + 1._dp 
        sigma2 = TL * TY - mu**2
        
        ! mean and variance for unconditional growth
        ! mu = (L_inf_mu-size) * (1._dp-exp(.5_dp * prod_sd**2-prod_mu))
        ! sigma2 = (L_inf_sd**2 + (L_inf_mu-size)**2) * (exp(2 * prod_sd**2-2 * prod_mu)-2 * exp(.5_dp * prod_sd**2-prod_mu) + 1._dp)-((exp(.5_dp * prod_sd**2-prod_mu)-1)**2) * ((L_inf_mu-size)**2)
        ! mu = max(mu,0._dp)

        sigma = sqrt(sigma2)
        !write(*,*) size, mu, sigma ,L_inf_mu,L_inf_sd,prod_mu,prod_sd
        
        return
    end subroutine increment_mean_std
    
    !==================================================================================================================
    !>
    !> @public @memberof Growth_Class
    !> @fn H_MN18
    !>
    !> Given (MN18 Appendix B)
    !>
    !> @f$\Phi_N@f$ denotes the normal <b>cumulative</b> distribution function.
    !>
    !> @f$\phi_N@f$ denotes the normal <b>density</b> function.
    !>
    !> @f[
    !> H_{MN18}(x, \sigma, \omega) = \frac{1}{\omega}[x\Phi_N(x,0,\sigma^2) + \sigma^2\phi_N(x,0,\sigma^2)]
    !> @f]
    !>
    !> WAS
    !> @f[
    !> H_{MN18}(x, \sigma, \omega) = \frac{1}{\omega}(x*f+\sigma^2 * f)
    !> @f]
    !> where @f$f = \Phi_N@f$ 
    !>
    !> @param[in] x - evaluation point
    !> @param[in] sigma - paramaters defined within MN18
    !> @param[in] w  - paramaters defined within MN18
    !>
    !> @returns H - variable 
    !==================================================================================================================
    real(dp) function H_MN18(x,sigma,w)
        real(dp), INTENT(IN) ::sigma,w,x
        real(dp) y

        !H_MN18 = (1._dp / w) * (x * Norm_Cumul_Dist_Fcn(x, 0._dp, sigma**2) + (sigma**2) * Norm_Density_Fcn(x, 0._dp, sigma**2))
        y = x / (sigma * sqrt(2._dp))
        H_MN18 = (1._dp / w) * ((x / 2._dp) * ( 1._dp + erf( y)) + (sigma / sqrt(2._dp * pi)) * exp(-y*y))

    endfunction H_MN18
    
    !==================================================================================================================
    !>
    !> @public @memberof Growth_Class
    !> @fn Norm_Cumul_Dist_Fcn
    !> computation of normal cumulative distribution function
    !>
    !> @f[
    !> \Phi(x,\mu,\sigma) = \frac{1}{2}(1+Erf(\frac{x-\mu}{\sigma\sqrt{2}}))
    !> @f]
    !>
    !> @param[in] mu - mean 
    !> @param[in] sigma - standard deviation
    !> @param[in] x - evaluation point
    !>
    !> @returns normal cdf value at x, f(x|mu,sigma)
    !==================================================================================================================
    real(dp) function Norm_Cumul_Dist_Fcn(x, mu, sigma)
        real(dp), intent(in) :: mu,sigma,x
        Norm_Cumul_Dist_Fcn = 0.5_dp * ( 1._dp + erf( (x - mu) / (sigma * sqrt(2._dp) ) ))
    endfunction Norm_Cumul_Dist_Fcn

    !==================================================================================================================
    !>
    !> @public @memberof Growth_Class
    !> @fn Norm_Density_Fcn
    !> computation of normal density function
    !>
    !> @f[
    !> \phi(x,\mu,\sigma) = \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{(x-\mu)^2}{2\sigma^2}}
    !> @f]
    !>
    !> @param[in] mu - mean 
    !> @param[in] sigma - standard deviation
    !> @param[in] x - evaluation point
    !>
    !> @returns normal density function at x
    !==================================================================================================================
    real(dp) function Norm_Density_Fcn(x, mu, sigma)
        real(dp), intent(in) :: mu,sigma,x
        real(dp) tmp
        tmp = 1._dp / (sigma * sqrt(2._dp*pi))
        Norm_Density_Fcn = tmp * exp(-(x - mu)**2 / (2._dp* sigma**2))
    endfunction Norm_Density_Fcn

    !==================================================================================================================
    !>
    !> @public @memberof Growth_Class
    !>
    !> @param[in,out]    G - growth transition matrix with negative growth lumped into 0 growth
    !==================================================================================================================
    subroutine enforce_non_negative_growth(G)
        real(dp), INTENT(INOUT) :: G(num_size_classes,*)
        ! local
        integer j,k
        real(dp) col_sum
        
        ! Transitioning from I_k to I_j
        ! if j < k, force G(j,k) = 0. Scallop does not shrink
        do k = 2, num_size_classes
            do j = 1,k-1
                G(j,k) = 0.
            enddo
        enddo
        
        ! force column col_sum to equal 1.0
        ! That is col_sum of a probability distribution function 
        do k = 1, num_size_classes
            col_sum = 0.
            do j = k, num_size_classes
                if(G(j,k) .lt. 0.) G(j,k) = 0.
                col_sum = col_sum + G(j,k)
            enddo
            do j = k, num_size_classes
                ! because of above loop we already know that G(j,k) >= 0
                ! therefore the col_sum will be >= 0
                if(col_sum .gt. 0.)then
                    G(j,k) = G(j,k)/col_sum ! if a+b+c+..z = col_sum then a/col_sum+b/col_sum+c/col_sum+ ..z/col_sum = 1.0
                else
                    ! col_sum == 0
                    if(j .eq. k) G(j,k) = 1._dp
                endif
            enddo

        enddo
        
        return
    end subroutine enforce_non_negative_growth
           
           
    !==================================================================================================================
    !>
    !> @fn Time_To_Grow
    !> @public @memberof Growth_Class
    !> @brief Computes growth in scallop population.
    !>
    !> Computes the overall growth of the scallop population over a time period of (num_time_steps * delta_time) 
    !> in units of years, typically one year with delta_time as a percent of year.
    !>
    !> For each time step, @f$\delta_t@f$
    !>  - Computes mortality based on current state.
    !>  - Computes increase in population due to recruitment, @f$\vec{R}@f$,if within recruitment months, i.e. Jan to April 10th
    !> @f[
    !> \vec{S} = \vec{S} + \delta_t\frac{\vec{R}}{RecruitDuration}
    !> @f]
    !>  - Adjusts population based on von Bertalanffy growth
    !> @f[
    !> \vec{S} = \left| G \right| \times \vec{S} 
    !> @f]
    !>  - Compute overall mortality, <b>M</b>
    !> @f[
    !> \vec{M} = \vec{M}_{nat} + Fishing *( \vec{M}_{selectivity} + \vec{M}_{incidental} + \vec{M}_{discard})
    !> @f]
    !>
    !>  - Compute new state
    !> @f[
    !> \vec{S_{t+1}} = \vec{S_t} * (1- \delta_t * \vec{M})
    !> @f]
    !> 
    !> @param[in] growth object to hold growth simulation paramters
    !> @param[in,out] mortality object to hold mortality simulation parameters
    !> @param[in,out] recruit object to hold recruitment simulation parameters
    !> @param[in,out] state vector of the current state in scallops per square meter
    !> @param[in] fishing_effort vector of fishing effort by location
    !> @param[out] state_time_steps State at each time step
    !> @param[in] start_year under considration
    !==================================================================================================================
    function Time_To_Grow(ts, growth, mortality, recruit, state, fishing_effort, year)

        use Mortality_Mod, only : Mortality_Class, Compute_Natural_Mortality
        use Recruit_Mod, only : Recruitment_Class
        
        implicit none
        integer, intent(in) :: ts !time step
        type(Growth_Class), INTENT(IN):: growth
        type(Mortality_Class), INTENT(INOUT):: mortality
        type(Recruitment_Class), INTENT(INOUT):: recruit
        integer, intent(in) :: year
        real(dp),intent(inout) :: state(*)
        real(dp) :: Time_To_Grow(num_size_classes)
        real(dp), intent(in) :: fishing_effort
        real(dp) t
        integer Rindx
        real(dp), allocatable :: M(:), Rec(:)
            
        allocate(M(1:num_size_classes), Rec(1:num_size_classes))

        ! find location of current year
        Rindx = minloc( abs(recruit%year(1:recruit%n_year) - year), 1)
        Rec(1:num_size_classes) = 0.D0
        Rec(1:recruit%max_rec_ind) = recruit%recruitment(Rindx)/float(recruit%max_rec_ind)

        t = dfloat(ts) * delta_time

        ! Compute natural mortality based on current state
        mortality%natural_mortality(1:num_size_classes) = Compute_Natural_Mortality(recruit%max_rec_ind, mortality, state)

        ! adjust population state based on von Bertalanffy growth
        state(1:num_size_classes) = matmul(growth%G(1:num_size_classes, 1:num_size_classes), state(1:num_size_classes))

        ! Compute increase due to recruitment
        if ( ( t .gt. recruit%rec_start ) .and. ( t.le.recruit%rec_stop) ) state(1:num_size_classes) = &
        &       state(1:num_size_classes) + delta_time * Rec(1:num_size_classes) / (recruit%rec_stop-recruit%rec_start)

        ! Compute overall mortality
        M(1:num_size_classes) = mortality%natural_mortality(1:num_size_classes) &
        & + fishing_effort * ( mortality%selectivity(1:num_size_classes) &
        & + mortality%incidental + mortality%discard(1:num_size_classes) )

        ! Apply mortality and compute new state
        Time_To_Grow(1:num_size_classes) = state(1:num_size_classes) * (1.D0- delta_time * M(1:num_size_classes))

        deallocate(M, Rec)
        
        return
    endfunction Time_To_Grow
        
    !==================================================================================================================
    !> @fn Shell_to_Weight
    !> @public @memberof Growth_Class
    !>
    !> Computes weight given a shell height
    !>
    !> For Mid-Atlantic
    !>
    !> @f{eqnarray*}{
    !>    x &=& -9.48 + 2.51 * log(length_{mm}) \\
    !>        &-& 0.1743 - 0.059094 \\
    !>        &-& 0.0033 * depth    \\
    !>        &+& 0.021 * latitude  \\
    !>        &-& 0.031 * isClosed \\
    !>        &+& 0.00525 * log(length_{mm} * 21.0\\
    !>        &-& 0.000065 * 21.0  * depth
    !> @f}
    !>
    !> For Georges Bank
    !>
    !> @f{eqnarray*}{
    !>    x &=& -6.69 + 2.878 * log(length_{mm}) \\
    !>        &-& 0.0073 * depth \\
    !>        &-& 0.073 * latitude \\
    !>        &+& 1.28 * isClosed \\
    !>        &-& 0.25 * log(length_{mm}) * isClosed
    !> @f}
    !> @f[
    !> weight_g = e^{x}
    !> @f]
    !>
    !> @param[in] shell_length_mm The shell height, or length, in millimeters
    !> @param[in] is_closed Logic to indicate if grid is open (F) or closed (T) to fishing
    !> @param[in] depth The depth of the grid in meters
    !> @param[in] latitude Geographic coordinate
    !> @param[in] domain
    !>            - MA for Mid-Atlantic
    !>            - GB for Georges Bank
    !> @param[in] ispp Logic to indiate is Peter Pan???
    !> @returns weight in grams
    !==================================================================================================================
    elemental real(dp) function Shell_to_Weight(shell_length_mm, is_closed, depth, latitude, mgmt_idx)
        use globals
        implicit none
        real(dp) , intent(in) :: shell_length_mm, depth, latitude
        logical , intent(in) :: is_closed
        integer, intent(in) ::  mgmt_idx

        !mm to grams
        if(domain_name.eq.'GB')then
            !for GB scallops, open covariate is 1 in the former groundfish closed areas or access areas and 0 in the open areas (includes NLS-EXT and CAII-EXT)
            if(mgmt_idx .eq. 11) then
                Shell_to_Weight = exp(-11.84 + 3.167 * log(shell_length_mm)) !for peterpan 
            else
                Shell_to_Weight = exp(-6.69 + 2.878 * log(shell_length_mm) + (-0.0073 * depth) + (-0.073 * latitude) &
                &                + (1.28 - 0.25 * log(shell_length_mm)) * Logic_To_Double(is_closed))
            endif
        else
            ! Shell_to_Weight = exp(-9.48 + 2.51 * log(shell_length_mm) - 0.1743 - 0.059094 + (-0.0033 * depth) &
            ! &      + 0.021 * latitude + (-0.031 * Logic_To_Double(is_closed)) + 0.00525 * (log(shell_length_mm) * 21.) &
            ! &      + (-0.000065 * (21. * depth)))
            Shell_to_Weight = exp(-9.713394 + 2.62025 * log(shell_length_mm) - 0.004665 * depth + 0.021 * latitude &
            &                     - 0.031 * Logic_To_Double(is_closed))
        endif
        return
    endfunction Shell_to_Weight

    !==================================================================================================================
    !> @fn Load_Grid_State
    !> @public @memberof Growth_Class
    !>
    !> This function is used to set the grid parameters and the initial state to start the simulation. 
    !>
    !> It does so by reading the CSV file at file_name. This file has been generated by the TrawlData5mm.m 
    !> Matlab script. The format is for each grid in a row, the columns are
    !> Decimal Year, UTM X, UTM Y, Latitude, Longitude, UTM Z, Grid Is Closed, Followed by Scallop Density in Count/m^2
    !> sorted by shell length 30 to 150 mm in 5mm increments for 25 columns
    !>
    !> @param[in, out] grid Holds position information 
    !> @param[out] state Holds the initial state at various location specified by grid
    !> @param[in] file_name CSV name to be read in
    !>
    !==================================================================================================================
    integer function Load_Grid_State(grid, state, file_name)
        type(Data_Point_Class), intent(inout) :: grid(*)
        real(dp), intent(out):: state(1:num_dimensions, 1:num_size_classes)
        character(*), intent(in) :: file_name

        character(2000) input_str
        integer n, io, is_closed

        real(dp) year ! used as place holder when reading file_name

        PRINT *, 'OPENING ', file_name

        open(63, file=file_name, status='old')
        n = 0
        do
            read(63,'(a)',iostat=io) input_str
            if (io.lt.0) exit
            n=n+1
            read(input_str,*) year, grid(n)%x, grid(n)%y, grid(n)%lat, grid(n)%lon, grid(n)%z, &
            &               is_closed, state(n,1:num_size_classes)
            grid(n)%is_closed = (is_closed > 0)
            grid(n)%mgmt_area_index = 0
        end do
        close(63)
        write(*,*) term_blu, 'READ ', n, 'LINE(S)', term_blk

        Load_Grid_State = n

    endfunction Load_Grid_State
    
    
END MODULE Growth_Mod
