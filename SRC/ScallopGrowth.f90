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
!> where G is the transition matrix
!>  - Compute overall mortality, <b>M</b>
!> @f[
!> \vec{M} = \vec{M}_{nat} + Fishing * \vec{M}_{select} + \vec{M}_{incidental} + \vec{M}_{discard}
!> @f]
!>
!>  - Compute new state
!> @f[
!> \vec{S} = \vec{S} * (1- \delta_t * \vec{M})
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
!> G(y, l_{k}, \eta, c, \sigma, \omega_k)    = H_{MN18}(X(y,k-1), \sigma, [1-c]\omega_k)\\
!>                                           - H_{MN18}(X(y,k),   \sigma, [1-c]\omega_k)
!> @f]
!> @f[
!> X(y,k) = y - \eta - (1-c)l_{k}
!> @f]
!> @f[
!> c = 1.0 - e^{-K_{\mu} * \delta_t}
!> @f]
!> @f[
!> \eta = c * L_{{\infty}_\mu}
!> @f]
!> @f[
!> \omega_k = l_k - l_{k-1}
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
        implicit none
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
        real(dp) G(max_size_class, max_size_class)
    end type Growth_Class

    ! @private @memberof Growth_Mod
    integer, PRIVATE :: num_grids
    integer, PRIVATE :: num_size_classes
    character(2), PRIVATE :: domain_name
    real(dp), PRIVATE :: domain_area_sqm
    real(dp), PRIVATE :: grid_area_sqm
    integer, PRIVATE :: num_time_steps
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
    !> @param[in] domain_area,Size of domain under consideration in square meters
    !> @param[in] element_area, Size of grid in square meters
    !> @param[in] length_min Minimum length of shell being considered
    !> @param[in] length_delta Size step to sort classes 
    !>            @f$size_{max} = length_{min} + (numSizeClasses-1) * length_{delta}@f$
    !> @param[in] file_name The name of the file with initial state, i.e. scallops per sq meter
    !> @param[in] start_year Year in which to start simulation
    !> @param[out] state Initial state as set by initial conditions
    !> @param[in,out] weight_grams Computed combined scallop weight
    !> 
    !==================================================================================================================
    subroutine Set_Growth(growth, grid, shell_lengths, num_ts, num_sz_classes, dom_name, dom_area, element_area, &
        &                 length_min, length_delta, file_name, state, weight_grams)
        use Data_Point_Mod
        type(Growth_Class), intent(inout) :: growth(*)
        type(Data_Vector_Class), intent(in) :: grid
        real(dp), intent(inout) :: shell_lengths(num_size_classes)
        integer, intent(in) :: num_ts
        integer, intent(in) :: num_sz_classes
        character(2), intent(in) :: dom_name
        real(dp), intent(in) :: dom_area
        real(dp), intent(in) :: element_area
        integer, intent(in) :: length_min, length_delta
        character(*), intent(in) :: file_name
        real(dp), intent(out):: state(1:grid%len, 1:num_sz_classes)
        real(dp), intent(inout) :: weight_grams(1:grid%len, 1:num_sz_classes)
    
        integer n, j
        real(dp) Gpar(grid%len, growth_param_size)

        !> initalize private members
        num_grids = grid%len
        num_size_classes = num_sz_classes
        domain_name = dom_name
        domain_area_sqm = dom_area
        grid_area_sqm = element_area
        num_time_steps = num_ts
        delta_time = 1._dp / dfloat(num_ts)

        shell_lengths(1:num_size_classes) = Set_Shell_Heights(dfloat(length_min), dfloat(length_delta))
                
        if(domain_name .eq. 'GB')then
            do n=1,num_grids
                ! Get_Growth is expecting is_open, change to .NOT. is_closed?
                call Get_Growth_GB(grid%posn(n)%z, grid%posn(n)%lat, .NOT. grid%posn(n)%is_closed, &
                &                  growth(n)%L_inf_mu, growth(n)%K_mu,&
                &                  growth(n)%L_inf_sd, growth(n)%K_sd, grid%posn(n)%mgmt_area_index)
                growth(n)%G = Gen_Size_Trans_Matrix(growth(n)%L_inf_mu, growth(n)%L_inf_sd, &
                &                              growth(n)%K_mu, growth(n)%K_sd, shell_lengths, 'AppxC')
            enddo
        else
            do n=1,num_grids
                ! Get_Growth is expecting is_open, change to .NOT. is_closed?
                call Get_Growth_MA(grid%posn(n)%z, grid%posn(n)%lat, .NOT. grid%posn(n)%is_closed, &
                &                  growth(n)%L_inf_mu, growth(n)%K_mu,&
                &                  growth(n)%L_inf_sd, growth(n)%K_sd)
                growth(n)%G = Gen_Size_Trans_Matrix(growth(n)%L_inf_mu, growth(n)%L_inf_sd, &
                &                              growth(n)%K_mu, growth(n)%K_sd, shell_lengths, 'AppxC')
            enddo
        endif

        Gpar(1:num_grids,1) = growth(1:num_grids)%L_inf_mu
        Gpar(1:num_grids,2) = growth(1:num_grids)%L_inf_sd
        Gpar(1:num_grids,3) = growth(1:num_grids)%K_mu
        Gpar(1:num_grids,4) = growth(1:num_grids)%K_sd
        call Write_CSV(num_grids, growth_param_size, Gpar, growth_out_dir//'GrowthParOut.csv', num_grids)

        ! Establishes state from which growth simulation begins as a f(growth(n)%L_inf_mu, length)
        state = Set_Current_State(file_name, shell_lengths, growth)

        ! Compute Shell Height (mm) to Meat Weight (g)
        do n = 1, num_grids
            do j = 1, num_size_classes
                weight_grams(n,j) =  Shell_to_Weight(shell_lengths(j), grid%posn(n)%is_closed, &
                &                         grid%posn(n)%z, grid%posn(n)%lat, domain_name, .false.)
            enddo
            if ( (domain_name .eq. 'GB') .and. (grid%posn(n)%mgmt_area_index .eq. 11) ) &    ! Peter Pan 
            &    weight_grams(n,j) = Shell_to_Weight(shell_lengths(j), grid%posn(n)%is_closed, &
            &                               grid%posn(n)%z, grid%posn(n)%lat, domain_name, .true.)
        enddo
        call Write_CSV(num_grids, num_size_classes, weight_grams, growth_out_dir//'WeightShellHeight.csv', num_grids)

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
    function Set_Current_State(file_name, length, growth)
        use globals
        
        implicit none
        character(*), intent(in) :: file_name
        type(Growth_Class), intent(in):: growth(*)
        real(dp) :: Set_Current_State(1:num_grids, 1:num_size_classes)
        real(dp), intent(in):: length(*)
        integer n, j, num_rows, num_cols
        real(dp) :: state_local(1:num_grids, 1:num_size_classes) 

        num_rows = num_grids
        num_cols = num_size_classes
        
        call Read_CSV(num_rows, num_cols, file_name, state_local, num_grids)
        PRINT *, term_blu
        PRINT '(A,A)', ' READ FROM FILE: ', file_name
        PRINT '(A,I6)', ' LENGTH = ', num_rows
        PRINT '(A,I5)', ' NUMBER OF SCALLOP SIZE CLASSES: ', num_size_classes
        PRINT *, term_blk

        if (num_rows > num_grids) num_rows = num_grids
        do n=1,num_rows
            do j=num_size_classes,2,-1
                if( (length(j) .gt. growth(n)%L_inf_mu) .and. (state_local(n,j) .gt. 0.D0) )then
                    ! lump scallops into smaller class
                    state_local(n,j-1) = state_local(n,j-1) + state_local(n,j)
                    state_local(n,j) = 0.D0
                endif
            enddo
        enddo

        Set_Current_State = state_local

        return
    endfunction Set_Current_State

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
        call Write_CSV(num_size_classes,num_size_classes, Gen_Size_Trans_Matrix, file_name,num_size_classes)

        return
    end function Gen_Size_Trans_Matrix

    !==================================================================================================================
    !> @fn Set_Shell_Heights
    !> @public @memberof Growth_Class
    !>
    !> setup shell shell_lengths intervals
    !> - length_min
    !> - length_min + length_delta
    !> - @f$height_{shell}(n) = length_{min} + (n-1) * length_{delta}@f$
    !>
    !> @param[out] shell_height length in millimeters of shell
    !> @param[in] length_min Size of smallest size class
    !> @param[in] length_delta amount between size classes
    !==================================================================================================================
    function Set_Shell_Heights(length_min, length_delta)
        real(dp) :: Set_Shell_Heights(num_size_classes)
        real(dp), intent(in) :: length_min, length_delta
        integer n  ! loop counter

        Set_Shell_Heights(1) = length_min
        do n=2, num_size_classes
            Set_Shell_Heights(n) = Set_Shell_Heights(n-1) + length_delta
        enddo
        PRINT '(A, F6.2, A, F6.2)', ' Shell size MIN, MAX', Set_Shell_Heights(1), ', ', Set_Shell_Heights(num_size_classes)
        if (num_size_classes .GT. max_size_class) then
            PRINT *, term_red, 'WARNING number of size classes too large', num_size_classes, '>', max_size_class, term_blk
        else
            PRINT *, term_blu, 'OKAY number of size classes', num_size_classes, '<=', max_size_class, term_blk
        endif
        return
    endfunction Set_Shell_Heights

    !==================================================================================================================
    !>
    !> @public @memberof Growth_Class
    !>
    !> Provides growth parameters L and K parameters for Georges Bank.
    !> From R code sent by Dvora July 28th 2021
    !>
    !> @param[in] depth in meters
    !> @param[in] lat Geospatial coordinate, Latitude
    !> @param[in] is_open Logical that indicates if grid is open for fishing
    !> @param[out] L_inf_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] L_inf_sd  standard deviation von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_sd standard deviation von Bertlanaffy asymptotic growth parameter
    !> @param[in] area_index index to indicate management area
    !==================================================================================================================
    subroutine Get_Growth_GB(depth, lat, is_open, L_inf_mu, K_mu, L_inf_sd, K_sd, area_index)
        real(dp), intent(in) :: depth, lat
        logical, intent(in) :: is_open
        real(dp), intent(out) :: L_inf_mu, K_mu
        real(dp), intent(out) :: L_inf_sd, K_sd
        integer, intent(in) :: area_index

        real(dp) fixed_coef(6), random_eff(3)
        real(dp) depth_norm, latitude, intercept, slope, random_intercept, random_slope,randomCov
        real(dp) mean_start_shell_height, mean_depth, mean_lat, mean_ring2

        parameter(mean_start_shell_height = 94.73,  mean_depth = 72.89,  mean_lat = 41.04,  mean_ring2 = 109.576)
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
            &    * Logic_To_Double(is_open) + mean_ring2
            slope = ( fixed_coef(2) + fixed_coef(6)* depth_norm ) / mean_start_shell_height
            random_intercept = random_eff(1)
            random_slope = random_eff(2) / mean_start_shell_height**2
            randomCov = random_eff(3) / mean_start_shell_height

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
    !> @param[in] is_open Logical that indicates if grid is open for fishing
    !> @param[out] L_inf_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_mu  von Bertlanaffy asymptotic growth parameter
    !> @param[out] L_inf_sd  standard deviation von Bertlanaffy asymptotic growth parameter
    !> @param[out] K_sd standard deviation von Bertlanaffy asymptotic growth parameter
    !==================================================================================================================
    subroutine Get_Growth_MA(depth, lat, is_open, L_inf_mu, K_mu, L_inf_sd, K_sd)
        real(dp), intent(in)::depth, lat
        logical, intent(in)::is_open
        real(dp), intent(out)::L_inf_mu,K_mu
        real(dp), intent(out) :: L_inf_sd, K_sd

        real(dp) fixed_coef(5),random_eff(3)
        real(dp) depth_norm, latitude, intercept, slope, random_intercept, random_slope, randomCov
        real(dp) mean_start_shell_height, mean_depth, mean_lat, mean_ring2

        parameter(mean_start_shell_height = 85.74,  mean_depth = 52.79,  mean_lat = 38.99,  mean_ring2 = 106.17 )
                fixed_coef = (/0.951,48.333,-9.53,-37.51,-2.31 /)  !fixed effects coef
        ! intercept, slope, coef of depth, latitude, closed/open for intercept term
        random_eff = (/38.35,13.27,-20.77 /)  !random effects, intercept, slope, cov 
        depth_norm = depth / mean_depth
        latitude = lat/mean_lat
        intercept = fixed_coef(1)+fixed_coef(3) * depth_norm + fixed_coef(4) *latitude &
        &         + fixed_coef(5) * Logic_To_Double(is_open) + mean_ring2
        slope = fixed_coef(2) / mean_start_shell_height
        random_intercept = random_eff(1)
        random_slope = random_eff(2) / mean_start_shell_height**2
        randomCov = random_eff(3) / mean_start_shell_height
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
    !> G(y, l_{k}, \eta, c, \sigma, \omega_k)    = H_{MN18}(X(y,k-1), \sigma, [1-c]\omega_k)\\
    !>                                             - H_{MN18}(X(y,k),   \sigma, [1-c]\omega_k)
    !> @f]
    !> @f[
    !> X(y,k) = y - \eta - (1-c)l_{k}
    !> @f]
    !> @f[
    !> c = 1.0 - e^{-K_{\mu} * \delta_t}
    !> @f]
    !> @f[
    !> \eta = c * L_{{\infty}_\mu}
    !> @f]
    !> @f[
    !> \omega_k = l_k - l_{k-1}
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
        
        H = (L_inf_mu / 2._dp) * (1._dp - erf(y)) - (L_inf_sd / sqrt(2._dp * pi)) * exp(-1._dp * y * y)
        B = (1._dp / 2._dp) * (erf(-y) + 1._dp)
        n_data = 1
        a = 3. / 2._dp
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

        H_MN18 = (1._dp / w) * (x * Norm_Cumul_Dist_Fcn(x, 0._dp, sigma**2) + (sigma**2) * Norm_Density_Fcn(x, 0._dp, sigma**2))
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
        real(dp) sum
        
        ! Transitioning from I_k to I_j
        ! if j < k, force G(j,k) = 0. Scallop does not shrink
        do k = 2, num_size_classes
            do j = 1,k-1
                G(j,k) = 0.
            enddo
        enddo
        
        ! force column sum to equal 1.0
        ! That is sum of a probability distribution function 
        do k = 1, num_size_classes
            sum = 0.
            do j = k, num_size_classes
                if(G(j,k) .lt. 0.) G(j,k) = 0.
                sum = sum + G(j,k)
            enddo
            do j = k, num_size_classes
                ! because of above loop we already know that G(j,k) >= 0
                ! therefore the sum will be >= 0
                if(sum .gt. 0.)then
                    G(j,k) = G(j,k)/sum ! if a+b+c+..z = sum then a/sum+b/sum+c/sum+ ..z/sum = 1.0
                else
                    ! sum == 0
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
    !> \vec{M} = \vec{M}_{nat} + Fishing * \vec{M}_{select} + \vec{M}_{incidental} + \vec{M}_{discard}
    !> @f]
    !>
    !>  - Compute new state
    !> @f[
    !> \vec{S} = \vec{S} * (1- \delta_t * \vec{M})
    !> @f]
    !> 
    !> @param[in] growth object to hold growth simulation paramters
    !> @param[in,out] mortality object to hold mortality simulation parameters
    !> @param[in,out] recruit object to hold recruitment simulation parameters
    !> @param[in,out] state vector of the current state in scallops per square meter
    !> @param[in] fishing_effort vector of fishing effort by location
    !> @param[out] state_time_steps State at each time step
    !> @param[in] year under considration
    !==================================================================================================================
    function Time_To_Grow(growth, mortality, recruit, state, fishing_effort, year)

        use Mortality_Mod, only : Mortality_Class, Mortality_Density_Dependent
        use Recruit_Mod, only : Recruitment_Class
        
        implicit none
        type(Growth_Class), INTENT(IN):: growth
        type(Mortality_Class), INTENT(INOUT):: mortality
        type(Recruitment_Class), INTENT(INOUT):: recruit
        integer, intent(in) :: year
        real(dp),intent(inout)    :: state(*)
        real(dp) :: Time_To_Grow(num_time_steps, num_size_classes)
        real(dp), intent(in) :: fishing_effort
        real(dp), allocatable :: Stmp(:)
        real(dp) t
        integer nt
        integer Rindx
        real(dp), allocatable :: M(:), Rec(:)
            
        allocate(Stmp(1:num_size_classes), M(1:num_size_classes), &
        &       Rec(1:num_size_classes))
        
        Rindx = minloc( abs(recruit%year(1:recruit%n_year)-year ), 1)
        Rec(1:num_size_classes) = 0.D0
        Rec(1:recruit%max_rec_ind) = recruit%recruitment(RIndx)/float(recruit%max_rec_ind)
        
        do nt = 1, num_time_steps
            t = float(nt)*delta_time
            ! Computes mortality based on current state
            call Mortality_Density_Dependent(recruit%max_rec_ind, mortality, state)
            ! Compute increase due to recruitment
            if ( ( t .gt. recruit%rec_start ) .and. ( t.le.recruit%rec_stop) ) &
              state(1:num_size_classes) = state(1:num_size_classes) + delta_time*Rec(1:num_size_classes) &
              &                           / (recruit%rec_stop-recruit%rec_start)
            ! adjust based on von Bertalanffy growth
            Stmp(1:num_size_classes) = matmul(growth%G(1:num_size_classes, 1:num_size_classes), state(1:num_size_classes))
            ! Compute overall mortality
            M(1:num_size_classes) = mortality%natural_mortality(1:num_size_classes) &
            & + fishing_effort * ( mortality%select(1:num_size_classes) &
            & + mortality%incidental + mortality%discard(1:num_size_classes) )
            ! Compute new state
            state(1:num_size_classes) = Stmp(1:num_size_classes) *(1.D0- delta_time * M(1:num_size_classes))
            Time_To_Grow(nt, 1:num_size_classes) = state(1:num_size_classes)
        enddo
        deallocate(Stmp, M, Rec)
        
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
    !>    exp &=& -9.713394 + 2.51 * log(height_{mm}) \\
    !>        &+& (-0.0033 * depth) + 0.021 * latitude \\
    !>        &+& (-0.031 * isClosed) + 0.00525 * (log(height_{mm}) * 21.0) \\
    !>        &+& (-0.000065 * (21.0 * depth))
    !> @f}
    !> @f[
    !> weight = e^{exp}
    !> @f]
    !>
    !> @param[in] shell_height_mm The shell height, or length, in millimeters
    !> @param[in] is_closed Logic to indicate if grid is open (F) or closed (T) to fishing
    !> @param[in] depth The depth of the grid in meters
    !> @param[in] latitude Geographic coordinate
    !> @param[in] domain
    !>            - MA for Mid-Atlantic
    !>            - GB for Georges Bank
    !> @param[in] ispp Logic to indiate is Peter Pan???
    !> @returns weight in grams
    !==================================================================================================================
    real(dp) function Shell_to_Weight(shell_height_mm, is_closed, depth, latitude, domain, ispp)
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
    
    
END MODULE Growth_Mod
