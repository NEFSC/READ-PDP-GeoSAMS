!-------------------------------------------------------------------------
! MN18 refers to
! Miller, R. B. and Nottingham, 2018,
! "Improved approximations for estimation of size-transition probabilities within size-structured models"
!-------------------------------------------------------------------------
MODULE Growth_Mod
    use globals
    implicit none
    integer, parameter :: growth_param_size = 4
    character(*), parameter :: output_dir = 'Output/'
    type Growth_Struct
        real(dp) L_inf_mu, K_mu  ! mean 
        real(dp) L_inf_sd, K_sd  ! standard deviation
        real(dp) G(max_size_class, max_size_class)
        character(2) region
        logical is_closed
        integer num_size_classes
    end type Growth_Struct


    CONTAINS


    subroutine Gen_Trans_Matrix(growth, lengths, num_size_classes, delta_time)
        !-----------------------------------------------------------------------
        ! Keston Smith (IBSS corp) June-July 2021
        !-----------------------------------------------------------------------
        !use SSTMmod
        implicit none
        real(dp), intent(in):: lengths(*), delta_time
        integer, intent(in)::num_size_classes
        real(dp), allocatable :: G(:,:)
        integer j
        character(72) file_name
        real(dp) K_mu, K_sd, L_inf_mu, L_inf_sd
        type(Growth_Struct):: growth

        allocate(G(1:num_size_classes,1:num_size_classes) )
        L_inf_mu = growth%L_inf_mu
        L_inf_sd = growth%L_inf_sd
        K_mu = growth%K_mu
        K_sd = growth%K_sd

        call MN18_appndxC_transition_matrix(L_inf_mu, K_mu, L_inf_sd, K_sd,lengths, &
        &      delta_time, G, num_size_classes, num_size_classes)
        
        ! If growth increments are assumed normally distributed the left hand tail of the distribution 
        ! can lead to unrealistic "shrinking" transitions
        call enforce_non_negative_growth(G, num_size_classes, num_size_classes)
        do j = 1,num_size_classes
            if(lengths(j).gt.L_inf_mu)then
                G(j,1:num_size_classes) = 0._dp
                G(j,j) = 1._dp
            endif
        enddo

        growth%G(1:num_size_classes, 1:num_size_classes) = G(1:num_size_classes, 1:num_size_classes)
        
        ! output transition matrix
         file_name = output_dir//'Growth.csv'
        call Write_CSV(num_size_classes,num_size_classes,G,file_name,num_size_classes)
        
        deallocate( G )
        return
    end subroutine Gen_Trans_Matrix

    !---------------------------------------------------------------------------------------------------
    ! setup shell lengths intervals
    !-------------------------------
    subroutine Set_Shell_Height_Intervals(shell_height, num_size_classes, lengthMin, lengthDelta)
        real(dp),intent(out) :: shell_height(*)
        integer,intent(in) :: num_size_classes
        real(dp), intent(in) :: lengthMin, lengthDelta
        integer n  ! loop counter

        shell_height(1) = lengthMin
        do n=2, num_size_classes
            shell_height(n) = shell_height(n-1) + lengthDelta
        enddo
        PRINT '(A, F6.2, A, F6.2)', ' Shell size MIN, MAX', shell_height(1), ', ', shell_height(num_size_classes)
        if (num_size_classes .GT. max_size_class) then
            PRINT *, term_red, 'WARNING number of size classes too large', num_size_classes, '>', max_size_class, term_blk
        else
            PRINT *, term_blu, 'OKAY number of size classes', num_size_classes, '<=', max_size_class, term_blk
        endif
        return
    end subroutine Set_Shell_Height_Intervals

    subroutine Set_Growth(growth, grid, lengths, delta_time, num_grids, num_size_classes, domain_name)
        use Data_Point_Mod
        type(Growth_Struct), intent(inout):: growth(*) 
        type(Data_Point_Struct):: grid
        real(dp), intent(in):: lengths(*),delta_time
        integer, intent(in)::num_grids,num_size_classes
        integer n
        character(2) domain_name
        real(dp) Gpar(num_grids, growth_param_size)
        
        growth(1:num_grids)%num_size_classes=num_size_classes
        if(domain_name(1:2).eq.'GB')then
            do n=1,num_grids
                call Get_Growth_GB(grid%z(n), grid%lat(n), grid%is_closed(n), growth(n)%L_inf_mu, growth(n)%K_mu)
                growth(n)%L_inf_sd = 14.5D0
                growth(n)%K_sd = .12D0
                if( grid%management_area(n) .eq. 11 ) then
                    !Peter Pan region Linfity = 110.3 K = 0.423 Th
                    growth(n)%L_inf_mu=110.3D0
                    growth(n)%K_mu=0.423D0
                endif
            enddo
        endif
        if(domain_name(1:2).eq.'MA')then
            do n=1,num_grids
                call Get_Growth_MA(grid%z(n), grid%lat(n), grid%is_closed(n), growth(n)%L_inf_mu, growth(n)%K_mu)
                growth(n)%L_inf_sd = 10.8D0
                growth(n)%K_sd = .045D0
            enddo
        endif
        do n=1, num_grids
            call Gen_Trans_Matrix(growth(n), lengths, num_size_classes, delta_time)
        enddo
        !growth(1:num_grids)%IsClosed = grid%IsClosed(1:num_grids) 
        growth(1:num_grids)%num_size_classes = num_size_classes
        Gpar(1:num_grids,1) = growth(1:num_grids)%L_inf_mu
        Gpar(1:num_grids,2) = growth(1:num_grids)%L_inf_sd
        Gpar(1:num_grids,3) = growth(1:num_grids)%K_mu
        Gpar(1:num_grids,4) = growth(1:num_grids)%K_sd
        call Write_CSV(num_grids, growth_param_size, Gpar, output_dir//'GrowthParOut.csv', num_grids)
        
        return 
    end subroutine Set_Growth
        

    subroutine Get_Growth_GB(depth, lat, is_open, Linf, K)
        ! Provides growth parameters Linf and K for Georges Bank.
        ! From R code sent by Dvora July 28th 2021
        real(dp), intent(in) :: depth, lat
        logical, intent(in) :: is_open
        real(dp), intent(out) :: Linf, K
        real(dp) fixed_coef(6), random_eff(3)
        real(dp) depth_norm, latitude, intercept, slope, random_intercept, random_slope,randomCov
        real(dp) mean_start_shell_height, mean_depth, mean_lat, mean_ring2

        parameter(mean_start_shell_height = 94.73,  mean_depth = 72.89,  mean_lat = 41.04,  mean_ring2 = 109.576)
        fixed_coef = (/ -34.96, 57.415, -8.51, -17.65, 0.9076, 3.741/)  !fixed effects coef
        ! intercept, slope, coef of depth, latitude, closed/open for intercept term
        random_eff = (/ 81.05, 51.38, -60.53 /)  !random effects, intercept, slope, cov
        depth_norm = depth / mean_depth
        latitude = Lat/mean_lat
        intercept = fixed_coef(1) + fixed_coef(3) * depth_norm + fixed_coef(4)*latitude + fixed_coef(5) &
        &    * Logic_To_Double(is_open) + mean_ring2
        slope = ( fixed_coef(2) + fixed_coef(6)* depth_norm ) / mean_start_shell_height
        random_intercept = random_eff(1)
        random_slope = random_eff(2) / mean_start_shell_height**2
        randomCov = random_eff(3) / mean_start_shell_height
        K = -log(slope) + random_slope/2. / slope**2
        Linf = (intercept/(1.-slope)) + (intercept * random_slope / (1. - slope) + randomCov) / (1. - slope**2)
        return
    end subroutine Get_Growth_GB
        
    subroutine Get_Growth_MA(depth, lat, is_open, Linf, K)
        ! Provides growth parameters Linf and K for Mid Atlantic.
        ! From R code sent by Dvora July 28th 2021
        real(dp), intent(in)::depth, lat
        logical, intent(in)::is_open
        real(dp), intent(out)::Linf,K
        real(dp) fixed_coef(5),random_eff(3)
        real(dp) depth_norm, latitude, intercept, slope, random_intercept, random_slope, randomCov
        real(dp) mean_start_shell_height, mean_depth, mean_lat, mean_ring2
        real(dp) open
        parameter(mean_start_shell_height = 85.74,  mean_depth = 52.79,  mean_lat = 38.99,  mean_ring2 = 106.17 )
        fixed_coef = (/0.951,48.333,-9.53,-37.51,-2.31 /)  !fixed effects coef
        ! intercept, slope, coef of depth, latitude, closed/open for intercept term
        random_eff = (/38.35,13.27,-20.77 /)  !random effects, intercept, slope, cov 
        if (is_open) then 
            open = 1._dp 
        else 
            open = 0._dp
        endif
        depth_norm = depth / mean_depth
        latitude = lat/mean_lat
        intercept = fixed_coef(1)+fixed_coef(3) * depth_norm + fixed_coef(4) *latitude + fixed_coef(5) * open + mean_ring2
        slope = fixed_coef(2) / mean_start_shell_height
        random_intercept = random_eff(1)
        random_slope = random_eff(2) / mean_start_shell_height**2
        randomCov = random_eff(3) / mean_start_shell_height
        K = -log(slope) + random_slope/2./slope**2
        Linf = (intercept / (1.-slope)) + (intercept*random_slope / (1.-slope) + randomCov) / (1.-slope**2)
        return
    end subroutine Get_Growth_MA
        
        
    !-----------------------------------------------------------------------
    ! Purpose: This subroutine computes a sizeclass transition matrix under the  assumption of von  Bertlanaffy growth.  It is assumed that the parameters of von BernBertlanaffy growth K and L_inf have    normal distributions.
    !
    ! inputs:
    !          L_inf_mu [real 1x1] = mean of von Bertlanaffy asymptotic growth parameter L_inf(see HC09     eqn 1)
    !          K_mu [real 1x1] =  mean of von Bertlanaffy growth parameter K(see HC09 eqn 1)
    !          L_inf_std [real 1x1] =  standard deviation of von Bertlanaffy asymptotic growth parameter  L_inf(see HC09 eqn 1)
    !          K_std [real 1x1] =  standard deviation of von Bertlanaffy growth parameter K(see HC09 eqn 1)
    !          lengths [real nx1] = lengths for each size class 
    !          delta_time [real 1x1] = time step for transition matrix in decmilal years
    !          num_size_classes [integer 1x1] = dimension of lengths and G 
    !
    !
    ! outputs: 
    ! G [real n x n] = size transition matrix estimated under the assumption of uniform size 
    !                  distribution within size interval and growth distribution evaluated at 
    !                  mid point of size interval. Derivation is from MN18 appendix C.  
    !                  Derivation of formula for growth increment mean and variance is in MN18eq7.pdf
    !
    ! history:  Written by keston Smith (IBSS corp) May 2021
    !-----------------------------------------------------------------------
    subroutine MN18_appndxC_transition_matrix(L_inf_mu,K_mu,L_inf_sd,K_sd,lengths,delta_time,G,num_size_classes,n)
        ! input variables
    
        integer, INTENT(IN) :: num_size_classes
        real(dp), INTENT(IN) :: L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, lengths(num_size_classes)
    
        ! output variables
    
        real(dp), INTENT(OUT) :: G(num_size_classes, num_size_classes)
    
        ! local variables
    
        real(dp) omega, mu, sigma, omega_avg
        integer j, k, n
        real(dp) Fya, Fyb, c, eta, Ha, Hb
    
        G = 0.
        ! MN18 p. 1312
        ! c = 1 – exp(–K * delta_t)
        ! and eta = c * L_infinity
        ! where L_infinity and K are the asymptotic size and growth rate parameters, respectively,
        ! of the corresponding von Bertalanffy curve. 
        c = 1._dp - exp(-K_mu * delta_time)
        eta = c * L_inf_mu
    
        do k = 2,n + 1
    
            ! calculate interval omega and midpoint

            ! MN18 p. 1312
            omega = lengths(k) - lengths(k-1) 
            ! average size
            omega_avg = (lengths(k) + lengths(k-1)) / 2._dp
    
            ! calculate increment mean,mu->evaluated at midpoint and increment standard deviation sigma->evaluated at midpoint   
            call increment_mean_std(L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, omega_avg, mu, sigma)
            do j = k,n + 1
                Ha = H_MN18(lengths(j) - eta - (1._dp - c) * lengths(k-1), sigma, (1._dp - c) * omega)
                Hb = H_MN18(lengths(j) - eta - (1._dp - c) * lengths(k),   sigma, (1._dp - c) * omega)
                Fya = Ha - Hb
    
                Ha = H_MN18(lengths(j-1) - eta - (1._dp - c) * lengths(k-1), sigma, (1._dp - c) * omega)
                Hb = H_MN18(lengths(j-1) - eta - (1._dp - c) * lengths(k),   sigma, (1._dp - c) * omega)
                Fyb = Ha - Hb
                G(j-1, k-1) = Fya - Fyb
            enddo
        enddo
        return
    end subroutine MN18_appndxC_transition_matrix

    !-----------------------------------------------------------------------
    ! Purpose: This subroutine computes a growth increment distribution parameters under 
    ! the  assumption of von Bertlanaffy growth and normally distributed growth increments. 
    ! It is assumed that the parameters of von BernBertlanaffy growth K and L_inf have
    ! normaldistributions.
    !
    ! inputs:
    ! L_inf_mu [real 1x1] = mean of von Bertlanaffy asymptotic growth parameterL_inf(see HC09 eqn 1)
    ! K_mu [real 1x1] =  mean of von Bertlanaffy growth parameter K(see HC09 eqn 1)
    ! L_inf_std [real 1x1] =  standard deviation of von Bertlanaffy asymptoticgrowth parameter L_inf(see  HC09 eqn 1)
    ! K_std [real 1x1] =  standard deviation of von Bertlanaffy growth parameter (see HC09 eqn 1)
    ! delta_time [real 1x1] = time step for transition matrix in decmilal years
    ! size [real 1x1] = size to estimate increment stats
    !
    !
    ! outputs: 
    ! mu [1x1] = mean of increment at size
    ! sigma [1x1]  = standard deviation of increment at size
    
    ! history:  Written by keston Smith (IBSS corp) May 2021
    !-----------------------------------------------------------------------
    subroutine increment_mean_std(L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, size, mu, sigma)
        
        ! input variables
        
        real(dp), INTENT(IN) :: L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, size
        
        ! output variables
        
        real(dp), INTENT(OUT) :: mu, sigma
        
        ! local variables
        
        real(dp) sigma2
        real(dp) prod_mu, prod_sd
        real(dp) pi, H, B, y, gammainc, TL, TY, Q, a
        integer n_data
        
        pi = 4._dp * ATAN(1._dp)
        
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
        
        mu = max(mu,0._dp)
        sigma = sqrt(sigma2)
        !write(*,*) size, mu, sigma ,L_inf_mu,L_inf_sd,prod_mu,prod_sd
        
        return
    end subroutine increment_mean_std
    
    ! from MN18 apendix B
    ! input variables
    ! x - evaluation point
    ! sigma,w - paramaters defined within MN18
    
    ! output variables
    ! H - variable 
    real(dp) function H_MN18(x,sigma,w)
        real(dp), INTENT(IN) ::sigma,w,x
        real(dp) f
        f = N_Cumul_Dens_Fcn(x, 0._dp, sigma)
        H_MN18 = (1._dp / w) * (x * f + (sigma**2) * f)
    endfunction H_MN18
    
    ! input variables
    !     G = growth transition matrix that may have effects of negative growth
    !     num_size_classes = leading dimension of G
    !     n  = actual number of size classes
    
    ! outut variables
    !     G - growth transition matrix with negative growth lumped into 0 growth
    subroutine enforce_non_negative_growth(G,num_size_classes,n)
        ! input variables
        
        integer, INTENT(IN) :: n,num_size_classes

        ! input/output variables
        real(dp) G(num_size_classes,*)
        ! local
        integer j,k
        real(dp) sum
        
        ! Transitioning from I_k to I_j
        ! if j < k, force G(j,k) = 0. Scallop does not shrink
        do k = 2,n
            do j = 1,k-1
                G(j,k) = 0.
            enddo
        enddo
        
        ! force column sum to equal 1.0
        ! That is sum of a probability distribution function 
        do k = 1,n
            sum = 0.
            do j = k,n
                if(G(j,k) .lt. 0.) G(j,k) = 0.
                sum = sum + G(j,k)
            enddo
            do j = k,n
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
           
           
    ! computation of normal cumulative density function
    ! input variables
    ! mu - mean 
    ! sigma - standard deviation
    ! x - evaluation point
    
    ! output variables
    ! f - normal cdf value at x, f(x|mu,sigma)
    real(dp) function N_Cumul_Dens_Fcn(x, mu, sigma)
        real(dp), intent(in) :: mu,sigma,x
        N_Cumul_Dens_Fcn = 0.5_dp * ( 1._dp + erf( (x - mu) / (sigma * sqrt(2._dp) ) ))
    endfunction N_Cumul_Dens_Fcn

    subroutine Time_To_Grow(growth, mortality, recruit, state, fishing_effort, delta_time, &
    &                      num_time_steps, state_time_steps, domain_area, domain_name, year)

        use Mortality_Mod
        use Recruit_Mod
        
        implicit none
        type(Growth_Struct), INTENT(IN):: growth
        type(Mortality_Struct), INTENT(INOUT):: mortality
        type(Recruit_Struct), INTENT(INOUT):: recruit
        integer, intent(in)     :: num_time_steps, year
        character(2), intent(in) :: domain_name
        real(dp),intent(inout)    :: state(*)
        real(dp), intent(out)     ::state_time_steps(num_time_steps,*)
        real(dp), intent(in)      :: delta_time, domain_area, fishing_effort
        real(dp), allocatable :: G(:,:), Stmp(:)
        real(dp) t
        integer nt
        integer num_size_classes, Rindx
        real(dp), allocatable :: M(:), Rec(:)
            
        
        num_size_classes = growth%num_size_classes
        
        allocate(G(1:num_size_classes, 1:num_size_classes), Stmp(1:num_size_classes), M(1:num_size_classes), &
        &       Rec(1:num_size_classes))
        
        Rindx = minloc( abs(recruit%year(1:recruit%n_year)-year ), 1)
        Rec(1:num_size_classes) = 0.D0
        Rec(1:recruit%max_rec_ind) = recruit%recruitment(RIndx)/float(recruit%max_rec_ind)
        
        G(1:num_size_classes, 1:num_size_classes) = growth%G(1:num_size_classes, 1:num_size_classes)
        
        do nt = 1, num_time_steps
            t = float(nt)*delta_time
            call Mortality_Density_Dependent(recruit, mortality, state, domain_area, domain_name)
            !Rec(1:num_size_classes) = 0.D0
        !    if ( ( t .gt. recruit%rec_start ) .and. ( t.le.recruit%rec_stop) ) then&
        !      state(1:num_size_classes) = state(1:num_size_classes) + delta_time*Rec(1:num_size_classes)/(recruit%rec_stop-recruit%rec_start)
            if ( ( t .gt. recruit%rec_start ) .and. ( t.le.recruit%rec_stop) ) &
              state(1:num_size_classes) = state(1:num_size_classes) + delta_time*Rec(1:num_size_classes) &
              &                           / (recruit%rec_stop-recruit%rec_start)
            
            Stmp(1:num_size_classes) = matmul(G(1:num_size_classes, 1:num_size_classes), state(1:num_size_classes))
            M(1:num_size_classes) = mortality%natural_mortality(1:num_size_classes) + fishing_effort &
            &    * ( mortality%select(1:num_size_classes)+ mortality%incidental + mortality%discard(1:num_size_classes) )
            state(1:num_size_classes) = Stmp(1:num_size_classes) *(1.D0- delta_time * M(1:num_size_classes))
            state_time_steps(nt, 1:num_size_classes) = state(1:num_size_classes)
        enddo
        deallocate(G, Stmp, M, Rec)
        
        return
    endsubroutine Time_To_Grow
        
        
END MODULE Growth_Mod
