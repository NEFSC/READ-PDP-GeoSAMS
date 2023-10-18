MODULE Growth_Mod
    use globals
    integer, parameter :: max_size_class = (165-30) / 5 + 1 ! not 25,
    integer, parameter :: growth_param_size = 4
    type Growth_Struct
        real(dp) L_inf_mu, K_mu
        real(dp) L_inf_sd, K_sd
        real(dp) G(max_size_class, max_size_class)
        character(2) region
        logical is_closed
        integer num_size_classes
    end type Growth_Struct


    CONTAINS

    subroutine Gen_Trans_Matrix(growth, lengths, num_size_class, delta_time)
        !-----------------------------------------------------------------------
        ! Keston Smith (IBSS corp) June-July 2021
        !-----------------------------------------------------------------------
        !use SSTMmod
        implicit none
        real(dp), intent(in):: lengths(*), delta_time
        integer, intent(in)::num_size_class
        real(dp), allocatable :: G(:,:)
        integer j
        character(72) file_name
        real(dp) K_mu, K_sd, L_inf_mu, L_inf_sd
        
        type(Growth_Struct):: growth
        allocate(G(1:num_size_class,1:num_size_class) )
        L_inf_mu = growth%L_inf_mu
        L_inf_sd = growth%L_inf_sd
        K_mu = growth%K_mu
        K_sd = growth%K_sd

        call MN18_appndxC_transition_matrix(L_inf_mu, K_mu, L_inf_sd, K_sd,lengths, delta_time, G, num_size_class, num_size_class)
        
        ! If growth increments are assumed normally distributed the left hand tail of the distribution can lead to unrealistic "shrinking" transitions
              
        call enforce_non_negative_growth(G, num_size_class, num_size_class)
        do j = 1,num_size_class
            if(lengths(j).gt.L_inf_mu)then
                G(j,1:num_size_class) = 0._dp
                G(j,j) = 1._dp
            endif
        enddo

        growth%G(1:num_size_class, 1:num_size_class) = G(1:num_size_class, 1:num_size_class)
        
        ! output transition matrix
         file_name = 'Growth.csv'
        call Write_CSV(num_size_class,num_size_class,G,file_name,num_size_class)
        
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
        return
    end subroutine Set_Shell_Height_Intervals

    subroutine Set_Growth(growth, grid, lengths, delta_time, data_len, num_size_classes, domain_name)
        use Data_Point_Mod
        implicit none
        type(Growth_Struct), intent(inout):: growth(*) 
        type(Data_Point):: grid
        real(dp), intent(in):: lengths(*),delta_time
        integer, intent(in)::data_len,num_size_classes
        integer n
        character(2) domain_name
        real(dp) Gpar(data_len, growth_param_size)
        
        if(domain_name(1:2).eq.'GB')then
            do n=1,data_len
                call Get_Growth_GB(grid%z(n), grid%lat(n), grid%is_closed(n), growth(n)%L_inf_mu, growth(n)%K_mu)
                growth(n)%L_inf_sd = 14.5D0
                growth(n)%K_sd = .12D0
                growth(1:data_len)%num_size_classes=num_size_classes
                if( grid%management_area(n) .eq. 11 ) then
                    !Peter Pan region Linfity = 110.3 K = 0.423 Th
                    growth(n)%L_inf_mu=110.3D0
                    growth(n)%K_mu=0.423D0
                endif
            enddo
        endif
        if(domain_name(1:2).eq.'MA')then
            do n=1,data_len
                call Get_Growth_MA(grid%z(n), grid%lat(n), grid%is_closed(n), growth(n)%L_inf_mu, growth(n)%K_mu)
                growth(n)%L_inf_sd = 10.8D0
                growth(n)%K_sd = .045D0
            enddo
        endif
        do n=1, data_len
            call Gen_Trans_Matrix(growth(n), lengths, num_size_classes, delta_time)
        enddo
        !growth(1:data_len)%IsClosed=grid%IsClosed(1:data_len) 
        growth(1:data_len)%num_size_classes=num_size_classes
        Gpar(1:data_len,1)=growth(1:data_len)%L_inf_mu
        Gpar(1:data_len,2)=growth(1:data_len)%L_inf_sd
        Gpar(1:data_len,3)=growth(1:data_len)%K_mu
        Gpar(1:data_len,4)=growth(1:data_len)%K_sd
        call Write_CSV(data_len, growth_param_size, Gpar, 'GrowthParOut.csv', data_len)
        
        return 
    end subroutine Set_Growth
        

    subroutine Get_Growth_GB(depth, lat, is_open, Linf, K)
        ! Provides growth parameters Linf and K for Georges Bank.
        ! From R code sent by Dvora July 28th 2021
        implicit none
        real(dp), intent(in) :: depth, lat
        logical, intent(in) :: is_open
        real(dp), intent(out) :: Linf, K
        real(dp) fixed_coef(6), random_eff(3)
        real(dp) depth_avg, latitude, intercept, slope, random_intercept, random_slope,randomCov
        real(dp) mean_start_shell_height, mean_depth, mean_lat, mean_ring2
        real(dp) open
        parameter(mean_start_shell_height = 94.73,  mean_depth = 72.89,  mean_lat = 41.04,  mean_ring2 = 109.576)
        fixed_coef = (/ -34.96, 57.415, -8.51, -17.65, 0.9076, 3.741/)  !fixed effects coef
        ! intercept, slope, coef of depth, latitude, closed/open for intercept term
        random_eff = (/ 81.05, 51.38, -60.53 /)  !random effects, intercept, slope, cov
        if (is_open) then 
            open = 1._dp 
        else 
            open = 0._dp
        endif
        depth_avg = depth / mean_depth
        latitude = Lat/mean_lat
        intercept = fixed_coef(1) + fixed_coef(3) * depth_avg + fixed_coef(4)*latitude + fixed_coef(5) * open + mean_ring2
        slope = ( fixed_coef(2) + fixed_coef(6)* depth_avg ) / mean_start_shell_height
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
        implicit none
        real(dp), intent(in)::depth, lat
        logical, intent(in)::is_open
        real(dp), intent(out)::Linf,K
        real(dp) fixed_coef(5),random_eff(3)
        real(dp) depth_avg, latitude, intercept, slope, random_intercept, random_slope, randomCov
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
        depth_avg = depth / mean_depth
        latitude = lat/mean_lat
        intercept = fixed_coef(1)+fixed_coef(3) * depth_avg + fixed_coef(4) *latitude + fixed_coef(5) * open + mean_ring2
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
    !          num_size_class [integer 1x1] = dimension of lengths and G 
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
    subroutine MN18_appndxC_transition_matrix(L_inf_mu,K_mu,L_inf_sd,K_sd,lengths,delta_time,G,num_size_class,n)
        implicit none
    
        ! input variables
    
        integer, INTENT(IN) :: num_size_class
        real(dp), INTENT(IN) :: L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, lengths(num_size_class)
    
        ! output variables
    
        real(dp), INTENT(OUT) :: G(num_size_class, num_size_class)
    
        ! local variables
    
        real(dp) w, mu, sigma, mp
        integer j, k, n
        real(dp) Fya, Fyb, c, eta, Ha, Hb
    
        G = 0.
    
        c = 1._dp - exp(-K_mu * delta_time)
        eta = c * L_inf_mu
    
        do k = 2,n + 1
    
            ! calculate interval width and midpoint
    
            w = lengths(k) - lengths(k-1) 
            mp = (lengths(k) + lengths(k-1)) / 2._dp
    
            ! calculate increment mean,mu->evaluated at midpoint and increment standard deviation sigma->evaluated at midpoint   
            call increment_mean_std(L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, mp, mu, sigma)
            do j = k,n + 1
                call H_MN18(lengths(j) - eta - (1._dp - c) * lengths(k-1), sigma, (1._dp - c) * w, Ha)
                call H_MN18(lengths(j) - eta - (1._dp - c) * lengths(k),   sigma, (1._dp - c) * w, Hb)
                Fya = Ha - Hb
    
                call H_MN18(lengths(j-1) - eta - (1._dp - c) * lengths(k-1), sigma, (1._dp - c) * w, Ha)
                call H_MN18(lengths(j-1) - eta - (1._dp - c) * lengths(k),   sigma, (1._dp - c) * w, Hb)
                Fyb = Ha - Hb
                G(j-1, k-1) = Fya - Fyb
            enddo
        enddo
        return
    end subroutine MN18_appndxC_transition_matrix

    subroutine increment_mean_std(L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, u, mu, sigma)
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
        ! u [real 1x1] = size to estimate increment stats
        !
        !
        ! outputs: 
        ! mu [1x1] = mean of increment at u
        ! sigma [1x1]  = standard deviation of increment at u
        
        ! history:  Written by keston Smith (IBSS corp) May 2021
        !-----------------------------------------------------------------------
        implicit none
        
        ! input variables
        
        real(dp), INTENT(IN) :: L_inf_mu, K_mu, L_inf_sd, K_sd, delta_time, u
        
        ! output variables
        
        real(dp), INTENT(OUT) :: mu, sigma
        
        ! local variables
        
        real(dp) sigma2
        real(dp) m, s, p, r
        real(dp) pi, H, B, y, gammainc, TL, TY, Q, a
        integer n_data
        
        pi = 4._dp * ATAN(1._dp)
        
        ! change notation and remove dimension from K for convinience
              
        m = L_inf_mu
        s = L_inf_sd
        p = K_mu * delta_time
        r = K_sd * delta_time
        
        y = (u - m) / (s * sqrt(2._dp))
        
        H = (m / 2._dp) * (1._dp - erf(y)) - (s / sqrt(2._dp * pi)) * exp(-1._dp * y * y)
        B = (1._dp / 2._dp) * (erf(-y) + 1._dp)
        n_data = 1
        a = 3. / 2._dp
        call gamma_inc_values ( n_data, a, y**2, gammainc )
        mu = (H / B - u) * (1._dp - exp(.5_dp * r**2 - p))
        mu = max(mu, 0._dp)
        Q = sqrt(pi) * (1._dp-erf(y))
        TL = (m -u)**2 + ( 2._dp * sqrt(2._dp) * s * (m - u) * exp(-y**2) ) / Q + 2._dp * (s**2) * gammainc / Q
        TY = exp(2._dp * r**2 - 2._dp * p) - 2._dp * exp(.5_dp * r**2 - p)  + 1._dp 
        sigma2 = TL * TY - mu**2
        
        ! mean and variance for unconditional growth
        ! mu = (m-u) * (1._dp-exp(.5_dp * r**2-p))
        ! sigma2 = (s**2 + (m-u)**2) * (exp(2 * r**2-2 * p)-2 * exp(.5_dp * r**2-p) + 1._dp)-((exp(.5_dp * r**2-p)-1)**2) * ((m-u)**2)
        
        mu = max(mu,0._dp)
        sigma = sqrt(sigma2)
        !write(*,*) u, mu, sigma ,m,s,p,r
        
        return
    end subroutine increment_mean_std
    
    subroutine H_MN18(x,sigma,w,H)
        ! from MN18 apendix B
        ! input variables
        ! x - evaluation point
        ! sigma,w - paramaters defined within MN18
        
        ! output variables
        ! H - variable 
        implicit none
        real(dp), INTENT(IN) ::sigma,w,x
        real(dp), INTENT(OUT) ::H
        real(dp) f,zero
        zero = 0.
        call Ncdf(x,zero,sigma,f)
        H = (1._dp / w) * (x * f + (sigma**2) * f)
        return
    end subroutine H_MN18
    
    subroutine enforce_non_negative_growth(G,num_size_class,n)
        ! input variables
        !     G = growth transition matrix that may have effects of negative growth
        !     num_size_class = leading dimension of G
        !     n  = actual number of size classes
        
        ! outut variables
        !     G - growth transition matrix with negative growth lumped into 0 growth
        
        implicit none
        
        ! input variables
        
        integer, INTENT(IN) :: n,num_size_class
        
        ! input/output variables
        real(dp) G(num_size_class,*)
        ! local
        integer j,k
        real(dp) S      
        
        do k = 2,n
            do j = 1,k-1
                G(j,k) = 0.
            enddo
        enddo
        
        do k = 1,n
            S = 0.
            do j = k,n
                if(G(j,k).lt.0.) G(j,k) = 0.
                S = S + G(j,k)
            enddo
            if (S.lt.1._dp) then
                G(k,k) = G(k,k) + (1._dp-S)
            endif
            do j = k,n
                if(S.gt.0.)then
                    G(j,k) = G(j,k)/S
                else
                    G(j,k) = 0.
                   if(j.eq.k)G(j,k) = 1._dp
                endif
            enddo
        enddo
        
        return
    end subroutine enforce_non_negative_growth
           
           
    subroutine Ncdf(x,mu,sigma,f)
        ! computation of normal cdf
        ! input variables
        ! mu - mean 
        ! sigma - standard deviation
        ! x - evaluation point
        
        ! output variables
        ! f - normal cdf value at x, f(x|mu,sigma)
        implicit none
        real(dp) mu,sigma,x,f
        f = 0.5_dp * ( 1._dp + erf( (x - mu) / (sigma * sqrt(2._dp) ) ))
        return
    end subroutine Ncdf
        
END MODULE Growth_Mod
