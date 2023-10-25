module Mortality_Mod              
    use globals
    implicit none
    type Mortality_Struct
       real(dp) natural_mortality(max_size_class)
       real(dp) incidental
       real(dp) discard(max_size_class)
       real(dp) fishing_effort(max_num_years)
       real(dp) a(max_num_years), b(max_num_years), c(max_num_years), d(max_num_years)
       !integer Stype(max_num_years) !NOT USED
       integer year(max_num_years)
       real(dp) select(max_size_class)
       character(2) region
       logical is_closed
       integer num_size_classes, num_years
       real(dp) natural_mort_adult, natural_mort_juv
       real(dp) alpha(1:max_size_class)
       integer management_area
    end type Mortality_Struct

    CONTAINS

    subroutine SetMortality(mortality, grid, l , num_grids, num_size_classes, domain_name)
        use Data_Point_Mod 
        
        type(Mortality_Struct), intent(inout):: mortality(*)
        type(Data_Point_Struct), intent(in) :: grid
        real(dp), intent(in):: l(*)
        integer num_grids, num_size_classes, yr_index, j, num_years, year, k
        character(2) domain_name
        real(dp) fishing_by_region(max_num_years, 4), a, h0
        !
        ! Load parameters for fishing selectivity
        !(1+exp(.1*(l-70))).^-1)
        call Read_CSV(num_years, 4, 'Data/FYrGBcGBoMA.csv', fishing_by_region, max_num_years)
        !Assign Fishing presure, selectivity parameters, and Natural Mortality from CASA model
        if (domain_name.eq.'MA') mortality(1:num_grids)%natural_mort_adult = .25D0
        !if (domain_name.eq.'MA') mortality(1:num_grids)%natural_mort_adult = .3D0
        if (domain_name.eq.'GB') mortality(1:num_grids)%natural_mort_adult = .2D0
        a = 0.1
        if (domain_name.eq.'MA') h0 = 65.
        if (domain_name.eq.'GB') h0 = 70.
        yr_index = 0
        do year = 1975, 2019 !TODO
            yr_index = yr_index + 1
            do j = 1, num_grids
                mortality(j)%num_size_classes = num_size_classes
                if (domain_name(1:2).eq.'MA') then
                    mortality(j)%fishing_effort(yr_index) = fishing_by_region(yr_index, 4)
                elseif (domain_name(1:2).eq.'GB') then
                    if (grid%is_closed(j)) then
                        mortality(j)%fishing_effort(yr_index) = fishing_by_region(yr_index, 2)        
                    else
                        mortality(j)%fishing_effort(yr_index) = fishing_by_region(yr_index, 3)
                    endif        
                endif
                mortality(j)%select(1:num_size_classes) = Ring_Size_Selectivity(year, l(1:num_size_classes), num_size_classes, &
                &                          mortality(j)%is_closed, domain_name)
                mortality(j)%incidental = 0.1D0
                if(domain_name(1:2).eq.'MA') mortality(j)%incidental = 0.05D0
                mortality(j)%discard(1:num_size_classes) = 0.2*mortality(j)%select(1:num_size_classes)
                do k = 1, num_size_classes
                    if(l(k).gt.90.)mortality(j)%discard(k) = 0.D0
                enddo
                mortality(j)%is_closed = grid%is_closed(j)
                mortality(j)%natural_mortality(1:num_size_classes) = mortality(j)%natural_mort_adult
                mortality(j)%alpha(1:num_size_classes) =  1. - 1. / ( 1. + exp( - a*( l(1:num_size_classes) - h0 ) ) )  
            enddo
            mortality(1:num_grids)%year(yr_index) = year
        enddo
        mortality(1:num_grids)%num_years = yr_index
        
    return
    endsubroutine SetMortality

    !subroutine Ring_Size_Selectivity(year, shell_height, select_vector_len, num_size_classes, is_closed, domain_name)
    !Purpose: Assign size class fishing selectivity based on increasing logistic function
    !
    ! Inputs:
    !   shell_height (real(dp))    length n vector of shell lengths
    !   a, b         (real(dp))    parameters of logistic selectivity curve
    ! 	year, num_size_classes      (integer)	Year to determine if this is before 2005 (3.5" rings) or after (4" rings)Number of size classes
    !
    ! Outputs:
    ! 	select_vector_len       (real(dp))    length num_size_classes vector of selectivity
    !-----------------------------------------------------------------------
    ! Keston Smith (IBSS corp) May 2022
    !-----------------------------------------------------------------------
    
    ! May/6/2022
    !Deborah Hart - NOAA Federal
    !4:22 PM (2 hours ago)
    !to me
    !MA: 20.5079 0.19845
    !GBOp: 21.7345 0.2193
    !GB Cl: 17.72 0.15795
    ! Select = 1/(1+exp(selecta(region, subarea)-selectb(region, subarea)*Size))
    !3.5 inch rings
    !1/(1+exp(15.16-0.2021*Size))
    function Ring_Size_Selectivity(year, shell_height, num_size_classes, is_closed, domain_name)
        implicit none
        integer, intent(in):: year, num_size_classes
        logical is_closed
        character(2), intent(in):: domain_name
        real(dp), intent(in)::shell_height(*)
        real(dp) Ring_Size_Selectivity(num_size_classes)
        real(dp) a, b
        
        if(year.lt.2005)then! 3.5" Rings
            a = 15.16
            b = .2021
        else !4" Ringsa=
            if(domain_name(1:2).eq.'MA')then
                    a = 20.5079
                    b = 0.19845
            else
                if(is_closed) then
                    a = 17.72
                    b = 0.15795
                else
                    a = 21.7345
                    b =  0.2193
                endif
        
            endif
        endif
        Ring_Size_Selectivity(1:num_size_classes) = 1.D0 / ( 1.D0 + exp( a - b * shell_height(1:num_size_classes) ) )

    endfunction Ring_Size_Selectivity
        
end module Mortality_Mod
   