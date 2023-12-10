!>----------------------------------------------------------------------------------------------------------------
!> @page page3 Mortality_Mod
!>
!> @section Msec1 Mortality Class
!>
!> Scallop mortality is caused by both natural and man-made means, i.e. fishing.
!>  - For MA, natural mortality is determined to be 25%
!>  - For GB, it is at 20%
!>
!> @subsection Msubsec1 Ring_Size_Selectivity
!> Assign size class fishing selectivity based on increasing logistic function
!> @f[
!> Selectivity = \frac{1}{ 1 + e^{ a - b * length_{shell}}}
!> @f]
!>
!>----------------------------------------------------------------------------------------------------------------
module Mortality_Mod
    use globals
    use Data_Point_Mod, only : num_dimensions
    implicit none

    !> @class Mortality_Class
    !! 
    !! Subroutines that determine expected mortality of scallops
    type Mortality_Class
        !> @public @memberof Mortality_Class
        !! Attrition due to natural mortality
        real(dp) natural_mortality(max_size_class)
        !> @public @memberof Mortality_Class
        real(dp) incidental
        !> @public @memberof Mortality_Class
        real(dp) discard(max_size_class)
        !> @public @memberof Mortality_Class
        real(dp) fishing_effort(max_num_years)
        !> @public @memberof Mortality_Class
        integer year(max_num_years)
        !> @public @memberof Mortality_Class
        real(dp) select(max_size_class)
        !> @public @memberof Mortality_Class
        logical is_closed
        !> @public @memberof Mortality_Class
        integer num_years
        !> @public @memberof Mortality_Class
        real(dp) natural_mort_adult, natural_mort_juv
        !> @public @memberof Mortality_Class
        real(dp) alpha(1:max_size_class)
        !> @public @memberof Mortality_Class
        integer mgmt_area_index
    end type Mortality_Class

    ! @private @memberof Mortality_Class
    integer, PRIVATE :: num_grids
    integer, PRIVATE :: num_size_classes
    character(2), PRIVATE :: domain_name
    real(dp), PRIVATE :: domain_area_sqm
    real(dp), PRIVATE :: grid_area_sqm

    CONTAINS

    !==================================================================================================================
    !> @public @memberof Mortality_Class
    !>
    !> Initializes mortality for startup
    !>
    !> @param[in,out] mortality Parameters that identify how the scallop should reaches mortality
    !> @param[in] grid Vector that identifies the geospatial locations under simulation
    !> @param[in] shell_lengths Vector of the size, or length, of scallops
    !> @param[in] num_sz_classes Number of size classes to set private member
    !> @param[in] domain_name Name of domain being simulate, 'MA' or 'GB'
    !> @param[in] domain_area,Size of domain under consideration in square meters
    !> @param[in] element_area, Size of grid in square meters
    !> 
    !==================================================================================================================
    subroutine Set_Mortality(mortality, grid, shell_lengths, num_sz_classes, dom_name, dom_area, element_area)
        use Data_Point_Mod 
        
        type(Mortality_Class), intent(inout):: mortality(*)
        type(Data_Vector_Class), intent(in) :: grid
        real(dp), intent(in):: shell_lengths(*)
        integer, intent(in) :: num_sz_classes
        character(2), intent(in) :: dom_name
        real(dp), intent(in) :: dom_area
        real(dp), intent(in) :: element_area
        integer yr_index, j, num_years, year
        real(dp) fishing_by_region(max_num_years, 4), length_0

        !! initalize private members
        num_grids = grid%len
        num_size_classes = num_sz_classes
        domain_name = dom_name
        domain_area_sqm = dom_area
        grid_area_sqm = element_area

        do j=1, num_grids
            mortality(j)%mgmt_area_index = grid%posn(j)%mgmt_area_index
        enddo

        !
        ! Load parameters for fishing selectivity
        !(1+exp(.1*(shell_lengths-70))).^-1)

        !Assign Fishing pressure, selectivity parameters, and Natural Mortality from CASA model
        if (domain_name .eq. 'MA') then
            mortality(1:num_grids)%natural_mort_adult = .25D0
            mortality(1:num_grids)%incidental = 0.05D0
            length_0 = 65._dp
        else
            mortality(1:num_grids)%natural_mort_adult = .2D0
            mortality(1:num_grids)%incidental = 0.1D0
            length_0 = 70._dp
        endif

        !a = 0.1D0
        mortality(1:num_grids)%is_closed = grid%posn(1:num_grids)%is_closed
        do j = 1, num_grids
            !3!
            mortality(j)%natural_mortality(1:num_size_classes) = mortality(j)%natural_mort_adult
            mortality(j)%alpha(1:num_size_classes) =  1._dp &
            &     - 1._dp / ( 1._dp + exp( - ( shell_lengths(1:num_size_classes)/10._dp - length_0 ) ) )  
        enddo

        call Read_CSV(num_years, 4, 'Data/FYrGBcGBoMA.csv', fishing_by_region, size(fishing_by_region,1))
        yr_index = 0
        !!!do year = 1975, 2019
        ! these years match what is in 'Data/FYrGBcGBoMA.csv'
        do year = int(fishing_by_region(1,1)), int(fishing_by_region(num_years,1))
            yr_index = yr_index + 1
            ! TODO This loop is essentially
            ! For all values j, mortality(j)%fishing_effort(yr_index) = a set value
            !do j = 1, num_grids
            if (domain_name(1:2).eq.'MA') then
                mortality(1:num_grids)%fishing_effort(yr_index) = fishing_by_region(yr_index, 4)
            else ! must be GB
                do j = 1, num_grids
                    if (grid%posn(j)%is_closed) then
                        mortality(j)%fishing_effort(yr_index) = fishing_by_region(yr_index, 2)        
                    else
                        mortality(j)%fishing_effort(yr_index) = fishing_by_region(yr_index, 3)
                    endif
                enddo
            endif
            ! TODO DONE mortality(j)%select(1:num_size_classes) is not indexed by year
            ! These values get overwritten for each value of year
            ! Furthermore, these values are overwritten again in the MAIN LOOP
            ! Moved to MAIN LOOP, especially since mortality(j)%is_closed was being used before it was set
            !!! mortality(j)%select(1:num_size_classes) = Ring_Size_Selectivity(year, shell_lengths(1:num_size_classes), &
            !!! &                                          mortality(j)%is_closed)
            ! TODO should this equation be included in MAIN LOOP as well, it is dependent on %select
            ! Moved to MAIN LOOP
            !!! mortality(j)%discard(1:num_size_classes) = 0.2*mortality(j)%select(1:num_size_classes)

            ! TODO the remaining values are independent of year and yr_index and will keep getting set 
            ! to the same values
            !1!mortality(j)%incidental = 0.1D0
            !1!if(domain_name(1:2).eq.'MA') mortality(j)%incidental = 0.05D0
            !!! Moved to MAIN LOOP
            !!!do k = 1, num_size_classes
            !!!    if(shell_lengths(k).gt.90.)mortality(j)%discard(k) = 0.D0
            !!!enddo
            !3!mortality(j)%is_closed = grid%posn(j)%is_closed
            !3!mortality(j)%natural_mortality(1:num_size_classes) = mortality(j)%natural_mort_adult
            !3!mortality(j)%alpha(1:num_size_classes) =  1. - 1. / ( 1. + exp( - a*( shell_lengths(1:num_size_classes) - length_0 ) ) )  
            mortality(1:num_grids)%year(yr_index) = year
        enddo
        mortality(1:num_grids)%num_years = yr_index
        
    return
    endsubroutine Set_Mortality

    !==================================================================================================================
    !> @fn Ring_Size_Selectivity
    !> @public @memberof Mortality_Class
    !>
    !> Purpose: Assign size class fishing selectivity based on increasing logistic function
    !> @f[
    !> Selectivity = \frac{1}{ 1 + e^{ a - b * length_{shell}}}
    !> @f]
    !>
    !> @param[in] shell_length (real(dp))    length n vector of shell lengths
    !> @param[in] a, b         (real(dp))    parameters of logistic selectivity curve
    !> @param[in] year      (integer)	Year to determine if this is before 2005 (3.5" rings) or after (4" rings)
    !> @param[in] is_closed true if grid is closed to fishing
    !>
    !> @author Keston Smith (IBSS corp) May 2022
    !> 
    !> @returns  length num_size_classes vector of selectivity
    !==================================================================================================================
    elemental function Ring_Size_Selectivity(year, shell_length, is_closed)
        implicit none
        integer, intent(in) :: year
        real(dp), intent(in)::shell_length
        logical, intent(in) :: is_closed
        real(dp) Ring_Size_Selectivity

        real(dp) a, b
        
        if(year .lt. 2005) then ! 3.5" Rings
            a = 15.16
            b = .2021
        else !4" Ringsa=
            if(domain_name(1:2) .eq. 'MA')then
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
        Ring_Size_Selectivity = 1.D0 / ( 1.D0 + exp( a - b * shell_length ) )

    endfunction Ring_Size_Selectivity

    !==================================================================================================================
    !> @fn Set_Fishing
    !> @public @memberof Mortality_Class
    !>
    !> Determines a real value of mortality due to fishing given a fishing type
    !>
    !> @param[in] fishing_type 3 character string 'USD', 'BMS', 'CAS'
    !>  - USD: fishing proportional to value of stock
    !>  - BMS: fishing proportional to biomass
    !>  - CAS: fishing spatially constant with region, CASA
    !>
    !> @param[in] year
    !> @param[in] state matrix|num_grids by num_size classes| current state in scallops per square meter
    !> @param[in] weight_grams matrix|num_grids by num_size classes|
    !> @param[in] mortality vector(num_grids)
    !> @results fishing mortality
    !==================================================================================================================
    function Set_Fishing(fishing_type, year, state, weight_grams, mortality)
        implicit none
        integer, intent(in):: year
        real(dp), intent(in):: state(num_dimensions, *), weight_grams(num_dimensions, * )
        type(Mortality_Class), INTENT(IN):: mortality(*)
        real(dp) :: Set_Fishing( num_grids )
        character(*), intent(in):: fishing_type !> < string inputs
        integer Mindx, n
        real(dp) catch_open, catch_closed, TotalCatch

        call Get_Total_Catch(year, catch_closed, catch_open)
        TotalCatch = catch_open+catch_closed

        Mindx = minloc( abs(mortality(1)%year(1:mortality(1)%num_years) - year ), 1)
        select case (fishing_type(1:3))
            case('USD')
                Set_Fishing (1:num_grids) = Set_Fishing_Effort_Weight_USD(year, mortality, TotalCatch, &
                &           catch_open, catch_closed, state, weight_grams)
            case('BMS')
                Set_Fishing(1:num_grids) = Set_Fishing_Effort_Weight_X(mortality, TotalCatch, 1.D0, 1.D0, &
                &           catch_open, catch_closed, state,weight_grams)
            case('CAS')
                Set_Fishing(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
            case default
                Set_Fishing(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
                write( * ,  * )'Unkown fishing fishing_type:', fishing_type, &
                &    '.  Using spatially constant fishing_effort from CASA model'
        end select
        !enforce no fishing on Closed Area 2 North
        if (domain_name(1:2).eq.'GB') then
            do n = 1, num_grids
                if (mortality(n)%mgmt_area_index.eq.6) Set_Fishing(n) = 0.D0
            enddo
        endif

        return
    endfunction Set_Fishing

    !==================================================================================================================
    !> @fn Dollars_Per_Pound
    !> @public @memberof Mortality_Class
    !> @brief Compute value of scallop population.
    !>
    !> Value is based on population structure. 
    !> The population is sorted into size count bucket classes U10, 10-20, 20-30, 30+ 
    !> and the value based on these classes and the year is read from the file 
    !> "Data/ScallopPrice.csv".
    !>
    !>
    !> @param[in] year	- current year
    !> @param[in] scallops_per_sqm	[num_size_classes] - Scallop Population density by shell height size class
    !> @param[in] meat_weight_grams	[num_size_classes] - Weight meat per individual scallop in each size class
    !> @returns dollars per square meter
    !>
    !> @author Keston Smith 2022
    !==================================================================================================================
    !Hi Keston. Attached are landings (metric tons), value (thousand $) and price ($/lb) from 1998 to 2021. 
    !The market categories are given in 
    !NESPP4: 8002 = U10, 8003 = 10-20, 8004 = 20-30, 8005 = 30-40, 8006 = 40-50, 8007 = 50-60, 8008 = 60+, 8009 = unclassified
    real(dp) function Dollars_Per_Pound(year, scallops_per_sqm, meat_weight_grams)
        implicit none
        real(dp), intent(in):: scallops_per_sqm( * ), meat_weight_grams( * )
        integer, intent(in):: year
        real(dp) cnt10, cnt10to20, cnt20to30, cnt30plus
        real(dp) ScallopPrice(50, 5);
        integer NPriceYears, indx

        ! convert weight in grams to count per pound and sort
        call Scallops_To_Counts(scallops_per_sqm, meat_weight_grams, cnt10, cnt10to20, cnt20to30, cnt30plus)

        ! Read in price per pound
        !'"decmal year", "U10", "10-20", "20-30", "30+"';
        call Read_CSV(NPriceYears, 5, 'Data/ScallopPrice.csv', ScallopPrice, size(ScallopPrice,1))

        ! Find index into ScallopPrice for the current year
        indx = minloc( abs( float(year)-ScallopPrice(1:NPriceYears, 1)  ), 1  )

        ! Find total dollar amount of scallops
        Dollars_Per_Pound = cnt10 * ScallopPrice(indx, 2)+cnt10to20 * ScallopPrice(indx, 3)+&
                    cnt20to30 * ScallopPrice(indx, 4)+cnt30plus * ScallopPrice(indx, 5)
        return
    endfunction Dollars_Per_Pound

    !==================================================================================================================
    !> @public @memberof Mortality_Class
    !> Purpose: Convert Scallop density by shell height and meat wieght to count data.  The count data 
    !> are divided into 
    !> cnt10- 10 or less scallops per pound.
    !> cnt10to20 10-20 scallops per pound.
    !> cnt20to30 20-30 scallops per pound.
    !> cnt30+ 30 or more scallops per pound.
    !>
    !> @param[in]  scallops_per_sqm (real) length num_size_classes vector of scallops by size class
    !> @param[in]  meat_weight_grams (real) length num_size_classes vector of weight of individual scallops by size class
    !>
    !> @param[out] cnt10 number of scallops wich get binned into U10
    !> @param[out] cnt10to20 number of scallops wich get binned into U10-20
    !> @param[out] cnt20to30 number of scallops wich get binned into U20-30
    !> @param[out] cnt30 number of scallops wich get binned into U30+
    !==================================================================================================================
    subroutine Scallops_To_Counts(scallops_per_sqm, meat_weight_grams, cnt10, cnt10to20, cnt20to30, cnt30plus)
        implicit none
        real(dp), intent(in):: scallops_per_sqm( * ), meat_weight_grams( * )
        real(dp), intent(out):: cnt10, cnt10to20, cnt20to30, cnt30plus
        real(dp) PoundsPerScallop(num_size_classes)
        integer j
        PoundsPerScallop(1:num_size_classes) = meat_weight_grams(1:num_size_classes)/grams_per_pound
        cnt10 = 0.
        cnt10to20 = 0.
        cnt20to30 = 0.
        cnt30plus = 0.
        do j = 1, num_size_classes
            if( PoundsPerScallop(j).gt. 1._dp/10._dp ) then
                cnt10 = cnt10 + scallops_per_sqm(j)
            elseif (PoundsPerScallop(j) .gt. 1._dp/20._dp) then
                cnt10to20 = cnt10to20 + scallops_per_sqm(j)
            elseif (PoundsPerScallop(j).gt. 1._dp/30._dp) then
                cnt20to30 = cnt20to30 + scallops_per_sqm(j)
            else
                cnt30plus = cnt30plus + scallops_per_sqm(j)
            endif
        enddo

        return
    endsubroutine Scallops_To_Counts

    !---------------------------------------------------------------------------------------------------
    ! Fishing contributes to Mortality due to fishing
    !---------------------------------------------------------------------------------------------------
    !==================================================================================================================
    !> @fn Set_Fishing_Effort_Weight_USD
    !> @public @memberof Mortality_Class
    !> Purpose: Set proportional rate of fishing mortality, fishing_effort, based on monetary value of catch. The at each grid
    !> point scallops are sorted into size bins U10.10-20, 20-30, 30+.  The price for each size class is loaded 
    !> for the year and the monetary value of scallops is determined at each grid point.  Fishing effort is 
    !> to be porportional to the value of scallops.  This version has seperate catches for closed and open areas. 
    !> 
    !> @param[in] year (integer) current year
    !> @param[in] mortality (mortality mod) see main program
    !> @param[in] catch
    !> @param[in] catch_open (real(dp)) Total catch in open areas in Metric Tons in year
    !> @param[in] catch_closed (real(dp)) catch in closed areas in Metric Tons in year
    !> @param[in] state current amount of scallops per square meter
    !> @param[in] weight_grams (real(dp)) Weight of scallop by node and size class
    !>
    !> Output:
    !>   fishing_effort based on monetary value (real(dp)) [num_grids] rate of fishing mortality
    !==================================================================================================================
    function Set_Fishing_Effort_Weight_USD(year, mortality, catch, catch_open, catch_closed, state, weight_grams)
        implicit none
        integer j
        real(dp), intent(in):: catch, catch_open, catch_closed, weight_grams(num_dimensions,*), state(num_dimensions,*)
        integer, intent(in):: year
        real(dp) :: Set_Fishing_Effort_Weight_USD( num_grids )
        type(Mortality_Class), INTENT(IN):: mortality( * )
        real(dp) total_catch_per_dollar, Cclosed, Copen, total_dollars_open, total_dollars_closed
        real(dp) USD_per_pound(num_grids) ! was just USD
        real(dp) total_weight_lbs(num_grids) ! was SWI
        real(dp) scallops_per_sqm(num_size_classes)
        real(dp) total_dollars( num_grids )

        do j = 1, num_grids
            scallops_per_sqm = mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes)
            USD_per_pound(j) = Dollars_Per_Pound(year, scallops_per_sqm, weight_grams(j, 1:num_size_classes))
            ! TODO Cash is determined for $/pound, this next line is using total weight in grams
            ! total_weight_lbs(j) = sum( mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes)  *  &
            ! &             weight_grams(j, 1:num_size_classes)/(10.D0**6) )
            ! FIXED with GramsPerPound
            total_weight_lbs(j) = sum(scallops_per_sqm * weight_grams(j, 1:num_size_classes) / grams_per_pound)
        enddo

        total_dollars = USD_per_pound * total_weight_lbs * grid_area_sqm
        if (domain_name .eq. 'MA') then
            total_catch_per_dollar = catch / sum(total_dollars)
            Set_Fishing_Effort_Weight_USD(1:num_grids) = total_catch_per_dollar  *  USD_per_pound(1:num_grids) 
        else
            total_dollars_open = sum( total_dollars * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed)))
            total_dollars_closed = sum( total_dollars * Logic_To_Double( mortality(1:num_grids)%is_closed) )
            if (total_dollars_open .le. 0.) total_dollars_open = 1.
            if (total_dollars_closed .le. 0.) total_dollars_closed = 1.
            Cclosed = catch_closed / total_dollars_closed
            Copen = catch_open / total_dollars_open
            do j = 1, num_grids
                if(mortality(j)%is_closed) then
                    Set_Fishing_Effort_Weight_USD(j) = Cclosed  *  USD_per_pound(j) 
                else 
                    Set_Fishing_Effort_Weight_USD(j) = Copen  *  USD_per_pound(j) 
                endif
            enddo
        endif
    return
    endfunction Set_Fishing_Effort_Weight_USD
    
    !==================================================================================================================
    !> @fn Set_Fishing_Effort_Weight_USD
    !> @public @memberof Mortality_Class
    !> Purpose: Set proportional rate of fishing mortality, fishing_effort, based on weight of catch. The at each grid
    !> point scallops are sorted into size bins U10.10-20, 20-30, 30+.  The price for each size class is loaded 
    !> for the year and the monatary value of scallops is determined at each grid point.  Fishing effort is 
    !> to be porportional to the value of scallops.  This version has seperate catches for closed and open areas. 
    !> 
    !> @param[in] mortality (mortality mod) see main program
    !> @param[in] catch
    !> @param[in] alpha, beta
    !> @param[in] catch_open (real(dp)) Total catch in open areas in Metric Tons in year
    !> @param[in] catch_closed (real(dp)) catch in closed areas in Metric Tons in year
    !> @param[in] state current amount of scallops per square meter
    !> @param[in] weight_grams (real(dp)) Weight of scallop by node and size class
    !>
    !> @returns fishing_effort  based on weight (real(dp)) [num_grids] rate of fishing mortality
    !==================================================================================================================
    function Set_Fishing_Effort_Weight_X(mortality, catch, alpha, beta, catch_open, catch_closed, state, weight_grams)
        implicit none
        integer j
        real(dp), intent(in):: state(num_dimensions,*),weight_grams(num_dimensions,*),catch_open,catch_closed,catch,alpha,beta
        real(dp) Set_Fishing_Effort_Weight_X( num_grids )
        type(Mortality_Class), INTENT(IN):: mortality( * )
        real(dp) SI(num_grids), tmp(num_grids)
        real(dp) total_weight_grams(num_grids) ! was SWI
        real(dp) SIclosed, SWIclosed, SIopen, SWIopen, Cclosed, Copen

        do j = 1, num_grids
            SI(j) = sum( mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes) )
            if(SI(j) .le. 0.) SI(j) = 1.D0
            total_weight_grams(j) = sum( mortality(j)%select(1:num_size_classes) * (weight_grams(j, 1:num_size_classes)/10._dp**6) &
            &        * state(j, 1:num_size_classes) )
        enddo
    
        if (domain_name .eq. 'MA') then
            tmp(1:num_grids) = total_weight_grams(1:num_grids)**alpha  / SI(1:num_grids)**beta
            Set_Fishing_Effort_Weight_X(1:num_grids) = &
            &    tmp(1:num_grids) * catch / sum( grid_area_sqm * tmp(1:num_grids) * total_weight_grams(1:num_grids) )
        else
            SWIopen = sum( total_weight_grams(1:num_grids) &
            &               * grid_area_sqm * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed)))
            SWIclosed = sum(total_weight_grams(1:num_grids) * grid_area_sqm * Logic_To_Double( mortality(1:num_grids)%is_closed))
            SIopen = sum(SI(1:num_grids)  * grid_area_sqm * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed)) )
            SIclosed = sum(SI(1:num_grids)  * grid_area_sqm * Logic_To_Double( mortality(1:num_grids)%is_closed)  )
    
            if(SIopen.le.0.)SIopen = 1.
            if(SIclosed.le.0.)SIclosed = 1.
            if(SWIopen.le.0.)SWIopen = 1.
            if(SWIclosed.le.0.)SWIclosed = 1.
    
            Cclosed = catch_closed / SWIclosed
            Copen = catch_open / SWIopen
            do j = 1, num_grids
                if(mortality(j)%is_closed) then
                    Set_Fishing_Effort_Weight_X(j) = Cclosed  *  total_weight_grams(j) 
                else
                    Set_Fishing_Effort_Weight_X(j) = Copen  *  total_weight_grams(j)
                endif
            enddo
        endif
    
        return
    endfunction Set_Fishing_Effort_Weight_X
    

    !==================================================================================================================
    !> @public @memberof Mortality_Class
    !> Computes the total catch for both open and closed areas.
    !>
    !> @param[in] year
    !>
    !> @param[out] total_catch_closed
    !> @param[out] total_catch_open
    !==================================================================================================================
    subroutine Get_Total_Catch(year,total_catch_closed,total_catch_open)
        implicit none
        integer num_years_max
        parameter (num_years_max=50)
        integer, intent(in):: year 
        real(dp), intent(out):: total_catch_open,total_catch_closed
        real(dp), save ::  M(num_years_max,4)
        logical, save :: first_call = .true.
        integer, save :: yr_indx = 1
        integer, save :: num_years = num_years_max

        if (first_call) then
            !regional landings in metric tons
            call Read_CSV(num_years,4,'Data/Landings_75-19nh.csv',M,size(M,1))
            first_call = .false.
        endif
        
        do while (floor(M(yr_indx,1)).lt.year )
            yr_indx = yr_indx + 1
        enddo
        if(yr_indx.gt.num_years) yr_indx = num_years
        select case (domain_name)
            case('GB')
                total_catch_closed = M(yr_indx,2)
                total_catch_open = M(yr_indx,3)
            case('MA')
                total_catch_closed = 0.D0
                total_catch_open = M(yr_indx,4)
            case default
                write(*,*)'Unkown domain name', domain_name
        end select
        
        return
    endsubroutine Get_Total_Catch

    !==================================================================================================================
    !> @public @memberof Mortality_Class
    !>
    !> Computes the total number of scallops, <b>S</b>, in millions. 
    !> Then recomputes juvenile mortality as a function of S.
    !> @f[
    !> M_{juv} = \begin{cases} 
    !>         e^{1.093 * log(S) - 9.701}, & \text{if } S > 1400 \text{ million (2030?)} \\
    !>         M_{adult},                                   & \text{otherwise}
    !> \end{cases}
    !> @f]
    !> 
    !> A similar formula for GB Open:
    !> @f[
    !> M_{juv} = \begin{cases} 
    !>         e^{(1.226*log(S)-10.49)}, &  \text{if } S > 1400 \text{ million (2030?)} \\
    !>         M_{adult},                                  & \text{otherwise}
    !> \end{cases}
    !> @f]
    !> where @f$M_{adult}@f$ is 0.25 if MA or 0.2 if GB
    !>
    !> TODO: At present the computation does not use the conditional but rather whichever is greater
    !>
    !> Decreasing logistic function,
    !> @f[
    !> \alpha(length) = 1-\frac{1}{1+e^{-length_0[length-a]}}
    !> @f]
    !>
    !> TODO, current alpha equation is:
    !> @f[
    !> \alpha(length) = 1-\frac{1}{1+e^{- a*( length/10.0-length_0 )}}
    !> @f]
    !> where @f$h_0@f$ is 65 if MA or 70 if GB
    !>
    !> Finally
    !> @f[
    !> M_{nat} = \alpha * M_{juv} + (1-\alpha) M_{adult}
    !> @f]
    !> 
    !> @param[in] recruit
    !> @param[in,out] mortality
    !> @param[in] state  Current state of scallop population in scallops/m^2
    !> @returns natural_mortality and juvenile mortality
    !>
    !==================================================================================================================------------------------------
    function Compute_Natural_Mortality(max_rec_ind, mortality, state)
        implicit none
        real(dp),intent(in) :: state(*)
        type(Mortality_Class), INTENT(INOUT):: mortality
        real(dp) Compute_Natural_Mortality(1:num_size_classes)
        integer, INTENT(IN):: max_rec_ind
        real(dp) recruits

        ! Find the total sum of scallops per sq meter time region area yields total estimate of scallops, 
        ! recruits is the number of scallops in millions

        recruits = sum(state(1:max_rec_ind)) * domain_area_sqm/(10.**6)
        if(domain_name(1:2).eq.'MA')then
            mortality%natural_mort_juv = max( mortality%natural_mort_adult , exp(1.093_dp * log(recruits) - 9.701_dp) )
        else
            mortality%natural_mort_juv = max( mortality%natural_mort_adult , exp(1.226_dp * log(recruits) - 10.49_dp ))
        endif

        Compute_Natural_Mortality(1:num_size_classes) = mortality%alpha(1:num_size_classes) * mortality%natural_mort_juv &
        &                       + (1._dp - mortality%alpha(1:num_size_classes)) * mortality%natural_mort_adult
        return
    endfunction Compute_Natural_Mortality

end module Mortality_Mod
   