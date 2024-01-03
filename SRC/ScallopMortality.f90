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
    use Grid_Manager_Mod, only : Grid_Data_Class
    implicit none

    !> @class Mortality_Class
    !! 
    !! Subroutines that determine expected mortality of scallops
    type Mortality_Class
        !> @public @memberof Mortality_Class
        !! Attrition due to natural mortality
        real(dp) natural_mortality(num_size_classes)
        !> @public @memberof Mortality_Class
        real(dp) incidental
        !> @public @memberof Mortality_Class
        real(dp) discard(num_size_classes)
        !> @public @memberof Mortality_Class
        real(dp) fishing_effort(max_num_years)
        !> @public @memberof Mortality_Class
        integer year(max_num_years)
        !> @public @memberof Mortality_Class
        real(dp) selectivity(num_size_classes)
        real(dp) selectivity_open(num_size_classes)
        real(dp) selectivity_closed(num_size_classes)
        !> @public @memberof Mortality_Class
        integer num_years
        !> @public @memberof Mortality_Class
        real(dp) natural_mort_adult, natural_mort_juv
        !> @public @memberof Mortality_Class
        real(dp) alpha(1:num_size_classes)
    end type Mortality_Class

    ! @private @memberof Mortality_Class
    character(72), PRIVATE :: config_file_name
    integer, PRIVATE :: num_grids
    character(2), PRIVATE :: domain_name
    real(dp), PRIVATE :: domain_area_sqm
    integer, PRIVATE :: num_time_steps
    real(dp), PRIVATE :: delta_time

    ! configuration parameters
    real(dp), PRIVATE :: ma_cull_size_mm
    real(dp), PRIVATE :: ma_discard
    real(dp), PRIVATE :: gb_cull_size_mm
    real(dp), PRIVATE :: gb_discard

    real(dp), PRIVATE :: ma_fselect_a
    real(dp), PRIVATE :: ma_fselect_b
    real(dp), PRIVATE :: gbc_fselect_a
    real(dp), PRIVATE :: gbc_fselect_b
    real(dp), PRIVATE :: gbo_fselect_a
    real(dp), PRIVATE :: gbo_fselect_b

    real(dp), PRIVATE :: ma_mort_adult
    real(dp), PRIVATE :: ma_incidental
    real(dp), PRIVATE :: ma_length_0
    real(dp), PRIVATE :: gb_mort_adult
    real(dp), PRIVATE :: gb_incidental
    real(dp), PRIVATE :: gb_length_0

    real(dp), PRIVATE, allocatable :: expl_biomass_by_loc(:)
    real(dp), PRIVATE, allocatable :: USD_per_sqm_by_loc(:)
    real(dp), PRIVATE, allocatable :: expl_scallops_psqm_by_loc(:)
    real(dp), PRIVATE, allocatable :: expl_num_by_loc(:)
    real(dp), PRIVATE, allocatable :: F_mort_by_loc(:)
    real(dp), PRIVATE, allocatable :: landings_by_num(:)
    real(dp), PRIVATE, allocatable :: landings_wgt_grams(:)
    real(dp), PRIVATE, allocatable :: landings_wgt_grams_open(:)
    real(dp), PRIVATE, allocatable :: landings_wgt_grams_closed(:)

    real(dp), PRIVATE :: expl_scallops_psqm_at_size(num_size_classes)
    real(dp), PRIVATE :: landings_at_size(num_size_classes)
    real(dp), PRIVATE :: landings_at_size_open(num_size_classes)
    real(dp), PRIVATE :: landings_at_size_closed(num_size_classes)


    CONTAINS

    subroutine Destructor()
        deallocate(expl_biomass_by_loc)
        deallocate(USD_per_sqm_by_loc)
        deallocate(expl_scallops_psqm_by_loc)
        deallocate(expl_num_by_loc)
        deallocate(F_mort_by_loc)

        deallocate(landings_by_num)
        deallocate(landings_wgt_grams)
        deallocate(landings_wgt_grams_open)
        deallocate(landings_wgt_grams_closed)
    endsubroutine Destructor

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
    !> 
    !==================================================================================================================
    subroutine Set_Mortality(mortality, grid, shell_lengths, dom_name, dom_area, num_ts, ts_per_year, ngrids)
        implicit none
        
        type(Mortality_Class), intent(inout):: mortality(*)
        type(Grid_Data_Class), intent(in) :: grid(*)
        real(dp), intent(in):: shell_lengths(*)
        character(2), intent(in) :: dom_name
        real(dp), intent(in) :: dom_area
        integer, intent(in) :: num_ts, ts_per_year
        integer, intent(in) :: ngrids
        integer yr_index, j, k, num_years, year
        real(dp) fishing_by_region(max_num_years, 4), length_0

        ! default values
        ma_cull_size_mm = 90.
        ma_discard = 0.2
        ma_fselect_a = 20.5079
        ma_fselect_b = 0.19845

        gb_cull_size_mm = 100.
        gb_discard = 0.2
        gbc_fselect_a = 17.72
        gbc_fselect_b = 0.15795
        gbo_fselect_a = 21.7345
        gbo_fselect_b = 0.2193
        ma_mort_adult = .25D0
        ma_incidental = 0.05D0
        ma_length_0 = 65._dp
        gb_mort_adult = .2D0
        gb_incidental = 0.1D0
        gb_length_0 = 70._dp

        call Read_Configuration()

        !! initalize private members
        num_grids = ngrids
        domain_name = dom_name
        domain_area_sqm = dom_area
        num_time_steps = num_ts
        delta_time = 1._dp / dfloat(ts_per_year)

        allocate(expl_biomass_by_loc(num_grids))
        allocate(USD_per_sqm_by_loc(num_grids))
        allocate(expl_scallops_psqm_by_loc(num_grids))
        allocate(expl_num_by_loc(num_grids))
        allocate(F_mort_by_loc(num_grids))

        allocate(landings_by_num(num_grids))
        allocate(landings_wgt_grams(num_grids))
        allocate(landings_wgt_grams_open(num_grids))
        allocate(landings_wgt_grams_closed(num_grids))
        !
        ! Load parameters for fishing selectivity
        !(1+exp(.1*(shell_lengths-70))).^-1)

        !Assign Fishing pressure, selectivity parameters, and Natural Mortality from CASA model
        if (domain_name .eq. 'MA') then
            !TO DO add to params file
            mortality(1:num_grids)%natural_mort_adult = ma_mort_adult
            mortality(1:num_grids)%incidental = ma_incidental
            length_0 = ma_length_0
        else
            mortality(1:num_grids)%natural_mort_adult = gb_mort_adult
            mortality(1:num_grids)%incidental = gb_incidental
            length_0 = gb_length_0
        endif

        do j = 1, num_grids
            mortality(j)%natural_mortality(1:num_size_classes) = mortality(j)%natural_mort_adult
            mortality(j)%alpha(1:num_size_classes) =  &
            &    1._dp  - 1._dp / ( 1._dp + exp( - ( shell_lengths(1:num_size_classes)/10._dp - length_0 ) ) )
            
            mortality(j)%selectivity = Ring_Size_Selectivity(shell_lengths(1:num_size_classes), grid(j)%is_closed) 
            mortality(j)%selectivity_open = mortality(j)%selectivity * Logic_To_Double(.NOT. grid(j)%is_closed)
            mortality(j)%selectivity_closed = mortality(j)%selectivity * Logic_To_Double(grid(j)%is_closed)

            do k = 1, num_size_classes
                if (domain_name .eq. 'MA') then
                    if(shell_lengths(k) .gt. ma_cull_size_mm) then
                        mortality(j)%discard(k) = 0.D0
                    else
                        mortality(j)%discard(k) = ma_discard * mortality(j)%selectivity(k)
                    endif
                else
                    if ((shell_lengths(k) .gt. gb_cull_size_mm) .and. (grid(j)%is_closed)) then
                        mortality(j)%discard(k) = 0.D0
                    else
                        mortality(j)%discard(k) = gb_discard * mortality(j)%selectivity(k)
                    endif
                endif
            enddo
        enddo

        call Read_CSV(num_years, 4, 'Data/FYrGBcGBoMA.csv', fishing_by_region, size(fishing_by_region,1))
        yr_index = 0
        do year = int(fishing_by_region(1,1)), int(fishing_by_region(num_years,1))
            yr_index = yr_index + 1
            ! TODO This loop is essentially
            ! For all values j, mortality(j)%fishing_effort(yr_index) = a set value
            !do j = 1, num_grids
            if (domain_name(1:2).eq.'MA') then
                mortality(1:num_grids)%fishing_effort(yr_index) = fishing_by_region(yr_index, 4)
            else ! must be GB
                do j = 1, num_grids
                    if (grid(j)%is_closed) then
                        mortality(j)%fishing_effort(yr_index) = fishing_by_region(yr_index, 2)        
                    else
                        mortality(j)%fishing_effort(yr_index) = fishing_by_region(yr_index, 3)
                    endif
                enddo
            endif
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
    !>
    !> 3.5" rings were used from 1996-2004, 3.25" rings in 1995, and 3" rings through 1994. 
    !> We don't have curves for the 3 and 3.25" ring dredges. To estimate these selectivity curves, 
    !> I would simply shift the 3.5" ring curve to the left by 13 (for 3" rings) or 6 mm (for 3.25" rings).
    !> 
    !> The primary purpose of GEOSAMS is for forecasting, where all of this is irrelevant, to do some hindcasting
    !> as a way of testing the model, in which case getting the historical selectivity right is important.
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
    elemental function Ring_Size_Selectivity(shell_length, is_closed)
        implicit none
        real(dp), intent(in)::shell_length
        logical, intent(in) :: is_closed
        real(dp) Ring_Size_Selectivity

        real(dp) a, b
        
        if(domain_name(1:2) .eq. 'MA')then
            a = ma_fselect_a
            b = ma_fselect_b
        else
            if(is_closed) then
                a = gbc_fselect_a
                b = gbc_fselect_b
            else
                a = gbo_fselect_a
                b = gbo_fselect_b
            endif
        endif
        Ring_Size_Selectivity = 1.D0 / ( 1.D0 + exp( a - b * (shell_length+shell_len_delta/2._dp)))

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
    function Set_Fishing(fishing_type, year, ts, state, weight_grams, mortality, grid)
        implicit none
        character(*), intent(in):: fishing_type !> < string inputs
        integer, intent(in):: year, ts
        ! need num_dimensions as that is how variable was allocated to get column memory access correct
        real(dp), intent(in):: state(num_dimensions, num_size_classes), weight_grams(num_dimensions, num_size_classes )
        type(Mortality_Class), intent(in):: mortality(*)
        type(Grid_Data_Class), intent(in) :: grid(*)
        real(dp) :: Set_Fishing(num_grids)
        integer Mindx, loc
        real(dp) catch_open, catch_closed, total_catch
        real(dp) c_mort !constant for fishing mortality

        !=============================================================
        ! these sums are over num_size_classes
        do loc = 1,num_grids
            ! dot_product(selectivity, state)
            expl_scallops_psqm_by_loc(loc) = & 
            &    dot_product(mortality(loc)%selectivity(1:num_size_classes), state(loc,1:num_size_classes))

            ! dot_product(selectivity, state) * grid_area_sqm
            expl_num_by_loc(loc) = expl_scallops_psqm_by_loc(loc) * grid_area_sqm

            ! selectivity * state - at this location
            expl_scallops_psqm_at_size(1:num_size_classes) = mortality(loc)%selectivity(:) * state(loc, :)
            ! dot_product(selectivity * state, weight)
            expl_biomass_by_loc(loc) = dot_product(expl_scallops_psqm_at_size(1:num_size_classes), &
            &                                      weight_grams(loc,1:num_size_classes))

            ! Dollars_Per_SqM ultimately uses expl_scallops_psqm_at_size -> Scallops_To_Counts
            USD_per_sqm_by_loc(loc) = Dollars_Per_SqM(year, weight_grams(loc, 1:num_size_classes))
        enddo

        ! now sum here over num_grids 
        c_mort = Fishing_Mortality() / dot_product(expl_biomass_by_loc(:), expl_num_by_loc(:) / sum(expl_num_by_loc))

        do loc = 1,num_grids
            F_mort_by_loc(loc) = c_mort * expl_biomass_by_loc(loc)
        
            ! (1._dp - exp(-F * delta_time)) * state * grid_area_sqm  * selectivity
            landings_at_size(:) = (1._dp - exp(-F_mort_by_loc(loc) * delta_time)) &
            &    * state(loc, 1:num_size_classes) * grid_area_sqm&
            &    * mortality(loc)%selectivity(1:num_size_classes)
            landings_at_size_open(:) = (1._dp - exp(-F_mort_by_loc(loc)  * delta_time)) &
            &    * state(loc, 1:num_size_classes) * grid_area_sqm&
            &    * mortality(loc)%selectivity_open(1:num_size_classes)
            landings_at_size_closed(:) = (1._dp - exp(-F_mort_by_loc(loc)  * delta_time)) &
            &    * state(loc, 1:num_size_classes) * grid_area_sqm&
            &    * mortality(loc)%selectivity_closed(1:num_size_classes)
        
            ! (1._dp - exp(-F * delta_time))
            ! * dot_product(selectivity * state * grid_area_sqm,  weight)
            landings_wgt_grams(loc)        =&
            &    dot_product(landings_at_size(:),        weight_grams(loc,1:num_size_classes))
            landings_wgt_grams_open(loc)   =&
            &    dot_product(landings_at_size_open(:),   weight_grams(loc,1:num_size_classes))
            landings_wgt_grams_closed(loc) =&
            &    dot_product(landings_at_size_closed(:), weight_grams(loc,1:num_size_classes))
        
            ! (1._dp - exp(-F * delta_time)) * selectivity * state * grid_area_sqm
            landings_by_num(loc)    = sum(landings_at_size(:))
        enddo
        !=============================================================
        !=============================================================
        ! originally in metric tons, Set_Fishing_Effort_Weight_xx modified to accept grams
        total_catch = sum(landings_wgt_grams)
        catch_open = sum(landings_wgt_grams_open)
        catch_closed = sum(landings_wgt_grams_closed)
        !=============================================================

        select case (fishing_type(1:3))
            case('USD')
                Set_Fishing (1:num_grids) = Set_Fishing_Effort_Weight_USD(grid(1:num_grids)%is_closed, &
                &                           total_catch, catch_open, catch_closed)
            case('BMS')
                Set_Fishing(1:num_grids) = Set_Fishing_Effort_Weight_BMS(grid(1:num_grids)%is_closed, &
                &                           total_catch, catch_open, catch_closed)
            case('CAS')
                Mindx = minloc( abs(mortality(1)%year(1:mortality(1)%num_years) - year ), 1)
                Set_Fishing(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
            case default
                Mindx = minloc( abs(mortality(1)%year(1:mortality(1)%num_years) - year ), 1)
                Set_Fishing(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
                write( * ,  * )'Unkown fishing fishing_type:', fishing_type, &
                &    '.  Using spatially constant fishing_effort from CASA model'
        end select

        ! Report current timestep results
        call Mortality_Write_At_Timestep(year, ts, state, weight_grams, mortality, Set_Fishing(:))

        return
    endfunction Set_Fishing

    !==================================================================================================================
    !> @fn Dollars_Per_SqM
    !> @public @memberof Mortality_Class
    !> @brief Compute value of scallop population at a specific grid location
    !>
    !> Value is based on population structure. 
    !> The population is sorted into size count bucket classes U10, 10-20, 20-30, 30+ 
    !> and the value based on these classes and the year is read from the file 
    !> "Data/ScallopPrice.csv".
    !>
    !>
    !> @param[in] year	- current year
    !> @param[in] meat_weight_grams	[num_size_classes] - Weight meat per individual scallop in each size class
    !> @returns dollars per square meter
    !>
    !> @author Keston Smith 2022
    !==================================================================================================================
    !Hi Keston. Attached are landings (metric tons), value (thousand $) and price ($/lb) from 1998 to 2021. 
    !The market categories are given in 
    !NESPP4: 8002 = U10, 8003 = 10-20, 8004 = 20-30, 8005 = 30-40, 8006 = 40-50, 8007 = 50-60, 8008 = 60+, 8009 = unclassified
    real(dp) function Dollars_Per_SqM(year, meat_weight_grams)
        implicit none
        real(dp), intent(in):: meat_weight_grams(*)
        integer, intent(in):: year
        real(dp) cnt10, cnt10to20, cnt20to30, cnt30plus
        real(dp) ScallopPrice(50, 5);
        integer NPriceYears, indx

        ! convert weight in grams to count per pound and sort
        call Scallops_To_Counts(meat_weight_grams, cnt10, cnt10to20, cnt20to30, cnt30plus)

        ! Read in price per pound
        !'"decmal year", "U10", "10-20", "20-30", "30+"';
        call Read_CSV(NPriceYears, 5, 'Data/ScallopPrice.csv', ScallopPrice, size(ScallopPrice,1))

        ! Find index into ScallopPrice for the current year
        indx = minloc( abs( float(year)-ScallopPrice(1:NPriceYears, 1)  ), 1  )

        ! Find total dollar amount of scallops
        Dollars_Per_SqM = cnt10 * ScallopPrice(indx, 2)+cnt10to20 * ScallopPrice(indx, 3)+&
                    cnt20to30 * ScallopPrice(indx, 4)+cnt30plus * ScallopPrice(indx, 5)
        return
    endfunction Dollars_Per_SqM

    !==================================================================================================================
    !> @public @memberof Mortality_Class
    !> Purpose: Convert Scallop density by shell height and meat wieght to count data.  The count data 
    !> are divided into 
    !> cnt10- 10 or less scallops per pound.
    !> cnt10to20 10-20 scallops per pound.
    !> cnt20to30 20-30 scallops per pound.
    !> cnt30+ 30 or more scallops per pound.
    !>
    !> @param[in]  meat_weight_grams (real) length num_size_classes vector of weight of individual scallops by size class
    !>
    !> @param[out] cnt10 number of scallops wich get binned into U10
    !> @param[out] cnt10to20 number of scallops wich get binned into U10-20
    !> @param[out] cnt20to30 number of scallops wich get binned into U20-30
    !> @param[out] cnt30 number of scallops wich get binned into U30+
    !==================================================================================================================
    subroutine Scallops_To_Counts(meat_weight_grams, cnt10, cnt10to20, cnt20to30, cnt30plus)
        implicit none
        real(dp), intent(in):: meat_weight_grams(*)
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
                cnt10 = cnt10 + expl_scallops_psqm_at_size(j)
            elseif (PoundsPerScallop(j) .gt. 1._dp/20._dp) then
                cnt10to20 = cnt10to20 + expl_scallops_psqm_at_size(j)
            elseif (PoundsPerScallop(j).gt. 1._dp/30._dp) then
                cnt20to30 = cnt20to30 + expl_scallops_psqm_at_size(j)
            else
                cnt30plus = cnt30plus + expl_scallops_psqm_at_size(j)
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
    !> @param[in] mortality (mortality mod) see main program
    !> @param[in] catch total landings in grams
    !> @param[in] catch_open total landings from open areas in grams
    !> @param[in] catch_closed total landings from closed areas in grams
    !> @returns Unitless value of fishing_effort based on monetary value 
    !==================================================================================================================
    function Set_Fishing_Effort_Weight_USD(is_closed, catch, catch_open, catch_closed)
        implicit none
        integer j
        logical, INTENT(IN):: is_closed(num_grids)
        real(dp), intent(in):: catch, catch_open, catch_closed
        real(dp) :: Set_Fishing_Effort_Weight_USD(num_grids)
        real(dp) Cclosed, Copen, total_dollars_open, total_dollars_closed
        real(dp) tmp

        if (domain_name .eq. 'MA') then
            tmp = dot_product(USD_per_sqm_by_loc(:), expl_biomass_by_loc(:)) * grid_area_sqm
            if (tmp.eq.0) tmp = 1._dp
            Set_Fishing_Effort_Weight_USD(:) = catch * USD_per_sqm_by_loc(:) / tmp 
        else
            do j = 1, num_grids
                if(is_closed(j)) then
                    total_dollars_closed = (dot_product(USD_per_sqm_by_loc(:) &
                    &   * Logic_To_Double(is_closed(1:num_grids)), expl_biomass_by_loc(:))) * grid_area_sqm
                    if (total_dollars_closed .eq. 0.) total_dollars_closed = 1.
                    Cclosed = USD_per_sqm_by_loc(j) * catch_closed / total_dollars_closed
                    Set_Fishing_Effort_Weight_USD(j) = Cclosed
                else 
                    total_dollars_open = (dot_product(USD_per_sqm_by_loc(:) &
                    &   * Logic_To_Double(.NOT. is_closed(1:num_grids)), expl_biomass_by_loc(:))) * grid_area_sqm
                    if (total_dollars_open .eq. 0.) total_dollars_open = 1.
                    Copen = USD_per_sqm_by_loc(j) * catch_open / total_dollars_open
                    Set_Fishing_Effort_Weight_USD(j) = Copen 
                endif
            enddo
        endif
    return
    endfunction Set_Fishing_Effort_Weight_USD
    
    !==================================================================================================================
    !> @fn Set_Fishing_Effort_Weight_BMS
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
    function Set_Fishing_Effort_Weight_BMS(is_closed, catch, catch_open, catch_closed)
        implicit none
        real(dp) Set_Fishing_Effort_Weight_BMS( num_grids )
        logical, INTENT(IN):: is_closed(num_grids)
        real(dp), INTENT(INOUT)::  catch, catch_closed, catch_open
        integer j,n
        real(dp) exp_bms_closed, exp_bms_open
        real(dp) tmp

        if (domain_name .eq. 'MA') then
            do j = 1, num_grids
                tmp = 0;
                do n = 1, num_grids
                    if (expl_scallops_psqm_by_loc(n).eq.0.0) then
                        tmp = tmp + expl_biomass_by_loc(n) * expl_biomass_by_loc(n)
                    else
                        tmp = tmp + expl_biomass_by_loc(n) * expl_biomass_by_loc(n) / expl_scallops_psqm_by_loc(n)
                    endif
                enddo
                ! 
                Set_Fishing_Effort_Weight_BMS(j) = &
                &   (expl_biomass_by_loc(j) * catch / expl_scallops_psqm_by_loc(j) ) / (tmp  * grid_area_sqm)
            enddo
        else
            exp_bms_open = sum( Logic_To_Double(.NOT. is_closed(1:num_grids)) * expl_biomass_by_loc(:))
            exp_bms_closed = sum( Logic_To_Double(is_closed(1:num_grids)) * expl_biomass_by_loc(:))
            if(exp_bms_open.eq.0.)exp_bms_open = 1.
            if(exp_bms_closed.eq.0.)exp_bms_closed = 1.
    
            do j = 1, num_grids
                if(.NOT. is_closed(j)) then
                    Set_Fishing_Effort_Weight_BMS(j) = catch_open * expl_biomass_by_loc(j) / (exp_bms_open * grid_area_sqm)
                else
                    Set_Fishing_Effort_Weight_BMS(j) = catch_closed * expl_biomass_by_loc(j) / (exp_bms_closed * grid_area_sqm)
                endif
            enddo
        endif
    
        return
    endfunction Set_Fishing_Effort_Weight_BMS
    

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
    !> @fn Compute_Natural_Mortality
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

    !==================================================================================================================
    !> @public @memberof Mortality_Class
    !> @fn Compute_Natural_Mortality
    !> Computes Fishing Mortality
    !>
    !==================================================================================================================
    elemental real(dp) function Fishing_Mortality()
        Fishing_Mortality = 0.4_dp
    endfunction Fishing_Mortality

    !-----------------------------------------------------------------------------------------------
    !! @public @memberof Mortality_Class
    !> Used during instantiation to set the name of the file to read to for configuration parameters
    !> @brief Read Input File
    !> 
    !> Sets name of a configuration file, 'config_file_name.cfg'
    !-----------------------------------------------------------------------------------------------
    subroutine Set_Config_File_Name(fname)
        character(*), intent(in) :: fname
        config_file_name = config_dir//fname
    endsubroutine Set_Config_File_Name

    !-----------------------------------------------------------------------
    !> @public @memberof Mortality_Class
    !> Read_Configuration
    !> @brief Read Input File
    !> 
    !> Reads a configuration file, 'config_file_name.cfg', to set data parameters for Mortality
    !>
    !-----------------------------------------------------------------------
    subroutine Read_Configuration() !,num_monte_carlo_iter)
    
        implicit none
        character(100) input_string
        character(85) tag
        character(15) value
        integer j, k, io

        write(*,*) ' READING IN ', config_file_name

        open(read_dev,file=config_file_name)
        do
            input_string=""
            read(read_dev,'(a)',iostat=io) input_string
            if (io.lt.0) exit
            if (input_string(1:1) .NE. '#') then
                j = scan(input_string,"=",back=.true.)
                tag = trim(adjustl(input_string(1:j-1)))
                ! explicitly ignore inline comment
                k = scan(input_string,"#",back=.true.)
                if (k .EQ. 0) k = len(input_string)
                value =  trim(adjustl(input_string(j+1:k-1)))

                select case(tag)
                case ('MA Cull size')
                    read(value, *) ma_cull_size_mm

                case('MA Discard')
                    read(value, *) ma_discard

                case('GB Cull size')
                    read(value, *) gb_cull_size_mm

                case('GB Discard')
                    read(value, *) gb_discard

                case('MA FSelectA')
                    read(value, *) ma_fselect_a

                case('MA FSelectB')
                    read(value, *) ma_fselect_b

                case('GB Closed FSelectA')
                    read(value, *) gbc_fselect_a

                case('GB Closed FSelectB')
                    read(value, *) gbc_fselect_b

                case('GB Open FSelectA')
                    read(value, *) gbo_fselect_a

                case('GB Open FSelectB')
                    read(value, *) gbo_fselect_b

                case('MA Adult Mortality')
                    read(value, *) ma_mort_adult

                case('GB Adult Mortality')
                    read(value, *) gb_mort_adult

                case('MA Incidental')
                    read(value, *) ma_incidental

                case('GB Incidental')
                    read(value, *) gb_incidental

                case('MA Length_0')
                    read(value, *) ma_length_0

                case('GB Length_0')
                    read(value, *) gb_length_0

                case default
                    write(*,*) term_red, 'Unrecognized line in ',config_file_name
                    write(*,*) 'Unknown Line-> ',input_string, term_blk
                end select
            endif
        end do
        close(read_dev)
        return
    end subroutine Read_Configuration
    
    !==================================================================================================================
    !> @public @memberof Mortality_Class
    !>
    !> Initializes growth for startup
    !>
    !==================================================================================================================
    subroutine Mortality_Write_At_Timestep(year, ts, state, weight_grams, mortality, set_fishing_by_loc)
        integer, intent(in) :: year
        integer, intent(in) :: ts
        real(dp), intent(in) :: state(num_dimensions, num_size_classes)
        real(dp), intent(in) :: weight_grams(num_dimensions, num_size_classes)
        type(Mortality_Class), intent(in):: mortality(*)
        real(dp), intent(in) :: set_fishing_by_loc(*)

        integer loc
        character(20) buf
        character(5) gbuf
        real(dp) :: M(num_size_classes)
        real(dp), allocatable :: abundance(:)
        real(dp), allocatable :: bms(:)

        allocate(abundance(num_grids))
        allocate(bms(num_grids))

       ! write entire state for all grids in separate time stamp files
        write(buf,'(A8,I4,A1,I0.3)') '_AtTime_',year, '_', ts
        call Write_CSV(num_grids, num_size_classes, state, &
        &            output_dir//'State'//domain_name//trim(buf)//'.csv',size(state,1), .FALSE.)

        do loc = 1, num_grids
            !=======================================================================================
            ! CSV files at grid # over time: for each grid;  (number of timesteps by number of size classes)
            !=======================================================================================

            write(gbuf,'(I0.5)') loc
            call Write_CSV(1, num_size_classes, state(loc,1:num_size_classes), &
            &    output_dir//'State'//domain_name//'_AtGrid-'//gbuf//'_BySizeOverTime.csv', 1, (ts .ne. 0))

            ! Compute overall mortality
            M(1:num_size_classes) = mortality(loc)%natural_mortality(1:num_size_classes) &
            & + set_fishing_by_loc(loc) * ( mortality(loc)%selectivity(1:num_size_classes) &
            & + mortality(loc)%incidental + mortality(loc)%discard(1:num_size_classes) )
            call Write_CSV(1, num_size_classes, M(1:num_size_classes), &
            &    output_dir//'TotalMort'//domain_name//'_AtGrid-'//gbuf//'_BySizeOverTime.csv', 1, (ts .ne. 0))

            !=======================================================================================


            abundance(loc) = sum(state(loc,1:num_size_classes))                                  ! scallops per sq meter
            bms(loc) = dot_product(state(loc,1:num_size_classes), &                              ! metric tons per sq meter
            &                      weight_grams(loc,1:num_size_classes) / grams_per_metric_ton)

            ! Fslct(loc,1:num_size_classes) = mortality(loc)%selectivity(1:num_size_classes)
            ! ExplBMS(loc) = sum( mortality(loc)%selectivity(1:num_size_classes) * samp(loc,1:num_size_classes) &
            ! &            * weight_grams(loc,1:num_size_classes)/(10.**6) )
            ! StateCapt(loc) = sum(samp(loc,1:num_size_classes) * mortality(loc)%selectivity(1:num_size_classes))
            ! mortality(loc)%natural_mortality(1:num_size_classes) = &
            ! &           Compute_Natural_Mortality(recruit(loc)%max_rec_ind, mortality(loc), samp(loc,1:num_size_classes))
            ! NatMort(loc,1:num_size_classes) = mortality(loc)%natural_mortality(1:num_size_classes)
            ! FishMort(loc,1:num_size_classes) = fishing_effort(loc) * mortality(loc)%selectivity(1:num_size_classes)
            ! call Scallops_To_Counts(weight_grams(loc,1:num_size_classes), cnts(loc,1), cnts(loc,2), cnts(loc,3), cnts(loc,4))
            ! Dollars_Per_SqMeter(loc) = Dollars_Per_SqM(year, weight_grams(loc,1:num_size_classes))

        enddo

        ! CSV files (number of timestep by number of grids)
        call Write_CSV(1, num_grids, abundance,           output_dir//'Abundance_psqm_'//domain_name//'.csv',    1, (ts .ne. 0))
        call Write_CSV(1, num_grids, abundance*grid_area_sqm, output_dir//'Abundance_'//domain_name//'.csv',     1, (ts .ne. 0))
        call Write_CSV(1, num_grids, bms,                 output_dir//'BMS_mtpsqm_'//domain_name//'.csv',        1, (ts .ne. 0))
        call Write_CSV(1, num_grids, bms*grid_area_sqm,   output_dir//'BMS_mt_'//domain_name//'.csv',            1, (ts .ne. 0))
        call Write_CSV(1, num_grids, landings_by_num,     output_dir//'NumLandings'//domain_name//'.csv',1, (ts .ne. 0))
        call Write_CSV(1, num_grids, expl_biomass_by_loc(1:num_grids)*grid_area_sqm/grams_per_metric_ton,&
        &                                                  output_dir//'ExplBMS_mt_'//domain_name//'.csv',    1, (ts .ne. 0))
        call Write_CSV(1, num_grids, expl_biomass_by_loc(1:num_grids)/grams_per_metric_ton,&
        &                                                  output_dir//'ExplBMS_mtpsqm_'//domain_name//'.csv',    1, (ts .ne. 0))
        call Write_CSV(1, num_grids, set_fishing_by_loc,  output_dir//'FishingEffort'//domain_name//'.csv',       1, (ts .ne. 0))
        call Write_CSV(1, num_grids, F_mort_by_loc,       output_dir//'FishingMort'//domain_name//'.csv',         1, (ts .ne. 0))
        call Write_CSV(1, num_grids, USD_per_sqm_by_loc*grid_area_sqm,  output_dir//'USD_psqm_'//domain_name//'.csv',1, (ts .ne. 0))
        call Write_CSV(1, num_grids, landings_wgt_grams(1:num_grids)/grams_per_metric_ton, &
        &                                                 output_dir//'WgtLandings_mt'//domain_name//'.csv',1, (ts .ne. 0))
                    
        deallocate(abundance)
        deallocate(bms)
        return
    endsubroutine Mortality_Write_At_Timestep

end module Mortality_Mod
