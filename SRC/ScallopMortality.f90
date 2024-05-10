!>----------------------------------------------------------------------------------------------------------------
!> @page page3 Mortality_Mod
!>
!> @section p3p1 Mortality Class
!> The methods in this class are used to determine the selectiviy and discard of the scallops based on shell
!> length and location.
!>
!> @subsection p3p1p1 Set_Mortality
!> Instantiates private members for this class. 
!>   - Reads in its configuration parameters and stores to private members.
!>   - Loads Fishing Mortalities, if enabled by GridManager
!>   - Sets up a repository for key values to allow offline analysis
!>   - Loads historical data for Fishing Effort
!>   - Set selectivity as computed by Ring_Size_Selectivity based on shell length and grid location
!>
!> @subsection p3p1p2 Read_Configuration
!> Opens the configuration file specified in the simulation configuration file and as set by @a Set_Config_File_Name
!>
!> @subsection p3p1p3 Load_Fishing_Mortalities
!> Opens the configuration file specified in the Mortality configuration file and as set by @a Set_Fishing_Mortality
!> 
!> @subsection p3p1p4 Ring_Size_Selectivity
!> Assign size class fishing selectivity based on increasing logistic function
!> @f[
!> Selectivity = \frac{1}{ 1 + e^{ a - b * length_{shell}}}
!> @f]
!>
!> @subsection p3p1p5 Set_Fishing_Effort
!> @subsection p3p1p6 Dollars_Per_SqM
!> @subsection p3p1p7 Scallops_To_Counts
!> @subsection p3p1p8 Set_Fishing_Effort_Weight_USD
!> @subsection p3p1p9 Set_Fishing_Effort_Weight_BMS
!> @subsection p3p1p11 Compute_Natural_Mortality
!> @subsection p3p1p12 Set_Fishing_Mortality
!> @subsection p3p1p13 Set_Config_File_Name
!> @subsection p3p1p14 Set_Fishing_Mort_File_Name
!> @subsection p3p1p15 Mortality_Write_At_Timestep
!> @subsection p3p1p16 Set_Discard
!>
!>----------------------------------------------------------------------------------------------------------------
module Mortality_Mod
use globals
use Grid_Manager_Mod, only : Grid_Data_Class, Get_Num_Of_Areas
implicit none

!> @class Mortality_Class
!! 
!! Subroutines that determine expected mortality of scallops
type Mortality_Class
    !! @public @memberof Mortality_Class
    !! Attrition due to natural mortality
    real(dp) natural_mortality(num_size_classes)
    !! @public @memberof Mortality_Class
    real(dp) incidental
    !! @public @memberof Mortality_Class
    real(dp) discard(num_size_classes)
    !! @public @memberof Mortality_Class
    real(dp) fishing_effort(max_num_years)
    !! @public @memberof Mortality_Class
    integer year(max_num_years)
    !! @public @memberof Mortality_Class
    real(dp) selectivity(num_size_classes)
    real(dp) selectivity_open(num_size_classes)
    real(dp) selectivity_closed(num_size_classes)
    !! @public @memberof Mortality_Class
    integer num_years
    !! @public @memberof Mortality_Class
    real(dp) natural_mort_adult, natural_mort_juv
    !! @public @memberof Mortality_Class
    real(dp) alpha(1:num_size_classes)
end type Mortality_Class

type FishingMortality
    integer year
    integer n_areas
    integer area_list(max_num_areas)
    real(dp) area_fish_mort(max_num_areas)
endtype FishingMortality

type DataForPlots
    logical plot_ABUN ! abundance scallops per square meter
    logical plot_BMMT ! biomass in metric tons
    logical plot_EBMS ! exploitable biomass in metric tons
    logical plot_FEFF ! Fishing Effort
    logical plot_FMOR ! Fishing Mortality
    logical plot_LAND ! Landings by number of scallops
    logical plot_LNDW ! Landings by weight in grams
    logical plot_LPUE ! Landing Per Unit Effor, (per day)
    logical plot_RECR ! Recruitment
endtype DataForPlots

! @private @memberof Mortality_Class
character(fname_len), PRIVATE :: config_file_name
character(fname_len), PRIVATE :: fishing_mort_fname
type(FishingMortality), PRIVATE :: fmort_list(max_num_years)
character(3), PRIVATE ::  fishing_type      !> Fishing can be USD,  BMS,  or,  CAS

! if a fmort_list file exists then use data, otherwise use_spec_access_data is false
logical, PRIVATE :: use_spec_access_data
integer, PRIVATE :: num_in_list
integer, PRIVATE :: num_grids
integer, PRIVATE :: num_areas
character(domain_len), PRIVATE :: domain_name
real(dp), PRIVATE :: domain_area_sqm
integer, PRIVATE :: num_time_steps
integer, PRIVATE :: ts_per_year
real(dp), PRIVATE :: delta_time
logical, PRIVATE :: save_by_stratum

! configuration parameters
real(dp), PRIVATE :: fishing_mort
real(dp), PRIVATE :: alpha_mort
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

! Used for LPUE
real(dp), PRIVATE :: lpue_slope
real(dp), PRIVATE :: lpue_slope2
real(dp), PRIVATE :: lpue_intercept
integer,  PRIVATE :: max_per_day
real(dp), PRIVATE :: max_time_hpd
real(dp), PRIVATE :: dredge_width_m
real(dp), PRIVATE :: towing_speed_knots

real(dp), PRIVATE, allocatable :: expl_biomass_gpsqm(:)
real(dp), PRIVATE, allocatable :: USD_per_sqm(:)
real(dp), PRIVATE, allocatable :: expl_scallops_psqm(:)
real(dp), PRIVATE, allocatable :: expl_num(:)
real(dp), PRIVATE, allocatable :: F_mort(:)
real(dp), PRIVATE, allocatable :: F_mort_raw(:)
real(dp), PRIVATE, allocatable :: landings_by_num(:)
real(dp), PRIVATE, allocatable :: landings_wgt_grams(:)
real(dp), PRIVATE, allocatable :: landings_wgt_grams_open(:)
real(dp), PRIVATE, allocatable :: landings_wgt_grams_closed(:)
real(dp), PRIVATE, allocatable :: lpue(:) !, dredge_time(:), dredge_area(:)
real(dp), PRIVATE, allocatable :: fishing_effort(:)

real(dp), PRIVATE :: expl_scallops_psqm_at_size(num_size_classes)
real(dp), PRIVATE :: landings_at_size(num_size_classes)
real(dp), PRIVATE :: landings_at_size_open(num_size_classes)
real(dp), PRIVATE :: landings_at_size_closed(num_size_classes)

type(DataForPlots), PRIVATE :: data_select

CONTAINS

subroutine Set_Select_Data(value)
    type(DataForPlots), intent(in) :: value
    data_select = value
endsubroutine Set_Select_Data

!==================================================================================================================
!! @public @memberof Mortality_Class
!==================================================================================================================
subroutine Destructor()
    deallocate(expl_biomass_gpsqm)

    deallocate(USD_per_sqm)
    deallocate(expl_scallops_psqm)
    deallocate(expl_num)
    deallocate(F_mort)
    deallocate(F_mort_raw)

    deallocate(landings_by_num)
    deallocate(landings_wgt_grams)
    deallocate(landings_wgt_grams_open)
    deallocate(landings_wgt_grams_closed)
    deallocate(lpue) !, dredge_time, dredge_area)
    deallocate(fishing_effort)
endsubroutine Destructor

!==================================================================================================================
!! @public @memberof Mortality_Class
!> @param[in,out] mortality Parameters that identify how the scallop should reaches mortality
!> @param[in] grid Vector that identifies the geospatial locations under simulation
!> @param[in] shell_lengths Vector of the size, or length, of scallops
!> @param[in] num_sz_classes Number of size classes to set private member
!> @param[in] domain_name Name of domain being simulate, 'MA' or 'GB'
!> @param[in] domain_area,Size of domain under consideration in square meters
!> 
!==================================================================================================================
subroutine Set_Mortality(mortality, grid, shell_lengths, dom_name, dom_area, num_ts, ts_py, ngrids, save_by_strat)
    implicit none
    
    type(Mortality_Class), intent(inout):: mortality(*)
    type(Grid_Data_Class), intent(in) :: grid(*)
    real(dp), intent(in):: shell_lengths(*)
    character(domain_len), intent(in) :: dom_name
    real(dp), intent(in) :: dom_area
    integer, intent(in) :: num_ts, ts_py
    integer, intent(in) :: ngrids
    logical, intent(in) :: save_by_strat
    integer yr_index, j, num_years, year
    real(dp) fishing_by_region(max_num_years, 4), length_0
    real(dp) cull_size, discard
    logical this_grid_is_closed

    !---------------------------------------------------
    ! initalize private members
    !---------------------------------------------------
    num_grids = ngrids
    num_areas = Get_Num_Of_Areas()
    domain_name = dom_name
    domain_area_sqm = dom_area
    num_time_steps = num_ts
    ts_per_year = ts_py
    delta_time = 1._dp / dfloat(ts_per_year)
    save_by_stratum = save_by_strat

    !---------------------------------------------------
    ! default values
    !---------------------------------------------------
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

    call Load_Fishing_Mortalities()

    allocate(expl_biomass_gpsqm(num_grids))
    allocate(USD_per_sqm(num_grids))
    allocate(expl_scallops_psqm(num_grids))
    allocate(expl_num(num_grids))
    allocate(F_mort(num_grids))
    allocate(F_mort_raw(num_grids))
    allocate(lpue(num_grids)) !, dredge_time(num_grids), dredge_area(num_grids))
    allocate(fishing_effort(num_grids))

    allocate(landings_by_num(num_grids))
    allocate(landings_wgt_grams(num_grids))
    allocate(landings_wgt_grams_open(num_grids))
    allocate(landings_wgt_grams_closed(num_grids))

    !---------------------------------------------------
    ! Set mortality parameters
    !---------------------------------------------------
    if (domain_name .eq. 'MA') then
        !TO DO natural_mort_adult seems to be a constant, does each grid need its own value?
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
        ! Load parameters for fishing selectivity 
        ! alpha = (1+exp(.1*(shell_lengths-70))).^-1)
        mortality(j)%alpha(1:num_size_classes) =  &
        &    1._dp  - 1._dp / ( 1._dp + exp( - (shell_lengths(1:num_size_classes) - length_0) /10._dp ) )

        mortality(j)%selectivity = Ring_Size_Selectivity(shell_lengths(1:num_size_classes), grid(j)%is_closed) 
        mortality(j)%selectivity_open = mortality(j)%selectivity * Logic_To_Double(.NOT. grid(j)%is_closed)
        mortality(j)%selectivity_closed = mortality(j)%selectivity * Logic_To_Double(grid(j)%is_closed)

        if (domain_name .eq. 'MA') then
            cull_size = ma_cull_size_mm
            discard = ma_discard
            this_grid_is_closed = .TRUE. ! always set discard to 0 if greater than cull size
        else
            cull_size = gb_cull_size_mm
            discard = gb_discard
            this_grid_is_closed = (grid(j)%is_closed)
        endif
        mortality(j)%discard(1:num_size_classes) = Set_Discard(shell_lengths(1:num_size_classes), &
        &             mortality(j)%selectivity(1:num_size_classes), cull_size, discard, this_grid_is_closed)
    enddo  ! num_grids

    !---------------------------------------------------
    ! Set fishing effort. NOTE Only used when fishing effort is determined by CAS
    !---------------------------------------------------
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
!! @public @memberof Mortality_Class
!> Open file given by fishing_mort_fname. Reads in the year and number of entries in the list. 
!> If the number of entries exceeds the number of areas loaded by the GridManager then show the error and stop.
!> Otherwise reads in the
!> vectors for area list indices and for fishing mortality
!==================================================================================================================
subroutine Load_Fishing_Mortalities()
    character(csv_line_len) input_str, sub_str
    integer j, k, n, io

    if (.not. use_spec_access_data) then
        write(*,*) term_yel, 'SPECIAL ACCESS FISHING MORT NOT IN USE', term_blk
        return
    endif

    write(*,*)  'OPENING ', fishing_mort_fname

    open(63, file=fishing_mort_fname, status='old')
    n = 0
    do
        read(63,'(a)',iostat=io) input_str
        if (io.lt.0) exit

        if (input_str(1:1) .ne. '#') then
            n = n+1

            sub_str = input_str
            j = len(input_str)

            read(sub_str,*) fmort_list(n)%year
            j = index(sub_str,',')
            sub_str = sub_str(j+1:)

            read(sub_str,*) fmort_list(n)%n_areas
            if (fmort_list(n)%n_areas > num_areas) then
                write(*,*) term_red, 'NUM OF AREAS VALUE TOO LARGE in', trim(fishing_mort_fname)
                write(*,'(A,A,I4,A,A)') ' EXPECTING', term_blk, num_areas, term_red, ' OR LESS'
                write(*,* ) trim(input_str), term_blk
                stop 1
            endif
            j = index(sub_str,',')
            sub_str = sub_str(j+1:)

            k = fmort_list(n)%n_areas
            read(sub_str,*) fmort_list(n)%area_list(1:k), fmort_list(n)%area_fish_mort(1:k)
        endif
    enddo
    close(63)
    write(*,*) term_blu, 'READ ', n, 'YEAR ENTRIES', term_blk
    num_in_list = n

endsubroutine Load_Fishing_Mortalities

!==================================================================================================================
!! @public @memberof Mortality_Class
!>
!> Purpose: Assign size class fishing selectivity based on increasing logistic function
!> @f[
!> Selectivity = \frac{1}{ 1 + exp(a - b * length_{shell})}
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
!! @public @memberof Mortality_Class
!>
!> Determines a real value of mortality due to fishing given a fishing type
!>
!> @param[in] year
!> @param[in] state matrix|num_grids by num_size classes| current state in scallops per square meter
!> @param[in] weight_grams matrix|num_grids by num_size classes|
!> @param[in] mortality vector(num_grids)
!> @results fishing mortality
!==================================================================================================================
function Set_Fishing_Effort(year, ts, state, weight_grams, mortality, grid)
    implicit none
    integer, intent(in):: year, ts
    real(dp), intent(in):: state(num_grids, num_size_classes), weight_grams(num_grids, num_size_classes )
    type(Mortality_Class), intent(in):: mortality(*)
    type(Grid_Data_Class), intent(in) :: grid(*)
    real(dp) :: Set_Fishing_Effort(num_grids)
    integer Mindx, loc
    real(dp) catch_open, catch_closed, total_catch
    real(dp) c_mort !constant for fishing mortality
    real(dp) f_avg

    !=============================================================
    ! these sums are over num_size_classes
    do loc = 1,num_grids
        ! dot_product(selectivity, state)
        expl_scallops_psqm(loc) = & 
        &    dot_product(mortality(loc)%selectivity(1:num_size_classes), state(loc,1:num_size_classes))

        ! dot_product(selectivity, state) * grid_area_sqm
        expl_num(loc) = expl_scallops_psqm(loc) * grid_area_sqm

        ! selectivity * state - at this location
        expl_scallops_psqm_at_size(1:num_size_classes) = mortality(loc)%selectivity(:) * state(loc, :) 
        ! dot_product(selectivity * state, weight)
        expl_biomass_gpsqm(loc) = dot_product(expl_scallops_psqm_at_size(1:num_size_classes), &
        &                                      weight_grams(loc,1:num_size_classes))

        ! Dollars_Per_SqM ultimately uses expl_scallops_psqm_at_size -> Scallops_To_Counts
        USD_per_sqm(loc) = Dollars_Per_SqM(year, weight_grams(loc, 1:num_size_classes))
    enddo

    F_mort_raw(:) = Set_Fishing_Mortality(grid(1:num_grids), year, .false., 0._dp)

    ! So for open areas, an overall fishing mortality F_avg would be specified and then F at each location
    ! would be computed so that:
    !  (1) The weighted (by exploitable numbers) average F over all locations is equal to F_avg and 
    !  (2) F at each location is proportional to LPUE^alpha. 
    ! This would also apply to special access areas, but each one would have their own specified F, 
    ! and the average would only be for those points within that access area.
    !f_avg = dot_product(expl_num, F_mort_raw) / sum(expl_num)
    
    !call  Calc_LPUE(expl_biomass_gpsqm, expl_scallops_psqm, lpue, dredge_time, dredge_area)
    lpue = Calc_LPUE(expl_biomass_gpsqm, expl_scallops_psqm)

    c_mort = fishing_mort * sum(expl_num) / dot_product(expl_num, lpue**alpha_mort) 
    F_mort(:) = c_mort * lpue(:)**alpha_mort
    F_mort(:) = Set_Fishing_Mortality(grid(1:num_grids), year, .true., F_mort(:))
    f_avg = dot_product(expl_num, F_mort) / sum(expl_num)

    do loc = 1, num_grids

        ! (1._dp - exp(-F * delta_time)) * state * grid_area_sqm  * selectivity
        landings_at_size(:) = (1._dp - exp(-F_mort(loc) * delta_time)) &
        &    * state(loc, 1:num_size_classes) * grid_area_sqm&
        &    * mortality(loc)%selectivity(1:num_size_classes)
        landings_at_size_open(:) = (1._dp - exp(-F_mort(loc)  * delta_time)) &
        &    * state(loc, 1:num_size_classes) * grid_area_sqm&
        &    * mortality(loc)%selectivity_open(1:num_size_classes)
        landings_at_size_closed(:) = (1._dp - exp(-F_mort(loc)  * delta_time)) &
        &    * state(loc, 1:num_size_classes) * grid_area_sqm&
        &    * mortality(loc)%selectivity_closed(1:num_size_classes)
    
        ! (1._dp - exp(-F * delta_time))
        ! * dot_product(selectivity * state * grid_area_sqm, weight)
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
    ! originally in metric tons, Set_Fishing_Effort_Weight_xx modified to accept grams
    total_catch = sum(landings_wgt_grams)
    catch_open = sum(landings_wgt_grams_open)
    catch_closed = sum(landings_wgt_grams_closed)
    !=============================================================

    select case (fishing_type(1:3))
        case('USD')
            fishing_effort (1:num_grids) = Set_Fishing_Effort_Weight_USD(grid(1:num_grids)%is_closed, &
            &                           total_catch, catch_open, catch_closed)
        case('BMS')
            fishing_effort(1:num_grids) = Set_Fishing_Effort_Weight_BMS(grid(1:num_grids)%is_closed, &
            &                           total_catch, catch_open, catch_closed)
        case('CAS')
            Mindx = minloc( abs(mortality(1)%year(1:mortality(1)%num_years) - year ), 1)
            fishing_effort(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
        case default
            Mindx = minloc( abs(mortality(1)%year(1:mortality(1)%num_years) - year ), 1)
            fishing_effort(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
            write(*,*)'Unkown fishing fishing_type:', fishing_type, &
            &    '.  Using spatially constant fishing_effort from CASA model'
    end select

    ! Report current timestep results
    call Mortality_Write_At_Timestep(year, ts, state, weight_grams, mortality, grid)
    Set_Fishing_Effort(:) = fishing_effort(:)

    return
endfunction Set_Fishing_Effort

!==================================================================================================================
!! @public @memberof Mortality_Class
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
    if ((year-ScallopPrice(indx, 1)) .NE. 0) then
        write(*,*) term_red, 'Scallop Price Year not found in Data/ScallopPrice.csv', year, term_blk
        stop 1
    endif

    ! Find total dollar amount of scallops
    Dollars_Per_SqM = cnt10 * ScallopPrice(indx, 2)+cnt10to20 * ScallopPrice(indx, 3)+&
                cnt20to30 * ScallopPrice(indx, 4)+cnt30plus * ScallopPrice(indx, 5)
    return
endfunction Dollars_Per_SqM

!==================================================================================================================
!! @public @memberof Mortality_Class
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
!! @public @memberof Mortality_Class
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
        tmp = dot_product(USD_per_sqm(:), expl_biomass_gpsqm(:)) * grid_area_sqm
        if (tmp.eq.0) tmp = 1._dp
        Set_Fishing_Effort_Weight_USD(:) = catch * USD_per_sqm(:) / tmp 
    else
        do j = 1, num_grids
            if(is_closed(j)) then
                total_dollars_closed = (dot_product(USD_per_sqm(:) &
                &   * Logic_To_Double(is_closed(1:num_grids)), expl_biomass_gpsqm(:))) * grid_area_sqm
                if (total_dollars_closed .eq. 0.) total_dollars_closed = 1.
                Cclosed = USD_per_sqm(j) * catch_closed / total_dollars_closed
                Set_Fishing_Effort_Weight_USD(j) = Cclosed
            else 
                total_dollars_open = (dot_product(USD_per_sqm(:) &
                &   * Logic_To_Double(.NOT. is_closed(1:num_grids)), expl_biomass_gpsqm(:))) * grid_area_sqm
                if (total_dollars_open .eq. 0.) total_dollars_open = 1.
                Copen = USD_per_sqm(j) * catch_open / total_dollars_open
                Set_Fishing_Effort_Weight_USD(j) = Copen 
            endif
        enddo
    endif
return
endfunction Set_Fishing_Effort_Weight_USD

!==================================================================================================================
!! @public @memberof Mortality_Class
!> Purpose: Set proportional rate of fishing mortality, fishing_effort, based on weight of catch.
!>
!> - expl_biomass_gpsqm(:) Member variable precomputed
!> - expl_scallops_psqm(:) Member variable precomputed
!> - rms = sum(expl_biomass_gpsqm()**2) / expl_scallops_psqm(n)
!> - Set_Fishing_Effort_Weight_BMS = (expl_biomass_gpsqm(:) * catch / expl_scallops_psqm(:) ) / (rms * grid_area_sqm)
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
    real(dp) rms

    if (domain_name .eq. 'MA') then
        do j = 1, num_grids
            rms = 0._dp
            do n = 1, num_grids
                if (expl_scallops_psqm(n).eq.0.0) then
                    ! if expl_scallops_psqm(n) is 0 then stands to reason that there would 
                    ! be zero biomass as well
                    ! rms = rms + 0, i.e. no change
                else
                    rms = rms + expl_biomass_gpsqm(n) * expl_biomass_gpsqm(n) / expl_scallops_psqm(n)
                endif
            enddo
            ! 
            if (expl_scallops_psqm(j) .EQ. 0._DP) then
                ! if expl_scallops_psqm(n) is 0 then stands to reason that there would 
                ! be zero biomass as well and thus no fishing effort.
                Set_Fishing_Effort_Weight_BMS(j) = 0._dp
            else
                rms = (expl_biomass_gpsqm(j) * catch / expl_scallops_psqm(j) ) / (rms  * grid_area_sqm)
                if (isnan(rms)) then
                    write(*,*) term_red, 'Set_Fishing_Effort_Weight_BMS FAILED; Divide by 0?', term_blk
                    STOP 1
                else
                    Set_Fishing_Effort_Weight_BMS(j) = rms
                endif
            endif
        enddo
    else
        exp_bms_open = sum( Logic_To_Double(.NOT. is_closed(1:num_grids)) * expl_biomass_gpsqm(:))
        exp_bms_closed = sum( Logic_To_Double(is_closed(1:num_grids)) * expl_biomass_gpsqm(:))
        if(exp_bms_open.eq.0.)exp_bms_open = 1.
        if(exp_bms_closed.eq.0.)exp_bms_closed = 1.

        do j = 1, num_grids
            if(.NOT. is_closed(j)) then
                Set_Fishing_Effort_Weight_BMS(j) = catch_open * expl_biomass_gpsqm(j) / (exp_bms_open * grid_area_sqm)
            else
                Set_Fishing_Effort_Weight_BMS(j) = catch_closed * expl_biomass_gpsqm(j) / (exp_bms_closed * grid_area_sqm)
            endif
        enddo
    endif

    return
endfunction Set_Fishing_Effort_Weight_BMS

!==================================================================================================================
!! @public @memberof Mortality_Class
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
!! @public @memberof Mortality_Class
!> Computes Fishing Mortality
!>
!> There is a year list for each year of interest, up to a total number of years of max_num_years
!> For each list item there are two vectors. 
!>   - The first vector is a list of special access by index.
!>   - The second vector is a list of corresponding fishing mortalities for that area
!> Thus, if the current simulation year is in the year list
!>   - Check if the grids
!>
!==================================================================================================================
elemental real(dp) function Set_Fishing_Mortality(grid, year, use_f_loc, f_loc)
type(Grid_Data_Class), intent(in) :: grid
integer, intent(in) :: year
logical, intent(in) :: use_f_loc
real(dp), intent(in) :: f_loc

integer index, area_idx, j
integer k

    ! set default value
    if (grid%is_closed) then
        Set_Fishing_Mortality = 0._dp
    else
        if (use_f_loc) then
            Set_Fishing_Mortality = f_loc
        else
            Set_Fishing_Mortality = fishing_mort
        endif
    endif

    !
    ! otherwise use value if location is in special access area
    !
    area_idx = grid%special_access_index
    ! if data has been loaded and grid is in a special access area
    if ((use_spec_access_data) .AND. (area_idx > 0)) then
        ! now check if special access is in list and use it's fishing mortality
        ! look for year
        do j = 1, num_in_list
            if( fmort_list(j)%year .eq. year) exit
        enddo

        if (j <= num_in_list) then
            ! found year, now look for special access
            ! grid is within a special access
            k = fmort_list(j)%n_areas
            index = findloc(fmort_list(j)%area_list(1:k), area_idx, 1)

            ! check if special access in in the list
            if (index > 0) then
                Set_Fishing_Mortality = fmort_list(j)%area_fish_mort(index)
            endif
        endif
    endif
endfunction Set_Fishing_Mortality

!-----------------------------------------------------------------------------------------------
!! @public @memberof Mortality_Class
!> Used during instantiation to set the name of the file to read to for configuration parameters
!> @brief Read Input File
!> 
!> Sets name of a configuration file, 'config_file_name.cfg'
!-----------------------------------------------------------------------------------------------
subroutine Set_Config_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    config_file_name = config_dir//fname
    inquire(file=config_file_name, exist=exists)

    if (exists) then
        PRINT *, term_blu, trim(config_file_name), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(config_file_name), ' NOT FOUND', term_blk
        stop 1
    endif
endsubroutine Set_Config_File_Name

subroutine Set_Fishing_Mort_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    use_spec_access_data =  ((fname(1:4) .ne. 'NONE') .and. (num_areas .ne. 0))

    if (use_spec_access_data) then
        fishing_mort_fname = config_dir//fname
        inquire(file=fishing_mort_fname, exist=exists)

        if (exists) then
            PRINT *, term_blu, trim(fishing_mort_fname), ' FOUND', term_blk
        else
            PRINT *, term_red, trim(fishing_mort_fname), ' NOT FOUND', term_blk
            stop 1
        endif
    else
        if (num_areas > 0) write(*,*) term_yel, 'YEARLY FISHING MORTALITY DATA FILE SET TO ', term_blk, '"NONE"',  &
        &                     term_yel, ' USING DEFAULT VALUES FOR FISH MORTALITY', term_blk
    endif
endsubroutine Set_Fishing_Mort_File_Name

!-----------------------------------------------------------------------
!! @public @memberof Mortality_Class
!> Read_Configuration
!> @brief Read Input File
!> 
!> Reads a configuration file, 'config_file_name.cfg', to set data parameters for Mortality
!>
!-----------------------------------------------------------------------
subroutine Read_Configuration()

    implicit none
    character(line_len) input_string
    character(tag_len) tag
    character(value_len) value
    integer j, k, io

    write(*,*) 'READING IN ', config_file_name

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
            if (k .EQ. 0) k = len(input_string) + 1
            value =  trim(adjustl(input_string(j+1:k-1)))

            select case(tag)
            case ('Fishing Mortality')
                read(value, *) fishing_mort

            case('Fishing')
                fishing_type = trim(adjustl(value))
                if (.not. ( any ((/ fishing_type.eq.'USD', fishing_type.eq.'BMS', fishing_type.eq.'CAS'/)) )) then
                    write(*,*) term_red, ' **** INVALID FISHING TYPE: ', fishing_type, term_blk
                    stop 1
                endif

            case ('Alpha Mortality')
                read(value, *) alpha_mort

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

            case('Fishing Mortality File')
                call Set_Fishing_Mort_File_Name(trim(adjustl(value)))

            case('LPUE Slope')
                read(value, *) lpue_slope

            case('LPUE Slope2')
                read(value, *) lpue_slope2

            case('LPUE Intercept')
                read(value, *) lpue_intercept

            case('Max Per Day')
                read(value, *) max_per_day

            case('Max Time')
                read(value, *) max_time_hpd

            case('Dredge Width')
                read(value, *) dredge_width_m

            case('Towing Speed')
                read(value, *) towing_speed_knots

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
!! @public @memberof Mortality_Class
!>
!> Initializes growth for startup
!>
!==================================================================================================================
subroutine Mortality_Write_At_Timestep(year, ts, state, weight_grams, mortality, grid)
integer, intent(in) :: year, ts
! state is allocated before the number of grids is known
real(dp), intent(in) :: state(num_grids, num_size_classes)
! weight_grams is allocated once the number of grids is known
real(dp), intent(in) :: weight_grams(num_grids, num_size_classes)
type(Mortality_Class), intent(in):: mortality(*)
type(Grid_Data_Class), intent(in) :: grid(*)

integer loc
! character(20) buf
real(dp) :: M(num_size_classes)
real(dp), allocatable :: abundance(:)
real(dp), allocatable :: bms(:)
real(dp), allocatable :: ebms_mt(:)
character(6) buf

allocate(abundance(num_grids))
allocate(bms(num_grids), ebms_mt(num_grids))

do loc = 1, num_grids
    ! Compute overall mortality
    M(1:num_size_classes) = mortality(loc)%natural_mortality(1:num_size_classes) &
    & + fishing_effort(loc) * ( mortality(loc)%selectivity(1:num_size_classes) &
    & + mortality(loc)%incidental + mortality(loc)%discard(1:num_size_classes) )

    abundance(loc) = sum(state(loc,1:num_size_classes)) ! scallops per sq meter
    bms(loc) = dot_product(state(loc,1:num_size_classes), weight_grams(loc,1:num_size_classes) / grams_per_metric_ton) ! metric tons per sq meter
enddo


ebms_mt(1:num_grids) = expl_biomass_gpsqm(1:num_grids)*grid_area_sqm/grams_per_metric_ton

! This data is on the survey grid
! (lat,lon) by ts
if (data_select%plot_ABUN) &
&    call Write_Column_CSV(num_grids, abundance(:), 'ABUN', &
&    output_dir//'Lat_Lon_Surv_ABUN_'//domain_name//'.csv',.true.)

if (data_select%plot_BMMT) &
&    call Write_Column_CSV(num_grids, bms(:), 'BMMT',&
&    output_dir//'Lat_Lon_Surv_BMMT_'//domain_name//'.csv',.true.)

if (data_select%plot_EBMS) &
&    call Write_Column_CSV(num_grids, ebms_mt(:), 'EBMS', &
&    output_dir//'Lat_Lon_Surv_EBMS_'//domain_name//'.csv',.true.)

if (data_select%plot_FEFF) &
&    call Write_Column_CSV(num_grids, fishing_effort(1:num_grids), 'Feffort',&
&    output_dir//'Lat_Lon_Surv_FEFF_'//domain_name//'.csv',.true.)

if (data_select%plot_FMOR) &
&    call Write_Column_CSV(num_grids, F_mort(1:num_grids), 'FMORT',&
&    output_dir//'Lat_Lon_Surv_FMOR_'//domain_name//'.csv',.true.)

if (data_select%plot_LAND) &
&    call Write_Column_CSV(num_grids, landings_by_num(:), 'Landings', &
&    output_dir//'Lat_Lon_Surv_LAND_'//domain_name//'.csv',.true.)

if (data_select%plot_LNDW) &
&    call Write_Column_CSV(num_grids, landings_wgt_grams(:), 'LNDW', &
&    output_dir//'Lat_Lon_Surv_LNDW_'//domain_name//'.csv',.true.)

if (data_select%plot_LPUE) &
&    call Write_Column_CSV(num_grids, lpue(:), 'LPUE',&
&    output_dir//'Lat_Lon_Surv_LPUE_'//domain_name//'.csv',.true.)

! write annual results, i.e. every ts_per_year
! This data is later interpolated to MA or GB Grid
if (mod(ts, ts_per_year) .eq. 1) then
    if (ts .eq. 1) then
        write(buf,'(I4,A2)') year, '_0'
    else
        write(buf,'(I4)') year
    endif

    if (save_by_stratum) then
        if (data_select%plot_ABUN) &
        &    call Write_Column_CSV_By_Stratum(num_grids, abundance(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'ABUN', &
        &    data_dir//'X_Y_ABUN_'//domain_name//trim(buf), .true.)

        if (data_select%plot_BMMT) &
        &    call Write_Column_CSV_By_Stratum(num_grids, bms(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'BMMT', &
        &    data_dir//'X_Y_BMMT_'//domain_name//trim(buf), .true.)

        if (data_select%plot_EBMS) &
        &    call Write_Column_CSV_By_Stratum(num_grids, ebms_mt(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'EBMS', &
        &    data_dir//'X_Y_EBMS_'//domain_name//trim(buf), .true.)

        if (data_select%plot_FEFF) &
        &    call Write_Column_CSV_By_Stratum(num_grids, fishing_effort(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'FEFF', &
        &    data_dir//'X_Y_FEFF_'//domain_name//trim(buf), .true.)

        if (data_select%plot_FMOR) &
        &    call Write_Column_CSV_By_Stratum(num_grids, F_mort(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'FMOR', &
        &    data_dir//'X_Y_FMOR_'//domain_name//trim(buf), .true.)

        if (data_select%plot_LAND) &
        &    call Write_Column_CSV_By_Stratum(num_grids, landings_by_num(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'LAND', &
        &    data_dir//'X_Y_LAND_'//domain_name//trim(buf), .true.)

        if (data_select%plot_LNDW) &
        &    call Write_Column_CSV_By_Stratum(num_grids, landings_wgt_grams(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'LNDW', &
        &    data_dir//'X_Y_LNDW_'//domain_name//trim(buf), .true.)

        if (data_select%plot_LPUE) &
        &    call Write_Column_CSV_By_Stratum(num_grids, lpue(:), grid(1:num_grids)%lat, grid(1:num_grids)%lon, &
        &    grid(1:num_grids)%stratum, 'LPUE', &
        &    data_dir//'X_Y_LPUE_'//domain_name//trim(buf), .true.)

    else
        if (data_select%plot_ABUN) &
        &    call Write_Column_CSV(num_grids, abundance(:),'ABUN',&
        &    data_dir//'X_Y_ABUN_'//domain_name//trim(buf)//'.csv', .true.)

        if (data_select%plot_BMMT) &
        &    call Write_Column_CSV(num_grids, bms(:),'BMMT',&
        &    data_dir//'X_Y_BMMT_'//domain_name//trim(buf)//'.csv', .true.)

        if (data_select%plot_EBMS) &
        &    call Write_Column_CSV(num_grids, ebms_mt(:), 'EBMS',&
        &    data_dir//'X_Y_EBMS_'//domain_name//trim(buf)//'.csv', .true.)

        if (data_select%plot_FEFF) &
        &    call Write_Column_CSV(num_grids, fishing_effort(1:num_grids), 'FEFF',&
        &    data_dir//'X_Y_FEFF_'//domain_name//trim(buf)//'.csv', .true.)

        if (data_select%plot_FMOR) &
        &    call Write_Column_CSV(num_grids, F_mort(1:num_grids), 'FMOR',&
        &    data_dir//'X_Y_FMOR_'//domain_name//trim(buf)//'.csv', .true.)

        if (data_select%plot_LAND) &
        &    call Write_Column_CSV(num_grids, landings_by_num(:), 'Landings', &
        &    data_dir//'X_Y_LAND_'//domain_name//trim(buf)//'.csv', .true.)

        if (data_select%plot_LNDW) &
        &    call Write_Column_CSV(num_grids, landings_wgt_grams(:), 'LNDW', &
        &    data_dir//'X_Y_LNDW_'//domain_name//trim(buf)//'.csv', .true.)

        if (data_select%plot_LPUE) &
        &    call Write_Column_CSV(num_grids, lpue(:), 'LPUE',&
        &    data_dir//'X_Y_LPUE_'//domain_name//trim(buf)//'.csv', .true.)

    endif
endif ! if(mod()) = 1
           
deallocate(abundance)
deallocate(bms, ebms_mt)
return
endsubroutine Mortality_Write_At_Timestep

!==================================================================================================================
!! @public @memberof Mortality_Class
!> Computes element of discard vector
!> @param[in] length, vector element for shell length
!> @parma[in] cull_size, determins shell length below which are discarded
!> @param[in] discard, percentage of selectivity that will be discarded
!> @param[in] selectivity, vector element that determines scallops harvested
!==================================================================================================================
elemental real(dp) function Set_Discard(length, selectivity, cull_size, discard, is_closed)
    real(dp), intent(in) :: length, cull_size, discard, selectivity
    logical, intent(in) :: is_closed
    if ((length .gt. cull_size) .and. is_closed) then
        Set_Discard = 0.D0
    else
        Set_Discard = discard * selectivity
    endif
endfunction Set_Discard

!==================================================================================================================
!! @public @memberof Mortality_Class
!> Computes catch as pounds per day
!> @parma[in]  expl_biomass          !  Expl biomass
!> @parma[in]  expl_scallops         !  Expl Number of Scallops
!> @param[out] dredge_time_hrs       !  dredge bottom time
!> @param[out] dredge_area_sqnm      !  area swept per day
!>
!>  EBiomass/ENumber = ESize
!>  Total Weight of a Tow / Number of scallops caught = mean weight of individual scallop
!>  expl_biomass_gpsqm(grid) * 4516 / expl_scallops_psqm(grid) * 4516 = expl_weight_g
!>                             xxxx                                     xxxx
!>
!==================================================================================================================
!elemental subroutine Calc_LPUE(expl_biomass, expl_scallops, lpue_ppd, dredge_time_hrs, dredge_area_sqnm)
elemental real(dp) function Calc_LPUE(expl_biomass, expl_scallops)
    real(dp), intent(in) ::  expl_biomass          !  expl biomass
    real(dp), intent(in) ::  expl_scallops         !  expl number of scallops
    ! real(dp), intent(out) :: lpue_ppd              !  catch rate in pounds per day
    ! real(dp), intent(out) :: dredge_time_hrs       !  dredge bottom time
    ! real(dp), intent(out) :: dredge_area_sqnm      !  area swept per day
    real(dp) lpue1_ppd, lpue_limit_ppd
    real(dp) expl_weight_g, expl_biomass_gptow

    if ( expl_scallops > 0._dp) then
        expl_weight_g = expl_biomass / expl_scallops    ! mean weight of individual expl scallop
    else
        expl_weight_g = 0._dp
    endif
    expl_biomass_gptow = expl_biomass * tow_area_sqm ! exploitable biomass in tow

    lpue1_ppd = min(lpue_slope2 * expl_biomass_gptow, lpue_slope * expl_biomass_gptow + lpue_intercept)  !piecewise linear relationship between exploitable biomass and lpue_ppd
    lpue_limit_ppd = max_per_day * expl_weight_g / grams_per_pound !Max weight of scallops that can be shucked in one day
    Calc_LPUE = min(lpue1_ppd, lpue_limit_ppd) !Calc_LPUE is constrained by shucking limit
    ! if (lpue1_ppd > 0._dp) then
    !     dredge_time_hrs = max_time_hpd * Calc_LPUE / lpue1_ppd  !Time dredge on bottom in hours
    ! else
    !     dredge_time_hrs = 0._dp
    ! endif
    ! dredge_area_sqnm = dredge_time_hrs * towing_speed_knots * dredge_width_m / meters_per_naut_mile  !Bottom area swept in nautical miles
endfunction Calc_LPUE

!==================================================================================================================
!! @public @memberof Mortality_Class
!>
!==================================================================================================================
function Get_Fishing_Type()
    character(3) Get_Fishing_Type
    Get_Fishing_Type = fishing_type
endfunction Get_Fishing_Type

end module Mortality_Mod
