!>----------------------------------------------------------------------------------------------------------------
!> @page page3 Mortality_Mod
!>
!> The methods in this class are used to determine the selectiviy and discard of the scallops based on shell
!> length and location.
!>
!> Set_Mortality
!>
!> Instantiates private members for this class. 
!>   - Reads in its configuration parameters and stores to private members.
!>   - Loads Fishing Mortalities, if enabled by GridManager
!>   - Sets up a repository for key values to allow offline analysis
!>   - Loads historical data for Fishing Effort
!>   - Set selectivity as computed by Ring_Size_Selectivity based on shell length and grid location
!>
!> @section p3p1 Read_Configuration
!> Opens the configuration file specified in the simulation configuration file and as set by @a Set_Config_File_Name
!>
!> @section p3p2 Load_Fishing_Mortalities
!> Opens the configuration file specified in the Mortality configuration file and as set by @a Set_Fishing_Mortality
!> 
!> @section p3p3 Set Mortality
!> Mortality = @f$Mortality_{adult}@f$ 
!>
!> @section p3p4 Compute Alpha
!> @f[
!> Alpha_{mortality} =  1.0  - \frac{1.0}{( 1.0 + exp( - (shell_{lengths}(:) – length_0) /10.0 ) )}
!> @f]
!> 
!> @section p3p5 Compute Selectivity
!> Assign size class fishing selectivity based on increasing logistic function
!> @f[
!> selectivity = \frac{1}{ 1 + exp(F_{sel_a} - F_{sel_b} * (shell_{len} + shell_{len_{delta}}/2.0))}
!> @f]
!>
!> @section p3p6 Compute Discard
!>
!> @f[
!> Discard = \begin{cases} 
!> 0.0, & \text{if } length > cullSize\text{ or gridIsClosed} \\
!> discard_{region} * selectivity,                                   & \text{otherwise}
!> \end{cases}
!> @f]
!>
!> @section p3p7 Mortality_Write_At_Timestep
!>
module Mortality_Mod
use globals
use Grid_Manager_Mod, only : Grid_Data_Class, Get_Num_Of_Areas
implicit none

!> @class Mortality_Class
!! 
!! Subroutines that determine expected mortality of scallops
type Mortality_Class
    !! Attrition due to natural mortality
    real(dp) natural_mortality(num_size_classes)
    real(dp) incidental
    real(dp) discard(num_size_classes)
    real(dp) selectivity(num_size_classes)
    real(dp) natural_mort_adult, natural_mort_juv
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
    logical plot_BIOM ! biomass in gpsm
    logical plot_EBMS ! exploitable biomass in metric tons
    logical plot_FEFF ! Fishing Effort
    logical plot_FMOR ! Fishing Mortality
    logical plot_LAND ! Landings by number of scallops
    logical plot_LNDW ! Landings by weight in grams
    logical plot_LPUE ! Landing Per Unit Effort, (per day)
    logical plot_RECR ! Recruitment in scallops per square meter
endtype DataForPlots

! @private @memberof Mortality_Class
character(fname_len), PRIVATE :: config_file_name
character(fname_len), PRIVATE :: fishing_mort_fname
type(FishingMortality), PRIVATE :: fmort_list(max_num_years)

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
real(dp), PRIVATE, allocatable :: expl_scallops_psqm(:)
real(dp), PRIVATE, allocatable :: F_mort(:)
real(dp), PRIVATE, allocatable :: landings_by_num(:) !TODO not really used for other computations
real(dp), PRIVATE, allocatable :: landings_wgt_grams(:)
real(dp), PRIVATE, allocatable :: lpue(:)
real(dp), PRIVATE, allocatable :: fishing_effort(:)
#ifdef ACCUM_LANDINGS
real(dp), PRIVATE, allocatable :: landings_accum(:)
real(dp), PRIVATE, allocatable :: landings_wgt_accum(:)
real(dp), PRIVATE, allocatable :: lpue_accum(:)
#endif

real(dp), PRIVATE :: expl_scallops_psqm_at_size(num_size_classes)
real(dp), PRIVATE :: landings_at_size(num_size_classes)

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

    deallocate(expl_scallops_psqm)
    deallocate(F_mort)

    deallocate(landings_by_num)
    deallocate(landings_wgt_grams)
    deallocate(lpue)
    deallocate(fishing_effort)
#ifdef ACCUM_LANDINGS
    deallocate(landings_accum, landings_wgt_accum, lpue_accum)
#endif
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
subroutine Set_Mortality(mortality, grid, shell_lengths, dom_name, dom_area, num_ts, ts_py, ngrids)
    implicit none
    
    type(Mortality_Class), intent(inout):: mortality(*)
    type(Grid_Data_Class), intent(in) :: grid(*)
    real(dp), intent(in):: shell_lengths(*)
    character(domain_len), intent(in) :: dom_name
    real(dp), intent(in) :: dom_area
    integer, intent(in) :: num_ts, ts_py
    integer, intent(in) :: ngrids
    integer j
    real(dp) length_0
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
    allocate(expl_scallops_psqm(num_grids))
    allocate(F_mort(num_grids))
    allocate(lpue(num_grids))
    allocate(fishing_effort(num_grids))

    allocate(landings_by_num(num_grids))
    allocate(landings_wgt_grams(num_grids))
#ifdef ACCUM_LANDINGS
    allocate(landings_wgt_accum(num_grids), lpue_accum(num_grids), landings_accum(num_grids))

    landings_accum(:) = 0.D0
    landings_wgt_accum(:) = 0.D0
    lpue_accum(:) = 0.D0
#endif


#ifdef ACCUM_LANDINGS
    write(*,*) term_yel, 'Mortality: Accumulating landing values', term_blk
#else
    write(*,*) term_yel, 'Mortality: Landing values at timestep', term_blk
#endif
    !---------------------------------------------------
    ! Set mortality parameters
    !---------------------------------------------------
    do j = 1, num_grids
        if (grid(j)%lon > ma_gb_border) then ! GB
            mortality(j)%natural_mort_adult = gb_mort_adult
            mortality(j)%incidental = gb_incidental
            length_0 = gb_length_0
            cull_size = gb_cull_size_mm
            discard = gb_discard
            this_grid_is_closed = (grid(j)%is_closed)
        else
            mortality(j)%natural_mort_adult = ma_mort_adult
            mortality(j)%incidental = ma_incidental
            length_0 = ma_length_0
            cull_size = ma_cull_size_mm
            discard = ma_discard
            this_grid_is_closed = .TRUE. ! always set discard to 0 if greater than cull size
        endif
    
        mortality(j)%natural_mortality(1:num_size_classes) = mortality(j)%natural_mort_adult
        ! Load parameters for fishing selectivity 
        ! alpha = (1+exp(.1*(shell_lengths-70))).^-1)
        mortality(j)%alpha(1:num_size_classes) =  &
        &    1._dp  - 1._dp / ( 1._dp + exp( - (shell_lengths(1:num_size_classes) - length_0) /10._dp ) )

        mortality(j)%selectivity = Ring_Size_Selectivity(shell_lengths(1:num_size_classes), grid(j)%is_closed, grid(j)%lon) 

        mortality(j)%discard(1:num_size_classes) = Set_Discard(shell_lengths(1:num_size_classes), &
        &             mortality(j)%selectivity(1:num_size_classes), cull_size, discard, this_grid_is_closed)
    enddo  ! num_grids   
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
elemental function Ring_Size_Selectivity(shell_length, is_closed, longitude)
    implicit none
    real(dp), intent(in)::shell_length, longitude
    logical, intent(in) :: is_closed
    real(dp) Ring_Size_Selectivity

    real(dp) select_a, select_b

    if (longitude > ma_gb_border) then ! GB
        if(is_closed) then
            select_a = gbc_fselect_a
            select_b = gbc_fselect_b
        else
            select_a = gbo_fselect_a
            select_b = gbo_fselect_b
        endif
    else
        select_a = ma_fselect_a
        select_b = ma_fselect_b
    endif
    Ring_Size_Selectivity = 1.D0 / ( 1.D0 + exp( select_a - select_b * (shell_length+shell_len_delta/2._dp)))

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
function Set_Fishing_Effort(year, ts, state_mat, weight_grams, mortality, grid)
    implicit none
    integer, intent(in):: year, ts
    real(dp), intent(in):: state_mat(1:num_grids, 1:num_size_classes), weight_grams(1:num_grids, 1:num_size_classes )
    type(Mortality_Class), intent(in):: mortality(*)
    type(Grid_Data_Class), intent(in) :: grid(*)
    real(dp) :: Set_Fishing_Effort(num_grids)
    integer loc, n
    real(dp) total_catch
    real(dp) c_mort !constant for fishing mortality
    real(dp) tmp

    !=============================================================
    ! these sums are over num_size_classes
    do loc = 1,num_grids
        ! dot_product(selectivity, state)
        expl_scallops_psqm(loc) = & 
        &    dot_product(mortality(loc)%selectivity(1:num_size_classes), state_mat(loc,1:num_size_classes))

        ! selectivity * state - at this location
        expl_scallops_psqm_at_size(1:num_size_classes) = mortality(loc)%selectivity(:) * state_mat(loc,1:num_size_classes) 
        ! dot_product(selectivity * state, weight)
        expl_biomass_gpsqm(loc) = dot_product(expl_scallops_psqm_at_size(1:num_size_classes), &
        &                                      weight_grams(loc,1:num_size_classes))
    enddo

    ! So for open areas, an overall fishing mortality F_avg would be specified and then F at each location
    ! would be computed so that:
    !  (1) The weighted (by exploitable numbers) average F over all locations is equal to F_avg and 
    !  (2) F at each location is proportional to LPUE^alpha. 
    ! This would also apply to special access areas, but each one would have their own specified F, 
    ! and the average would only be for those points within that access area.
    
    lpue = Calc_LPUE(expl_biomass_gpsqm, expl_scallops_psqm)
#ifdef ACCUM_LANDINGS
    lpue_accum(:) = lpue_accum(:) + lpue(:)
#endif

    c_mort = fishing_mort * sum(expl_biomass_gpsqm(:)) / dot_product(expl_biomass_gpsqm(:), lpue(:)**alpha_mort) 
    F_mort(:) = c_mort * lpue(:)**alpha_mort
    F_mort(:) = Set_Fishing_Mortality(grid(1:num_grids), year, .true., F_mort(:))

    do loc = 1, num_grids

        ! (1._dp - exp(-F * delta_time)) * state * grid_area_sqm  * selectivity
        landings_at_size(:) = (1._dp - exp(-F_mort(loc) * delta_time)) &
        &    * state_mat(loc, :) * grid_area_sqm * mortality(loc)%selectivity(:)
    
        ! (1._dp - exp(-F * delta_time))
        ! * dot_product(selectivity * state * grid_area_sqm, weight)
        landings_wgt_grams(loc) = dot_product(landings_at_size(:), weight_grams(loc,:))

        ! (1._dp - exp(-F * delta_time)) * selectivity * state * grid_area_sqm
        landings_by_num(loc) = sum(landings_at_size(:))

#ifdef ACCUM_LANDINGS
        landings_wgt_accum(loc) = landings_wgt_accum(loc) + landings_wgt_grams(loc)
        landings_accum(loc) = landings_accum(loc) + landings_by_num(loc)
#endif
    enddo

    ! Report current timestep results
    call Mortality_Write_At_Timestep(year, ts, state_mat, weight_grams, grid)

    do loc = 1, num_grids
        ! if expl_scallops_psqm(loc) is 0 then stands to reason that there would 
        ! be zero biomass as well and thus no fishing effort.
        if (grid(loc)%is_closed .OR. (expl_scallops_psqm(loc) .EQ. 0._dp)) then
            fishing_effort(loc) = 0._dp
        else
            tmp = 0._dp
            do n = 1, num_grids
                if (expl_scallops_psqm(n).eq.0.0) then
                    ! if expl_scallops_psqm(n) is 0 then stands to reason that there would 
                    ! be zero biomass as well
                    ! tmp = tmp + 0, i.e. no change
                else
                    tmp = tmp + expl_biomass_gpsqm(n) * expl_biomass_gpsqm(n) / expl_scallops_psqm(n)
                endif
            enddo
            ! 
            total_catch = sum(landings_wgt_grams)
            tmp = (expl_biomass_gpsqm(loc) * total_catch / expl_scallops_psqm(loc) ) / (tmp  * grid_area_sqm)
            if (isnan(tmp)) then
                write(*,*) term_red, 'Set_Fishing_Effort_Weight_BMS FAILED; Divide by 0?', term_blk
                STOP 1
            else
                fishing_effort(loc) = tmp
            endif
        endif
    enddo

    Set_Fishing_Effort(:) = fishing_effort(:)

    return
endfunction Set_Fishing_Effort

!==================================================================================================================
!! @public @memberof Mortality_Class
!>
!> Computes the total number of scallops, <b>S</b>, in millions. 
!> Then recomputes juvenile mortality as a function of S.
!> @f[
!> M_{juv} = \begin{cases} 
!>         exp(1.093 * log(S) - 9.701), & \text{if } S > 1400 \text{ million (2030?)} \\
!>         0.25,                                   & \text{otherwise}
!> \end{cases}
!> @f]
!> 
!> A similar formula for GB Open:
!> @f[
!> M_{juv} = \begin{cases} 
!>         exp((1.226*log(S)-10.49)), &  \text{if } S > 1400 \text{ million (2030?)} \\
!>         0.2,                                  & \text{otherwise}
!> \end{cases}
!> @f]
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
!> @param[in] state_vector  Current state_vector of scallop population in scallops/m^2
!> @returns natural_mortality and juvenile mortality
!>
!==================================================================================================================------------------------------
function Compute_Natural_Mortality(max_rec_ind, mortality, state_vector, longitude)
    implicit none
    integer, INTENT(IN):: max_rec_ind
    type(Mortality_Class), INTENT(INOUT):: mortality
    real(dp), intent(in) :: state_vector(*)
    real(dp), intent(in) :: longitude
    real(dp) Compute_Natural_Mortality(1:num_size_classes)
    real(dp) recruits

    ! Find the total sum of scallops per sq meter time region area yields total estimate of scallops, 
    ! recruits is the number of scallops in millions

    !!!!recruits = sum(state_vector(1:max_rec_ind)) * domain_area_sqm/(10.**6)
    ! Above equation causes state values to go negative resulting in negative output values
    ! Furthermore, state_vector() is scallop density by size at a given location, not the whole region
    recruits = sum(state_vector(1:max_rec_ind)) * grid_area_sqm/(10.**6)
    if (longitude > ma_gb_border) then ! GB
        if (recruits > 1400._dp) then
            mortality%natural_mort_juv = max( mortality%natural_mort_adult , exp(1.226_dp * log(recruits) - 10.49_dp ))
        else
            mortality%natural_mort_juv = 0.2_dp
        endif
    else
        if (recruits > 1400._dp) then
            mortality%natural_mort_juv = max( mortality%natural_mort_adult , exp(1.093_dp * log(recruits) - 9.701_dp) )
        else
            mortality%natural_mort_juv = 0.25_dp
        endif
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

    config_file_name = config_dir_sim//fname
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
        fishing_mort_fname = config_dir_special//fname
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
subroutine Mortality_Write_At_Timestep(year, ts, state_mat, weight_grams, grid)
integer, intent(in) :: year, ts
real(dp), intent(in) :: state_mat(1:num_grids, 1:num_size_classes)
! weight_grams is allocated once the number of grids is known
real(dp), intent(in) :: weight_grams(1:num_grids, 1:num_size_classes)
type(Grid_Data_Class), intent(in) :: grid(*)

integer loc
! character(20) buf
real(dp), allocatable :: abundance(:)
real(dp), allocatable :: bms(:)
real(dp), allocatable :: ebms_mt(:)
character(6) buf

allocate(abundance(num_grids))
allocate(bms(num_grids), ebms_mt(num_grids))

do loc = 1, num_grids
    abundance(loc) = sum(state_mat(loc,1:num_size_classes))*grid_area_sqm ! scallops
    bms(loc) = dot_product(state_mat(loc,1:num_size_classes), weight_grams(loc,1:num_size_classes) ) !/ grams_per_metric_ton) ! metric tons per sq meter
    ebms_mt(loc) = expl_biomass_gpsqm(loc)*grid_area_sqm/grams_per_metric_ton
enddo

! This data is on the survey grid
! (lat,lon) by ts
if (data_select%plot_ABUN) &
&    call Write_Column_CSV(num_grids, abundance(:), 'ABUN', &
&    output_dir//'Lat_Lon_Surv_ABUN_'//domain_name//'.csv',.true.)

if (data_select%plot_BIOM) &
&    call Write_Column_CSV(num_grids, bms(:), 'BIOM',&
&    output_dir//'Lat_Lon_Surv_BIOM_'//domain_name//'.csv',.true.)

if (data_select%plot_EBMS) &
&    call Write_Column_CSV(num_grids, ebms_mt(:), 'EBMS', &
&    output_dir//'Lat_Lon_Surv_EBMS_'//domain_name//'.csv',.true.)

if (data_select%plot_FEFF) &
&    call Write_Column_CSV(num_grids, fishing_effort(1:num_grids), 'Feffort',&
&    output_dir//'Lat_Lon_Surv_FEFF_'//domain_name//'.csv',.true.)

if (data_select%plot_FMOR) &
&    call Write_Column_CSV(num_grids, F_mort(1:num_grids), 'FMORT',&
&    output_dir//'Lat_Lon_Surv_FMOR_'//domain_name//'.csv',.true.)

! landings_accum or landings_by_num
! landings_wgt_accum or landings_wgt_grams
! lpue_accum or lpue
#ifdef ACCUM_LANDINGS
if (data_select%plot_LAND) &
&    call Write_Column_CSV(num_grids, landings_accum(:), 'Landings', & 
&    output_dir//'Lat_Lon_Surv_LAND_'//domain_name//'.csv',.true.)

if (data_select%plot_LNDW) & 
&    call Write_Column_CSV(num_grids, landings_wgt_accum(:), 'LNDW', & 
&    output_dir//'Lat_Lon_Surv_LNDW_'//domain_name//'.csv',.true.)

if (data_select%plot_LPUE) &
&    call Write_Column_CSV(num_grids, lpue_accum(:), 'LPUE',&          
&    output_dir//'Lat_Lon_Surv_LPUE_'//domain_name//'.csv',.true.)
#else
if (data_select%plot_LAND) &
&    call Write_Column_CSV(num_grids, landings_by_num(:), 'Landings', &
&    output_dir//'Lat_Lon_Surv_LAND_'//domain_name//'.csv',.true.)

if (data_select%plot_LNDW) & 
&    call Write_Column_CSV(num_grids, landings_wgt_grams(:), 'LNDW', &
&    output_dir//'Lat_Lon_Surv_LNDW_'//domain_name//'.csv',.true.)

if (data_select%plot_LPUE) &
&    call Write_Column_CSV(num_grids, lpue(:), 'LPUE',&
&    output_dir//'Lat_Lon_Surv_LPUE_'//domain_name//'.csv',.true.)
#endif
! write annual results, i.e. every ts_per_year
! This data is later interpolated to MA or GB Grid
if (mod(ts, ts_per_year) .eq. 1) then
    if (ts .eq. 1) then
        write(buf,'(I4,A2)') year ! this is the intial state
    else
        write(buf,'(I4)') year+1
    endif

    if (data_select%plot_ABUN) &
    &    call Write_Column_CSV_By_Region(num_grids, abundance(:), grid(1:num_grids)%lon, 'ABUN', &
    &    data_dir//'X_Y_ABUN_'//domain_name//trim(buf), .true.)

    if (data_select%plot_BIOM) &
    &    call Write_Column_CSV_By_Region(num_grids, bms(:), grid(1:num_grids)%lon, 'BIOM', &
    &    data_dir//'X_Y_BIOM_'//domain_name//trim(buf), .true.)

    if (data_select%plot_EBMS) &
    &    call Write_Column_CSV_By_Region(num_grids, ebms_mt(:), grid(1:num_grids)%lon, 'EBMS', &
    &    data_dir//'X_Y_EBMS_'//domain_name//trim(buf), .true.)

    if (data_select%plot_FEFF) &
    &    call Write_Column_CSV_By_Region(num_grids, fishing_effort(:), grid(1:num_grids)%lon, 'FEFF', &
    &    data_dir//'X_Y_FEFF_'//domain_name//trim(buf), .true.)

    if (data_select%plot_FMOR) &
    &    call Write_Column_CSV_By_Region(num_grids, F_mort(:), grid(1:num_grids)%lon, 'FMOR', &
    &    data_dir//'X_Y_FMOR_'//domain_name//trim(buf), .true.)

! landings_accum or landings_by_num
! landings_wgt_accum or landings_wgt_grams
! lpue_accum or lpue
#ifdef ACCUM_LANDINGS
    if (data_select%plot_LAND) then
        call Write_Column_CSV_By_Region(num_grids, landings_accum(:), grid(1:num_grids)%lon, 'LAND', &
        &    data_dir//'X_Y_LAND_'//domain_name//trim(buf), .true.)
        landings_accum(:) = 0.D0
    endif

    if (data_select%plot_LNDW) then
        call Write_Column_CSV_By_Region(num_grids, landings_wgt_accum(:), grid(1:num_grids)%lon, 'LNDW', &
        &    data_dir//'X_Y_LNDW_'//domain_name//trim(buf), .true.)
        landings_wgt_accum(:) = 0.D0
    endif

    if (data_select%plot_LPUE) then 
        call Write_Column_CSV_By_Region(num_grids, lpue_accum(:), grid(1:num_grids)%lon, 'LPUE', &
        &    data_dir//'X_Y_LPUE_'//domain_name//trim(buf), .true.)
        lpue_accum(:) = 0.D0
    endif
#else
    if (data_select%plot_LAND) &
    &    call Write_Column_CSV_By_Region(num_grids, landings_by_num(:), grid(1:num_grids)%lon, 'LAND', &
    &    data_dir//'X_Y_LAND_'//domain_name//trim(buf), .true.)

    if (data_select%plot_LNDW) &
    &    call Write_Column_CSV_By_Region(num_grids, landings_wgt_grams(:), grid(1:num_grids)%lon, 'LNDW', &
    &    data_dir//'X_Y_LNDW_'//domain_name//trim(buf), .true.)

    if (data_select%plot_LPUE) &
    &    call Write_Column_CSV_By_Region(num_grids, lpue(:), grid(1:num_grids)%lon, 'LPUE', &
    &    data_dir//'X_Y_LPUE_'//domain_name//trim(buf), .true.)
#endif

endif ! if(mod()) = 1
           
deallocate(abundance)
deallocate(bms, ebms_mt)
return
endsubroutine Mortality_Write_At_Timestep

!==================================================================================================================
!! @public @memberof Mortality_Class
!> Computes element of discard vector
!> @param[in] length, vector element for shell length
!> @param[in] cull_size, determins shell length below which are discarded
!> @param[in] discard, percentage of selectivity that will be discarded
!> @param[in] selectivity, vector element that determines scallops harvested
!==================================================================================================================
elemental real(dp) function Set_Discard(length, selectivity, cull_size, discard, is_closed)
    real(dp), intent(in) :: length, cull_size, discard, selectivity
    logical, intent(in) :: is_closed
    if ((length .gt. cull_size) .or. is_closed) then
        Set_Discard = 0.D0
    else
        Set_Discard = discard * selectivity
    endif
endfunction Set_Discard

!==================================================================================================================
!! @public @memberof Mortality_Class
!> Computes catch as pounds per day
!> @param[in]  expl_biomass          !  Expl biomass
!> @param[in]  expl_scallops         !  Expl Number of Scallops
!> @param[out] dredge_time_hrs       !  dredge bottom time
!> @param[out] dredge_area_sqnm      !  area swept per day
!>
!>  EBiomass/ENumber = ESize
!>  Total Weight of a Tow / Number of scallops caught = mean weight of individual scallop
!>  expl_biomass_gpsqm(grid) * 4516 / expl_scallops_psqm(grid) * 4516 = mean_expl_wght_g
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
    real(dp) mean_expl_wght_g, expl_biomass_g_in_tow

    if ( expl_scallops > 0._dp) then
        mean_expl_wght_g = expl_biomass / expl_scallops    ! mean weight of individual expl scallop
    else
        mean_expl_wght_g = 0._dp
    endif
    expl_biomass_g_in_tow = expl_biomass * tow_area_sqm ! exploitable biomass in tow

    lpue1_ppd = min(lpue_slope2 * expl_biomass_g_in_tow, lpue_slope * expl_biomass_g_in_tow + lpue_intercept)  !piecewise linear relationship between exploitable biomass and lpue_ppd
    lpue_limit_ppd = max_per_day * mean_expl_wght_g / grams_per_pound !Max weight of scallops that can be shucked in one day
    Calc_LPUE = min(lpue1_ppd, lpue_limit_ppd) !Calc_LPUE is constrained by shucking limit
    ! if (lpue1_ppd > 0._dp) then
    !     dredge_time_hrs = max_time_hpd * Calc_LPUE / lpue1_ppd  !Time dredge on bottom in hours
    ! else
    !     dredge_time_hrs = 0._dp
    ! endif
    ! dredge_area_sqnm = dredge_time_hrs * towing_speed_knots * dredge_width_m / meters_per_naut_mile  !Bottom area swept in nautical miles
endfunction Calc_LPUE

end module Mortality_Mod
