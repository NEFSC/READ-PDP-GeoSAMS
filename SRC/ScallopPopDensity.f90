PROGRAM ScallopPopDensity
!> @mainpage Scallop Population Density
!! This program is used to compute Scallop Density after a given growth period
!!
!! @section ms1 Initialize Simulation Parameters
!! @subsection ms1p1 Read Input
!! Values are read in from file name given on command line, e.g. ScallopPopDensity.exe @b Scallop.cfg
!!  - Maximum Number of Grids: Largest number of grids expected by intialization files
!!  - Domain Name: MA or GB
!!  - Beging Year: Four digit year, YYYY
!!  - Ending Year: Four digit year, YYYY
!!  - Fishing type: One of USD, BMS, or CAS
!!  - Time steps per Year: number of time steps each year
!!
!! The following are used to name configuration files used by other modules
!!  - Mortality Config File
!!  - Recruit Config File
!!  - Grid Manager Config File
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
!! That is (150 - 30) / 5 + 1, or 25 size classes
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
!!  - L30mm = (@f$L_{\infty_\mu}@f$(n) - 30) * exp(-@f$K_\mu@f$(n))
!!  - For each class, j
!!      - If (length(n) <= L30mm) recruit(n).max_rec_ind = j
!!
!! @subsection ms1p4 Instantiate Mortality
!! The simulation next instantiates how mortality is defined.
!!
!! <table>
!! <caption id="multi_row">Mortality</caption>
!! <tr><th>Region<th>Adult<th>Incidental<th>Base Length\n l<SUB>0</SUB>
!! <tr><td>MA<td>25%<td>5%<td>65.0
!! <tr><td>GB<td>20%<td>10%<td>70.0
!! </table>
!! @subsubsection ms1p4p1 Compute alpha
!! @f[
!! \alpha(l) = 1-\frac{1}{1+e^{- a( l/10.0-l_0 )}}
!! @f]
!!
!! @subsubsection ms1p4p2 Compute Fishing Effort
!!
!! This Fishing Effort is used by a CAS simulation. Fishing effort is defined by year and region 
!! from past history @a Data/FYrGBcGBoMA.csv
!! 
!! <table>
!! <caption id="multi_row">Fishing Effort</caption>
!! <tr><th>Year<th>GB Closed<th>GB Open<th>MA
!! <tr><td>2000<td>0.07<td>0.54<td>0.42
!! </table>
!!
!! @section ms2 Main Loop
!! 
!! @subsection ms2p1 For each time step
!!
!! @subsubsection mss2p1p1 Set Fishing Effort
!! Here there is defined a fishing effort that is independent of mortality. 
!! Whereas the mortality fishing effort is a function of region and historical data, 
!! this fishing effort is a function of cost, biomass or as a spatial constant within region. 
!!
!! @subsubsection mss2p1p2 For each grid
!!
!! @paragraph mss2p1p2p1 Compute natural mortality
!! Determine the number of scallops in millions, S, given the current state
!! @f[
!! S = state * domainArea
!! @f]
!! This is used to determine the juvenile mortality. Adult mortality was defined at module instantiation.
!!
!! Mid-Atlantic:
!! @f[
!! M_{juv} = \begin{cases} 
!!         e^{1.093 * log(S) - 9.701}, & \text{if } S > 1400 \text{ million} \\
!!         M_{adult},                                   & \text{otherwise}
!! \end{cases}
!! @f]
!! 
!! Georges Bank:
!! @f[
!! M_{juv} = \begin{cases} 
!!         e^{(1.226*log(S)-10.49)}, &  \text{if } S > 1400 \text{ million} \\
!!         M_{adult},                                  & \text{otherwise}
!! \end{cases}
!! @f]
!! where @f$M_{adult}@f$ is 0.25 if MA or 0.2 if GB\n
!! Finally
!! @f[
!! M_{nat} = \alpha * M_{juv} + (1-\alpha) M_{adult}
!! @f]
!! 
!! @paragraph mss2p1p2p2 Adjust population state based on von Bertalanffy growth
!! @f[
!! \vec{S} = \left| G \right| \times \vec{S} 
!! @f]
!!
!! @paragraph mss2p1p2p3 Compute increase in population due to recruitment, R
!! If within recruitment period, i.e. Jan 1st to April 10th
!! @f[
!! \vec{S} = \vec{S} + \delta_t\frac{\vec{R}}{RecruitDuration}
!! @f]
!! @paragraph mss2p1p2p4 Compute Overall Mortality
!! @f[
!! \vec{M} = \vec{M}_{nat} + Fishing *( \vec{M}_{selectivity} + \vec{M}_{incidental} + \vec{M}_{discard})
!! @f]
!! @paragraph mss2p1p2p5 Compute effect of mortality to arrive at new state
!! @f[
!! \vec{S}_{t+1} = \vec{S}_t * (1- \delta_t * \vec{M})
!! @f]
!----------------------------------------------------------------------------

use globals
use Growth_Mod
use Grid_Manager_Mod
use Recruit_Mod
use Mortality_Mod

implicit none

integer n !> loop count
logical exists

integer max_num_grids
character(2) domain_name
integer start_year, stop_year
character(3) fishing_type      !> Fishing can be USD,  BMS,  or,  CAS
integer num_time_steps
integer ts_per_year
real(dp) delta_time
integer num_years
integer year
character(fname_len) file_name

real(dp) domain_area
integer num_grids, ts

real(dp) :: shell_length_mm(num_size_classes)

type(Grid_Data_Class), allocatable :: grid(:)
real(dp), allocatable :: state(:, :)

type(Growth_Class), allocatable :: growth(:)
type(Mortality_Class), allocatable :: mortality(:)
type(Recruitment_Class), allocatable :: recruit(:)
real(dp), allocatable :: weight_grams(:,:)
real(dp), allocatable :: fishing_effort(:) ! rate of fishing mortality

character(fname_len) :: arg
integer pct_comp

call get_command_argument(1, arg)
file_name = config_dir//trim(arg)
if (len_trim(file_name) == 0) then
    write(*,*) term_red, 'No configuration file', term_blk
    stop
endif
inquire(file=file_name, exist=exists)

if (exists) then
    PRINT *, term_blu, trim(file_name), ' FOUND', term_blk
else
    PRINT *, term_red, trim(file_name), ' NOT FOUND', term_blk
    stop
endif

!==================================================================================================================
!  - I. Read Configuration file 'Scallop.inp'
!==================================================================================================================
call Read_Startup_Config(max_num_grids, domain_name, file_name, start_year, stop_year, fishing_type, ts_per_year)

! time parameters
num_years = stop_year - start_year + 1
num_time_steps = num_years * ts_per_year + 1 ! +1 to capture last data from last step
delta_time = 1._dp / dfloat(ts_per_year)

! Force correct region
! Data/bin5mm2005MA.csv
file_name(16:17) = domain_name

write(*,*) '========================================================'
write(*,'(A,I6)') ' Max Expected #grids ', max_num_grids
write(*,'(A,A6)') ' Domain:             ', domain_name
write(*,'(A,I6)') ' Start Year:         ', start_year
write(*,'(A,I6)') ' Stop Year:          ', stop_year
write(*,'(A,A6)') ' Fishing Type:       ', fishing_type
write(*,'(A,I6,A,F7.4)') ' Time steps/year:', ts_per_year, ' delta ', delta_time
write(*,'(A,I6,A,F7.4)') ' Total number time steps:', num_time_steps
write(*,*) '========================================================'

! allow for maximum expected number of grids
allocate(grid(1:max_num_grids))
allocate(state(1:max_num_grids,1:num_size_classes))

!==================================================================================================================
!  - II. Instantiate mods, that is objects
!==================================================================================================================
call Set_Grid_Manager(max_num_grids, state, grid, num_grids)

! number of grids known, allocated remaining data
allocate(growth(1:num_grids), mortality(1:num_grids), recruit(1:num_grids) )
allocate(weight_grams(1:num_grids,1:num_size_classes))
allocate(fishing_effort(1:num_grids))

call Set_Growth(growth, grid, shell_length_mm, num_time_steps, ts_per_year, domain_name, domain_area,&
&              state, weight_grams, num_grids, max_num_grids)
call Set_Recruitment(recruit, num_grids, domain_name, domain_area, &
&                    growth(1:num_grids)%L_inf_mu, growth(1:num_grids)%K_mu, shell_length_mm)
call Set_Mortality(mortality, grid, shell_length_mm, domain_name, domain_area, num_time_steps, &
&                    ts_per_year, num_grids, max_num_grids)

!==================================================================================================================
!  - III. MAIN LOOP
! Start simulation
!==================================================================================================================

! write latitude and longitude out for later use by Matlab Geographic Scatter Plot ------------------------------
call Write_Column_CSV(num_grids, grid(:)%lat, output_dir//'X_Y_EBMS_'//domain_name//'.csv',.false.)
call Write_Column_CSV(num_grids, grid(:)%lon, output_dir//'X_Y_EBMS_'//domain_name//'.csv',.true.)
call Write_Column_CSV(num_grids, grid(:)%lat, output_dir//'X_Y_Feffort_'//domain_name//'.csv',.false.)
call Write_Column_CSV(num_grids, grid(:)%lon, output_dir//'X_Y_Feffort_'//domain_name//'.csv',.true.)
!----------------------------------------------------------------------------------------------------------------
year = start_year
do ts = 1, num_time_steps
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    !  i. Determine fishing effort
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    fishing_effort(1:num_grids) = Set_Fishing(fishing_type, year, ts-1, state, weight_grams, mortality, grid)

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    !  ii. For each grid, evaluate growth state for this time step
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    do n = 1, num_grids
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        !  a. Compute new state
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        state(n, 1:num_size_classes) = Time_To_Grow(ts, growth(n), mortality(n), recruit(n), &
        &                         state(n, 1:num_size_classes), fishing_effort(n), year)
    enddo ! n = 1, num_grids

    ! finished with time_steps_year, increment year
    if ((mod(ts, ts_per_year) .eq. 1) .and. (ts > 1))then
        year = year + 1
    endif

    pct_comp = 100* ts * num_grids/(num_time_steps*num_grids)
    if (mod(pct_comp, 10) .eq. 0) write(*,*) term_grn, '% comp', pct_comp, term_blk

enddo ! ts = 1, num_time_steps

deallocate(grid, growth, mortality, recruit, state, weight_grams, fishing_effort)
call Destructor()
END PROGRAM ScallopPopDensity

!-----------------------------------------------------------------------
!! Read_Startup_Config
!> @brief Read Input File
!> 
!> Reads a configuration file, 'Scallop.inp', to set data parameters for simulation
!>
!> 
!> @param[out] domain_name can be either 
!>             MA MidAtlantic or 
!>             GB GeorgesBank
!> @param[out] init_cond_file_name File name that contains intial simulation conditions
!> @param[out] start_year Starting year for simulation read from config file
!> @param[out] stop_year  End year for simulation read from config file
!> @param[out] fishing_type Fishing can be USD, BMS, or, CAS
!> @param[out] time_steps_per_year Number of times steps to evaluate growth
!> @param[out] num_monte_carlo_iter Number of iterations for Monte Carlo simulation
!-----------------------------------------------------------------------
subroutine Read_Startup_Config(max_num_grids, domain_name, file_name, start_year, stop_year, fishing_type,time_steps_per_year)
    use globals
    use Mortality_Mod, only : Mortality_Set_Config_File_Name => Set_Config_File_Name
    use Recruit_Mod, only : Recruit_Set_Config_File_Name => Set_Config_File_Name
    use Grid_Manager_Mod, only : GridMgr_Set_Config_File_Name => Set_Config_File_Name

    implicit none
    integer, intent(out) :: max_num_grids
    character(2),intent(out):: domain_name
    character(*),intent(in):: file_name
    character(3),intent(out):: fishing_type
    integer, intent(out) :: start_year, stop_year, time_steps_per_year ! , num_monte_carlo_iter

    integer j, k, io
    character(line_len) input_string
    character(tag_len) tag
    character(value_len) value

    open(read_dev,file=file_name)
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

            select case (tag)
            case('Domain Name')
                domain_name = trim(adjustl(value))
                if (.not. ( any ((/ domain_name.eq.'MA', domain_name.eq.'GB'/)) )) then
                    write(*,*) term_red, ' **** INVALID DOMAIN NAME: ', domain_name, term_blk
                    stop
                endif

            case('Beginning Year')
                read(value,*) start_year

            case('Ending Year')
                read(value,*) stop_year

            case('Time steps per Year')
                read(value,*) time_steps_per_year

            case('Fishing')
                fishing_type = trim(adjustl(value))
                if (.not. ( any ((/ fishing_type.eq.'USD', fishing_type.eq.'BMS', fishing_type.eq.'CAS'/)) )) then
                    write(*,*) term_red, ' **** INVALID FISHING TYPE: ', fishing_type, term_blk
                    stop
                endif

            case('Grid Manager Config File')
                call GridMgr_Set_Config_File_Name(trim(adjustl(value)))

            case('Mortality Config File')
                call Mortality_Set_Config_File_Name(trim(adjustl(value)))

            case('Recruit Config File')
                call Recruit_Set_Config_File_Name(trim(adjustl(value)))

            case('Max Number of Grids')
                read(value,*) max_num_grids

            case default
                write(*,*) term_red, 'Unrecognized line in ',file_name
                write(*,*) 'Unknown Line-> ',input_string, term_blk
                stop
            end select
        endif
    end do
    close(read_dev)
    return
end subroutine Read_Startup_Config
