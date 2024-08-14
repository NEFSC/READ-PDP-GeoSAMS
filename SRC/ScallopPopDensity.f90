PROGRAM ScallopPopDensity
!> @mainpage Scallop Population Density
!!
!! This program is used to compute Scallop Density after a given growth period
!!
!! @section ms1 Initialize Simulation Parameters
!! @subsection ms1p1 Read Input
!! Values are read in from file name given on command line, e.g. ScallopPopDensity.exe @b Scallop.cfg
!!  - Time steps per Year: number of time steps each year
!!  - Save By Stratum: Used in GB to make break up region into smoother shapes, i.e. rather than clover leaf.
!!
!! The following are used to name configuration files used by other modules
!!  - Mortality Config File
!!  - Recruit Config File
!!  - Grid Manager Config File
!!
!! Additional parameters are placed on the command line to facilitate batch processing
!! - Start Year
!! - Stop Year
!! - Domain Abbreviation
!!   - MA, or
!!   - GB
!!
!! @subsection ms1p2 Instantiate Growth Module
!! The simulation then instantiates parameters that define how growth occurs
!!
!! @subsubsection ms1p2p1 Load Grid and Initial State
!! The initial state is defined by a hardcode data file named as follows:
!! - Data/bin5mmYYYY[MA|GB].csv\n
!!  where the year, YYYY, is defined by the Start Year and MA or GB is specified by the given domain name.
!! The data in each file, Data/bin5mmYYYY[MA|GB].csv has grid information for where each grid is located and its depth. 
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
!! ShellToWeight = exp( &-& 6.69 + 2.878 * log(shell_{length}) \\
!!                      &-& 0.0073 * depth - 0.073 * latitude \\
!!                      &+& (1.28 - 0.25 * log(shell_{length}) * isClosed )
!! @f}
!!
!!
!! MA\n
!! @f{eqnarray*}{
!! ShellToWeight = exp( &-& 9.713394 + 2.62025 * log(shell_{length}) \\
!!                      &-& 0.004665 * depth + 0.021 * latitude \\
!!                      &-& 0.031 * isClosed)
!! @f}
!!
!! where @a isClosed is 1 if closed or 0 if open
!!
!! @subsubsection ms1p2p3 Compute Growth Parameters, given depth, latitude, and isClosed
!! - @f$L_{\infty_\mu}@f$
!! - @f$L_{\infty_\sigma}@f$
!! - @f$K_\mu@f$
!! - @f$K_\sigma@f$
!!
!! @subsubsection ms1p2p4 Compute G matrix for given growth parameters
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
!! @paragraph ms1p3p1p1 For years start_year to stop_year
!! Data is read in from RecruitEstimates/RecruitEstimateDNYYYY.txt
!!
!! @subsubsection ms1p3p2 This method is effectively setting
!! For year in start_year to stop_year
!!  - Year_index = year - start_year + 1
!!  - for year_index in [1..max]\n
!!      -recruitment(year_index) = RecruitEstimate
!!      - year(year_index) = year
!!      - rec_start = Start Period, typically 0/365, or January 1st
!!      - rec_stop = Stop Period, typically 100/365, or April 10
!!
!! @subsubsection ms1p3p3 It then quantizes recruitment,
!! For each grid, n
!!  - L30mm = (@f$L_{\infty_\mu}@f$(n) - 30) * exp(-@f$K_\mu@f$(n))
!!  - For each class, j
!!      - If (length(n) <= L30mm) recruit(n).max_rec_ind = j
!!
!! @subsection ms1p4 Instantiate Mortality
!! The simulation next sets mortality values.
!!
!! <table>
!! <caption id="multi_row">Mortality</caption>
!! <tr><th>Region<th>Adult<th>Incidental<th>Base Length\n l<SUB>0</SUB>
!! <tr><td>MA<td>25%<td>5%<td>65.0
!! <tr><td>GB<td>20%<td>10%<td>70.0
!! </table>
!! @subsubsection ms1p4p1 Compute alpha
!! @f[
!! \alpha(l) = 1-\frac{1}{1+e^{-(l-l_0)/10.0}}
!! @f]
!!
!! @subsubsection ms1p4p2 Compute Fishing Effort
!!
!! @paragraph ms1p4p2p1 Compute landings at size for each grid location
!!
!! Given
!! The number of scallops per square meter:
!! @f[
!! \vec{scallops} = \vec{selectivity_{loc}} \cdot \vec{state_{loc}}
!! @f]
!! and the exploitable biomass in grams per square meter
!! @f[
!! EBMS_{loc} = \vec{scallops} \cdot \vec{weight_{loc}}
!! @f]
!!
!! @f[
!! \vec{landings_{size}} = (1.0 - e^{(-F_{{mort}_{loc}} * \delta_))} * \vec{state_{loc}} * gridArea * \vec{selectivity_{loc}}
!! @f]
!!
!! @paragraph ms1p4p2p2 Compute landings by weight
!!
!! @f[
!! \vec{landings_{wgt}} = \vec{landings_{size}} \cdot \vec{weight} = catch
!! @f]
!! This is also considered total_catch
!!
!! @paragraph ms1p4p2p3 Total catch is used compute fishing effort
!!
!! @f[
!! rms = \sum_{loc=1}^{n} \frac{EBMS(loc)^2}{scallops(loc))}
!! @f]
!!
!! @f[
!! FishingEffort = \frac{ \vec{EBMS} * catch / \vec{scallops}}{rms * gridArea}
!! @f]
!!
!! @section ms2 Main Loop
!! 
!! @subsection ms2p1 For each time step
!!
!! @subsubsection mss2p1p1 Set Fishing Effort
!!
!! Here there is defined a fishing effort that is independent of mortality. 
!! Whereas the mortality fishing effort is a function of region and historical data, 
!! this fishing effort is a function of cost, biomass or as a spatial constant within region. 
!!
!! @subsection ms2p2 For each grid
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
!!         exp(1.093 * log(S) - 9.701), & \text{if } S > 1400 \text{ million} \\
!!         0.25,                                   & \text{otherwise}
!! \end{cases}
!! @f]
!! 
!! Georges Bank:
!! @f[
!! M_{juv} = \begin{cases} 
!!         exp((1.226*log(S)-10.49)), &  \text{if } S > 1400 \text{ million} \\
!!         0.2,                                  & \text{otherwise}
!! \end{cases}
!! @f]
!!
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

character(domain_len) domain_name
integer start_year, stop_year
integer recruit_yr_strt, recruit_yr_stop
integer num_time_steps
integer ts_per_year
integer num_years
integer year
character(4) buf
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

type(DataForPlots) plot_data_sel

integer pct_comp

!==================================================================================================================
!  - I. Read Configuration file 'Scallop.inp'
!==================================================================================================================
call Read_Startup_Config(ts_per_year, start_year, stop_year, domain_name, plot_data_sel)

!==================================================================================================================

! time parameters
num_years = stop_year - start_year + 1
num_time_steps = num_years * ts_per_year + 1 ! +1 to capture last data from last step

!==================================================================================================================
!  - II. Instantiate mods, that is objects
!==================================================================================================================
! Setting initial conditions file name
! Force year and domain name given values in scallop configuration file
! Data/bin5mmYYYYDN.csv
write(buf,'(I4)') start_year
file_name = 'Data/bin5mm'//buf//domain_name//'.csv'
call Set_Init_Cond_File_Name(file_name)

! First read to see how many records are defined. Read again later in GridManager::Load_Grid_State
num_grids = Set_Num_Grids()
allocate(grid(1:num_grids))
allocate(state(1:num_grids,1:num_size_classes))

call Set_Grid_Manager(state, grid, num_grids, domain_name)

! number of grids known, allocated remaining data
allocate(growth(1:num_grids), mortality(1:num_grids), recruit(1:num_grids) )
allocate(weight_grams(1:num_grids,1:num_size_classes))
allocate(fishing_effort(1:num_grids))

call Set_Growth(growth, grid, shell_length_mm, num_time_steps, ts_per_year, domain_name, domain_area,&
&              state, weight_grams, num_grids)
call Set_Recruitment(recruit, num_grids, domain_name, domain_area, recruit_yr_strt, recruit_yr_stop,&
&                    growth(1:num_grids)%L_inf_mu, growth(1:num_grids)%K_mu, shell_length_mm, start_year, stop_year)
call Set_Mortality(mortality, grid, shell_length_mm, domain_name, domain_area, num_time_steps, &
&                    ts_per_year, num_grids)

write(*,*) '========================================================'
write(*,'(A,I6)') ' Working with #grids ', num_grids
write(*,'(A,A6)') ' Domain:             ', domain_name
write(*,'(A,I6)') ' Start Year:         ', start_year
write(*,'(A,I6)') ' Stop Year:          ', stop_year
write(*,'(A,I6,A,F7.4)') ' Time steps/year:', ts_per_year
write(*,'(A,I6,A,F7.4)') ' Total number time steps:', num_time_steps
write(*,'(A,I6)') ' Recruit Start Year: ', recruit_yr_strt
write(*,'(A,I6)') ' Recruit Stop Year:  ', recruit_yr_stop
write(*,*) '========================================================'


!==================================================================================================================
!  - III. MAIN LOOP
! Start simulation
!==================================================================================================================
call Setup_Data_Files(plot_data_sel, num_grids, grid, domain_name, start_year, stop_year)

year = start_year
do ts = 1, num_time_steps
    if (plot_data_sel%plot_RECR) call Write_Recruit_Estimates(ts, ts_per_year, num_grids, grid, domain_name, &
    &    year, start_year, recruit)
    
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    !  i. Determine fishing effort
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    fishing_effort(1:num_grids) = Set_Fishing_Effort(year, ts, state, weight_grams, mortality, grid)

    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    !  ii. For each grid, evaluate growth state for this time step
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    do n = 1, num_grids
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        !  a. Compute new state
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        state(n, 1:num_size_classes) = Time_To_Grow(ts, growth(n), mortality(n), recruit(n), &
        &                         state(n, 1:num_size_classes), fishing_effort(n), year, grid(n)%lon)
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
stop 0
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
!> @param[out] time_steps_per_year Number of times steps to evaluate growth
!> @param[out] num_monte_carlo_iter Number of iterations for Monte Carlo simulation
!-----------------------------------------------------------------------
subroutine Read_Startup_Config(time_steps_per_year, start_year, stop_year,&
    & domain_name, plot_data_sel)
    use globals
    use Mortality_Mod, only : Mortality_Set_Config_File_Name => Set_Config_File_Name, DataForPlots, Set_Select_Data
    use Recruit_Mod, only : Recruit_Set_Config_File_Name => Set_Config_File_Name
    use Grid_Manager_Mod, only : GridMgr_Set_Config_File_Name => Set_Config_File_Name

    implicit none
    integer, intent(out) :: time_steps_per_year ! , num_monte_carlo_iter
    integer, intent(out) :: start_year, stop_year
    character(domain_len), intent(out) :: domain_name
    type(DataForPlots), intent(out) :: plot_data_sel

    integer j, k, io
    character(line_len) input_string
    character(tag_len) tag
    character(value_len) value

    integer ncla
    logical exists
    character(fname_len) file_name
    character(fname_len) arg

    ! default values
    time_steps_per_year = 13
    plot_data_sel = DataForPlots(.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.)

    !----------------------------------------------------------------------------
    ! First consider command line arguments
    !----------------------------------------------------------------------------
    ncla=command_argument_count()
    if (ncla .lt. 4) then
        write(*,*) term_red, 'Missing parameters', term_blk
        call get_command(arg)
        write(*,*) term_blu,'Typical use: $ ', term_yel, &
        &    '.\SRC\ScallopPopDensity.exe Scallop.cfg StartYear StopYear Domain' , term_blk
        stop 1
    endif

    call get_command_argument(1, arg)
    file_name = config_dir_sim//trim(arg)
    inquire(file=file_name, exist=exists)
    if (exists) then
        PRINT *, term_blu, trim(file_name), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(file_name), ' NOT FOUND', term_blk
        stop 1
    endif

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
            case('Time steps per Year')
                read(value,*) time_steps_per_year

            case('Grid Manager Config File')
                call GridMgr_Set_Config_File_Name(trim(adjustl(value)))

            case('Mortality Config File')
                call Mortality_Set_Config_File_Name(trim(adjustl(value)))

            case('Recruit Config File')
                call Recruit_Set_Config_File_Name(trim(adjustl(value)))

            case('Select Abundance')
                plot_data_sel%plot_ABUN = .true.
            case('Select BMS')
                plot_data_sel%plot_BMMT = .true.
            case('Select Expl BMS')
                plot_data_sel%plot_EBMS = .true.
            case('Select Fishing Effort')
                plot_data_sel%plot_FEFF = .true.
            case('Select Fishing Mortality')
                plot_data_sel%plot_FMOR = .true.
            case('Select Landings by Number')
                plot_data_sel%plot_LAND = .true.
            case('Select Landings by Weight')
                plot_data_sel%plot_LNDW = .true.
            case('Select LPUE')
                plot_data_sel%plot_LPUE = .true.
            case('Select RECR')
                plot_data_sel%plot_RECR = .true.

            case default
                write(*,*) term_red, 'Unrecognized line in ',file_name
                write(*,*) 'Unknown Line-> ',input_string, term_blk
                stop 1
            end select
        endif
    end do
    close(read_dev)

    ! setter private variable for Mortality
    call Set_Select_Data(plot_data_sel)

    ! override configuration file parameters
    ! to aid batch processing, get remaining parameters from command line
    call get_command_argument(2, arg)
    read(arg,*) start_year
    
    call get_command_argument(3, arg)
    read(arg,*) stop_year
    
    call get_command_argument(4, domain_name)
    if (.not. ( any ((/ domain_name.eq.'MA', domain_name.eq.'GB', domain_name.eq.'AL'/)) )) then
        write(*,*) term_red, ' **** INVALID DOMAIN NAME: ', domain_name, term_blk
        stop 1
    endif
    
    return
end subroutine Read_Startup_Config

!--------------------------------------------------------------------------------
!! Write_Lat_Lon_Preamble
!> @brief Writes lat and lon columns with headers to named file
!--------------------------------------------------------------------------------
subroutine Write_Lat_Lon_Preamble(num_grids, grid, fname)
use globals
use Grid_Manager_Mod
integer, intent(in) :: num_grids
type(Grid_Data_Class), intent(in) :: grid(*)
character(*), intent(in) :: fname

call Write_Column_CSV(num_grids, grid(1:num_grids)%lat, 'LAT', fname,.false.)
call Write_Column_CSV(num_grids, grid(1:num_grids)%lon, 'LON', fname,.true.)

endsubroutine Write_Lat_Lon_Preamble

!--------------------------------------------------------------------------------
!! Write_X_Y_Preamble
!> @brief Writes year, UTM-X, UTM-Y, and Depth columns with headers to named file
!--------------------------------------------------------------------------------
subroutine Write_X_Y_Preamble(num_grids, grid, yr_offset, fname)
use globals
use Grid_Manager_Mod
integer, intent(in) :: num_grids
type(Grid_Data_Class), intent(in) :: grid(*)
real(dp), intent(in) :: yr_offset
character(*), intent(in) :: fname

character(fname_len) file_name
integer k
k = index(fname, '.') -1
file_name = fname(1:k)
call Write_Column_CSV_By_Region(num_grids, grid(1:num_grids)%year+yr_offset, &
&                                           grid(1:num_grids)%lon, &
&                                           'YEAR', trim(file_name),.false.)
call Write_Column_CSV_By_Region(num_grids, grid(1:num_grids)%x, &
&                                           grid(1:num_grids)%lon, &
&                                           'UTM_X', trim(file_name),.true.)
call Write_Column_CSV_By_Region(num_grids, grid(1:num_grids)%y, &
&                                           grid(1:num_grids)%lon, &
&                                           'UTM_Y', trim(file_name),.true.)
call Write_Column_CSV_By_Region(num_grids, grid(1:num_grids)%z, &
&                                           grid(1:num_grids)%lon, &
&                                           'DEPTH', trim(file_name),.true.)

endsubroutine Write_X_Y_Preamble
!--------------------------------------------------------------------------------------------------
!! Purpose: Write values of a matrix (f) to a csv file in exponential format by column rather than row
!> Inputs:
!>  n (integer) number of rows in f 
!>  m (integer) number of columns in f
!>  header string to write as a column header
!>  f (real(dp)) values to write to csv file
!> file_name (character(72)) filename to write f to in csv format
!--------------------------------------------------------------------------------------------------
subroutine Write_Column_CSV_By_Region(n,f, lon, header,file_name,append)
use globals
use Grid_Manager_Mod
implicit none
integer, intent(in):: n
real(dp), intent(in):: f(*), lon(*)
character(*), intent(in) :: header
character(*), intent(in) ::file_name
logical, intent(in) :: append
integer k, io
character(fname_len) fmtstr
character(1) cr
character(5000) input_str
character(5000) output_str
integer, parameter :: temp_dev = 70
integer, parameter :: appd_dev = 80
integer offset
integer line_count(num_regions)

line_count = (/0,0/)

if (append) then
    ! read existing files by region0
    ! create a temp file by region to write output then move temp to existing file_name
    ! read and write header row for each temp file
    do offset = 1, num_regions
        open(appd_dev+offset, file=file_name//trim(rgn(offset))//'.csv', status='old')
        open(unit=temp_dev+offset, iostat=io, file=output_dir//'TEMP'//trim(rgn(offset)), status='replace')
        read(appd_dev+offset,'(A)',iostat=io) input_str
        write(output_str,'(A,A,A)') trim(input_str),',',header
        write(temp_dev+offset, '(A)'//NEW_LINE(cr)) trim(output_str)
    enddo
    do k=1,n
        if (lon(k) > ma_gb_border) then
            ! GB
            offset = 1
        else
            ! MA
            offset = 2
        endif
        line_count(offset) = line_count(offset) + 1
        read(appd_dev+offset,'(A)',iostat=io) input_str
        if (f(k) < 0.0) then
            write(*,'(A,A,A,A,A,A,A,I5,A,A,A)') term_yel, 'WARNING: Negative value in: ', term_blk, &
            &  file_name//trim(rgn(offset))//'.csv', term_yel, ' line: ', term_blk, line_count(offset), &
            &  term_yel, ' set to 0.00000', term_blk
            write(output_str,'(A,A,(ES14.7 : ))') trim(input_str),',',0.D0
        ! sometimes f(k) takes on the value of E-311, which can not be read back in
        ! seems the minimum value is E-300, even with using format ES15.7E3 
        elseif (f(k) < zero_threshold) then
            write(output_str,'(A,A,(ES14.7 : ))') trim(input_str),',',0.D0
        else
            write(output_str,'(A,A,(ES14.7 : ))') trim(input_str),',',f(k)
        endif
        write(temp_dev+offset, '(A)'//NEW_LINE(cr)) trim(output_str)
    enddo
    do k = 1, num_regions
        close(appd_dev+k)
        close(temp_dev+k)
    enddo
    ! copy temp file to file name and delete temp file
    do offset = 1,num_regions
        open(temp_dev+offset, file=output_dir//'TEMP'//trim(rgn(offset)), status='old')
        open(unit=appd_dev+offset, iostat=io, file=file_name//trim(rgn(offset))//'.csv', status='replace')
        ! number of rows in temp file varies read until no more lines
        do
            read(temp_dev+offset,'(A)',iostat=io) input_str
            if (io.lt.0) exit
            write(appd_dev+offset,'(A)') trim(input_str)
        enddo
        close(appd_dev+offset)
        close(temp_dev+offset, status='delete')
    enddo
else
    ! empty file, write header to first line
    fmtstr='(ES14.7 : )'//NEW_LINE(cr)
    do offset = 1,num_regions
        open(appd_dev+offset, file=file_name//trim(rgn(offset))//'.csv')
        write(appd_dev+offset, '(A)') header
    enddo
    
    ! write data to first column
    do k=1,n
        if (lon(k) > ma_gb_border) then
            ! GB
            offset = 1
        else
            ! MA
            offset = 2
        endif
        write(appd_dev+offset,fmtstr) f(k)
    enddo
    do offset = 1,num_regions
        close(appd_dev+offset)
    enddo
endif
endsubroutine Write_Column_CSV_By_Region

!-----------------------------------------------------------------------------------------------------------
!! Purpose: This method is used to setup the output data files that are used for plotting and interpolation
!-----------------------------------------------------------------------------------------------------------
subroutine Setup_Data_Files(plot_data_sel, num_grids, grid, domain_name, start_year, stop_year)
use globals
use Grid_Manager_Mod
use Mortality_Mod
use Recruit_Mod
type(DataForPlots), intent(in) :: plot_data_sel
integer, intent(in) :: num_grids
type(Grid_Data_Class), intent(in) :: grid(*)
character(domain_len), intent(out) :: domain_name
integer, intent(in) :: start_year, stop_year

character(4) buf
real(dp) yr_offset

! write latitude and longitude out for later use by Matlab Geographic Scatter Plot ------------------------------ 
if (plot_data_sel%plot_ABUN) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_ABUN_'//domain_name//'.csv')
if (plot_data_sel%plot_BMMT) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_BMMT_'//domain_name//'.csv')
if (plot_data_sel%plot_EBMS) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_EBMS_'//domain_name//'.csv')
if (plot_data_sel%plot_FEFF) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_FEFF_'//domain_name//'.csv')
if (plot_data_sel%plot_FMOR) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_FMOR_'//domain_name//'.csv')
if (plot_data_sel%plot_LAND) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_LAND_'//domain_name//'.csv')
if (plot_data_sel%plot_LNDW) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_LNDW_'//domain_name//'.csv')
if (plot_data_sel%plot_LPUE) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_LPUE_'//domain_name//'.csv')
if (plot_data_sel%plot_RECR) call Write_Lat_Lon_Preamble(num_grids, grid, output_dir//'Lat_Lon_Surv_RECR_'//domain_name//'.csv')

! Write similar data for later interpolation by UK (Universal Kriging)
! Year-1 is the initial state
yr_offset = -1.
do n = start_year-1, stop_year
    write(buf,'(I4)') n
    if (plot_data_sel%plot_ABUN) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_ABUN_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_BMMT) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_BMMT_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_EBMS) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_EBMS_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_FEFF) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_FEFF_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_FMOR) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_FMOR_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_LAND) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_LAND_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_LNDW) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_LNDW_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_LPUE) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_LPUE_'//domain_name//buf//'.csv')

    if (plot_data_sel%plot_RECR) &
    &    call Write_X_Y_Preamble(num_grids, grid, yr_offset, data_dir//'X_Y_RECR_'//domain_name//buf//'.csv')

    yr_offset = yr_offset + 1.0
end do
endsubroutine Setup_Data_Files

!-----------------------------------------------------------------------------------------------------------
!! Purpose: This method is used to setup the write recruitment data files at each time step
!-----------------------------------------------------------------------------------------------------------
subroutine Write_Recruit_Estimates(ts, ts_per_year, num_grids, grid, domain_name, year, start_year, recruit)
use globals
use Grid_Manager_Mod
use Mortality_Mod
use Recruit_Mod
integer, intent(in) :: ts,ts_per_year, num_grids
type(Grid_Data_Class), intent(in) :: grid(*)
character(domain_len), intent(out) :: domain_name
integer, intent(in) :: year, start_year
type(Recruitment_Class), intent(in) :: recruit(*)

character(4) buf
integer recr_idx
character(fname_len) file_name

recr_idx = year - start_year + 1

if (mod(ts, ts_per_year) .eq. 1) then
    if (ts .eq. 1) then
        write(buf,'(I4)') year-1
    else
        write(buf,'(I4)') year
    endif
    call Write_Column_CSV_By_Region(num_grids, recruit(1:num_grids)%recruitment(recr_idx), &
    &            grid(1:num_grids)%lon, 'RECR', data_dir//'X_Y_RECR_'//domain_name//trim(buf), .true.)
endif

! Write data for survey grid
file_name = output_dir//'Lat_Lon_Surv_RECR_'//domain_name//'.csv'
call Write_Column_CSV(num_grids, recruit(1:num_grids)%recruitment(recr_idx), 'Recruitment', file_name, .true.)
        
endsubroutine Write_Recruit_Estimates