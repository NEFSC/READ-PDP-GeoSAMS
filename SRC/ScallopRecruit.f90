!>---------------------------------------------------------------------------------------------------------------------
!> @page page2 Recruit_Mod
!>
!> @section Rsec1 Recruitment Class
!> An array of weights is computed based on the number of recruitment years that favors more recent recruit estimates. The weighting is then used to randomly choose an index into the available recruit data. This index is used to preload the recruit data into the Recruitment Class structure.
!> - recruitment() = data read in from randomly chosen recruit file
!> - year() = simulation year
!> - rec_start = a decimal value given as day of the year divided by 365.25. Typically 0, which would be January 1
!> - rec_stop = decimal value given as day of the year divided by 365.25. Typically 100/365.25, which is April 10th.
!>
!>---------------------------------------------------------------------------------------------------------------------
module Recruit_Mod
use globals
implicit none
integer, parameter :: max_n_year = 50

!> @class Recruitment_Class
!! 
!! Subroutines that determine expected growth of scallops
type Recruitment_Class
    real(dp) recruitment(max_n_year)
    real(dp) rec_start
    real(dp) rec_stop
    integer year(max_n_year)
    integer n_year
    !! max_rec_ind is the largest size class treated as a recruit
    integer max_rec_ind
end type Recruitment_Class

! @private @memberof Recruit_Mod
character(fname_len), PRIVATE :: config_file_name
integer, PRIVATE :: num_grids
character(domain_len), PRIVATE :: domain_name
real(dp), PRIVATE :: domain_area_sqm

integer, PRIVATE :: recruit_yr_strt ! start of available recruitment data
integer, PRIVATE :: recruit_yr_stop  ! end of available recruitment data
integer, PRIVATE :: n_rand_yrs
integer, PRIVATE :: sim_start_year  ! start of the growth period
integer, PRIVATE :: sim_stop_year   ! end of the growth period
real(dp), PRIVATE :: recr_period_start ! day of year as a fraction of year, Jan 1 is 1/365
real(dp), PRIVATE :: recr_period_stop  ! day of year as a fraction of year, Apr 10 is 100/365

real(dp), PRIVATE, allocatable :: weights(:)
real(dp), PRIVATE :: wsum          ! sum of weights array

CONTAINS

!==================================================================================================================
!! @public @memberof Recruitment_Class
!> Set_Recruitment
!> @brief Sets recruitment parameters
!> @param[in,out] recruit 
!> @param[in] n_grids, The number of grids under consideration, sets private value num_grids
!> @param[in] dom_name, The doomain being simulated, sets private value domain_name. Should be 
!>             MA MidAtlantic or 
!>             GB GeorgesBank
!> @param[in] dom_area the total area in square meters, sets domain_area_sqm
!> @param[in] L_inf_mu asymptotic size, average
!> @param[in] K_mu Brody growth coefficient K, average
!> @param[in] shell_length_mm Shell height in millimeters
!> @param[out] recr_yr_strt year start of available data
!> @param[out] recr_yr_stop year stop of available data
!> @param[in] yr_start simulation start year
!> @param[in] yr_stop simulation end year
!==================================================================================================================
subroutine Set_Recruitment(recruit, n_grids, dom_name, dom_area, recr_yr_strt, recr_yr_stop, &
    & L_inf_mu, K_mu, shell_length_mm, yr_start,  yr_stop)
    use globals
    type(Recruitment_Class), intent(inout) :: recruit(*)
    integer, intent(in) :: n_grids
    character(domain_len), intent(in) :: dom_name
    real(dp), intent(in) :: dom_area
    integer, intent(out) :: recr_yr_strt, recr_yr_stop
    real(dp), intent(in) :: L_inf_mu(*)
    real(dp), intent(in) :: K_mu(*)
    real(dp), intent(in) :: shell_length_mm(*)
    integer, intent(in) :: yr_start,  yr_stop

    integer n, j, year, year_index, random_year
    real(dp) tmp(n_grids)
    character(72) buf
    real(dp) L30mm

    character(fname_len) fname
    logical exists

    ! Used to define weighting
    !
    !       |
    ! sill  +                =========
    !       |             //!
    !       |           //  !
    !       |         //    !
    !       |       //      !
    !       |     //        !
    !       |   //          !
    !       | //            !
    !       ----------------+----------
    !                      range
    real(dp) range
    real(dp) sill
    integer rand_idx

    call Read_Configuration()
    ! 
    sim_start_year = yr_start
    sim_stop_year  = yr_stop
    recr_yr_strt = recruit_yr_strt
    recr_yr_stop = recruit_yr_stop
    n_rand_yrs = recruit_yr_stop - recruit_yr_strt + 1

    allocate(weights(1:n_rand_yrs))

    ! setting up spherical weighting
    range = 0.9D0 * n_rand_yrs
    sill = n_rand_yrs

    do j = 1, n_rand_yrs
        if (j <= range) then
            weights(j) = sill * (1.5 * (j/range) - 0.5 * (j/range)**3)
        else
            weights(j) = sill
        endif
    enddo
    wsum = sum( weights )

    !! initalize private members
    num_grids = n_grids
    domain_name = dom_name
    domain_area_sqm = dom_area

    !-------------------------------------------------------------------------
    ! This next section is effectively setting
    ! For all j in [1..num_grids]
    !   for year_index in [1..max]
    !       recruitment(year_index) = RecruitEstimate
    !       year(year_index) = year, i.e. 1979 + (year_idx - 1)
    !       rec_start = 1/365, or January 1st
    !       rec_stop = 100/365, or April 10 
    !-------------------------------------------------------------------------
    year_index = 0
    do year = sim_start_year, sim_stop_year
        year_index = year_index + 1

        rand_idx = random_index()
        random_year = recruit_yr_strt - 1 + rand_idx

        write(buf,'(I4)')random_year
        fname = rec_input_dir//'RecruitEstimate'//domain_name//trim(adjustl(buf))//'.txt'
        inquire(file=fname, exist=exists)
        if (exists) then
            PRINT *, term_blu, trim(fname), ' FOUND', term_blk, year, year_index, rand_idx
        else
            PRINT *, term_red, trim(fname), ' NOT FOUND', term_blk
            stop 1
        endif
        n=num_grids
        call Read_Scalar_Field(fname, tmp, n)
        if (n .NE. num_grids) then
            PRINT *, term_red, 'OOPS something went wrong reading recruits', &
            & term_blk, n, term_red, ' in file does not match expected', term_blk, num_grids
            STOP 1
        endif
        do j = 1,num_grids
            recruit(j)%recruitment(year_index) = tmp(j)
            recruit(j)%year(year_index) = year
            recruit(j)%rec_start = recr_period_start
            recruit(j)%rec_stop = recr_period_stop
        enddo
    enddo
    
    recruit(1:num_grids)%n_year = year_index
    !-------------------------------------------------------------------------

    ! quantize recruitment
    ! open(write_dev,file = init_cond_dir//'RecIndx.txt')
    do n = 1, num_grids
        L30mm = (L_inf_mu(n) - dfloat(shell_len_min)) * exp(-K_mu(n))
        do j=1, num_size_classes 
            if (shell_length_mm(j) .le. L30mm) recruit(n)%max_rec_ind = j
        enddo
        ! write(write_dev,*) n, L30mm, recruit(n)%max_rec_ind
    enddo
    ! close(write_dev)

    deallocate(weights)

    return
endsubroutine Set_Recruitment

!! 
!> @brief Defines a weighted distribution as defined in weights
!> @returns a value 1 <= x <= n_rand_yrs
integer function Random_Index()
    integer :: idx

    real(dp) x, prob

    call random_number( x )

    prob = 0
    do idx = 1, n_rand_yrs
        prob = prob + weights( idx ) / wsum   !! 0 < prob < 1
        if ( x <= prob ) exit
    enddo
    Random_Index = idx
endfunction
    
!-----------------------------------------------------------------------------------------------
!! @public @memberof Recruitment_Class
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

!-----------------------------------------------------------------------
!! @public @memberof Recruitment_Class
!> Read_Configuration
!> @brief Read Input File
!> 
!> Reads a configuration file, 'config_file_name.cfg', to set data parameters for Recruitment
!-----------------------------------------------------------------------
subroutine Read_Configuration()

    implicit none
    character(line_len) input_string
    character(tag_len) tag
    character(value_len) value
    integer j, k, io

    ! set default values
    recr_period_start = 0./365.
    recr_period_stop = 100./365.

    recruit_yr_strt = 2012
    recruit_yr_stop = 2023

    write(*,*) 'READING IN ', config_file_name

    open(read_dev,file = config_file_name)
    do
        input_string=""
        read(read_dev,'(a)',iostat=io) input_string
        if (io.lt.0) exit

        if (input_string(1:1) .NE. '#') then
            j = scan(input_string,"=",back=.true.)
            tag = trim(adjustl(input_string(1:j-1)))
            ! explicitly ignore inline comment
            k = scan(input_string,"#",back=.true.)
            if (k .EQ. 0) k = len(input_string)+1
            value =  trim(adjustl(input_string(j+1:k-1)))

            select case (tag)
            case('Start Period')
                read(value, *) recr_period_start
                recr_period_start = recr_period_start / 365._dp

            case('Stop Period')
                read(value, *) recr_period_stop
                recr_period_stop = recr_period_stop / 365._dp

            case('Recruit Year Strt')
                read(value,*) recruit_yr_strt

            case('Recruit Year Stop')
                read(value,*) recruit_yr_stop

            case default
                write(*,*) term_red, 'Unrecognized line in ',config_file_name
                write(*,*) 'Unknown Line-> ',input_string, term_blk
                stop 1
            end select
        endif
    end do
    close(read_dev)
    return
end subroutine Read_Configuration

endmodule Recruit_Mod
