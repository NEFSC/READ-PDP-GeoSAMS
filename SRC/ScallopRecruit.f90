!>---------------------------------------------------------------------------------------------------------------------
!> @page page2 Recruit_Mod
!>
!> @section Rsec1 Recruitment Class
!> Recruitment is treated as a spatially correlated random variable.  Recruit estimates at each node are read in from 
!> files stored in directories KrigingEstimates/SimDNYYYY/KrigingEstimate.txt, where DN is ['MA', 'GB'] and YY is the
!> year 1979 - 2019
!> 
!> Within the Population Dynamics portion 
!> 
!> 
!> @subsection Rsubsec1 Implementation of random recruitment
!> Spatial fields of recruitment are generated by the software located in directory "~/UKsrc".  The output files, 
!> such as: "KrigingEstimates/SimDNYYYY/RandomFieldxx.txt" contain @f$n_{nodes}@f$ vectors of independent 
!> random fields conditioned on survey observations from "Data/RecruitsYYYYDN.csv", 
!> where xx runs from @f$1@f$ to @f$N_{rand}@f$ and @f$year@f$ 1979 to 2018.
!>
!> @subsection Rsubsec2 Interpolation Algorithm
!> The interpolation of recruit data is carried out with a Universal Kriging (UK) algorithm allowing for sampling from 
!> the posterior distribution. 
!>
!> @subsubsection Rsubsubsec1 Universal Kriging
!> Universal Kriging (UK) is a generalization of ordinary kriging in which a set of spatial functions are used to model 
!> the trend of a set of point observations.  The underlying model is:
!>
!> @f[
!> f(x,y,H(x,y),\lambda)=\sum_{k=1}^{n_f} f_k(x,y,H(x,y),\lambda_k) +\epsilon(x,y) 
!> @f]
!>
!> where @f$f_k@f$ are the known spatial functions and @f$\epsilon(x,y)@f$ is a zero mean, spatially correlated, 
!> stationary random process with semi-variogram @f$\gamma(s)@f$. For a summary of UK see Cressie 1993, pages 151 -180.
!> The spatially variable @f$x@f$ here is taken to include latitude, longitude and, bathymetric depth
!> (@f$x=[lat,lon,z(lat,lon)]@f$).
!> 
!> @subsubsection Rsubsubsec2 Spatial functions
!> The spatial functions (SF) used here are  a set of one dimensional, bounded, C-infinity functions with two parameters,
!> \\
!> Gaussian Bump:
!> @f[
!> f_a (s,\lambda,x_0) = \exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
!> @f]
!> Logistic curve:
!> @f[
!> f_b (s,\lambda,x_0) = \frac{1}{1+\exp( -\frac{s-x_0}{\lambda} ) }
!> @f]
!> ''Sin Exp" curve:
!> @f[
!> f_c (s,\lambda,x_0) = \sin(\frac{s-x_0}{\lambda})\exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
!> @f]
!> ''Cos Exp" curve:
!> @f[
!> f_c (s,\lambda,x_0) = \cos(\frac{s-x_0}{\lambda})\exp( - \left(\frac{s-x_0}{\lambda}\right)^2 )
!> @f]
!> In all of the function form @f$\lambda@f$ controls the width of the transition and $x_0$ the transition point. 
!> 
!> After fitting these to the bathymetric variable (H) we can introduce interaction. Allowing interaction terms for the spatial
!> functions depending on bathymetry only we can define, @f$g_j(x,H,\lambda^j,{x_0}^j,\lambda_k,{x_0}^k)=f_j(x)f_k ( H )@f$
!> @f[
!> f(x,y,H)=\sum_i f_i(H,\lambda^i,z_0^i) + \sum_j f_{j_x}(x,\lambda^{j_x},x_0^{j_x}) f_k(z,\lambda^k,x_0^k)+ \sum_j f_{j_y}(y,\lambda^{j_y},x_0^{j_y}) f_k(z,\lambda^k,x_0^k)
!> @f]
!> Some parametric functions for spatial fitting on the continental shelf.
!> Here @f$z@f$ is bathymetric depth. We start by fitting nonlinear parameters @f$\lambda^{c,s}@f$ and @f$x_0^{c,s}@f$ to log recruitment for "cross shelf" structure.  
!> The non linear fitting is done with standard linear regression. i.e.
!> @f[
!> %(\hat{\lambda},\hat{x}_0)=argmin\left( \sum_i \left(A + B f(s_i ,\lambda,x_0) - d_i\right)^2 \right)
!> @f]
!> @f[
!> f(x,y,z)=\beta_0+\sum_i \beta_i f_i(z) + \sum_j \beta_j g_j(x,z)+\sum_k \beta_k g_k(y,z)+ \epsilon
!> @f]
!> where @f$\beta_i@f$ are coefficients for the spatial functions and @f$\epsilon@f$ is the zero mean noise process associated with UK.
!> 
!> @subsubsection Rsubsubsec3 Fitting non-linear parameters
!> A brute force approach is taken to fitting the nonlinear parameters @f$x_0@f$ and @f$\lambda@f$.  A search range is determined based on
!> the geographic range of the observations.  The parameters are then fit to minimize the misfit to observations.   
!>  subroutine @f${\it FitNLSFunc}@f$ parameter np).  The nonlinear parameters are fit by minimizing RMS misfit to the simple least squares 
!> fit with a smoothness penalty,
!> @f[
!> J(x_0,\lambda)=\sqrt{ \frac{1}{n} \sum_i (d_i-a - b f(x_i|\lambda,x_0))^2 }+S(\lambda,x_0)
!> @f]
!> Where @f$S(\lambda,x_0)=\int_{-\infty}^\infty f''(x) ^2 d x= S(\lambda)@f$ is a roughness penalty, @f$a@f$ and @f$b@f$ are temporarily 
!> assigned (by least squares) constants fit to minimize @f$J@f$.  @f$S@f$ is proportional to @f$\lambda^{-3}@f$ for all examples used here 
!> (see subroutine @f${\it NLSFuncPen}@f$).  Other one dimensional function forms can be added to the software in subroutine NLSFunc and 
!> NLSFFuncPen.
!> 
!> A smoothness penalty is imposed for each function based on the analytic 
!> 
!> @subsection Rsubsec3 Residual process
!> After performing an ordinary least squares fit for the SF coeficients, @f$\beta@f$, we have an estimate of @f$\epsilon@f$. 
!> An empirical variogram is computed subroutine @f${\it variogramF}@f$, and variogram parameters are fit (again by brute force).  
!> The variogram forms allowed are "spherical", "exponential", and "gaussian".  The form is hard-coded in the main program, 
!> UniversalKriging.f90.
!>
!> @subsubsection Rsubsubsec4 Posterior sampling
!> With the fitting of the residual we have a covariance for @f$\epsilon@f$ and the estimation problem becomes one of Generalized Least 
!> Squares (GLS).  Posterior sampling is then conducted  achieved posterior sampling is Treating the 
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
    !> @public @memberof Recruitment_Class
    real(dp) recruitment(max_n_year)
    !> @public @memberof Recruitment_Class
    real(dp) rec_start
    !> @public @memberof Recruitment_Class
    real(dp) rec_stop
    !> @public @memberof Recruitment_Class
    integer year(max_n_year)
    !> @public @memberof Recruitment_Class
    integer n_year
    !> @public @memberof Recruitment_Class
    !! max_rec_ind is the largest size class treated as a recruit
    integer max_rec_ind
end type Recruitment_Class

! @private @memberof Recruit_Mod
character(fname_len), PRIVATE :: config_file_name
integer, PRIVATE :: num_grids
character(2), PRIVATE :: domain_name
real(dp), PRIVATE :: domain_area_sqm

logical, PRIVATE :: use_random_rec
integer, PRIVATE :: recr_start_year ! historical data interpolated at start
integer, PRIVATE :: recr_stop_year ! historical data interpolated at stop
integer, PRIVATE :: recr_all_rand_stop ! all random recruitment, start year is recr_stop_year + 1
real(dp), PRIVATE :: recr_period_start ! day of year as a fraction of year, Jan 1 is 1/365
real(dp), PRIVATE :: recr_period_stop  ! day of year as a fraction of year, Apr 10 is 100/365

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
!> @param[in] num_sz_classes
!> @param[in] L_inf_mu asymptotic size, average
!> @param[in] K_mu Brody growth coefficient K, average
!> @param[in] shell_length_mm Shell height in millimeters
!==================================================================================================================
subroutine Set_Recruitment(recruit, n_grids, dom_name, dom_area, L_inf_mu, K_mu, shell_length_mm)
    use globals
    type(Recruitment_Class), intent(inout) :: recruit(*)
    integer, intent(in) :: n_grids
    character(2), intent(in) :: dom_name
    real(dp), intent(in) :: dom_area
    real(dp), intent(in) :: L_inf_mu(*)
    real(dp), intent(in) :: K_mu(*)
    real(dp), intent(in) :: shell_length_mm(*)

    integer n, j, year, year_index
    real(dp) tmp(n_grids)
    character(72) buf
    real(dp) L30mm

    ! set default values
    recr_period_start = 1./365.
    recr_period_stop = 100./365.
    recr_start_year = 1979
    recr_stop_year = 2018
    recr_all_rand_stop = 2025

    call Read_Configuration()

    !! initalize private members
    num_grids = n_grids
    domain_name = dom_name
    domain_area_sqm = dom_area

    !-------------------------------------------------------------------------
    ! This next section is effectively setting
    ! For all j in [1..num_grids]
    !   for year_index in [1..max]
    !       recruitment(year_index) = KrigingEstimate
    !       year(year_index) = year, i.e. 1979 + (year_idx - 1)
    !       rec_start = 1/365, or January 1st
    !       rec_stop = 100/365, or April 10 
    !-------------------------------------------------------------------------
    write(*,*)'Is random Rec',use_random_rec
    year_index = 0
    do year = recr_start_year, recr_stop_year
        year_index = year_index + 1
        write(buf,'(I6)')year
        if (use_random_rec) then
            ! TODO replace magic number 100
            tmp = Random_Recruits(year, year, 100, use_random_rec)
        else
            call Read_Scalar_Field(rec_input_dir//'Sim'//domain_name//trim(adjustl(buf))//'/KrigingEstimate.txt',tmp, num_grids)
        endif
        do j = 1,num_grids
            recruit(j)%recruitment(year_index) = tmp(j)
            recruit(j)%year(year_index) = year
            recruit(j)%rec_start = recr_period_start
            recruit(j)%rec_stop = recr_period_stop
        enddo
    enddo
    
    do year = recr_stop_year+1, recr_all_rand_stop
        year_index = year_index + 1
        write(buf,'(I6)')year
        tmp = Random_Recruits(recr_start_year, recr_stop_year, 100, .true.)
        do j = 1,num_grids
            recruit(j)%recruitment(year_index) = tmp(j)
            recruit(j)%year(year_index) = year
            recruit(j)%rec_start = recr_period_start
            recruit(j)%rec_stop = recr_period_stop
        enddo
    enddo
    recruit(1:num_grids)%n_year = year_index
    !-------------------------------------------------------------------------

    year_index = 0
    do year = recr_start_year,recr_all_rand_stop
        year_index = year_index + 1
        write(buf,'(I6)')year
        tmp(1:num_grids) = recruit(1:num_grids)%Recruitment(year_index)
        call Write_Scalar_Field(num_grids,tmp,rec_output_dir//'RecruitFieldIn'//trim(adjustl(buf))//'.txt')
    enddo

    ! quantize recruitment
    open(write_dev,file = init_cond_dir//'RecIndx.txt')
    do n = 1, num_grids
        L30mm = (L_inf_mu(n) - dfloat(shell_len_min)) * exp(-K_mu(n))
        do j=1, num_size_classes 
            if (shell_length_mm(j) .le. L30mm) recruit(n)%max_rec_ind = j
        enddo
        write(write_dev,*) n, L30mm, recruit(n)%max_rec_ind
    enddo
    close(write_dev)

    return
endsubroutine Set_Recruitment
    

!==================================================================================================================
!! @public @memberof Recruitment_Class
!==================================================================================================================
function Random_Recruits(start_year, stop_year, num_sim, rescale)
    real(dp) :: Random_Recruits(num_grids)
    integer, intent(in)::start_year, stop_year, num_sim
    real(dp), allocatable:: fishing_effort(:,:)
    logical, intent(in) :: rescale
    real(dp) p(2), mu, sig, mus, mur
    integer num_years_int, num_sims_rand
    character(6) buf
    allocate(fishing_effort(1:num_grids, 1:num_sim))
    call random_number(p(1:2))
    num_years_int = start_year + floor( float(stop_year - start_year + 1) * p(1) )
    num_sims_rand = ceiling( float(num_sim) * p(2) )
    write(buf,'(I6)') num_years_int
    
    call Read_Scalar_Field(rec_input_dir//'Sim'//domain_name//'Clim'//'/KrigingEstimate.txt', &
    &            fishing_effort(1:num_grids,num_sims_rand), num_grids)
    Random_Recruits(1:num_grids) = fishing_effort(1:num_grids, num_sims_rand)
    ! Adjust mean to random number based on historical record of mean. log(interanual mean)~ N(mu,sig)
    if (rescale)then
        !log normal average for 1979-2018
        if (domain_name(1:2).eq.'GB')then
            mu =  -3.0198
            sig =  1.1068
        else
            mu = -3.5329
            sig = 1.0036
        endif
        mus = sum(Random_Recruits(1:num_grids)) / float(num_grids)
        call random_number(p(1:2))
        mur = mu + sig * p(1)
        Random_Recruits(1:num_grids) = Random_Recruits(1:num_grids) * exp(mur) / mus
    endif
    deallocate(fishing_effort)
    
    return
endfunction Random_Recruits

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

    config_file_name = config_dir//fname
    inquire(file=config_file_name, exist=exists)

    if (exists) then
        PRINT *, term_blu, trim(config_file_name), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(config_file_name), ' NOT FOUND', term_blk
        stop
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
            case('Start Year')
                read(value, *) recr_start_year

            case('Stop Year')
                read(value, *)recr_stop_year

            case('All Random Stop Year')
                read(value, *)recr_all_rand_stop

            case('Start Period')
                read(value, *) recr_period_start
                recr_period_start = recr_period_start / 365._dp

            case('Stop Period')
                read(value, *) recr_period_stop
                recr_period_stop = recr_period_stop / 365._dp

            case('Use Random Recruitment')
                use_random_rec = (value(1:1) .eq. 'T')

            case default
                write(*,*) term_red, 'Unrecognized line in ',config_file_name
                write(*,*) 'Unknown Line-> ',input_string, term_blk
                stop
            end select
        endif
    end do
    close(read_dev)
    return
end subroutine Read_Configuration

endmodule Recruit_Mod
