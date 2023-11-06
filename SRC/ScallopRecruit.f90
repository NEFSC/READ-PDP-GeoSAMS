module Recruit_Mod
    use globals
    implicit none
    integer, parameter :: max_n_year = 50
    character(*), parameter :: rec_input_dir = 'KrigingEstimates/'
    character(*), parameter :: rec_output_dir = 'RecruitField/'

    type Recruit_Class
        real(dp) recruitment(max_n_year)
        real(dp) rec_start,rec_stop
        integer year(max_n_year)
        integer n_year, max_rec_ind ! max_rec_ind is the largest size class treated as a recruit
    end type Recruit_Class

    ! @private @memberof Recruit_Mod
    integer, PRIVATE :: num_size_classes
    character(2), PRIVATE :: region_name

    CONTAINS

    !-----------------------------------------------------------------------
    !> Set_Recruitment
    !! @brief Sets recruitment parameters
    !! @param[in,out] recruit 
    !! @param[in,out] num_grids
    !! @param[in] domain_name can be either 
    !!             MA MidAtlantic or 
    !!             GB GeorgesBank
    !! @param[in] is_random_rec
    !! @param[in] num_sz_classes
    !-----------------------------------------------------------------------
    subroutine Set_Recruitment(recruit, num_grids, domain_name, is_random_rec, num_sz_classes)
        type(Recruit_Class), intent(inout) :: recruit(*)
        integer, intent(in) :: num_grids
        character(2), intent(in) :: domain_name
        logical, intent(in) :: is_random_rec
        integer, intent(in) :: num_sz_classes
        integer j, year, year_index
        real(dp) tmp(num_grids)
        character(72) buf

        !! initalize private members
        num_size_classes = num_sz_classes
        region_name = domain_name

        write(*,*)'Is random Rec',is_random_rec
        year_index = 0
        do year = 1979, 2018      !! TODO make these variable
            year_index = year_index + 1
            write(buf,'(I6)')year
            if (is_random_rec) then
                call Random_Recruits(tmp,year,year,num_grids,100)
            else
                call Read_Scalar_Field(rec_input_dir//'Sim'//region_name//trim(adjustl(buf))//'/KrigingEstimate.txt',tmp, num_grids)
            endif
            do j = 1,num_grids
                recruit(j)%recruitment(year_index) = tmp(j)
                recruit(j)%year(year_index) = year
                ! value as a fraction of a year, i.e. Jan 30 is 8.22% of a year
                recruit(j)%rec_start = 1.D0/365.D0
                recruit(j)%rec_stop = 100.D0/365.D0 !TODO why 100?
            enddo
        enddo
        
        do year = 2019,2025
            year_index = year_index + 1
            write(buf,'(I6)')year
            !! TODO make these variable
            call Random_Recruits(tmp,1979,2018,num_grids,100)
            do j = 1,num_grids
                recruit(j)%recruitment(year_index) = tmp(j)
                recruit(j)%year(year_index) = year
                recruit(j)%rec_start = 1.D0/365.D0
                recruit(j)%rec_stop = 100.D0/365.D0
            enddo
        enddo
        recruit(1:num_grids)%n_year = year_index
        write(*,'(A,I3,A,F10.7)') ' Year Index = ',year_index,'  Recruitment is ', recruit(1)%recruitment(year_index)
        year_index = 0
        do year = 1979,2025
                year_index = year_index + 1
                write(buf,'(I6)')year
                tmp(1:num_grids) = recruit(1:num_grids)%Recruitment(year_index)
                call Write_Scalar_Field(num_grids,tmp,rec_output_dir//'RecruitFieldIn'//trim(adjustl(buf))//'.txt')
        enddo
        return
    endsubroutine Set_Recruitment
        

    !---------------------------------------------------------------------------------------------------
    subroutine Random_Recruits(R, start_year, stop_year, num_grids, num_sim)
        real(dp), intent(out) :: R(*)
        integer, intent(in)::start_year, stop_year, num_sim
        integer, intent(in)::num_grids
        real(dp), allocatable:: fishing_effort(:,:)
        real(dp) p(2), mu, sig, mus, mur
        integer num_years_int, num_sims_rand
        character(6) buf
        logical rescale
        rescale = .true.
        allocate(fishing_effort(1:num_grids, 1:num_sim))
        call random_number(p(1:2))
        num_years_int = start_year + floor( float(stop_year - start_year + 1) * p(1) )
        num_sims_rand = ceiling( float(num_sim) * p(2) )
        write(buf,'(I6)') num_years_int
        
        call Read_Scalar_Field(rec_input_dir//'Sim'//region_name//'Clim'//'/KrigingEstimate.txt', &
        &            fishing_effort(1:num_grids,num_sims_rand), num_grids)
        R(1:num_grids) = fishing_effort(1:num_grids, num_sims_rand)
        ! Adjust mean to random number based on historical record of mean. log(interanual mean)~ N(mu,sig)
        if (rescale)then
            !log normal average for 1979-2018
            if (region_name(1:2).eq.'GB')then
                mu =  -3.0198
                sig =  1.1068
            else
                mu = -3.5329
                sig = 1.0036
            endif
            mus = sum(R(1:num_grids)) / float(num_grids)
            call random_number(p(1:2))
            mur = mu + sig * p(1)
            R(1:num_grids) = R(1:num_grids) * exp(mur) / mus
        endif
        deallocate(fishing_effort)
        
        return
    endsubroutine Random_Recruits


    subroutine Mortality_Density_Dependent(recruit, mortality, state, domain_area)
        !---------------------------------------------------------------------------------------------------
        !Density dependant mortality is taken from personal communication with Dr. Dvora Hart late March 2022
            
        !Mar 24, 2022, 4:18 PM
        !Hi Keston. I have a relationship between juvenile natural mortality and recruitment for the Mid-Atlantic:
        
        !M_juv = exp(-9.701 + 1.093 Rec)  if Rec > 1400 million
        !else M_juv = 0.25
        
        !GB open and closed areas will be more difficult - let me see what I can do.
        !Dvora
        
        !Hi again Keston. I forgot to mention that recruits are in millions. Also, use a break point of 2030 million instead of 1400 million in order to have a continuous relationship.
        
        !I have a similar formula for GB Open:   M = exp(1.226*log(Rec)-10.49) when Rec > 1400 million, and 0.2 otherwise.  Again, Rec is in millions.
        !Dvora
        
        !Hi Dvora, For the decreasing logistic function, alpha(h) = 1 / ( 1 + exp( [h - h_0] / w ) ) do you have a preference for the parameters h_0 and w? I am using h_0
        !Deborah Hart - NOAA Federal <deborah.hart@noaa.gov>
        
        !Hi Keston. Sorry, we put more details in the latest draft, which I didn't send you.  The logistic function is of the form: \alpha(h) = 1-\frac{1}{1+\exp(-h_0[h-a])}  
        !We set h_0 to be 65 mm in the Mid-Atlantic and 70 mm on Georges, and a = 0.1 for both regions.
        !Dvora
            
        !	Mar 28, 2022, 1:18 PM
        !I think something isn't right here. is effectively M_juv = inf. I this a unit issue or am I missing something? Thanks, Keston
        !Deborah Hart - NOAA Federal <deborah.hart@noaa.gov>
        !Mar 28, 2022, 1:33 PM
        !Sorry Rec should be logged 
        !-----------------------------------------------------------------------------------------------------
        
        use Mortality_Mod, only : Mortality_Class
        
        implicit none
        real(dp),intent(in) :: state(*),domain_area
        type(Mortality_Class), INTENT(INOUT):: mortality
        type(Recruit_Class), INTENT(IN):: recruit
        real(dp) recruit_density

        if(region_name(1:2).eq.'MA')then
            recruit_density = sum(state(1:recruit%max_rec_ind)) * domain_area/(10.**6)
            mortality%natural_mort_juv = max( mortality%natural_mort_adult , exp(-9.701_dp + 1.093_dp * log(recruit_density)) )
        endif
        
        if(region_name(1:2).eq.'GB')then
            recruit_density = sum(state(1:recruit%max_rec_ind))*domain_area/(10.**6)
            mortality%natural_mort_juv = max( mortality%natural_mort_adult , exp(-10.49_dp + 1.226_dp * log(recruit_density)) )
        endif
        
        mortality%natural_mortality(1:num_size_classes) = mortality%alpha(1:num_size_classes) * mortality%natural_mort_juv &
        &                       + (1._dp - mortality%alpha(1:num_size_classes)) * mortality%natural_mort_adult
        return
    endsubroutine Mortality_Density_Dependent
        
endmodule Recruit_Mod
