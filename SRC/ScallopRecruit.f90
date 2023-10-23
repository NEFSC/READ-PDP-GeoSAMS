module Recruit_Mod
    use globals
    implicit none
    integer, parameter :: max_n_year = 50
    type Recruit_Struct
       real(dp) recruitment(max_n_year)
       real(dp) rec_start,rec_stop
       integer year(max_n_year)
       integer n_year, max_rec_ind ! max_rec_ind is the largest size class treated as a recruit
    end type Recruit_Struct

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
    !-----------------------------------------------------------------------
    subroutine Set_Recruitment(recruit, num_grids, domain_name, is_random_rec)
        type(Recruit_Struct), intent(inout) :: recruit(*)
        integer, intent(in) :: num_grids
        character(2), intent(in) :: domain_name
        logical, intent(in) :: is_random_rec
        integer j, yr, year_index
        real(dp) tmp(num_grids)
        character(72) buf

        write(*,*)'Is random Rec',is_random_rec
        year_index = 0
        do yr = 1979, 2018      !! TODO make these variable
            year_index = year_index + 1
            write(buf,'(I6)')yr
            if (is_random_rec) then
                call Random_Recruits(tmp,yr,yr,num_grids,100,domain_name)
            else
                call Read_Scalar_Field('Input/Sim'//domain_name//trim(adjustl(buf))//'/KrigingEstimate.txt',tmp, num_grids)
            endif
            do j=1,num_grids
                recruit(j)%recruitment(year_index) = tmp(j)
                recruit(j)%year(year_index) = yr
                ! value as a fraction of a year, i.e. Jan 30 is 8.22% of a year
                recruit(j)%rec_start = 1.D0/365.D0
                recruit(j)%rec_stop = 100.D0/365.D0 !TODO why 100?
            enddo
        enddo
        
        do yr=2019,2025
            year_index = year_index + 1
            write(buf,'(I6)')yr
            !! TODO make these variable
            call Random_Recruits(tmp,1979,2018,num_grids,100,domain_name)
            do j=1,num_grids
                recruit(j)%recruitment(year_index)=tmp(j)
                recruit(j)%year(year_index)=yr
                recruit(j)%rec_start = 1.D0/365.D0
                recruit(j)%rec_stop = 100.D0/365.D0
            enddo
        enddo
        recruit(1:num_grids)%n_year = year_index
        write(*,'(A,I3,A,F10.7)') ' Year Index = ',year_index,'  Recruitment is ', recruit(1)%recruitment(year_index)
        year_index = 0
        do yr=1979,2025
                year_index = year_index + 1
                write(buf,'(I6)')yr
                tmp(1:num_grids)=recruit(1:num_grids)%Recruitment(year_index)
                call Write_Scalar_Field(num_grids,tmp,'Output/RecruitFieldIn'//trim(adjustl(buf))//'.txt')
        enddo
        return
    endsubroutine Set_Recruitment
        

    !---------------------------------------------------------------------------------------------------
    subroutine Random_Recruits(R, start_year, stop_year, num_grids, num_sim, domain_name)
        real(dp), intent(out) :: R(*)
        integer, intent(in)::start_year, stop_year, num_sim
        integer, intent(in)::num_grids
        character (*), intent(in):: domain_name
        real(dp), allocatable:: fishing_effort(:,:)
        real(dp) p(2), mu, sig, mus, mur
        integer num_years_int, num_sims_rand
        character(6) buf
        logical rescale
        rescale=.true.
        allocate(fishing_effort(1:num_grids, 1:num_sim))
        call random_number(p(1:2))
        num_years_int = start_year + floor( float(stop_year - start_year + 1) * p(1) )
        num_sims_rand = ceiling( float(num_sim) * p(2) )
        write(buf,'(I6)') num_years_int
        
        call Read_Scalar_Field('Input/Sim'//domain_name//'Clim'//trim(adjustl(buf))//'/KrigingEstimate.txt', &
        &            fishing_effort(1:num_grids,num_sims_rand), num_grids)
        R(1:num_grids) = fishing_effort(1:num_grids, num_sims_rand)
        ! Adjust mean to random number based on historical record of mean. log(interanual mean)~ N(mu,sig)
        if (rescale)then
            !log normal average for 1979-2018
            if (domain_name(1:2).eq.'GB')then
                mu =  -3.0198
                sig =  1.1068
            else
                mu=-3.5329
                sig=1.0036
            endif
            mus = sum(R(1:num_grids)) / float(num_grids)
            call random_number(p(1:2))
            mur = mu + sig * p(1)
            R(1:num_grids) = R(1:num_grids) * exp(mur) / mus
        endif
        deallocate(fishing_effort)
        
        return
    endsubroutine Random_Recruits
    
endmodule Recruit_Mod
