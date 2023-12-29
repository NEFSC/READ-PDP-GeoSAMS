module Output_Mod
    use globals
    use Data_Point_Mod
    implicit none

    integer, PRIVATE :: num_grids
    character(2), PRIVATE :: domain_name
    real(dp), PRIVATE :: domain_area_sqm
    integer, PRIVATE :: start_year
    integer, PRIVATE :: stop_year
    integer, PRIVATE :: num_time_steps
    integer, PRIVATE :: ts_per_year


    CONTAINS

    subroutine Set_Scallop_Output( n_grids,reg_name,reg_area_sqm,start_yr,stop_yr,n_time_steps, ts_p_yr)
        integer, intent(in) :: n_grids
        character(2), intent(in) :: reg_name
        real(dp), intent(in) :: reg_area_sqm
        integer, intent(in) :: start_yr
        integer, intent(in) :: stop_yr
        integer, intent(in) :: n_time_steps, ts_p_yr

        !> initalize private members
        num_grids = n_grids
        domain_name = reg_name
        domain_area_sqm = reg_area_sqm
        start_year = start_yr
        stop_year = stop_yr
        num_time_steps = n_time_steps
        ts_per_year = ts_p_yr
        return
    endsubroutine Set_Scallop_Output
    
!     subroutine Scallop_Output_Annual(year, samp, fishing_effort, weight_grams, mortality, recruit, grid)
!         use Mortality_Mod, only : Mortality_Class, Compute_Natural_Mortality, Scallops_To_Counts, Dollars_Per_SqM
!         use Recruit_Mod, only : Recruitment_Class
!         use Data_Point_Mod, only : num_dimensions
!         implicit none
!         integer, intent(in):: year
!         real(dp), intent(in):: fishing_effort(*), weight_grams(num_dimensions,*), samp(num_dimensions,*)
!         type(Mortality_Class), INTENT(INOUT):: mortality(*)
!         type(Recruitment_Class), INTENT(IN):: recruit(*)
!         type(Data_Point_Class), intent(in) :: grid(*)
!         real(dp) cnts(num_grids,4)
!         integer n
!         real(dp) BMS(num_grids), ExplBMS(num_grids), Abundance(num_grids), Dollars_Per_SqMeter(num_grids)
!         real(dp) StateCapt(num_grids) !  , Recs(num_grids, num_size_classes)
!         real(dp) NatMort(num_grids, num_size_classes), FishMort(num_grids, num_size_classes)
!         real(dp) TotalMort(num_grids, num_size_classes), Fslct(num_grids, num_size_classes)
!         character(4) buf

!         do n = 1, num_grids
!             Fslct(n,1:num_size_classes) = mortality(n)%selectivity(1:num_size_classes)
!             BMS(n) = sum( samp(n,1:num_size_classes) * weight_grams(n,1:num_size_classes)/(10.**6) )
!             ExplBMS(n) = sum( mortality(n)%selectivity(1:num_size_classes) * samp(n,1:num_size_classes) &
!             &            * weight_grams(n,1:num_size_classes)/(10.**6) )
!             Abundance(n) = sum( samp(n,1:num_size_classes) )
!             StateCapt(n) = sum(samp(n,1:num_size_classes) * mortality(n)%selectivity(1:num_size_classes))
!             mortality(n)%natural_mortality(1:num_size_classes) = &
!             &           Compute_Natural_Mortality(recruit(n)%max_rec_ind, mortality(n), samp(n,1:num_size_classes))
!             NatMort(n,1:num_size_classes) = mortality(n)%natural_mortality(1:num_size_classes)
!             FishMort(n,1:num_size_classes) = fishing_effort(n) * mortality(n)%selectivity(1:num_size_classes)
!             call Scallops_To_Counts(weight_grams(n,1:num_size_classes), cnts(n,1), cnts(n,2), cnts(n,3), cnts(n,4))
!             Dollars_Per_SqMeter(n) = Dollars_Per_SqM(year, weight_grams(n,1:num_size_classes))
!             TotalMort(n,1:num_size_classes) = mortality(n)%natural_mortality(1:num_size_classes) &
!             &    + fishing_effort(n) * &
!             & (mortality(n)%selectivity(1:num_size_classes) + mortality(n)%incidental + mortality(n)%Discard(1:num_size_classes))
!         !  Recs(n,1:num_size_classes) = recruit(n)%RecVec(1:num_size_classes)
!         enddo
        
! write(buf,'(I4)') year
! call Write_Scalar_Field(num_grids,          Abundance,           output_dir//'Abundance'//buf//'.txt')
! call Write_Scalar_Field(num_grids,          BMS,                 output_dir//'BMS'//buf//'.txt')
! call Write_CSV(num_grids, 4,                cnts,                output_dir//'Cnts'//buf//'.csv', size(cnts,1))
! call Write_Scalar_Field(num_grids,          Dollars_Per_SqMeter, output_dir//'Dollars'//buf//'.txt')
! call Write_Scalar_Field(num_grids,          ExplBMS,             output_dir//'ExplBMS'//buf//'.txt')
! call Write_Scalar_Field(num_grids,          fishing_effort,      output_dir//'fishing_effort'//buf//'.txt')
! call Write_CSV(num_grids, num_size_classes, FishMort,            output_dir//'FishingMortality'//buf//'.csv', size(FishMort,1))
! call Write_CSV(num_grids, num_size_classes, Fslct,               output_dir//'Fslct'//buf//'.csv', size(Fslct,1))
! call Write_Scalar_Field(num_grids,          StateCapt,           output_dir//'IndvCaught'//buf//'.txt')
! call Write_CSV(num_grids, num_size_classes, NatMort,             output_dir//'NaturalMortality'//buf//'.csv', size(NatMort,1))
! call Write_CSV(num_grids, num_size_classes, Fslct,               output_dir//'Selectivity'//buf//'.csv', size(Fslct,1))
! call Write_CSV(num_grids, num_size_classes, samp,                output_dir//'State'//buf//'.csv', size(samp,1))
! call Write_CSV(num_grids, num_size_classes, TotalMort,           output_dir//'TotalMortality'//buf//'.csv', size(TotalMort,1))
! !call Write_CSV(num_grids, num_size_classes, Recs, 'RecruitInput'//buf//'.csv', size(Recs,1))
        
!         if(year.eq.start_year)then
!             open(66,file = output_dir//'CASACompTable.txt')
!             open(67,file = output_dir//'CASACompClosedTable.txt')
!             open(68,file = output_dir//'CASACompOpenTable.txt')
!             write(*,*)'     Year    Abundance       BMS     ExplBMS     FishedBMS '   
!             write(*,*)'     -----------------------------------------------'   
!         endif
        
!         write(66,*)year, grid_area_sqm * sum( Abundance(1:num_grids) ),&
!         &    grid_area_sqm * sum(BMS(1:num_grids)),&
!         &    grid_area_sqm * sum(ExplBMS(1:num_grids)),&
!         &    grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids))
!         write(67,*)year,  grid_area_sqm * sum( Abundance(1:num_grids) * Logic_To_Double( grid(1:num_grids)%is_closed )),&
!         &    grid_area_sqm * sum(BMS(1:num_grids) * Logic_To_Double(grid(1:num_grids)%is_closed)),&
!         &    grid_area_sqm * sum(ExplBMS(1:num_grids) * Logic_To_Double(grid(1:num_grids)%is_closed)),&
!         &    grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids )* &
!         &    Logic_To_Double(grid(1:num_grids)%is_closed))
!         write(68,*)year,  grid_area_sqm * sum( Abundance(1:num_grids) &
!         &    * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed)) ),&
!         &    grid_area_sqm * sum(BMS(1:num_grids) * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed))),&
!         &    grid_area_sqm * sum(ExplBMS(1:num_grids) * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed))),&
!         &    grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids) &
!         &    * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed)))
!         write(*,*)year,  floor(grid_area_sqm * sum( Abundance(1:num_grids) )/10.D0**6),&
!         &    floor(grid_area_sqm * sum(BMS(1:num_grids))),&
!         &    floor(grid_area_sqm * sum(ExplBMS(1:num_grids))),&
!         &    floor(grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids)))
        
!         if(year.eq.stop_year)then
!             close(66)
!             close(67)
!             close(68)
!         endif
        
!         return
!     endsubroutine Scallop_Output_Annual
    
    subroutine Scallop_Output_Regional_Avg(year, state_at_time_step, grid, grid_n, mid_year_sample)
        use Data_Point_Mod
        implicit none
        integer, intent(in) :: year, grid_n
        real(dp), intent(in) :: state_at_time_step(num_time_steps, num_size_classes)
        type(Data_Point_Class), intent(in) :: grid(*)
        real(dp), intent(out) :: mid_year_sample(num_size_classes)
        character(2) buf
        integer num_years, ntsX, Nregion, ntStart, ntStop, j
        real(dp), allocatable :: region_avg(:,:,:), StateTSx(:,:)
        integer, allocatable :: management_area_count(:)
        save region_avg
        Nregion = maxval(grid(1:num_grids)%mgmt_area_index)
        write(8,*)
        num_years = stop_year-start_year+1
        ntsX = num_time_steps*num_years

        allocate(region_avg(1:ntsX, 1:num_size_classes, 1:Nregion), &
        &        StateTSx(1:ntsX, 1:num_size_classes), &
        &        management_area_count(1:Nregion))

        if ((year.eq.start_year).and.(grid_n.eq.1))then
            open(56, file = output_dir//'ManAreaOut.txt')
            do j = 1, num_grids
                write(56,*)grid(j)%mgmt_area_index
            enddo
            close(56)
            region_avg(1:ntsX, 1:num_size_classes, 1:Nregion) = 0.D0
        endif
        
        ntStart = (year-start_year)*num_time_steps+1
        ntStop  = (year-start_year)*num_time_steps+num_time_steps
        region_avg(ntStart:ntStop, 1:num_size_classes, grid(grid_n)%mgmt_area_index) &
        &               = region_avg(ntStart:ntStop, 1:num_size_classes, grid(grid_n)%mgmt_area_index) &
        &                 + state_at_time_step(1:num_time_steps, 1:num_size_classes)
        if ((year.eq.stop_year).and.(grid_n.eq.num_grids))then
            ! write headings for next section
            write(*,*) '     Region #  Area Count    # non-zero '
            write(*,*) '     --------  ------------   ----------'
          
            management_area_count(1:Nregion) = 0
            do j = 1, num_grids
                management_area_count(grid(j)%mgmt_area_index) = management_area_count(grid(j)%mgmt_area_index)+1
            enddo
            do j = 1, Nregion
                StateTSx(1:ntsX, 1:num_size_classes) = region_avg(1:ntsX, 1:num_size_classes, j)/float(management_area_count(j))
                write(*, *) j, management_area_count(j), count(StateTSx/=0)
                if (j<10) then
                    write(buf, '(A,I1)') '0',j
                else
                    write(buf, '(I2)') j
                endif
                call Write_CSV(ntsX, num_size_classes, StateTSx, output_dir//'ManagementRegion'//buf//'.csv', size(StateTSx,1))
            enddo
            write(*,*) '----------------------------------------------'
        endif

        ! sample mid calander year for output
        mid_year_sample(1:num_size_classes) = state_at_time_step(floor(float(num_time_steps)/2.), 1:num_size_classes)

        deallocate( region_avg, StateTSx, management_area_count)
        
        return
    endsubroutine Scallop_Output_Regional_Avg

    !==================================================================================================================
    !> @public @memberof Growth_Class
    !>
    !> Initializes growth for startup
    !>
    !==================================================================================================================
    subroutine Scallop_Output_At_Timestep(year, ts, state, weight_grams, landings_by_num)!, fishing_effort, weight_grams, mortality, recruit, grid)
        implicit none
        integer, intent(in) :: year, ts
        real(dp), intent(in) :: state(num_dimensions, num_size_classes)
        real(dp), intent(in) :: weight_grams(num_dimensions, num_size_classes)
        real(dp), intent(in) :: landings_by_num(*)

        integer loc
        character(9) buf
        real(dp), allocatable :: abundance(:)
        real(dp), allocatable :: bms(:)

        ! initial state is not useful
        if (ts .eq. 0) return

        allocate(abundance(num_grids))
        allocate(bms(num_grids))

        do loc = 1, num_grids
            abundance(loc) = sum(state(loc,1:num_size_classes))
            bms(loc) = dot_product(state(loc,1:num_size_classes), &
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
            ! TotalMort(loc,1:num_size_classes) = mortality(loc)%natural_mortality(1:num_size_classes) &
            ! &    + fishing_effort(loc) * &
            ! & (mortality(loc)%selectivity(1:num_size_classes) + mortality(loc)%incidental + mortality(loc)%Discard(1:num_size_classes))
        enddo

write(buf,'(A1,I4,A1,I0.3)') '_',year, '_', ts
call Write_Scalar_Field(num_grids,          abundance,           output_dir//'Abundance'//buf//'.txt')
call Write_Scalar_Field(num_grids,          bms,                 output_dir//'BMS'//buf//'.txt')
call Write_Scalar_Field(num_grids,          landings_by_num,     output_dir//'NumLandings'//buf//'.txt')
! call Write_CSV(num_grids, 4,                cnts,                output_dir//'Cnts'//buf//'.csv', size(cnts,1))
! call Write_Scalar_Field(num_grids,          Dollars_Per_SqMeter, output_dir//'Dollars'//buf//'.txt')
! call Write_Scalar_Field(num_grids,          ExplBMS,             output_dir//'ExplBMS'//buf//'.txt')
! call Write_Scalar_Field(num_grids,          fishing_effort,      output_dir//'fishing_effort'//buf//'.txt')
! call Write_CSV(num_grids, num_size_classes, FishMort,            output_dir//'FishingMortality'//buf//'.csv', size(FishMort,1))
! call Write_CSV(num_grids, num_size_classes, Fslct,               output_dir//'Fslct'//buf//'.csv', size(Fslct,1))
! call Write_Scalar_Field(num_grids,          StateCapt,           output_dir//'IndvCaught'//buf//'.txt')
! call Write_CSV(num_grids, num_size_classes, NatMort,             output_dir//'NaturalMortality'//buf//'.csv', size(NatMort,1))
! call Write_CSV(num_grids, num_size_classes, Fslct,               output_dir//'Selectivity'//buf//'.csv', size(Fslct,1))
! call Write_CSV(num_grids, num_size_classes, samp,                output_dir//'State'//buf//'.csv', size(samp,1))
! call Write_CSV(num_grids, num_size_classes, TotalMort,           output_dir//'TotalMortality'//buf//'.csv', size(TotalMort,1))
!call Write_CSV(num_grids, num_size_classes, Recs, 'RecruitInput'//buf//'.csv', size(Recs,1))
        
        ! if(year.eq.start_year)then
        !     open(66,file = output_dir//'CASACompTable.txt')
        !     open(67,file = output_dir//'CASACompClosedTable.txt')
        !     open(68,file = output_dir//'CASACompOpenTable.txt')
        !     write(*,*)'     Year    Abundance       BMS     ExplBMS     FishedBMS '   
        !     write(*,*)'     -----------------------------------------------'   
        ! endif
        
        ! write(66,*)year, grid_area_sqm * sum( Abundance(1:num_grids) ),&
        ! &    grid_area_sqm * sum(BMS(1:num_grids)),&
        ! &    grid_area_sqm * sum(ExplBMS(1:num_grids)),&
        ! &    grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids))
        ! write(67,*)year,  grid_area_sqm * sum( Abundance(1:num_grids) * Logic_To_Double( grid(1:num_grids)%is_closed )),&
        ! &    grid_area_sqm * sum(BMS(1:num_grids) * Logic_To_Double(grid(1:num_grids)%is_closed)),&
        ! &    grid_area_sqm * sum(ExplBMS(1:num_grids) * Logic_To_Double(grid(1:num_grids)%is_closed)),&
        ! &    grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids )* &
        ! &    Logic_To_Double(grid(1:num_grids)%is_closed))
        ! write(68,*)year,  grid_area_sqm * sum( Abundance(1:num_grids) &
        ! &    * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed)) ),&
        ! &    grid_area_sqm * sum(BMS(1:num_grids) * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed))),&
        ! &    grid_area_sqm * sum(ExplBMS(1:num_grids) * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed))),&
        ! &    grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids) &
        ! &    * (Logic_To_Double(.NOT. grid(1:num_grids)%is_closed)))
        ! write(*,*)year,  floor(grid_area_sqm * sum( Abundance(1:num_grids) )/10.D0**6),&
        ! &    floor(grid_area_sqm * sum(BMS(1:num_grids))),&
        ! &    floor(grid_area_sqm * sum(ExplBMS(1:num_grids))),&
        ! &    floor(grid_area_sqm * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids)))
        
        ! if(year.eq.stop_year)then
        !     close(66)
        !     close(67)
        !     close(68)
        ! endif
        
        deallocate(abundance)
        deallocate(bms)
        return
    endsubroutine Scallop_Output_At_Timestep
    


    endmodule Output_Mod
