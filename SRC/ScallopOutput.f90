module Output_Mod
    use globals
    implicit none
    character(*), parameter :: output_dir = 'Results/'

    CONTAINS

    subroutine Scallop_Output(year, num_size_classes, num_grids, samp, fishing_effort, weight_grams, mortality, recruit, &
        &                  start_year, stop_year, element_area)
        use Mortality_Mod
        use Recruit_Mod, only : Recruitment_Class, Mortality_Density_Dependent
        implicit none
        integer, intent(in):: num_size_classes, year, num_grids, start_year, stop_year
        real(dp), intent(in):: fishing_effort(*), weight_grams(num_grids,*), samp(num_grids,*), element_area
        type(Mortality_Class), INTENT(INOUT):: mortality(*)
        type(Recruitment_Class), INTENT(IN):: recruit(*)
        real(dp) cnts(num_grids,4)
        integer n
        real(dp) BMS(num_grids), ExplBMS(num_grids), Abundance(num_grids), DollarsPerSqM(num_grids)
        real(dp) StateCapt(num_grids) !  , Recs(num_grids, num_size_classes)
        real(dp) NatMort(num_grids, num_size_classes), FishMort(num_grids, num_size_classes)
        real(dp) TotalMort(num_grids, num_size_classes), Fslct(num_grids, num_size_classes)
        character(72) buf

        do n = 1, num_grids
            Fslct(n,1:num_size_classes) = mortality(n)%select(1:num_size_classes)
            BMS(n) = sum( samp(n,1:num_size_classes) * weight_grams(n,1:num_size_classes)/(10.**6) )
            ExplBMS(n) = sum( mortality(n)%select(1:num_size_classes) * samp(n,1:num_size_classes) &
            &            * weight_grams(n,1:num_size_classes)/(10.**6) )
            Abundance(n) = sum( samp(n,1:num_size_classes) )
            StateCapt(n) = sum(samp(n,1:num_size_classes) * mortality(n)%select(1:num_size_classes))
            call Mortality_Density_Dependent(recruit(n), mortality(n), samp(n,1:num_size_classes))
            NatMort(n,1:num_size_classes) = mortality(n)%natural_mortality(1:num_size_classes)
            FishMort(n,1:num_size_classes) = fishing_effort(n) * mortality(n)%select(1:num_size_classes)
            call Scallops_To_Counts(samp(n,1:num_size_classes) * mortality(n)%select(1:num_size_classes), &
            &     weight_grams(n,1:num_size_classes), cnts(n,1), cnts(n,2), cnts(n,3), cnts(n,4))
            call Cash_Money(year, samp(n,1:num_size_classes) * mortality(n)%select(1:num_size_classes), &
            &     weight_grams(n,1:num_size_classes), DollarsPerSqM(n))
            TotalMort(n,1:num_size_classes) = mortality(n)%natural_mortality(1:num_size_classes)+fishing_effort(n) * &
            &    ( mortality(n)%select(1:num_size_classes)+ mortality(n)%incidental + mortality(n)%Discard(1:num_size_classes) )
        !  Recs(n,1:num_size_classes) = recruit(n)%RecVec(1:num_size_classes)
        enddo
        
        write(buf, '(I6)')year
        call Write_CSV(num_grids, num_size_classes, samp, output_dir//'State'//trim(adjustl(buf))//'.csv', num_grids)
        call Write_CSV(num_grids, num_size_classes, TotalMort, output_dir//'TotalMortality'//trim(adjustl(buf))//'.csv', num_grids)
        call Write_CSV(num_grids, num_size_classes, NatMort, output_dir//'NaturalMortality'//trim(adjustl(buf))//'.csv', num_grids)
        !call Write_CSV(num_grids, num_size_classes, Recs, 'RecruitInput'//trim(adjustl(buf))//'.csv', num_grids)
        call Write_CSV(num_grids, num_size_classes, FishMort, output_dir//'FishingMortality'//trim(adjustl(buf))//'.csv', num_grids)
        call Write_CSV(num_grids, num_size_classes, Fslct, output_dir//'Selectivity'//trim(adjustl(buf))//'.csv', num_grids)
        call Write_CSV(num_grids, 4, cnts, output_dir//'Cnts'//trim(adjustl(buf))//'.csv', num_grids)
        call Write_Scalar_Field(num_grids, DollarsPerSqM, output_dir//'Dollars'//trim(adjustl(buf))//'.txt')
        call Write_Scalar_Field(num_grids, fishing_effort, output_dir//'fishing_effort'//trim(adjustl(buf))//'.txt')
        call Write_Scalar_Field(num_grids, BMS, output_dir//'BMS'//trim(adjustl(buf))//'.txt')
        call Write_Scalar_Field(num_grids, ExplBMS, output_dir//'ExplBMS'//trim(adjustl(buf))//'.txt')
        call Write_Scalar_Field(num_grids, Abundance, output_dir//'Abundance'//trim(adjustl(buf))//'.txt')
        call Write_Scalar_Field(num_grids, StateCapt, output_dir//'IndvCaught'//trim(adjustl(buf))//'.txt')
        do n = 1, num_grids
            Fslct(n, 1:num_size_classes) = mortality(n)%select(1:num_size_classes)
        enddo
        call Write_CSV(num_grids, num_size_classes, Fslct, output_dir//'Fslct'//trim(adjustl(buf))//'.csv', num_grids)
        
        
        if(year.eq.start_year)then
            open(66,file = output_dir//'CASACompTable.txt')
            open(67,file = output_dir//'CASACompClosedTable.txt')
            open(68,file = output_dir//'CASACompOpenTable.txt')
            write(*,*)'     Year    Abundance       BMS     ExplBMS     FishedBMS '   
            write(*,*)'     -----------------------------------------------'   
        endif
        
        write(66,*)year, element_area * sum( Abundance(1:num_grids) ),&
        &    element_area * sum(BMS(1:num_grids)),&
        &    element_area * sum(ExplBMS(1:num_grids)),&
        &    element_area * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids))
        write(67,*)year,  element_area * sum( Abundance(1:num_grids) * Logic_To_Double( mortality(1:num_grids)%is_closed )),&
        &    element_area * sum(BMS(1:num_grids) * Logic_To_Double(mortality(1:num_grids)%is_closed)),&
        &    element_area * sum(ExplBMS(1:num_grids) * Logic_To_Double(mortality(1:num_grids)%is_closed)),&
        &    element_area * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids )* &
        &    Logic_To_Double(mortality(1:num_grids)%is_closed))
        write(68,*)year,  element_area * sum( Abundance(1:num_grids) &
        &    * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed)) ),&
        &    element_area * sum(BMS(1:num_grids) * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed))),&
        &    element_area * sum(ExplBMS(1:num_grids) * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed))),&
        &    element_area * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids) &
        &    * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed)))
        write(*,*)year,  floor(element_area * sum( Abundance(1:num_grids) )/10.D0**6),&
        &    floor(element_area * sum(BMS(1:num_grids))),&
        &    floor(element_area * sum(ExplBMS(1:num_grids))),&
        &    floor(element_area * sum(fishing_effort(1:num_grids)*ExplBMS(1:num_grids)))
        
        
        if(year.eq.stop_year)then
            close(66)
            close(67)
            close(68)
        endif
        
        return
    endsubroutine Scallop_Output
    

    subroutine Scallop_Output_Region(year, start_year, stop_year, num_size_classes, num_grids, state_time_steps, &
        &                          num_time_steps, grid, n)
        use Data_Point_Mod
        implicit none
        integer, intent(in) :: num_size_classes, year, num_grids, start_year, stop_year, num_time_steps, n
        real(dp), intent(in) :: state_time_steps(num_time_steps, *)
        type(Data_Vector_Class), intent(in) :: grid
        character(72) buf
        integer num_years, ntsX, Nregion, ntStart, ntStop, j
        real(dp), allocatable :: region_avg(:,:,:), StateTSx(:,:)
        integer, allocatable :: management_area_count(:)
        save region_avg
        Nregion = maxval(grid%posn(1:num_grids)%mgmt_area_index)
        write(8,*)
        num_years = stop_year-start_year+1
        ntsX = num_time_steps*num_years
        
        if ((year.eq.start_year).and.(n.eq.1))then
            open(56, file = output_dir//'ManAreaOut.txt')
            do j = 1, num_grids
                write(56,*)grid%posn(j)%mgmt_area_index
            enddo
            close(56)
            allocate(region_avg(1:ntsX, 1:num_size_classes, 1:Nregion))
            region_avg(1:ntsX, 1:num_size_classes, 1:Nregion) = 0.D0
        endif
        
        ntStart = (year-start_year)*num_time_steps+1
        ntStop  = (year-start_year)*num_time_steps+num_time_steps
        !do n = 1, num_grids
            region_avg(ntStart:ntStop, 1:num_size_classes, grid%posn(n)%mgmt_area_index) &
            &               = region_avg(ntStart:ntStop, 1:num_size_classes, grid%posn(n)%mgmt_area_index) &
            &                 + state_time_steps(1:num_time_steps, 1:num_size_classes)
        !enddo
        if ((year.eq.stop_year).and.(n.eq.num_grids))then
            allocate(StateTSx(1:ntsX, 1:num_size_classes), management_area_count(1:Nregion))
            management_area_count(1:Nregion) = 0
            do j = 1, num_grids
                management_area_count(grid%posn(j)%mgmt_area_index) = management_area_count(grid%posn(j)%mgmt_area_index)+1
            enddo
            do j = 1, Nregion
                write(*, *)'j = ', j, management_area_count(j)
                StateTSx(1:ntsX, 1:num_size_classes) = region_avg(1:ntsX, 1:num_size_classes, j)/float(management_area_count(j))
                write(buf, '(I6)') j
                call Write_CSV(ntsX, num_size_classes, StateTSx, output_dir//'ManagementRegion'//trim(adjustl(buf))//'.csv', ntsX)
            enddo
            deallocate( region_avg, StateTSx, management_area_count)
        endif
        
        return
    endsubroutine Scallop_Output_Region
    
endmodule Output_Mod
