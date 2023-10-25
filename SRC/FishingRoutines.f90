


!> @brief Short description
!! 
!! Longer description
subroutine Set_Fishing(fishing_type, domain_name, year, num_grids, state, weight_grams, mortality, fishing_effort, element_area)
    use globals
    use Mortality_Mod
    implicit none
    integer, intent(in):: num_grids, year
    real(dp), intent(in):: state(num_grids,  * ), weight_grams(num_grids,  * ), element_area
    type(Mortality_Struct), INTENT(IN):: mortality( * )
    real(dp), intent(out):: fishing_effort( * )
    character( * ), intent(in):: fishing_type, domain_name !> < string inputs
    integer num_size_classes, Mindx, n
    real(dp) catch_open, catch_closed, TotalCatch
    num_size_classes = mortality(1)%num_size_classes
    call Get_Total_Catch(year, domain_name, catch_closed, catch_open)
    TotalCatch = catch_open+catch_closed

    Mindx = minloc( abs(mortality(1)%year(1:mortality(1)%num_years) - year ), 1)
    select case (fishing_type(1:3))
        case('USD')
            if(domain_name(1:2).eq.'GB')&
                & call Set_Fish_Effort_Wgt_USD_CLOP(year, mortality, catch_open, catch_closed, fishing_effort, state, &
                &    weight_grams, num_grids, element_area)
            if(domain_name(1:2).eq.'MA')&
                &  call Set_Fishing_Effort_Weight_USD(year, mortality, TotalCatch, fishing_effort, state, weight_grams, num_grids, &
                &    element_area)
        case('BMS')
            if(domain_name(1:2).eq.'GB')&
                &  call Set_Fish_Effort_Wgt_X_CLOP(mortality, catch_open, catch_closed, fishing_effort, state, &
                &    weight_grams(1:num_grids, 1:num_size_classes)/10.**6, num_grids, element_area)
            if(domain_name(1:2).eq.'MA')&
                &  call Set_Fish_Effort_Wgt_X(mortality, TotalCatch, fishing_effort, state, & 
                &    weight_grams(1:num_grids, 1:num_size_classes)/10.**6, num_grids, element_area, 1.D0, 1.D0)
        case('CAS')
            fishing_effort(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
        case default
            fishing_effort(1:num_grids) = mortality(1:num_grids)%fishing_effort(Mindx)!CASA Fishing
            write( * ,  * )'Unkown fishing fishing_type:', fishing_type, &
            &    '.  Using spatially constant fishing_effort from CASA model'
    end select
    !enforce no fishing on Closed Area 2 North
    do n = 1, num_grids
        if ((domain_name(1:2).eq.'GB').and.(mortality(n)%management_area.eq.6)) fishing_effort(n) = 0.D0
    enddo

    return
endsubroutine Set_Fishing

!>
!! @brief Compute value of scallop population.
!!
!! Value is based on population structure. 
!! The population is sorted into size count bucket classes U10, 10-20, 20-30, 30+ 
!! and the value based on these classes and the year is read from the file 
!! "Data/ScallopPrice.csv".
!!
!!
!! @param[in] year	- current year
!! @param[in] num_size_classes	- number of size classes
!! @param[in] scallops_per_sqm	[num_size_classes] - Scallop Population density by shell height size class
!! @param[in] meat_weight_grams	[num_size_classes] - Weight meat per individual scallop in each size class
!!
!! @author Keston Smith 2022
!---------------------------------------------------------------------------------------------------


!Hi Keston. Attached are landings (metric tons), value (thousand $) and price ($/lb) from 1998 to 2021. 
!The market categories are given in 
!NESPP4: 8002 = U10, 8003 = 10-20, 8004 = 20-30, 8005 = 30-40, 8006 = 40-50, 8007 = 50-60, 8008 = 60+, 8009 = unclassified
subroutine Cash_Money(year, num_size_classes, scallops_per_sqm, meat_weight_grams, dollars_per_sqm)
    use globals
    implicit none
    real(dp), intent(in):: scallops_per_sqm( * ), meat_weight_grams( * )
    integer, intent(in):: num_size_classes, year
    real(dp), intent(out):: dollars_per_sqm
    real(dp) cnt10, cnt10to20, cnt20to30, cnt30plus
    real(dp) ScallopPrice(50, 5);
    integer NPriceYears, indx
    real, parameter :: GramsPerPound = 453.592

    !'"decmal year", "U10", "10-20", "20-30", "30+"';
    call Read_CSV(NPriceYears, 5, 'Data/ScallopPrice.csv', ScallopPrice, 50)
    call Scallops_To_Counts(scallops_per_sqm, meat_weight_grams, num_size_classes, cnt10, cnt10to20, cnt20to30, cnt30plus)
    indx = minloc( abs( float(year)-ScallopPrice(1:NPriceYears, 1)  ), 1  )
    dollars_per_sqm = cnt10 * ScallopPrice(indx, 2)+cnt10to20 * ScallopPrice(indx, 3)+&
                cnt20to30 * ScallopPrice(indx, 4)+cnt30plus * ScallopPrice(indx, 5)
    return
endsubroutine Cash_Money

!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!subroutine Scallops_To_Counts(scallops_per_sqm, meat_weight_grams, num_size_classes, cnt10, cnt10to20, cnt20to30, cnt30plus)
! Purpose: Convert Scallop density by shell height and meat wieght to count data.  The count data 
! are divided into 
! cnt10- 10 or less scallops per pound.
! cnt10to20 10-20 scallops per pound.
! cnt20to30 20-30 scallops per pound.
! cnt30+ 30 or more scallops per pound.
!
! Inputs
!   scallops_per_sqm (real) length num_size_classes vector of scallops by size class
!   meat_weight_grams (real) length num_size_classes vector of weight of individual scallops by size class
!   num_size_classes (integer) Number of size classes
!
! Outputs
!   cnt10 number of scallops wich get binned into U10
!   cnt10to20 number of scallops wich get binned into U10-20
!   cnt20to30 number of scallops wich get binned into U20-30
!   cnt30 number of scallops wich get binned into U30+
!---------------------------------------------------------------------------------------------------
subroutine Scallops_To_Counts(scallops_per_sqm, meat_weight_grams, num_size_classes, cnt10, cnt10to20, cnt20to30, cnt30plus)
    use globals
    implicit none
    real(dp), intent(in):: scallops_per_sqm( * ), meat_weight_grams( * )
    integer, intent(in):: num_size_classes
    real(dp), intent(out):: cnt10, cnt10to20, cnt20to30, cnt30plus
    real(dp) PoundsPerScallop(num_size_classes)
    integer j
    real, parameter :: GramsPerPound = 453.592
    PoundsPerScallop(1:num_size_classes) = meat_weight_grams(1:num_size_classes)/GramsPerPound
    cnt10 = 0.
    cnt10to20 = 0.
    cnt20to30 = 0.
    cnt30plus = 0.
    do j = 1, num_size_classes
        if( PoundsPerScallop(j).gt.1./10. )cnt10 = cnt10+scallops_per_sqm(j)
        if((PoundsPerScallop(j).le.1./10.).and.(PoundsPerScallop(j).gt.1./20.))&
            cnt10to20 = cnt10to20+scallops_per_sqm(j)
        if((PoundsPerScallop(j).le.1./20.).and.(PoundsPerScallop(j).gt.1./30.))&
            cnt20to30 = cnt20to30+scallops_per_sqm(j)
        if(PoundsPerScallop(j).le.1./30.)cnt30plus = cnt30plus+scallops_per_sqm(j)
    enddo

    return
endsubroutine Scallops_To_Counts
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
! subroutine Set_Fish_Effort_Wgt_USD_CLOP(year, mortality, catch_open, catch_closed, fishing_effort, state, weight_grams, num_grids, element_area)
! Purpose: Set proportional rate of fishing mortality, fishing_effort, based on monetary value of catch. The at each grid
! point scallops are sorted into size bins U10.10-20, 20-30, 30+.  The price for each size class is loaded 
! for the year and the monatary value of scallops is determined at each grid point.  Fishing effort is 
! to be porportional to the value of scallops.  This version has seperate catches for closed and open areas. 
! Inputs:
!   year (integer) current year
!   mortality (mortality mod) see main program
!   catch_open (real(dp)) Total catch in open areas in Metric Tons in year
!   catch_closed (real(dp)) Catch in closed areas in Metric Tons in year
!   state (real(dp))[num_grids x num_size_classes] Scallop population by size class
!   element_area (real(dp)) grid cell area
!   weight_grams (real(dp)) Weight of scallop by node and size class
!
! Output:
!   fishing_effort (real(dp)) [num_grids] rate of fishing mortality
!---------------------------------------------------------------------------------------------------
subroutine Set_Fish_Effort_Wgt_USD_CLOP(year, mortality, catch_open, catch_closed, fishing_effort, state, &
    &                  weight_grams, num_grids, element_area)
    use globals
    use Mortality_Mod
    implicit none
    integer num_grids, j, num_size_classes
    real(dp), intent(in):: catch_open, catch_closed, element_area, weight_grams(num_grids,  * ), state(num_grids,  * )
    integer, intent(in):: year
    real(dp), intent(inout):: fishing_effort( * )
    type(Mortality_Struct), INTENT(IN):: mortality( * )
    real(dp) lambda, USD(num_grids), SWI(num_grids), Cclosed, Copen, Sopen, Sclosed
    lambda = 1.0
    num_size_classes = mortality(1)%num_size_classes
    do j = 1, num_grids
        call Cash_Money(year, num_size_classes, mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes), &
        &              weight_grams(j, 1:num_size_classes), USD(j))    
        SWI(j) = sum( mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes)  *  &
        &             weight_grams(j, 1:num_size_classes)/(10.D0**6) )
    enddo
    Sopen = sum( USD(1:num_grids)  *  SWI(1:num_grids)  * element_area &
    &       * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed)) )
    Sclosed = sum( USD(1:num_grids)  *  SWI(1:num_grids)  * element_area &
    &       * Logic_To_Double( mortality(1:num_grids)%is_closed) )
    if(Sopen.le.0.)Sopen = 1.
    if(Sclosed.le.0.)Sclosed = 1.
    Cclosed = catch_closed / Sclosed
    Copen = catch_open / Sopen
    do j = 1, num_grids
        if(mortality(j)%is_closed) then
            fishing_effort(j) = Cclosed  *  USD(j) 
        else 
            fishing_effort(j) = Copen  *  USD(j) 
        endif
    enddo
    return
endsubroutine Set_Fish_Effort_Wgt_USD_CLOP

!---------------------------------------------------------------------------------------------------
! subroutine Set_Fishing_Effort_Weight_USD(year, mortality, Catch, fishing_effort, state, weight_grams, num_grids, element_area)
! Purpose: Set proportional rate of fishing mortality, fishing_effort, based on monetary value of catch. The at each grid
! point scallops are sorted into size bins U10.10-20, 20-30, 30+.  The price for each size class is loaded 
! for the year and the monatary value of scallops is determined at each grid point.  Fishing effort is 
! to be porportional to the value of scallops.  
! Inputs:
!   year (integer) current year
!   mortality (mortality mod) see main program
!   Catch (real(dp)) Total catch in Metric Tons in year
!   state (real(dp))[num_grids x num_size_classes] Scallop population by size class
!   element_area (real(dp)) grid cell area
!   weight_grams (real(dp)) Weight of scallop by node and size class
!
! Output:
!   fishing_effort (real(dp)) [num_grids] rate of fishing mortality
!---------------------------------------------------------------------------------------------------
subroutine Set_Fishing_Effort_Weight_USD(year, mortality, Catch, fishing_effort, state, weight_grams, num_grids, element_area)
    use globals
    use Mortality_Mod
    implicit none
    integer num_grids, j, num_size_classes
    real(dp), intent(in):: Catch, element_area, weight_grams(num_grids,  * ), state(num_grids,  * )
    integer, intent(in):: year
    real(dp), intent(inout):: fishing_effort( * )
    type(Mortality_Struct), INTENT(IN):: mortality( * )
    real(dp) lambda, USD(num_grids), SWI(num_grids), C
    lambda = 1.0
    num_size_classes = mortality(1)%num_size_classes
    do j = 1, num_grids
        call Cash_Money(year, num_size_classes, mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes), &
        &               weight_grams(j, 1:num_size_classes), USD(j))    
        SWI(j) = sum( mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes)  *  &
        &             weight_grams(j, 1:num_size_classes)/(10.D0**6) )
    enddo
    C = Catch / sum(   USD(1:num_grids)  *  SWI(1:num_grids)  * element_area   )
    fishing_effort(1:num_grids) = C  *  USD(1:num_grids) 
    return
endsubroutine Set_Fishing_Effort_Weight_USD

subroutine Set_Fish_Effort_Wgt_X_CLOP(mortality, catch_open, catch_closed, fishing_effort, state, W, num_grids, &
    &                                      element_area)
    use globals
    use Mortality_Mod

    implicit none
    integer num_grids, j, num_size_classes
    real(dp), intent(in):: state(num_grids,  * ), W(num_grids,  * ), catch_open, catch_closed, element_area
    real(dp), intent(inout):: fishing_effort( * )
    type(Mortality_Struct), INTENT(IN):: mortality( * )
    real(dp)SI(num_grids), SWI(num_grids)
    real(dp) SIclosed, SWIclosed, SIopen, SWIopen, Cclosed, Copen
    num_size_classes = mortality(1)%num_size_classes

    do j = 1, num_grids
        SI(j) = sum( mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes) )
        if(SI(j).le.0.)SI(j) = 1.D0
        SWI(j) = sum( mortality(j)%select(1:num_size_classes) * W(j, 1:num_size_classes) * state(j, 1:num_size_classes) )
    enddo


    SWIopen = sum(  SWI(1:num_grids)  * element_area * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed))  )
    SWIclosed = sum( SWI(1:num_grids)  * element_area * Logic_To_Double( mortality(1:num_grids)%is_closed)  )
    SIopen = sum(  SI(1:num_grids)  * element_area * (1._dp - Logic_To_Double(mortality(1:num_grids)%is_closed))  )
    SIclosed = sum( SI(1:num_grids)  * element_area * Logic_To_Double( mortality(1:num_grids)%is_closed)  )

    if(SIopen.le.0.)SIopen = 1.
    if(SIclosed.le.0.)SIclosed = 1.
    if(SWIopen.le.0.)SWIopen = 1.
    if(SWIclosed.le.0.)SWIclosed = 1.

    Cclosed = catch_closed / SWIclosed
    Copen = catch_open / SWIopen
    do j = 1, num_grids
        if(mortality(j)%is_closed) then
            fishing_effort(j) = Cclosed  *  SWI(j) 
        else 
            fishing_effort(j) = Copen  *  SWI(j)
        endif
    enddo

    return
endsubroutine Set_Fish_Effort_Wgt_X_CLOP


subroutine Set_Fish_Effort_Wgt_X(mortality, Catch, fishing_effort, state, W, num_grids, element_area, alpha, beta)
    use globals
    use Mortality_Mod

    implicit none
    integer num_grids, j, num_size_classes
    real(dp), intent(in):: state(num_grids,  * ), W(num_grids,  * ), Catch, element_area, alpha, beta
    real(dp), intent(inout):: fishing_effort( * )
    type(Mortality_Struct), INTENT(IN):: mortality( * )
    real(dp) SI(num_grids), SWI(num_grids)
    num_size_classes = mortality(1)%num_size_classes

    do j = 1, num_grids
        SI(j) = sum( mortality(j)%select(1:num_size_classes) * state(j, 1:num_size_classes) )
        if(SI(j).le.0.)SI(j) = 1.D0
        SWI(j) = sum( mortality(j)%select(1:num_size_classes) * W(j, 1:num_size_classes) * state(j, 1:num_size_classes) )
    enddo

    fishing_effort(1:num_grids) = SWI(1:num_grids)**alpha  / SI(1:num_grids)**beta
    fishing_effort(1:num_grids) = fishing_effort(1:num_grids)  *  Catch / &
    &         sum( element_area  *  fishing_effort(1:num_grids) * SWI(1:num_grids) )

    return
endsubroutine Set_Fish_Effort_Wgt_X

subroutine Get_Total_Catch(year,domain_name,total_catch_closed,total_catch_open)
    use globals
    use Data_Point_Mod 
    implicit none
    integer num_years_max
    parameter (num_years_max=50)
    character(2), intent(in):: domain_name
    integer, intent(in):: year 
    real(dp), intent(out):: total_catch_open,total_catch_closed
    real(dp), save ::  M(num_years_max,4)
    logical, save :: FirstCall = .true.
    integer, save :: YrIndx = 1
    integer, save :: num_years = num_years_max
    !write(*,*)'GTC',FirstCall,YrIndx,num_years,YrIndx
    if (FirstCall) then
        call Read_CSV(num_years,4,'Data/Landings_75-19nh.csv',M,num_years_max)!regional landings in metric tons
        FirstCall = .false.
    endif
    
    do while (floor(M(YrIndx,1)).lt.year )
        YrIndx=YrIndx+1
    enddo
    if(YrIndx.gt.num_years)YrIndx=num_years
    select case (domain_name)
        case('GB')
            total_catch_closed=M(YrIndx,2)
            total_catch_open=M(YrIndx,3)
        case('MA')
            total_catch_closed=0.D0
            total_catch_open=M(YrIndx,4)
        case default
            write(*,*)'Unkown domain name', domain_name
    end select
    
    return
endsubroutine Get_Total_Catch
    
