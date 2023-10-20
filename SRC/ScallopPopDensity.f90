PROGRAM ScallopPopDensity
    !> @mainpage Scallop Popupation Density
    !! This program is used to compute Scallop Density after a given growth period
    !!
    !! Steps
    !!  - I. Read Configuration file 'Scallop.inp'
    !!      - A. Get Domain Name
    !!      - B. Get Start Year
    !!      - C. Get End Year
    !!      - D. Time steps per Year
    !!      - E. Inintial Conditions as read from Output/SimMA2000/InitialCondition.csv
    !!      - F. Fishing Type: can be USD, BMS, or CAS
    !!      - G. Parameters for Recruitment 
    !!          - i.   Start Day
    !!          - ii.  Finish Day
    !!          - iii. Random recruit start year
    !!          - iv.  Number of random fields
    !!      - H. Parameters for Monte Carlo
    !!          - i.   Length
    !!          - ii.  Percentile values
    !!      - I. Expected Output Parameters
    !!      .
    !!  - II. Load Grid. 
    !!        Load grid coordinates and bathymetric depth from CSV file with 5 columns
    !!        representing an x coordinate, y coordinate, bathymetric depth (z), latitude, and longitude.
    !!
    !!  - III. Set Growth parameters and initial conditions


    use Growth_Mod
    use Data_Point_Mod
    use Recruit_Mod

    implicit none

    integer n, j  !> loop count

    character(2) domain_name
    integer start_year, stop_year
    logical is_rand_recr
    character(3) fishing_type      !> Fishing can be USD,  BMS,  or,  CAS
    integer num_time_steps
    integer num_monte_carlo_iter
    character(72) file_name
    real(dp) delta_time

    type(Data_Point):: grid
    real(dp) element_area, domain_area
    integer num_grids

    type(Growth_Struct), allocatable :: growth(:)
    !type(MortalityStruct), allocatable :: mortality(:)
    type(Recruit_Struct), allocatable :: recruit(:)

    integer, parameter :: num_size_classes = (165 - 30) / 5 + 1 ! 

    real(dp), allocatable :: state(:, :), shell_height(:)

    real(dp) L30mm


    call Read_Input(domain_name, file_name, start_year, stop_year, fishing_type, num_time_steps, num_monte_carlo_iter)

    delta_time = 1._dp / float(num_time_steps)
    
    is_rand_recr = .FALSE.
    
    write(*,*) '========================================================'
    write(*,'(A,A6)') ' Domain:         ', domain_name
    write(*,'(A,I6)') ' Start Year:     ', start_year
    write(*,'(A,I6)') ' Stop Year:      ', stop_year
    write(*,'(A,L6)') ' Random''d Recr:  ', is_rand_recr
    write(*,'(A,A6)') ' Fishing Type:   ', fishing_type
    write(*,'(A,I6,A,F7.4)') ' Time steps/year:', num_time_steps, ' delta ', delta_time
    write(*,*) '========================================================'
    
    call Load_Grid(grid%x, grid%y, grid%z, grid%lat, grid%lon, grid%len, grid%E, grid%num_elements, &
    &          grid%management_area, grid%is_closed, domain_name)
    write(*,'(A,I7)') ' Number of Grids:   ',grid%len
    write(*,'(A,I7)') ' Number of Squares: ', grid%num_elements
    num_grids = grid%len
    element_area = meters_per_naut_mile**2 ! convert 1 sq nm to sq m
    domain_area = float(num_grids) * element_area
    write(*,*) 'Domain Area: ', domain_area, ' square meters'
    
    write(*,*) '========================================================'

    !!!allocate(growth(1:num_grids) ,mortality(1:num_grids) ,recruit(1:num_grids) )
    allocate(growth(1:num_grids), recruit(1:num_grids))

    do j=1, num_grids
       growth(j)%region = domain_name
       !!!mortality(j)%region = domain_name
       grid%region(j) = domain_name
       !!!mortality(j)%ManagementArea = grid%ManagementArea(j)
    enddo
    
    allocate(shell_height(1:num_size_classes))
    call Set_Shell_Height_Intervals(shell_height, num_size_classes, 30.D0, 5.D0)
    
    !
    ! Assign recruitment by year and node
    !
    call Set_Growth(growth, grid, shell_height, delta_time, num_grids, num_size_classes, domain_name)
    call Set_Recruitment(recruit, num_grids, domain_name, is_rand_recr)
    !!!call SetMortality(mortality, grid, shell_height, num_grids, num_size_classes, domain_name)

    !allocate(state(1:num_grids,1:num_size_classes),StateTS(1:nts,1:num_size_classes),F(1:num_grids),Samp(1:num_grids, 1:num_size_classes),Wgrams(1:num_grids,1:num_size_classes))
    allocate(state(1:num_grids,1:num_size_classes))

    call Set_Initial_Conditions(num_grids, num_size_classes, shell_height, start_year, domain_name, growth, state)
    open(write_dev,file='Output/RecIndx.txt')
    do n = 1, num_grids
        L30mm = (growth(n) % L_inf_mu -30.D0) * exp(-growth(n) % K_mu)
        do j=1, num_size_classes 
            if (shell_height(j) .le. L30mm) recruit(n)%max_rec_ind = j
        enddo
        write(write_dev,*) n, L30mm, recruit(n)%max_rec_ind
    enddo
    close(write_dev)
    

END PROGRAM ScallopPopDensity

subroutine Set_Initial_Conditions(num_grids,num_size_classes,l,start_year,domain_name,growth,state)
    use globals
    use Growth_Mod
    
    implicit none
    type(Growth_Struct), intent(in):: growth(*)
    integer, intent(in):: num_grids, num_size_classes, start_year
    real(dp), intent(out):: state(num_grids, *)
    real(dp), intent(in):: l(*)
    integer n, j, num_rows, num_cols,tmp
    character(2) domain_name
    character(72) buffer, file_name
    character(3) fType
    
    write(buffer,'(I6)')start_year
    call Read_Input(domain_name,file_name,n,j,fType,tmp, num_rows)
    num_rows = num_grids
    num_cols = num_size_classes
    
    call Read_CSV(num_rows, num_cols, file_name, state, num_grids)
    PRINT *, term_blu
    PRINT '(A,A)', ' READ FROM FILE: ', file_name
    PRINT '(A,A)', ' USING DOMAIN:  ', domain_name
    PRINT '(A,I6)', ' LENGTH = ', num_rows
    PRINT '(A,I5,A)', ' NUMBER OF SCALLOP SIZE CLASSES: ', num_size_classes, term_blk

    if (num_rows > num_grids) num_rows = num_grids
    do n=1,num_rows
        do j=num_size_classes,2,-1
            !PRINT *, 'NN', n, ' CLASS ', j, ' STATE ', state(n,j)
            if( (l(j).gt.growth(n)%L_inf_mu) .and. (state(n,j).gt.0.D0) )then
                state(n,j-1)=state(n,j-1)+state(n,j)! lump scallops into smaller class
                state(n,j)=0.D0
            endif
        enddo
    enddo
    
    return
endsubroutine Set_Initial_Conditions
    