PROGRAM ScallopPopDensity
!> @mainpage Scallop Popupation Density
!! This program is used to compute Scallop Density after a given growth period

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
    integer data_len

    type(Growth_Struct), allocatable :: growth(:)
    !type(MortalityStruct), allocatable :: mortality(:)
    type(Recruit_Struct), allocatable :: recruit(:)

    integer, parameter :: num_size_classes = max_size_class ! not 25

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
    
    call Load_Grid(grid%x, grid%y, grid%z, grid%lat, grid%lon, grid%len, grid%E, grid%ne, &
    &          grid%management_area, grid%is_closed, domain_name)
    write(*,*)'grid length=',grid%len,' ', domain_name
    data_len = grid%len
    element_area = 1852.D0**2
    domain_area = float(data_len) * element_area
    write(*,*) 'Domain Area: ', domain_area

    write(*,*) '========================================================'

    !!!allocate(growth(1:data_len) ,mortality(1:data_len) ,recruit(1:data_len) )
    allocate(growth(1:data_len), recruit(1:data_len))

    do j=1, data_len
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
    call Set_Recruitment(recruit, data_len, domain_name, is_rand_recr)
    call Set_Growth(growth, grid, shell_height, delta_time, data_len, num_size_classes, domain_name)
    !!!call SetMortality(mortality, grid, shell_height, data_len, num_size_classes, domain_name)

    !allocate(state(1:data_len,1:num_size_classes),StateTS(1:nts,1:num_size_classes),F(1:data_len),Samp(1:data_len, 1:num_size_classes),Wgrams(1:data_len,1:num_size_classes))
    allocate(state(1:data_len,1:num_size_classes))

    call Set_Initial_Conditions(data_len, num_size_classes, shell_height, start_year, domain_name, growth, state)
    open(write_dev,file='RecIndx.txt')
    do n = 1, data_len
        L30mm = (growth(n) % L_inf_mu -30.D0) * exp(-growth(n) % K_mu)
        do j=1, num_size_classes 
            if (shell_height(j) .le. L30mm) recruit(n)%max_rec_ind = j
        enddo
        write(write_dev,*) n, L30mm, recruit(n)%max_rec_ind
    enddo
    close(write_dev)
    

END PROGRAM ScallopPopDensity

subroutine Set_Initial_Conditions(data_len,num_size_classes,l,start_year,domain_name,growth,state)
    use globals
    use Growth_Mod
    
    implicit none
    type(Growth_Struct), intent(in):: growth(*)
    integer, intent(in):: data_len, num_size_classes, start_year
    real(dp), intent(out):: state(data_len, *)
    real(dp), intent(in):: l(*)
    integer n, j, data_len_tmp, num_size_classes_tmp,tmp
    character(2) domain_name
    character(72) buffer, file_name
    character(3) :: fType
    
    write(buffer,'(I6)')start_year
    call Read_Input(domain_name,file_name,n,j,fType,tmp, data_len_tmp)
    data_len_tmp=data_len
    num_size_classes_tmp=num_size_classes
    
    write(*,*) file_name, domain_name
    call Read_CSV(data_len_tmp, num_size_classes_tmp, file_name, state, data_len)
    write(*,*) 'NSC ICS',num_size_classes_tmp, num_size_classes
    do n=1,data_len
        do j=num_size_classes,2,-1
            if( (l(j).gt.growth(n)%L_inf_mu) .and. (state(n,j).gt.0.D0) )then
                state(n,j-1)=state(n,j-1)+state(n,j)! lump scallops into smaller class
                state(n,j)=0.D0
            endif
        enddo
    enddo
    
    return
endsubroutine Set_Initial_Conditions
    