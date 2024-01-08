!---------------------------------------------------------------------------------------------------------------------
!> @page page4 Grid Manager Mod
!>
!> @section p4p1 Grid Manager Class
!> @subsection p4p1p1 Brief
!> The Grid Manager is responsible for setting up the grid by reading in each grid's coordinates from the
!> @a @b Initial_Conditions file named by the @a @b Grid_Manager_Config_File in Scallop.cfg.
!> 
!> The main program instantiates a Grid Manger by calling @a Set_Grid_Manager
!>
!> @subsection p4p1p2 Set Grid Manager
!> This routine initializes private variables. Calls @a @b Read_Configuration that reads in the Grid Manger configuration 
!> file as given by the main configuration and set via @a @b Set_Config_File_Name.
!>
!> @a @b Load_Grid_State loads the grid data from the file given in @a @b Initial_Conditions. This establishes the
!> number of grids, @a num_grids, and the initial state of the scallop density, @ state.
!>
!> If a special access area definitions are provided, these are loaded via @a @b Load_Area_Coordinates. 
!> Each grid location if then checked if it is in a special access area and identified as such by setting 
!> @a special_access_index to the index of the corresponding access area.
!>
!> @subsection p4p1p3 Read_Configuration
!> Reads given file name and scans each line, input string, for tag and value characters.
!> Also determines if special access areas are desired and if not sets @a use_spec_access_data to false
!>
!> @subsection p4p1p4 Load_Area_Coordinates
!> If @a use_spec_access_data is true then reads given file name. Scans each input line for an area longitude 
!> vector coordinates followed by latitude vector coordinates. The length of each vector must be equal and 
!> establishes the number of vertices, or edges i.e. @ n_sides that define the special access area.
!> The number of such vector pairs establishes the @a num_ares defined
!>
!> @subsection p4p1p5 Is_Grid_In_Special_Access
!> This method uses a grids longitude and latitude coordinates are in a special area. It does so by using a 
!> point in polygram algorithm. The data vector representation is used when calling @a @b Point_In_Polygon_Vector
!>
!> @section p4p3 Grid Manager Support Methods
!> @subsection p4p2p1 Set_Config_File_Name
!> Sets @a config_file_name for @a @b Read_Configuration
!>
!> @subsection p4p2p2 Set_Init_Cond_File_Name
!> Sets @a init_cond_fname for @a @b Load_Grid_State
!>
!> @subsection p4p2p3 Set_Special_Access_File_Name
!> @section p4p3 Point In Polygon
!> The Point_In_Polygon_Vector method is used to find if a point is in a polygon. The @a @b Grid_Manager also supports 
!> polygon data representation as an array of LonLatPoint points vial @a @b Point_In_Polygon_Points or as a 
!> n by 2, 2-dimensional array, where n is a maximum of @a max_sides edges.
!> @subsection p4p3p1 Point_In_Polygon_Points
!> @subsection p4p3p2 Point_In_Polygon_Array
!> @subsection p4p3p3 Point_In_Polygon_Vector
!>
!---------------------------------------------------------------------------------------------------------------------

module Grid_Manager_Mod
use globals
implicit none

!> @class Grid_Data_Class
type Grid_Data_Class
    !! @public @memberof Grid_Data_Class
    !! UTM Easting
    real(dp) x
    !! @public @memberof Grid_Data_Class
    !! UTM Northing
    real(dp) y
    !! @public @memberof Grid_Data_Class
    !! Longitude
    real(dp) lon
    !! @public @memberof Grid_Data_Class
    !! Latitude
    real(dp) lat
    !! @public @memberof Grid_Data_Class
    !! bathymetric depth at (x,y)
    real(dp) z
    !! @public @memberof Grid_Data_Class
    !! Indicates if grid is closed for fishing
    logical is_closed
    !! @public @memberof Grid_Data_Class
    !! Indexed special access
    integer special_access_index
end type Grid_Data_Class

type LonLatPoint
real(dp) lon
real(dp) lat
end type LonLatPoint

! These could be made allocatable, with an example provided here
! https://stackoverflow.com/questions/58976091/fortran-allocatable-array-of-allocatable-derived-type
! However, and as stated in the link, this could slow down execution times. Further, the memory savings
! is not significant. Sixteen bytes per each side whereas memory nowadays is measured in GBytes.
type LonLatVector
real(dp) lon(max_sides)
real(dp) lat(max_sides)
integer n_sides
end type LonLatVector

type(LonLatVector), PRIVATE :: area(max_num_areas)
integer, PRIVATE :: max_num_grids
integer, PRIVATE :: num_areas
integer, PRIVATE :: num_grids
logical, PRIVATE :: use_spec_access_data
character(fname_len), PRIVATE :: config_file_name
character(fname_len), PRIVATE :: init_cond_fname
character(fname_len), PRIVATE :: special_accesss_fname

CONTAINS

!==================================================================================================================
!! @public @memberof GridManager
!>
!> Initializes growth for startup
!>
!==================================================================================================================
subroutine Set_Grid_Manager(max_ngrids, state, grid, ngrids)
    integer, intent(in) :: max_ngrids
real(dp), intent(out):: state(1:max_ngrids, 1:num_size_classes)
    type(Grid_Data_Class), intent(out) :: grid(*)
    integer, intent(out) :: ngrids

    integer n, j
    character(fname_len) fname

    ! set private variables, needed by private methods
    max_num_grids = max_ngrids

    ! Used to verify grid in special access area
    fname = 'Results\GridLoc.txt'
    open(70, file=trim(fname))

    call Read_Configuration()

    ! Load Grid. 
    ! read in grid and state from file_name
    ngrids = Load_Grid_State(grid, state)

    ! set private variable
    num_grids = ngrids

    num_areas = Load_Area_Coordinates()
    write(*,*) '========================================================'
    write(*,'(A,I7)') ' Number of Areas: ', num_areas
    write(*,*) '========================================================'

    do n = 1, num_grids
        ! Check if any grids are in a special access
        j = Is_Grid_In_Special_Access(grid(n)%lon, grid(n)%lat)
        if (j > 0) then
            grid(n)%special_access_index = j
            write(70,*) 'Found grid', n, 'in area', j, ' IS CLOSED: ', grid(n)%is_closed
        endif
    enddo

    close(70)
endsubroutine Set_Grid_Manager

!-----------------------------------------------------------------------------------------------
!! @public @memberof GridManager
!> Used during instantiation to set the name of the file to read to for configuration parameters
!> @brief Read Input File
!> 
!> Sets file names for initial state data and special access data
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

!-----------------------------------------------------------------------------------------------
!! @public @memberof GridManager
!> Used during instantiation to set the name of the file to read to for grid locations, state
!> @brief Read Input File
!> 
!> Sets name of a configuration file, typical 'Data/bin5mmYYYY[MA|GB].csv'
!-----------------------------------------------------------------------------------------------
subroutine Set_Init_Cond_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    init_cond_fname = fname
    inquire(file=init_cond_fname, exist=exists)

    if (exists) then
        PRINT *, term_blu, trim(init_cond_fname), ' FOUND', term_blk
    else
        PRINT *, term_red, trim(init_cond_fname), ' NOT FOUND', term_blk
        stop
    endif
endsubroutine Set_Init_Cond_File_Name

!-----------------------------------------------------------------------------------------------
!! @public @memberof GridManager
!> Used during instantiation to set the name of the file to special access coordinates
!> @brief Read Input File
!> 
!> Sets file name for special access coordinates
!-----------------------------------------------------------------------------------------------
subroutine Set_Special_Access_File_Name(fname)
    character(*), intent(in) :: fname
    logical exists

    use_spec_access_data =  (fname(1:4) .ne. 'NONE')

    if (use_spec_access_data) then
        special_accesss_fname = config_dir//fname

        inquire(file=special_accesss_fname, exist=exists)

        if (exists) then
            PRINT *, term_blu, trim(special_accesss_fname), ' FOUND', term_blk
        else
            PRINT *, term_red, trim(special_accesss_fname), ' NOT FOUND', term_blk
            stop
        endif
    else
        write(*,*) term_yel, 'SPECIAL ACCESS DATA FILE SET TO ', term_blk, '"NONE"',  &
        &                     term_yel, ' USING DEFAULT VALUES FOR FISH MORTALITY', term_blk
    endif
endsubroutine Set_Special_Access_File_Name

!-----------------------------------------------------------------------
!! @public @memberof GridManager
!> Get'r function for private member num_areas
!-----------------------------------------------------------------------
integer function Get_Num_Of_Areas()
    Get_Num_Of_Areas = num_areas
endfunction

!-----------------------------------------------------------------------
!! @public @memberof GridManager
!> Read_Configuration
!> @brief Read Input File
!> 
!> Reads a configuration file
!>
!-----------------------------------------------------------------------
subroutine Read_Configuration()
    
    implicit none
    character(line_len) input_string
    character(tag_len) tag
    character(value_len) value
    integer j, k, io

    write(*,*) 'READING IN ', config_file_name

    open(read_dev,file=config_file_name)
    do
        input_string=""
        read(read_dev,'(a)',iostat=io) input_string
        if (io.lt.0) exit
        if (input_string(1:1) .NE. '#') then
            j = scan(input_string,"=",back=.true.)
            tag = trim(adjustl(input_string(1:j-1)))
            ! explicitly ignore inline comment
            k = scan(input_string,"#",back=.true.)
            if (k .EQ. 0) k = len(input_string) + 1
            value =  trim(adjustl(input_string(j+1:k-1)))

            select case(tag)
            case('Initial Conditions')
                call Set_Init_Cond_File_Name(trim(adjustl(value)))

            case('Special Access Config File')
                call Set_Special_Access_File_Name(trim(adjustl(value)))

            case default
                write(*,*) term_red, 'Unrecognized line in ',config_file_name
                write(*,*) 'Unknown Line-> ',input_string, term_blk
            end select
        endif
    end do
    close(read_dev)
    return
end subroutine Read_Configuration

!==================================================================================================================
!! @public @memberof Grid_Manager_Mod
!>
!> This function is used to set the grid parameters and the initial state to start the simulation. 
!>
!> It does so by reading the CSV file at file_name. This file has been generated by the TrawlData5mm.m 
!> Matlab script. The format is for each grid in a row, the columns are
!> Decimal Year, UTM X, UTM Y, Latitude, Longitude, UTM Z, Grid Is Closed, Followed by Scallop Density in Count/m^2
!> sorted by shell length 30 to 150 mm in 5mm increments for 25 columns
!>
!> @param[in, out] grid Holds position information 
!> @param[out] state Holds the initial state at various location specified by grid
!> @param[in] file_name CSV name to be read in
!>
!==================================================================================================================
integer function Load_Grid_State(grid, state)
    type(Grid_Data_Class), intent(out) :: grid(*)
    real(dp), intent(out):: state(1:max_num_grids, 1:num_size_classes)

    character(csv_line_len) input_str
    integer n, io, is_closed

    real(dp) year ! used as place holder when reading file_name

    PRINT *, 'OPENING ', init_cond_fname

    open(63, file=init_cond_fname, status='old')
    n = 0
    do
        read(63,'(a)',iostat=io) input_str
        if (io.lt.0) exit
        n=n+1
        if (n>max_num_grids) then
            write(*,'(A,A, I5, A, I5)') term_red, 'MAX NUMBER OF GRIDS EXCEEDED, AT ', n, ' EXPECTED ', max_num_grids
            write(*,*) 'CHANGE "Max Number of Grids" IN CONFIGURATION FILE', term_blk
        end if
        read(input_str,*) year, grid(n)%x, grid(n)%y, grid(n)%lat, grid(n)%lon, grid(n)%z, &
        &               is_closed, state(n,1:num_size_classes)
        grid(n)%is_closed = (is_closed > 0)
        grid(n)%special_access_index = 0
    end do
    close(63)
    write(*,*) term_blu, 'READ ', n, 'GRIDS', term_blk

    Load_Grid_State = n

endfunction Load_Grid_State

!==================================================================================================================
!! @public @memberof Grid_Manager_Mod
!==================================================================================================================
integer function Load_Area_Coordinates()
    ! 10 edges time 12 characters, SXX.XXXXXX, plus padding
    character(150) input_str, sub_str
    integer n, io
    integer edge_lon, edge_lat, j

    if (.not. use_spec_access_data) then
        Load_Area_Coordinates = 0
        write(*,*) term_blu, '                0 AREAS', term_blk
        return
    endif

    PRINT *, 'OPENING ', special_accesss_fname

    open(63, file=special_accesss_fname, status='old')
    n = 0
    do
        read(63,'(a)',iostat=io) input_str
        if (io.lt.0) exit

        if (input_str(1:1) .ne. '#') then

            n=n+1
            ! read longitude value
            edge_lon = 0
            sub_str = input_str
            j = len(input_str)

            do while (j.ne.0)
                edge_lon = edge_lon + 1
                read(sub_str,*) area(n)%lon(edge_lon)
                j = index(sub_str,',')
                sub_str = sub_str(j+1:)
            enddo

            ! read latitude value
            read(63,'(a)',iostat=io) input_str
            if (io.lt.0) exit

            edge_lat = 0
            sub_str = input_str
            j = len(input_str)

            do while (j.ne.0)
                edge_lat = edge_lat + 1
                read(sub_str,*) area(n)%lat(edge_lat)
                j = index(sub_str,',')
                sub_str = sub_str(j+1:)
            enddo

            if (edge_lon .ne. edge_lat) then
                write(*,*) term_red, 'INVALID SPECIAL ACCESS FILE: at set ', n, term_blk
                stop
            endif
            area(n)%n_sides = edge_lat
        endif
    end do
    close(63)
    write(*,*) term_blu, 'READ ', n, 'AREAS', term_blk
    num_areas = n
    Load_Area_Coordinates = n
endfunction

!==================================================================================================================
!! @public @memberof Grid_Manager_Mod
!==================================================================================================================
integer function Is_Grid_In_Special_Access(lon, lat)
    real(dp), intent(in) :: lon, lat

    integer a

    Is_Grid_In_Special_Access = -1
    if (use_spec_access_data) then
        do a = 1, num_areas
            if (Point_In_Polygon_Vector(area(a)%lon, area(a)%lat, lon, lat, area(a)%n_sides)) then
                Is_Grid_In_Special_Access = a
                exit
            endif
        enddo
    endif
endfunction Is_Grid_In_Special_Access

!==================================================================================================================
!! @public @memberof Grid_Manager_Mod
!> @param poly Array of LonLatPoint coordinates that define polygram,
!> @param point LonLatPoint coordinate of point we wish to determine if inside polygram
!> @param nodes the number of corners, edges, that define the polygon
!> @returns true if point is inside polygram, false if outsied
!>          if point is on an edge then is may return true of false
!==================================================================================================================
logical function Point_In_Polygon_Points(poly, point, nodes)

    type(LonLatPoint), intent(in) :: poly(*)
    type(LonLatPoint), intent(in) :: point
    integer, intent(in) :: nodes

    Point_In_Polygon_Points = Point_In_Polygon_Vector(poly(1:nodes)%lon, poly(1:nodes)%lat, point%lon, point%lat, nodes)

    return
endfunction Point_In_Polygon_Points

!==================================================================================================================
!! @public @memberof Grid_Manager_Mod
!> @param poly Array of x,y coordinates that define polygram,
!> @param point x,y coordinate of point we wish to determine if inside polygram
!> @param nodes the number of corners, edges, that define the polygon
!> @returns true if point is inside polygram, false if outsied
!>          if point is on an edge then is may return true of false
!==================================================================================================================
logical function Point_In_Polygon_Array(poly, point, nodes)

    real(dp), intent(in) :: poly(max_sides,2)
    real(dp), intent(in) :: point(2)
    integer, intent(in) :: nodes

    Point_In_Polygon_Array = Point_In_Polygon_Vector(poly(1:nodes,1), poly(1:nodes,2), point(1), point(2), nodes)

    return
endfunction Point_In_Polygon_Array

!==================================================================================================================
!! @public @memberof Grid_Manager_Mod
!>
!> First of all, notice that each iteration considers two adjacent points and the target point. 
!> Then the if statement evaluates two conditions:
!>
!> 1.) Y-value of our target point is within the range [verty[j], verty[i]).
!> 2.) X-value of our target point is below the linear line connecting the point j and i.
!> If you're having problems to see this second condition, just write down the linear equation of the line,
!> reorganize the expression a little bit and place testy as the free variable.
!>
!> Every time the above two conditions are met, we toggle the flag c. So we return true if 
!> above conditions are met odd number of times and false otherwise.
!>
!> http://alienryderflex.com/polygon/
!>
!> @param polyX Array of horizontal, coordinates of corners
!> @param polyY Array of vertical    coordinates of corners
!> @param x horizontal coordinate of point we wish to determine if inside polygram
!> @param y vertical   coordinate of point we wish to determine if inside polygram
!> @param nodes the number of corners, edges, that define the polygon
!> @returns true if point is inside polygram or if on vert or horiz edge,
!           false if outside
!>          if point is on rise of falling edge then it may return true or false
!==================================================================================================================
logical function Point_In_Polygon_Vector(polyX, polyY, x, y, nodes)

    real(dp), intent(in) :: polyX(*)
    real(dp), intent(in) :: polyY(*)
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y
    integer, intent(in) :: nodes

    integer   i, j
    logical inPolygon
    logical ind1, ind2, ind3

    inPolygon=.FALSE.
    j = nodes

    do i = 1, nodes
        ind1 = polyY(i) < y .AND. polyY(j) >= y
        ind2 = polyY(j) < y .AND. polyY(i) >= y
        ind3 = polyX(i) + (y - polyY(i)) / (polyY(j) - polyY(i)) * (polyX(j) - polyX(i)) < x
        if ((ind1 .or. ind2) .and. (ind3)) inPolygon = .NOT. inPolygon
        j = i
    enddo
    Point_In_Polygon_Vector = inPolygon

    return
endfunction Point_In_Polygon_Vector

endmodule Grid_Manager_Mod
