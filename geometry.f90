module Geometry
  use Types
  implicit none
  private

  type, public :: GRIDMASTER
     integer :: il_start=0, il_end=0, il_inc=0
     integer :: xl_start=0, xl_end=0, xl_inc=0
     real*8  :: il_origo=0., xl_origo=0.
     real*8  :: il_dist=0. , xl_dist=0.
     real*8    :: azimuth=0.
     real*8, dimension(2) :: MG1=(0.,0.), MG2=(0.,0.), MG3=(0.,0.), MG4=(0.,0.), MG_origo=(0.,0.)
   contains
  end type GRIDMASTER

  type, public :: GRIDPROC
     integer :: il_start=0, il_end=0, il_inc=0
     integer :: xl_start=0, xl_end=0, xl_inc=0
     real*8  :: il_dist=0. , xl_dist=0.
     real, dimension(2) :: PG1, PG2, PG3, PG4
   contains
  end type GRIDPROC
  
  !----------------------------------------------------------------------
  ! Possible to extract from /src/spl.r71/src/lib/geometry/geometry.f90
  type GEOMETRYPOINT
     double precision x    ! X-coordinate
     double precision y    ! Y-coordinate
  end type GEOMETRYPOINT

  type GEOMETRYIPOINT
     integer x             ! X-coordinate
     integer y             ! Y-coordinate
  end type GEOMETRYIPOINT

  !--- Geometry descriptor. Only the type itself is public.
  type GEOMETRYDESC 
     private                                           ! components are private
     double precision         :: xg0,yg0               ! Origin of survey area
     double precision         :: lxg,lyg               ! Lengths of survey area
     integer                  :: ilxg,ilyg             ! Lengths of survey area
     ! in grid points
     double precision         :: x0,y0                 ! Origin of local model
     integer                  :: ix0,iy0               ! Origin of local model
     ! in grid coordinates 
     double precision         :: lx,ly                 ! Length of local model
     integer                  :: ilx,ily               ! length of local model 
     ! in gridpoints 
     double precision         :: sxg,syg               ! Source position in 
     ! global coordinates
     integer                  :: isxg,isyg             ! Source position in 
     ! global grid coordinates
     double precision         :: sx,sy                 ! Source position in
     ! local coordinates
     integer                  :: isx,isy               ! Source position
     ! in local grid coordinates 
     double precision         :: dx,dy                 ! Sampling intervals
     integer,dimension(:,:),pointer    :: map          ! Map of regularized grid 
  end type GEOMETRYDESC

  type, public :: GRID
     integer :: number_inlines, number_xlines
     type(GRIDMASTER) :: master_grid
     type(GRIDPROC)   :: processing_grid
   contains
     procedure :: initialize_grid
     procedure :: initialize_grid_old
  end type GRID

contains
  
  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Set up the grids
  ! 
  !! @param grid
  !! @todo 
  !----------------------------------------------------------------------------
  subroutine initialize_grid(gridxx,file)
    implicit none
    class(GRID) :: gridxx
    character (len=*), intent(in) :: file
    ! local
    integer, dimension(2,4):: mgi, pgi
    real, dimension(2)     :: mgr, pgr
    real*8                   :: azimuth
    real*8 :: x,y,cell
    integer :: first,last,inc
    logical :: file_exist=.false., finished=.false.
    integer :: iounit, ios
    character (len=128) :: string

    ! check file
    INQUIRE(FILE=file, EXIST=file_exist)
    if (.not. file_exist) STOP '*** GEOM: Geometry xml file not there'

    ! Find IO unit
    OPEN(NEWUNIT=iounit,FILE=file,FORM='formatted',ACTION='read')

    do while (.not. finished)
       read (iounit,'(A)',iostat=ios) string
       if (ios .ne. 0) exit
       ! print *,trim(string)
       call get_coord(trim(string),'<Origo',gridxx%master_grid%il_origo,gridxx%master_grid%xl_origo)

       call get_coord(trim(string),'<MG1',gridxx%master_grid%MG1(1),gridxx%master_grid%MG1(2))
       call get_coord(trim(string),'<MG2',gridxx%master_grid%MG2(1),gridxx%master_grid%MG2(2))
       call get_coord(trim(string),'<MG3',gridxx%master_grid%MG3(1),gridxx%master_grid%MG3(2))
       call get_coord(trim(string),'<MG4',gridxx%master_grid%MG4(1),gridxx%master_grid%MG4(2))

       call get_line(trim(string),'<MGCross',gridxx%master_grid%xl_start,&
            &gridxx%master_grid%xl_end,gridxx%master_grid%xl_inc,gridxx%master_grid%xl_dist)

       call get_line(trim(string),'<MGInl',gridxx%master_grid%il_start,&
            &gridxx%master_grid%il_end,gridxx%master_grid%il_inc,gridxx%master_grid%il_dist)

       call get_line(trim(string),'<PGCross',gridxx%processing_grid%xl_start,&
            &gridxx%processing_grid%xl_end,gridxx%processing_grid%xl_inc,gridxx%processing_grid%xl_dist)

       call get_line(trim(string),'<PGInl',gridxx%processing_grid%il_start,&
            &gridxx%processing_grid%il_end,gridxx%processing_grid%il_inc,gridxx%processing_grid%il_dist)

       call get_azim(trim(string),'<AzimuthAngle>',gridxx%master_grid%azimuth)

       ! int *,'*********** AZIMUTH '

    end do

    if (ios < 0) print *,'*** GEOM - End of File'
    if (ios > 0) STOP '*** GEOM - Error reading File'

    CLOSE (iounit)

    contains
      
      subroutine get_coord(string,text,x,y)
        implicit none
        character (len=*) :: string,text
        real*8 :: x,y
        ! loc
        integer :: ilength
        character (len=len(string)) :: adj_str

        adj_str = trim(adjustl(string))
        ilength = len(trim(adjustl(text))) 
        ! print *,ilength,trim(string),trim(text)

        if ( adj_str(:ilength) .eq. text(:ilength)) then
           ! print *,ilength,trim(text),'* = *',adj_str
           call get_one(adj_str,'x="',x)
           call get_one(adj_str,'y="',y)
        end if

      end subroutine get_coord

      subroutine get_line(string,text,first,last,inc,cell)
        implicit none
        character (len=*) :: string,text
        integer :: first,last,inc
        real*8 :: cell
        ! loc
        integer :: ilength
        character (len=len(string)) :: adj_str

        adj_str = trim(adjustl(string))
        ilength = len(trim(adjustl(text))) 
        ! print *,'** get_line: ',ilength,trim(string),trim(text)

        if ( adj_str(:ilength) .eq. text(:ilength)) then
           ! print *,'-------',adj_str
           call get_onei(adj_str,'first="',first)
           call get_onei(adj_str,'last="',last)
           call get_onei(adj_str,'inc="',inc)
           call get_one(adj_str,'cell="',cell)
           ! print *,'Line',first,last,inc,cell
        end if

      end subroutine get_line

      subroutine get_azim(string,text,x)
        implicit none
        character (len=*) :: string, text
        real*8 :: x
        ! loc
        integer :: ilength, ipoint,iend
        character (len=len(string)) :: adj_str, a_str
        
        adj_str = trim(adjustl(string))
        ilength = len(trim(adjustl(text))) + 1
        if ( adj_str(:ilength-1) .eq. text(:ilength-1)) then

           iend = ilength + INDEX(trim(adj_str(ilength:)),'<') - 2
           ! print *,'* azim: ',ilength,iend,trim(adj_str),trim(adj_str(ilength:iend))
           a_str = trim(adj_str(ilength:iend))
           read (a_str,'(f10.5)') x
           ! print *,'x=',x
        end if

      end subroutine get_azim

      subroutine get_onei(string,text,x)
        implicit none
        character (len=*) :: string,text
        integer :: x
        ! loc
        integer :: ipoint, iend, i

        ipoint = INDEX(trim(string),trim(text)) + len(trim(text)) + 0
        iend   = ipoint + INDEX(trim(string(ipoint:)),'"') - 2
        ! print *,'ipoint=',ipoint, iend,' ',string(ipoint:iend)

        ! convert string to real
        read (string(ipoint:iend),'(i)') x
       
      end subroutine get_onei

      subroutine get_one(string,text,x)
        implicit none
        character (len=*) :: string,text
        real*8 :: x
        ! loc
        integer :: ipoint, iend, i

        ipoint = INDEX(trim(string),trim(text)) + len(trim(text)) 
        iend   = ipoint + INDEX(trim(string(ipoint:)),'"') - 2

        ! print *,'ipoint=',ipoint, iend,' ',string(ipoint:iend)

        ! convert string to real
        read (string(ipoint:iend),'(f15.5)') x
       
      end subroutine get_one

  end subroutine initialize_grid

  subroutine initialize_grid_old(gridxx,mgi,mgr,pgi,pgr,azimuth)
    implicit none
    class(GRID) :: gridxx
    integer, dimension(2,4),optional, intent(in) :: mgi, pgi
    real, dimension(2), optional, intent(in)     :: mgr, pgr
    real, optional, intent(in)                   :: azimuth

    ! set master grid geometry
    if (.not.present(mgi)) then
       print *,'No master grid provided'; STOP 'GRID: TERMINATE'
    else
       gridxx%master_grid%il_origo=real(mgi(1,1)); gridxx%master_grid%xl_origo=real(mgi(2,1))
       gridxx%master_grid%il_start=mgi(1,2); gridxx%master_grid%xl_start=mgi(2,2)
       gridxx%master_grid%il_end=mgi(1,3); gridxx%master_grid%xl_end=mgi(2,3)
       gridxx%master_grid%il_inc=mgi(1,4); gridxx%master_grid%xl_inc=mgi(2,4)
    end if
    if (present(mgr)) then
       gridxx%master_grid%il_dist=mgr(1); gridxx%master_grid%xl_dist=mgr(2)
    end if

    ! set processing grid
    if (present(pgi)) then
       gridxx%processing_grid%il_start=pgi(1,1); gridxx%processing_grid%xl_start=pgi(2,1)
       gridxx%processing_grid%il_end=pgi(1,2); gridxx%processing_grid%xl_end=pgi(2,2)
       gridxx%processing_grid%il_inc=pgi(1,3); gridxx%processing_grid%xl_inc=pgi(2,3)
       ! set lateral size of velocity model
       gridxx%number_inlines = INT( pgi(1,2) - pgi(1,1) + pgi(1,3) ) / pgi(1,3)
       gridxx%number_xlines  = INT( pgi(2,2) - pgi(2,1) + pgi(2,3) ) / pgi(2,3)
    end if

    if (present(pgr)) then
       gridxx%processing_grid%il_dist=pgr(1); gridxx%processing_grid%xl_dist=pgr(2)
    end if
    if (present(azimuth)) gridxx%master_grid%azimuth=azimuth

  end subroutine initialize_grid_old

end module Geometry
