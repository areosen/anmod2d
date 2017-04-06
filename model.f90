!------------------------------------------------------------------------------
! Module: Model
! 
!> @author
!> Are Osen, Statoil R&D Exploration Geophysics
!
!> @brief
!> Model loading, extra/interpolating, etc
! 
!! @todo Error detection and handling
!! @todo Extensions
!------------------------------------------------------------------------------
module Model
  use Types, wp => sp
  use Geometry, only : GRIDMASTER, GRIDPROC
  implicit none

  private
  integer, public,save :: inline_hdr_loc, xline_hdr_loc

  real(wp),parameter,private :: one = 1.0_wp  ! numeric constant
  
  type, public :: VELMOD ! l712 fx3d.f90
     ! private
     integer :: numb_il=0,numb_xl=0,numb_nz=0
     integer*8 :: numb_velfuncs=0
     real(wp) :: samp_il=0.,samp_xl=0.,samp_dz=0.
     character*80 :: EBCDIC(40)
     integer*2 :: BINHDR(200)
     real(wp), dimension(:,:,:), allocatable :: dat
     integer, dimension(:,:,:), allocatable :: hdr
     real(wp) :: fmax=0 ! 0.5*min(vel)/dx (*scale), scale ~ 0-0.5
     type(GRIDMASTER) :: master_grid
     type(GRIDPROC) :: processing_grid
   contains
     procedure :: initialize_grid
     procedure :: allocate_memory
     procedure :: load
  end type VELMOD

  ! ---------------------------------------------------------------------------
  ! Basic classes
  type, public, abstract :: model_class
     private
   contains
     private
     procedure(destroy_func), deferred, public :: destroy
  end type model_class

  abstract interface
     pure elemental subroutine destroy_func(me)
       import :: model_class
       implicit none
       class(model_class), intent(inout) :: me
     end subroutine destroy_func
  end interface

  ! ---------------------------------------------------------------------------
  ! Derived classes
  type, extends(model_class), public :: model_1d
     private
     real(wp), dimension(:),allocatable :: x,data
     integer :: ilox=1
   contains
     private
     procedure, public :: initialize => initialize_1d
     !procedure, public :: load       => load_1d
     procedure, public :: destroy    => destroy_1d
     ! final :: finalize_1d ! implement a call to destroy the data (ll158 of linear_interpolation_module.f90(finterp))
  end type model_1d

contains

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Load a file
  ! 
  !! @param velocity_model: Derived type to handle velocity models
  !! @param fileunit: Unit number for reading file
  !! @todo Load a file :)
  !----------------------------------------------------------------------------
  ! Test unit to see if a 3D model is read and gridded according to geometry definition
  subroutine load(velocity_model,fileunit,ntraces) ! Compare with SGYMODEL in segyprep-subs.f90
    implicit none
    class(VELMOD) :: velocity_model
    integer, intent(in) :: fileunit
    integer*8, intent(in) :: ntraces
    ! local
    integer :: i,ios,ipos,il_index,xl_index
    integer*2 :: trace_hdr2(120) !--------------------------------------------OBSOLETE. can rename hdr4 too
    integer*4 :: trace_hdr4(60)
    real, dimension(velocity_model%numb_nz) :: trace_dat
    real :: rvalue
    integer*8 :: ntraces_outside=0, ntraces_offgrid=0
    ! systime
    call StartTime()
    ! Read EBCDIC+BINHDR
    READ(UNIT=fileunit,POS=1) velocity_model%EBCDIC,velocity_model%BINHDR
    ! Read all traces
    load_all_functions: do i=1,ntraces ! velocity_model%numb_velfuncs
!       print *,i
!!$       print *,'****************************** STAGE HERE'; STOP 'STOP'
       trace_dat=0.
       ! Read both segy header and velocity function on one grid location
       READ(UNIT=fileunit,IOSTAT=ios) trace_hdr4, trace_dat
       if (ios < 0) STOP '*** VELO: Load fail - End of File'
       if (ios > 0) STOP '*** VELO: Load fail - Error reading File'
       ! Check if velocity function is correctly located according to processing grid
       if (  trace_hdr4(xline_hdr_loc) .lt. velocity_model%processing_grid%xl_start .or.&
            &trace_hdr4(xline_hdr_loc) .gt. velocity_model%processing_grid%xl_end .or.&
            &trace_hdr4(inline_hdr_loc) .lt. velocity_model%processing_grid%il_start .or.&
            &trace_hdr4(inline_hdr_loc) .gt. velocity_model%processing_grid%il_end) then
          ntraces_outside=ntraces_outside+1
          cycle load_all_functions
       end if
       if(    MOD(trace_hdr4(xline_hdr_loc) - velocity_model%processing_grid%xl_start,&
            &velocity_model%processing_grid%xl_inc) .ne. 0 .or.&
            & MOD(trace_hdr4(inline_hdr_loc) - velocity_model%processing_grid%il_start,&
            &velocity_model%processing_grid%il_inc) .ne. 0 ) then
          ntraces_offgrid=ntraces_offgrid+1
          cycle load_all_functions
       end if
       ! Store velocity function
       il_index=(trace_hdr4(inline_hdr_loc) - velocity_model%processing_grid%il_start)/&
            &velocity_model%processing_grid%il_inc + 1
       xl_index=(trace_hdr4(xline_hdr_loc) - velocity_model%processing_grid%xl_start)/&
            &velocity_model%processing_grid%xl_inc + 1

       velocity_model%dat(1:10,xl_index,il_index) = trace_dat(1:10)
       velocity_model%hdr(:,xl_index,il_index) = trace_hdr4(:)

    end do load_all_functions

    print *,'*** LOAD: Velocity functions outside processing grid',ntraces_outside
    print *,'*** LOAD: Velocity functions off     processing grid',ntraces_offgrid

    call EndTime('velocity_model%load')

  end subroutine load

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> 
  ! 
  !! @param 
  !----------------------------------------------------------------------------
  ! @todo: Add allocation check
  subroutine allocate_memory(velocity_model)
    implicit none
    class(VELMOD) :: velocity_model
    integer       :: ierr_allocate
    ! allocate for velocity values
    allocate( velocity_model%dat(velocity_model%numb_nz,velocity_model%numb_xl,velocity_model%numb_il),&
         &stat=ierr_allocate) ! @todo: Could use processing grid to define size of model array
    if (ierr_allocate .ne. 0) then
       print *,'Allocate problem!!!'
       STOP 'STOP'
    end if
    ! allocate for segy headers
    allocate( velocity_model%hdr(60,velocity_model%numb_xl,velocity_model%numb_il)) 
  end subroutine allocate_memory

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> 
  ! 
  !! @param 
  !----------------------------------------------------------------------------
  ! @todo: Maybe not use optional parameters
  subroutine initialize_grid(velocity_model,mgi,mgr,pgi,pgr,azimuth,dz,nz)
    implicit none
    class(VELMOD)                                :: velocity_model
    integer, dimension(2,4),optional, intent(in) :: mgi, pgi
    real, dimension(2), optional, intent(in)     :: mgr, pgr
    real, optional, intent(in)                   :: azimuth,dz
    integer, optional, intent(in)                :: nz
    ! set master grid geometry
    if (.not.present(mgi)) then
       print *,'No master grid set'; STOP 'MODEL: TERMINATE'
    else
       velocity_model%master_grid%il_origo=mgi(1,1); velocity_model%master_grid%xl_origo=mgi(2,1)
       velocity_model%master_grid%il_start=mgi(1,2); velocity_model%master_grid%xl_start=mgi(2,2)
       velocity_model%master_grid%il_end=mgi(1,3); velocity_model%master_grid%xl_end=mgi(2,3)
       velocity_model%master_grid%il_inc=mgi(1,4); velocity_model%master_grid%xl_inc=mgi(2,4)
    end if
    if (present(mgr)) then
       velocity_model%master_grid%il_dist=mgr(1); velocity_model%master_grid%xl_dist=mgr(2)
    end if
    ! set processing grid
    if (present(pgi)) then
       velocity_model%processing_grid%il_start=pgi(1,1); velocity_model%processing_grid%xl_start=pgi(2,1)
       velocity_model%processing_grid%il_end=pgi(1,2); velocity_model%processing_grid%xl_end=pgi(2,2)
       velocity_model%processing_grid%il_inc=pgi(1,3); velocity_model%processing_grid%xl_inc=pgi(2,3)
       ! set lateral size of velocity model
       velocity_model%numb_il = INT( pgi(1,2) - pgi(1,1) + pgi(1,3) ) / pgi(1,3)
       velocity_model%numb_xl = INT( pgi(2,2) - pgi(2,1) + pgi(2,3) ) / pgi(2,3)
       velocity_model%numb_velfuncs =  velocity_model%numb_il * velocity_model%numb_xl
    end if
    if (present(pgr)) then
       velocity_model%processing_grid%il_dist=pgr(1); velocity_model%processing_grid%xl_dist=pgr(2)
       velocity_model%samp_il = REAL( pgr(1)*pgi(1,3) )
       velocity_model%samp_xl = REAL( pgr(2)*pgi(2,3) )
    end if
    if (present(azimuth)) velocity_model%master_grid%azimuth=azimuth
    if (present(dz))      velocity_model%samp_dz=dz
    if (present(nz))      velocity_model%numb_nz=nz

  end subroutine initialize_grid
  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Check existence of a parameter
  ! 
  !! @param 
  !! @todo 
  !----------------------------------------------------------------------------
  pure elemental subroutine destroy_1d(me)

    implicit none

    class(model_1d),intent(inout) :: me

    if (allocated(me%data)) deallocate(me%data)
    if (allocated(me%x)) deallocate(me%x)
    me%ilox = 1

  end subroutine destroy_1d

  pure subroutine initialize_1d(me,x,f,istat)

    implicit none

    class(model_1d),intent(inout)         :: me
    real(wp),dimension(:),intent(in)      :: x
    real(wp),dimension(:),intent(in)      :: f
    integer,intent(out)                   :: istat  !! `0`  : no problems,
    !! `1`  : `x` is not strictly increasing,
    !! `10` : `x` is not equal to size(f,1).

    call me%destroy()

    istat = 0

    if (istat==0 .and. size(x)/=size(f,1)) istat = 10

    if (istat==0) then
       call check_inputs(x=x,ierr=istat)
       if (istat==0) then
          allocate(me%data(size(x))); me%data = f
          allocate(me%x(size(x))); me%x = x
       end if
    end if

  end subroutine initialize_1d

  pure subroutine load_1d(me,x,fx)

    implicit none

    class(model_1d),intent(inout)         :: me
    real(wp),intent(in)                   :: x
    real(wp),intent(out)                  :: fx  !! Interpolated \( f(x) \)

    integer,dimension(2) :: ix
    real(wp) :: p1
    real(wp) :: q1
    integer :: mflag

    call dintrv(me%x,x,me%ilox,ix(1),ix(2),mflag)

    q1 = (x-me%x(ix(1)))/(me%x(ix(2))-me%x(ix(1)))
    p1 = one-q1

    fx = p1*me%data(ix(1)) + q1*me%data(ix(2))

  end subroutine load_1d
    
!*****************************************************************************************
!>
!  Returns the indices in `xt` that bound `x`, to use for interpolation.
!  If outside the range, then the indices are returned that can
!  be used for extrapolation.
!  Precisely,
!
!```fortran
!         if            x < xt(1)   then ileft=1,   iright=2,    mflag=-1
!         if   xt(i) <= x < xt(i+1) then ileft=i,   iright=i+1,  mflag=0
!         if   xt(n) <= x           then ileft=n-1, iright=n,    mflag=1
!```
!
!### History
!
!  * interv written by carl de boor [5]
!  * dintrv author: amos, d. e., (snla) : date written 800901
!  * revision date 820801
!  * Jacob Williams, 2/24/2015 : updated to free-form Fortran.
!  * Jacob Williams, 2/17/2016 : additional refactoring (eliminated GOTOs).
!  * Jacob Williams, 2/22/2016 : modified bspline-fortran `dintrv` routine for
!    linear interpolation/extrapolation use.

  pure subroutine dintrv(xt,x,ilo,ileft,iright,mflag)

    implicit none

    real(wp),dimension(:),intent(in) :: xt     !! a knot or break point vector
    real(wp),intent(in)              :: x      !! argument
    integer,intent(inout)            :: ilo    !! an initialization parameter which must be set
    !! to 1 the first time the array `xt` is
    !! processed by dintrv. `ilo` contains information for
    !! efficient processing after the initial call and `ilo`
    !! must not be changed by the user.  each dimension
    !! requires a distinct `ilo` parameter.
    integer,intent(out)              :: ileft  !! left index
    integer,intent(out)              :: iright !! right index
    integer,intent(out)              :: mflag  !! signals when `x` lies out of bounds

    integer :: ihi, istep, imid, n

    n = size(xt)

    ihi = ilo + 1
    if ( ihi>=n ) then
       if ( x>=xt(n) ) then
          mflag = 1
          ileft = n-1
          iright= n
          return
       end if
       if ( n<=1 ) then
          mflag = -1
          ileft = 1
          iright= 2
          return
       end if
       ilo = n - 1
       ihi = n
    endif

    if ( x>=xt(ihi) ) then

       ! now x >= xt(ilo). find upper bound
       istep = 1
       do
          ilo = ihi
          ihi = ilo + istep
          if ( ihi>=n ) then
             if ( x>=xt(n) ) then
                mflag = 1
                ileft = n-1
                iright= n
                return
             end if
             ihi = n
          elseif ( x>=xt(ihi) ) then
             istep = istep*2
             cycle
          endif
          exit
       end do

    else

       if ( x>=xt(ilo) ) then
          mflag = 0
          ileft = ilo
          iright= ilo+1
          return
       end if
       ! now x <= xt(ihi). find lower bound
       istep = 1
       do
          ihi = ilo
          ilo = ihi - istep
          if ( ilo<=1 ) then
             ilo = 1
             if ( x<xt(1) ) then
                mflag = -1
                ileft = 1
                iright= 2
                return
             end if
          elseif ( x<xt(ilo) ) then
             istep = istep*2
             cycle
          endif
          exit
       end do

    endif

    ! now xt(ilo) <= x < xt(ihi). narrow the interval
    do
       imid = (ilo+ihi)/2
       if ( imid==ilo ) then
          mflag = 0
          ileft = ilo
          iright= ilo+1
          return
       end if
       ! note. it is assumed that imid = ilo in case ihi = ilo+1
       if ( x<xt(imid) ) then
          ihi = imid
       else
          ilo = imid
       endif
    end do

  end subroutine dintrv

!*****************************************************************************************
!>
!  Check the validity of the inputs to the initialize routines.
!  Prints warning message if there is an error,
!  and also sets `ierr` (/=0 if there were any errors).
!
!  Supports up to 6D: x,y,z,q,r,s
!
!# History
!  * Jacob Williams, 2/24/2015 : Created this routine.
!  * Jacob Williams, 2/23/2016 : modified for linear interp module.

  pure subroutine check_inputs(x,y,z,q,r,s,ierr)

    use iso_fortran_env,    only: error_unit

    implicit none

    real(wp),dimension(:),intent(in),optional  :: x     !! `x` abscissa vector
    real(wp),dimension(:),intent(in),optional  :: y     !! `y` abscissa vector
    real(wp),dimension(:),intent(in),optional  :: z     !! `z` abscissa vector
    real(wp),dimension(:),intent(in),optional  :: q     !! `q` abscissa vector
    real(wp),dimension(:),intent(in),optional  :: r     !! `r` abscissa vector
    real(wp),dimension(:),intent(in),optional  :: s     !! `s` abscissa vector
    integer,intent(out)                        :: ierr  !! `0` : no problems,
    !! `1` : `x` is not strictly increasing,
    !! `2` : `y` is not strictly increasing,
    !! `3` : `z` is not strictly increasing,
    !! `4` : `q` is not strictly increasing,
    !! `5` : `r` is not strictly increasing,
    !! `6` : `s` is not strictly increasing,

    ierr = 0  ! initialize

    if (present(x)) call check(x,1,ierr); if (ierr/=0) return
    if (present(y)) call check(y,2,ierr); if (ierr/=0) return
    if (present(z)) call check(z,3,ierr); if (ierr/=0) return
    if (present(q)) call check(q,4,ierr); if (ierr/=0) return
    if (present(r)) call check(r,5,ierr); if (ierr/=0) return
    if (present(s)) call check(s,6,ierr); if (ierr/=0) return

  contains

    pure subroutine check(v,error_code,ierr)

      implicit none

      real(wp),dimension(:),intent(in) :: v          !! abcissae vector
      integer,intent(in)               :: error_code !! error code for check
      integer,intent(inout)            :: ierr       !! will be set to `error_code` if there is a problem

      integer :: i  !! counter
      integer :: n  !! size of the input `v` array

      n = size(v)
      do i=2,n
         if (v(i) <= v(i-1)) then
            ierr = error_code
            exit
         end if
      end do

    end subroutine check

  end subroutine check_inputs


end module Model
