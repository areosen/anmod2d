!------------------------------------------------------------------------------
! Module: Getpar
! 
!> @author
!> Are Osen, Statoil R&D Exploration Geophysics
!
!> @brief
!> Handling of command line parameters
! 
!! @todo Error detection and handling
!! @todo Extensions
!------------------------------------------------------------------------------
module Getpar
  use Types
  implicit none

  interface get_par
     module procedure get_par_c
     module procedure get_par_i
     module procedure get_par_r
  end interface

contains

  subroutine get_par_r(parname,rpar,default)
    implicit none
    ! globals
    character (len=*)  :: parname
    real(sp)           :: rpar
    real(sp), optional :: default
    ! locals
    integer :: ipoint, iend, i
    character(len=256) :: in_string, option
    logical :: found=.false.
    ! start
    do i=1,COMMAND_ARGUMENT_COUNT()
       call GET_COMMAND_ARGUMENT(i,in_string)
       ipoint = index(in_string,'=')-1
       option = trim(adjustl(in_string(:ipoint)))
       if (option(:ipoint) == parname(:ipoint)) then
          iend = len(trim(adjustl(in_string))) 
          read(in_string(ipoint+2:iend),*) rpar
          found = .true.
          exit
       end if
    end do
    ! Use defaults if present
    if ( .not.found .and. present(default) ) rpar = default
    write(*,*) '*** PARM ',trim(parname),' = ', rpar
    found=.false.
  end subroutine get_par_r

  subroutine get_par_i(parname,ipar,default)
    implicit none
    ! globals
    character (len=*) :: parname
    integer           :: ipar
    integer, optional :: default
    ! locals
    integer :: ipoint, iend, i
    character(len=256) :: in_string, option
    logical :: found=.false.
    ! start
    do i=1,COMMAND_ARGUMENT_COUNT()
       call GET_COMMAND_ARGUMENT(i,in_string)
       ipoint = index(in_string,'=')-1
       option = trim(adjustl(in_string(:ipoint)))
       if (option(:ipoint) == parname(:ipoint)) then
          iend = len(trim(adjustl(in_string))) 
          read(in_string(ipoint+2:iend),*) ipar
          found = .true.
          exit
       end if
    end do
    ! Use defaults if present
    if ( .not.(found) .and. (present(default)) ) then
       ipar = default
    end if
    write(*,*) '*** PARM ',trim(parname),' = ', ipar
    found=.false.
  end subroutine get_par_i


  subroutine get_par_c(parname,cpar,default)
    implicit none
    ! globals
    character (len=*) :: parname
    character (len=*) :: cpar
    character (len=*), optional :: default
    ! locals
    integer :: ipoint, iend, i
    character(len=256) :: in_string, option
    logical :: found=.false.
    ! start
    found = .false.
    do i=1,COMMAND_ARGUMENT_COUNT()
       call GET_COMMAND_ARGUMENT(i,in_string)
       ipoint = index(in_string,'=')-1
       iend = len(trim(adjustl(in_string))) 
       option = trim(adjustl(in_string(:ipoint)))
       if (option(:ipoint) == parname(:ipoint)) then
          cpar = trim(in_string(ipoint+2:iend))
          found = .true.
          exit
       end if
    end do
    ! Use defaults if present
    if ( .not.(found) .and. (present(default)) ) then
       cpar = default
    end if
    if (found) write(*,*) '*** PARM ',parname,' = ', trim(adjustl(cpar))
  end subroutine get_par_c

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Check existence of a parameter
  ! 
  !! @param parname String that define name of parameter
  !! @todo Error detection and handling
  !----------------------------------------------------------------------------
  function par_exist(parname)
    implicit none
    logical :: par_exist
    character (len=*) :: parname
    ! local
    integer :: ipoint, i, num_options
    character(len=256) :: command, option
    ! Start
    ! print *,'parname:',parname
    par_exist=.false.
    num_options = COMMAND_ARGUMENT_COUNT()
    ! print *,'number  command line options=',num_options
    do i=1,num_options ! loop over number of command line options
       call GET_COMMAND_ARGUMENT(i,option)
       ! print *,i,trim(adjustl(option))
       if (option(:len(parname)) == parname(:len(parname))) then
          par_exist=.true. ! Found it
          exit
       end if
    end do
  end function par_exist

end module Getpar
