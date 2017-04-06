!------------------------------------------------------------------------------
! Module: Types
! 
!> @author
!> Are Osen, Statoil R&D Exploration Geophysics
!
!> @brief
!> Provide parameter definitions for accuracy, time, math and char 
! 
!! @todo Further extensions for necessary parameters
!------------------------------------------------------------------------------
module Types

  implicit none

  ! 4, 2, and 1 byte integers
  integer, parameter  :: i4b = selected_int_kind(9)
  integer, parameter  :: i2b = selected_int_kind(4)
  integer, parameter  :: i1b = selected_int_kind(2)

  ! single and double precision reals
  integer, parameter  :: sp  = kind(1.0)
  integer, parameter  :: dp  = kind(1.0D0)

  ! single and double precision complex
  integer, parameter  :: spc = kind((1.0,1.0))
  integer, parameter  :: dpc = kind((1.0D0,1.0D0))

  ! logical
  integer, parameter  :: lgt = kind(.true.)

  ! useful mathematical constants
  real(sp), parameter :: Pi = 3.141592653589793238462643383279502884197_sp
  real(sp), parameter :: PIO2 = 1.57079632679489661923132169163975144209858_sp
  real(sp), parameter :: Pi2 = 6.283185307179586476925286766559005768394_sp
  real(sp), parameter :: SQRT2 = 1.41421356237309504880168872420969807856967_sp
  real(dp), parameter :: PI_D = 3.141592653589793238462643383279502884197_dp
  real(dp), parameter :: PIO2_D= 1.57079632679489661923132169163975144209858_dp
  real(dp), parameter :: TWOPI_D=6.283185307179586476925286766559005768394_dp

  complex(spc) , parameter :: J = (0.,1.)
  
  ! set the standard I/O unit numbers
  integer, parameter :: STDIN=0            ! input from stream
  integer, parameter :: STDOUT=1           ! output to stream
  integer, parameter :: STDERR=2           ! errors

  integer, parameter :: STRLEN=256
  integer, save :: time0, itime, itick

contains

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Provide start time and end time
  ! 
  !! @param 
  !! @todo 
  !----------------------------------------------------------------------------
  subroutine StartTime()
    implicit none
    call system_clock(time0)
  end subroutine StartTime
  !
  integer function LapTime
    implicit none
    ! real :: time_lapse_sec
    call system_clock(itime, itick)
    if (itick /= 0) then       
       LapTime = real(itime-time0)/real(itick)
    else
       LapTime = 0
    end if
  end function LapTime
  ! subroutine LapTime( time_lapse_sec )
  !   implicit none
  !   real :: time_lapse_sec
  !   call system_clock(itime, itick)
  !   if (itick /= 0) then
  !      time_lapse_sec = real(itime-time0)/real(itick)
  !   end if
  ! end subroutine LapTime
  !
  subroutine EndTime( ProcName )
    implicit none
    character(len=*), intent(in), optional :: ProcName
    call system_clock(itime, itick)
    if (itick /= 0) then
       if (present(ProcName)) write (*,"(A)",advance="no") ProcName
       write (*,'(" complete in ", F10.3, " seconds")')       &
            &real(itime-time0)/real(itick)
    end if
  end subroutine EndTime
end module Types
