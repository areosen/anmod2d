!------------------------------------------------------------------------------
! Program: TestF90
! 
!> @author
!> Are Osen, Statoil R&D Exploration Geophysics
!
!> @brief
!> Test codes for IO and SEGY/SU file handling
! 
!! @todo Error detection and handling
!! @todo SU file handling
!  Make velocity model reader: Utilize fx3d.f90(Fx3dGetMod function: ~/src/spl.r71/src/lib/fx3d/fx3d.f90)
!------------------------------------------------------------------------------
!!
!! Compile: ifort types.f90 getpar.f90 geometry.f90 model.f90 segy.f90 test.f90 -o run.x
!!          ----------------- PS: ordering matters --------------------
!!
!! 
!! Execute: run.x option option option
!!
!------------------------------------------------------------------------------
program TestF90
  ! load modules
  use Types, wp => sp, wpc => spc ! Set working precision
  use Getpar
  use Segy
  use Model
  implicit none
  ! set global parameters and variables
  integer, dimension(2,5) :: umgi,upgi
  real, dimension(2) :: umgr, upgr
  type(SEGYOBJ) :: susynlv_segy, tommeliten_segy
  type(VELMOD)  :: tommeliten_vp
  integer, dimension(3) :: velmin,velmax

  ! Just a test allocation
  allocate(susynlv_segy%traces(10,10))

  ! Execute
  print *,'This is TestSegyRead!',char(10)

  ! Test command line reading
  if (COMMAND_ARGUMENT_COUNT() .lt.1) then
     print *,'run.x file= ilh=, xlh=',char(10)
     print *,'Geometry must be set in program'
     STOP 'stop'     
  end if
  
  ! Deconde command line
  if (par_exist('file')) call get_par('file',Tommeliten_segy%name)
  if (par_exist('ilh')) call get_par('ilh',inline_hdr_loc)
  if (par_exist('xlh')) call get_par('xlh',xline_hdr_loc)

  ! Do a check on byte ordering... not implemented function to handle this on any architecture
  segy_byte_swapped=find_byte_swap()
  if (.not.segy_byte_swapped) STOP 'ByteSwap must be set here'

  ! Initialize the files *** PERHAPS use filename in call instead of presetting
  ! call susynlv_segy%initialize_segyfile() ! (Susynlv_segy%filename)
  call tommeliten_segy%initialize_segyfile() ! (Susynlv_segy%filename)

  call tommeliten_segy%scan()

  print *,'--- In scan dev mode'

  STOP 'STOP'

  ! Set definitions of master and processing grids
  print *,'------------------------------------------------------------------ CONSIDER make master grid ascii file'
  umgi(:,1) = (/ 1, 1 /); umgi(:,2) = (/ 1, 1 /); umgi(:,3) = (/ 3000, 3000 /); umgi(:,4) = (/ 1, 1 /) 
  umgr(:) = (/ 12.5, 12.5 /)
  upgi(:,1) = (/ 884, 1880 /); upgi(:,2) = (/ 1676, 2600 /); upgi(:,3) = (/ 2, 2 /); 
  upgr(:) = (/ 12.5, 12.5 /)

  ! Initialize grids
  call tommeliten_vp%initialize_grid(mgi=umgi(:,:),mgr=umgr,pgi=upgi(:,:),pgr=upgr,azimuth=338.,&
       &dz=tommeliten_segy%dt,nz=tommeliten_segy%nsamp)

  ! Check velocity vs grid
  if (tommeliten_segy%ntraces /= tommeliten_vp%numb_velfuncs) then
     print *,'WARNING: Grid size and number of velocity functions do not match'
  end if

  print *,'*** MAST IL ',tommeliten_vp%master_grid%il_start,tommeliten_vp%master_grid%il_end
  print *,'*** MAST XL ',tommeliten_vp%master_grid%xl_start,tommeliten_vp%master_grid%xl_end
  print *,'*** MAST INC',tommeliten_vp%master_grid%il_inc,tommeliten_vp%master_grid%xl_inc
  print *,'*** MAST DEL',tommeliten_vp%master_grid%il_dist,tommeliten_vp%master_grid%xl_dist
  print *,'*** MAST AZI',tommeliten_vp%master_grid%azimuth

  print *,'*** PROC IL ',tommeliten_vp%processing_grid%il_start,tommeliten_vp%processing_grid%il_end
  print *,'*** PROC XL ',tommeliten_vp%processing_grid%xl_start,tommeliten_vp%processing_grid%xl_end
  print *,'*** PROC INC',tommeliten_vp%processing_grid%il_inc,tommeliten_vp%processing_grid%xl_inc
  print *,'*** PROC DEL',tommeliten_vp%processing_grid%il_dist,tommeliten_vp%processing_grid%xl_dist

  print *,'*** VELO ILs',tommeliten_vp%numb_il,tommeliten_vp%samp_il
  print *,'*** VELO XLs',tommeliten_vp%numb_xl,tommeliten_vp%samp_xl
  print *,'*** VELO Z  ',tommeliten_vp%numb_nz,tommeliten_vp%samp_dz

  ! Allocate space for velocity data and segy headers
  call tommeliten_vp%allocate_memory()

  ! Read model from segy file
  call tommeliten_vp%load(fileunit=tommeliten_segy%iounit,ntraces=tommeliten_segy%ntraces)

  ! Print min/max info
  velmin=minloc(tommeliten_vp%dat)
  print *,'*** VELO MIN',minval(tommeliten_vp%dat),velmin
  print *,'IL,XL,Z:',tommeliten_vp%hdr(inline_hdr_loc,velmin(2),velmin(3)),tommeliten_vp%hdr(xline_hdr_loc,velmin(2),velmin(3)),tommeliten_vp%samp_dz*(velmin(1)-1)
  velmax=maxloc(tommeliten_vp%dat)
  print *,'*** VELO MAX',maxval(tommeliten_vp%dat),velmax
  print *,'IL,XL,Z:',tommeliten_vp%hdr(inline_hdr_loc,velmax(2),velmax(3)),tommeliten_vp%hdr(xline_hdr_loc,velmax(2),velmax(3)),tommeliten_vp%samp_dz*(velmax(1)-1)

end program TestF90
