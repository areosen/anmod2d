!------------------------------------------------------------------------------
! Program: LoadModelFromSegy
! 
!> @author
!> Are Osen, Statoil R&D Exploration Geophysics
!
!> @brief
!> Test codes for IO and SEGY/SU file handling
! 
!! @todo Error detection and handling
!! @todo SU file handling
!! @todo Velocity model loading, using grid: Ok to have separate in model module
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
program LoadModelFromSegy
  ! load modules
  use Types, wp => sp, wpc => spc ! Set working precision
  use Getpar
  use Segy
  use Model
  use Geometry
  implicit none
  ! set global parameters and variables
  integer, dimension(2,5) :: umgi,upgi
  real, dimension(2) :: umgr, upgr
  type(SEGYOBJ)  :: dataset_segy
  type(VELMOD)   :: dataset_vp
  type(GRID)     :: project
  integer, dimension(3) :: velmin,velmax
  integer :: iensemble, position, ntraces, oensemble
  character(len=128) :: program_name

  integer :: il_calc, xl_calc
  real :: theta, alpha, phi
  real :: hypothenus_dist, rarg1, rarg2, delx,dely, ang_rad

  ! Execute
  call GET_COMMAND_ARGUMENT(0,program_name)
  print *,char(10),'This is ',trim(program_name),char(10)

  ! Test command line reading
  if (COMMAND_ARGUMENT_COUNT() .lt.1) then
     print *,trim(program_name),' file= geom= pri= sec= ter=',char(10)
     print *,'INFO:'
     print *,'Geometry must be set in program. XML support planned',&
          &'pri=il, sec=xl, ter=-: Default segy header locations to use'
     STOP 'stop'     
  end if
  
  ! Decode command line
  call get_par('file',dataset_segy%input)
  call get_par('pri',dataset_segy%primary,default='il')
  call get_par('sec',dataset_segy%secondary,default='xl')
  call get_par('ter',dataset_segy%tertiary)
  call get_par('geom',dataset_segy%geometryfile)

  ! Set up project geometry and notify 
  call project%initialize_grid(dataset_segy%geometryfile)

  print *,'*** MAST OO ',project%master_grid%il_origo,project%master_grid%xl_origo
  print *,'*** MAST IL ',project%master_grid%il_start,project%master_grid%il_end
  print *,'*** MAST XL ',project%master_grid%xl_start,project%master_grid%xl_end
  print *,'*** MAST INC',project%master_grid%il_inc,project%master_grid%xl_inc
  print *,'*** MAST DEL',project%master_grid%il_dist,project%master_grid%xl_dist
  print *,'*** MAST AZI',project%master_grid%azimuth

  print *,'*** PROC IL ',project%processing_grid%il_start,project%processing_grid%il_end
  print *,'*** PROC XL ',project%processing_grid%xl_start,project%processing_grid%xl_end
  print *,'*** PROC INC',project%processing_grid%il_inc,project%processing_grid%xl_inc
  print *,'*** PROC DEL',project%processing_grid%il_dist,project%processing_grid%xl_dist

  ! Do a check on byte ordering... not implemented function to handle this on any architecture
  segy_byte_swapped=find_byte_swap()
  if (.not.segy_byte_swapped) STOP 'ByteSwap must be set here'

  ! Initialize the files *** PERHAPS use filename in call instead of presetting Project
  call dataset_segy%initialize_input_segyfile() 
  call dataset_segy%sortkey(primary=dataset_segy%primary,&
       &secondary=dataset_segy%secondary,tertiary=dataset_segy%tertiary)

  ! Scan some min/max sort key values
  ! call StartTime()
  ! call dataset_segy%scan()
  ! call EndTime('Scan dataset')

  dataset_segy%output = "/work53/JS_WIRE/are.segy"
  call dataset_segy%initialize_output_segyfile()

  ! Set file pointer to first trace in dataset
  dataset_segy%pos_inp = 3601
  dataset_segy%pos_out = 3601
  oensemble = 0

  call StartTime()
  ensemble_read_loop: do iensemble=1, dataset_segy%max_ensembles

     ! Get an ensemble
     call dataset_segy%get_ensemble()

     ! calculate the IL/XL number of a trace in the ensemble
     ! Method:
     ! Calculate the angle phi, measured from the inline direction to
     ! the vector from origo to the location of interest.
     ! Then the transform angle alpha is survey azimuth minus phi.
     
     ! Fix su.hdr such as to use that instead of direct numeral references in arrays
     delx = REAL(dataset_segy%hdr_buffer(19,1)) - REAL(project%master_grid%il_origo)
     dely = REAL(dataset_segy%hdr_buffer(20,1)) - REAL(project%master_grid%xl_origo)

     rarg1 = delx**2 + dely**2

     hypothenus_dist = SQRT( rarg1 ) 

     if ( dely > 0.) then
        phi = ATAN( delx / dely ) / ( 4*ATAN(1.)) * 180.
     else
        phi = ATAN( delx / dely ) / ( 4*ATAN(1.)) * 180. + 180.
     end if

     alpha = phi - project%master_grid%azimuth

     ang_rad = alpha * 4 * ATAN( 1. ) / 180.

     rarg1 = SIN( ang_rad ) * hypothenus_dist / project%master_grid%il_dist
     rarg2 = COS( ang_rad ) * hypothenus_dist / project%master_grid%xl_dist

     ! Calculate the inline and corssline numbers for the given coordinate
     il_calc = NINT ( rarg1 ) + project%master_grid%il_start
     xl_calc = NINT ( rarg2 ) + project%master_grid%xl_start

     ! At Nth ensemble, print info
     if ( mod(iensemble,100) == 0 ) then

        ! Fix: su.hdr referral of header arrays
        ! Fix: Possibly add output of pkey and sec key headers
        write (*,'(A9,I6,F15.1,A6,4I6)') &
             &'ensemble',&
             &iensemble,&
             &REAL(dataset_segy%pos_inp)/1024/1024/LapTime(),&
             &'Mb/sec',&
             &dataset_segy%hdr_buffer(48,:1),&
             &MINVAL(dataset_segy%hdr_buffer(49,:dataset_segy%ntraces)),&
             &MAXVAL(dataset_segy%hdr_buffer(49,:dataset_segy%ntraces)),&
             &INT( dataset_segy%ntraces )

        ! DEBUG print: CALCULATION SEEM CORRECT: BUT ONLY FOR THE VELOCITY FILE.
        ! print *,'x',REAL(dataset_segy%hdr_buffer(19,1)),'y',REAL(dataset_segy%hdr_buffer(20,1))
        ! print *,delx,dely,hypothenus_dist
        ! print *,project%master_grid%azimuth,phi,alpha,il_calc,xl_calc

     end if

     ! Exit if no more traces
     if ( dataset_segy%ntraces == 0 ) then
        print *,'ensembles read:',iensemble
        exit ensemble_read_loop
     end if

     ! Put back to file
     if ( il_calc > 4350 .and. il_calc < 4370 ) then
        oensemble = oensemble + 1
        call dataset_segy%put_ensemble()
     end if

  end do ensemble_read_loop
  call EndTime('read ensembles')

  print *,'DEBUG: Plan to output number of ensembles: ',oensemble
 

  !------------------------------------------------------------------------------------
  ! To here
  !------------------------------------------------------------------------------------
  print *,'--- In dev mode'

  STOP 'STOP'

  ! ! Allocate space for velocity data and segy headers
  ! call dataset_vp%allocate_memory()

  ! ! Read model from segy file
  ! call dataset_vp%load(fileunit=dataset_segy%iounit,ntraces=dataset_segy%ntraces)

  ! ! Print min/max info
  ! velmin=minloc(dataset_vp%dat)
  ! print *,'*** VELO MIN',MINVAL(dataset_vp%dat),velmin
  ! print *,'IL,XL,Z:',dataset_vp%hdr(inline_hdr_loc,velmin(2),velmin(3)),dataset_vp%hdr(xline_hdr_loc,velmin(2),velmin(3)),dataset_vp%samp_dz*(velmin(1)-1)
  ! velmax=maxloc(dataset_vp%dat)
  ! print *,'*** VELO MAX',MAXVAL(dataset_vp%dat),velmax
  ! print *,'IL,XL,Z:',dataset_vp%hdr(inline_hdr_loc,velmax(2),velmax(3)),dataset_vp%hdr(xline_hdr_loc,velmax(2),velmax(3)),dataset_vp%samp_dz*(velmax(1)-1)

end program LoadModelFromSegy
