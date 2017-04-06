!------------------------------------------------------------------------------
! Module: Segy
! 
!> @author
!> Are Osen, Statoil R&D Exploration Geophysics
!
!> @brief
!> Make functions to IO segy or SU file
! 
!! @param SEGYOBJ Public structure to hold info on a SEGY file
!! @todo Error detection and handling
!! @todo SU file handling
!------------------------------------------------------------------------------
module Segy
  use Types
  implicit none
  ! variables
  logical, save :: segy_byte_swapped=.true. !< Normally true on Intel Linux
  type, public :: SEGYOBJ
     ! private
     character(len=STRLEN)  :: input, output, geometryfile
     integer                :: max_ensemble_size=1e5
     integer                :: max_ensembles=1e6
     integer                :: iounit, iounit_out
     integer*8              :: pos_inp, pos_out
     character*80           :: EBCDIC(40)
     integer*2              :: BINHDR(200)
     real                   :: binhdr_dt, dt
     integer                :: binhdr_nsamp, nsamp, format, ierr
     integer                :: scalel, scalco
     integer*8              :: ntraces, size, total_traces
     real*8                 :: bytes, kbytes, Mbytes, Gbytes, Tbytes
     integer                :: pkey, skey, tkey
     character(len=STRLEN)  :: primary, secondary, tertiary
     logical                :: is_open, exist, is_initialized
     real, dimension(:,:), allocatable :: trc_buffer      
     integer*4, dimension(:,:), allocatable :: hdr_buffer
   contains ! 
     procedure        :: initialize_input_segyfile
     procedure        :: initialize_output_segyfile
     procedure        :: scan
     procedure        :: sortkey
     procedure        :: load ! Obsolete: Only to use if should change to read from segy module
     procedure        :: get_ensemble
     procedure        :: put_ensemble
  end type SEGYOBJ

contains

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Read an ensemble of traces
  ! 
  !! @param segyfile
  !! @todo check file is ok
  !! @todo keep track of file position
  !----------------------------------------------------------------------------
  subroutine get_ensemble(segyfile)
    implicit none
    class(SEGYOBJ) :: segyfile
    integer :: itr, ios, irec
    integer*8 :: position, old_position
    logical :: read_trace=.true.

    ! set initial params
    segyfile%ntraces = 0
    irec = 240 + 4 * size(segyfile%trc_buffer,1)
    position = segyfile%pos_inp
    itr = 1

    ! read a single trace to get a header to compare to
    READ(UNIT=segyfile%iounit,IOSTAT=ios,POS=position) segyfile%hdr_buffer(:,itr), &
         &segyfile%trc_buffer(:,itr)
    position = position + irec
    if (ios == 0) then
       read_trace = .true.
    else
       print *,'FIRST TRACE IN ENSEMBLE NOT READ: ios=',ios,position
       read_trace = .false.
    end if

    ! loop through the file one trace at a time
    load_ensemble_traces: do while (read_trace)

       ! store file position, if the next trace is from the next ensemble 
       old_position = position

       ! Set trace index and read corresponding trace into ensemble
       itr = itr + 1
       READ(UNIT=segyfile%iounit,IOSTAT=ios,POS=position) segyfile%hdr_buffer(:,itr), &
            &segyfile%trc_buffer(:,itr)
       if (ios .ne. 0) exit load_ensemble_traces
       position = position + irec

       ! Debug print
       ! print *,itr,position, old_position,segyfile%hdr_buffer(segyfile%pkey,itr),&
       !      & segyfile%hdr_buffer(segyfile%skey,itr)

       ! Compare headers to verify if trace belong to current or next ensemble
       if ( segyfile%hdr_buffer(segyfile%pkey,itr) .ne. segyfile%hdr_buffer(segyfile%pkey,itr-1)) then

          segyfile%ntraces = itr - 1
          read_trace = .false.
          segyfile%pos_inp = old_position

       end if

    end do load_ensemble_traces

    if (segyfile%scalco .lt. 0) segyfile%hdr_buffer(19:22,:)=segyfile%hdr_buffer(19:22,:)/abs(segyfile%scalco)
    if (segyfile%scalco .gt. 0) segyfile%hdr_buffer(19:22,:)=segyfile%hdr_buffer(19:22,:)*abs(segyfile%scalco)

    if (ios < 0) print *,'*** SEGY: read fail - End of File'
    if (ios > 0) STOP '*** SEGY: read fail - Error reading File'

  end subroutine get_ensemble
  !
  !
  !
  subroutine put_ensemble(segyfile)
    implicit none
    class(SEGYOBJ) :: segyfile
    ! local
    integer :: itr, ios, irec
    integer*8 :: position, old_position
    logical :: read_trace=.true.

    ! check file is ok
    ! keep track of file position
    irec = 240 + 4 * size(segyfile%trc_buffer,1)
    position = segyfile%pos_out

    print *,'DEBUG: Dump ensemble',segyfile%iounit_out,segyfile%iounit

    if (segyfile%scalco .lt. 0) segyfile%hdr_buffer(19:22,:)=segyfile%hdr_buffer(19:22,:)*abs(segyfile%scalco)
    if (segyfile%scalco .gt. 0) segyfile%hdr_buffer(19:22,:)=segyfile%hdr_buffer(19:22,:)/abs(segyfile%scalco)

    dump_ensemble_traces: do itr=1,segyfile%ntraces

       ! write to iounit_out
       WRITE (UNIT=segyfile%iounit_out,IOSTAT=ios,POS=position) segyfile%hdr_buffer(:,itr), &
            &segyfile%trc_buffer(:,itr)
       position = position + irec
       if (ios == 0) then
          read_trace = .true.
       else
          print *,'SEGY WRITE ERROR: ios=',ios,position
          exit dump_ensemble_traces 
          read_trace = .false.
       end if

    end do dump_ensemble_traces

    if (ios < 0) print *,'*** SEGY: read fail - End of File'
    if (ios > 0) STOP '*** SEGY: read fail - Error reading File'

    segyfile%pos_out = position

  end subroutine put_ensemble
  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Set primary, secondary or tertiary sort key
  ! 
  !! @param segyfile 
  !! @todo 
  !----------------------------------------------------------------------------
  subroutine sortkey(segyfile,primary,secondary,tertiary)
    implicit none
    class(SEGYOBJ) :: segyfile
    character (len=*), intent(in), optional :: primary,secondary,tertiary
    ! defaults
    segyfile%pkey=1;
    segyfile%skey=1;
    segyfile%tkey=1;
    ! Set sort keys
    if(present(primary)) call set_key(primary,segyfile%pkey)
    if(present(secondary)) call set_key(secondary,segyfile%skey)
    if(present(tertiary)) call set_key(tertiary,segyfile%tkey)
    contains
      subroutine set_key(selkey,ikey)
        character (len=*) :: selkey
        integer :: ikey
        ! Set trace header according to sort name
        ! Remember to take hdr index=(byte-1)/4+1
        select case (trim(selkey))
           case ("il")
              print *,'SELECT inline'
              ikey=48 ! assume IL in byte 189-192
           case ("xl")
              print *,'SELECT xline'
              ikey=49 ! assume XL in byte 193-196
           case ("tracr")
              print *,'SELECT tracr'
              ikey=2 ! assume fldr in byte 5-8
           case ("fldr")
              print *,'SELECT fldr'
              ikey=3 ! assume fldr in byte 9-12
           case ("tracf")
              print *,'SELECT tracf (=chan)'
              ikey=4 ! assume tracf in byte 13-16
           case ("ep")
              print *,'SELECT ep'
              ikey=5 ! assume ep in byte 17-20
           case ("cdp")
              print *,'SELECT cdp'
              ikey=6 ! assume cdp in byte 21-24
           case ("cdpt")
              print *,'SELECT cdpt'
              ikey=7 ! assume cdpt in byte 25-28
           case ("offset")
              print *,'SELECT offset'
              ikey=10 ! assume offset in byte 37-40
           case default
              print *,'SELECT none - using tracl (byte 1-4)'
              ikey=1
           end select
      end subroutine set_key
  end subroutine sortkey

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Scan a file
  ! 
  !! @param segyfile 
  !! @todo make a sortmap table for direct access 
  !! @todo change from allocating an array - to just check trace-by-trace
  !----------------------------------------------------------------------------
  subroutine scan(segyfile)
    implicit none
    class(SEGYOBJ) :: segyfile
    ! local
    integer :: i,ios,ipos,il_index,xl_index
    integer*4 :: trace_hdr4(60,segyfile%total_traces) ! Fix: This is a huge vector on large files
    real :: rvalue
    integer :: ntraces_outside=0, ntraces_offgrid=0, ivalue, idiff
    integer*8 :: position
    integer :: file_skip
    ! initialize
    file_skip = 240 + 4*segyfile%nsamp
    trace_hdr4=0
    ! Skip EBCDIC+BINHDR
    position = 3601
    ! Read all traces
    load_all_functions: do i=1,segyfile%total_traces
       READ(UNIT=segyfile%iounit,POS=position,IOSTAT=ios) trace_hdr4(:,i)
       if (ios .ne. 0) exit load_all_functions
       position = position + file_skip
    end do load_all_functions
    if (ios < 0) STOP '*** SEGY: read fail - End of File'
    if (ios > 0) STOP '*** SEGY: read fail - Error reading File'
    ! print info
    print *,minval(trace_hdr4(segyfile%pkey,:)),maxval(trace_hdr4(segyfile%pkey,:))
    print *,minval(trace_hdr4(segyfile%skey,:)),maxval(trace_hdr4(segyfile%skey,:))
    print *,minval(trace_hdr4(segyfile%tkey,:)),maxval(trace_hdr4(segyfile%tkey,:))   ! Maybe make this print optional
    if (segyfile%scalco .lt. 0) trace_hdr4(19:22,:)=trace_hdr4(19:22,:)/abs(segyfile%scalco)
    if (segyfile%scalco .gt. 0) trace_hdr4(19:22,:)=trace_hdr4(19:22,:)*abs(segyfile%scalco)
    if (segyfile%scalco .lt. 0) trace_hdr4(46:47,:)=trace_hdr4(46:47,:)/abs(segyfile%scalco)
    if (segyfile%scalco .gt. 0) trace_hdr4(46:47,:)=trace_hdr4(46:47,:)*abs(segyfile%scalco)

       print *,"Sou-x",minval(trace_hdr4(19,:)),maxval(trace_hdr4(19,:))
       print *,"Sou-y",minval(trace_hdr4(20,:)),maxval(trace_hdr4(20,:))
       print *,"Rec-x",minval(trace_hdr4(21,:)),maxval(trace_hdr4(21,:))
       print *,"Rec-y",minval(trace_hdr4(22,:)),maxval(trace_hdr4(22,:))
       print *,"CDP-x",minval(trace_hdr4(46,:)),maxval(trace_hdr4(46,:))
       print *,"CDP-y",minval(trace_hdr4(47,:)),maxval(trace_hdr4(47,:))

  end subroutine scan

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Load a file
  ! 
  !! @param segyfile 
  !! @todo Load a file :)
  !----------------------------------------------------------------------------
  subroutine load(segyfile)
    implicit none
    class(SEGYOBJ) :: segyfile
    ! local
    integer :: i,ios,ipos,il_index,xl_index
    integer*2 :: trace_hdr2(120) !--------------------------------------------OBSOLETE. can rename hdr4 too
    integer*4 :: trace_hdr4(60)
    real :: rvalue
    integer :: ntraces_outside=0, ntraces_offgrid=0, ivalue, idiff
  end subroutine load

  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Verify existence, size and key content. Set boolean initialization flag
  ! 
  !! @param segyfile 
  !! @todo Error detection and handling
  !! @todo Use open with NEWUNIT to avoid scan for available unit
  !----------------------------------------------------------------------------
  subroutine initialize_output_segyfile(segyfile)
    implicit none
    class(SEGYOBJ) :: segyfile
    ! local
    integer :: i, file_pos, ios
    logical :: unit_is_open
    integer*8      :: number_of_bytes,number_of_traces, itest
    integer*2 :: trace_hdr2(120)
    integer*4 :: trace_hdr4(60)
    ! check existence, select free unit
    print *,'*** SEGY: Open file (assuming IBM format only): ',trim(adjustl(segyfile%output))
    print *,'*** SEGY: OPEN FILES: Warning - no proper error handling presently'
    INQUIRE(FILE=segyfile%output, OPENED=unit_is_open)
    if (unit_is_open) return


    PRINT *,'TEMP FIX iounit_out ' ! Should find safe setting of in/out units


    ! Find IO unit
    OPEN(NEWUNIT=segyfile%iounit_out,FILE=segyfile%output,FORM='unformatted',&
               &CONVERT='IBM',POSITION='rewind',ACCESS='stream',ACTION='write')

    ! EBCDIC+BINHDR
    WRITE(segyfile%iounit_out,POS=1,iostat=ios) segyfile%EBCDIC,segyfile%BINHDR


  end subroutine initialize_output_segyfile
  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Verify existence, size and key content. Set boolean initialization flag
  ! 
  !! @param segyfile 
  !! @todo Error detection and handling
  !! @todo Use open with NEWUNIT to avoid scan for available unit
  !----------------------------------------------------------------------------
  subroutine initialize_input_segyfile(segyfile)
    implicit none
    class(SEGYOBJ) :: segyfile
    ! local
    integer :: i, file_pos, ios, alloc_stat
    logical :: unit_is_open
    integer*8      :: number_of_bytes,number_of_traces, itest
    integer*2 :: trace_hdr2(120)
    integer*4 :: trace_hdr4(60)
    ! check if already done
    if (segyfile%is_initialized) then
       print *,'*** SEGY: ALREADY INITIALIZED'; return
    end if
    ! check existence, select free unit
    print *,'*** SEGY: Open file (assuming IBM format only): ',trim(adjustl(segyfile%input))
    print *,'*** SEGY: OPEN FILES: Warning - no proper error handling presently'
    INQUIRE(FILE=segyfile%input, EXIST=segyfile%exist)
    if (.not. segyfile%exist) STOP '*** SEGY: NO SUCH FILE'
    ! Find IO unit
    OPEN(NEWUNIT=segyfile%iounit,FILE=segyfile%input,FORM='unformatted',&
               &CONVERT='IBM',POSITION='append',ACCESS='stream',ACTION='read')
    INQUIRE(UNIT=segyfile%iounit,POS=segyfile%size,OPENED=segyfile%is_open); segyfile%size=segyfile%size-1
    ! EBCDIC+BINHDR
    READ(segyfile%iounit,POS=1, iostat=ios) segyfile%EBCDIC,segyfile%BINHDR
    if (ios < 0) STOP '*** SEGY BIN/EBCDIC read fail - End of File'
    if (ios > 0) STOP '*** SEGY BIN/EBCDIC read fail - Error reading File'
    ! Set two important headers
    segyfile%binhdr_dt      = segyfile%BINHDR(9) ! REAL(segyfile%BINHDR(9)/1000000.)
    segyfile%binhdr_nsamp   = segyfile%BINHDR(11)
    ! Check
    INQUIRE(UNIT=segyfile%iounit,POS=file_pos)
    ! print *,'DEBUG: POS=',file_pos
    ! print *,'DEBUG: binary header: dt,nt',segyfile%binhdr_dt,segyfile%binhdr_nsamp
    ! set more segy hdrs
    file_pos=3601
    READ(segyfile%iounit,POS=file_pos) trace_hdr2(:)
    segyfile%scalel = trace_hdr2(35)
    segyfile%scalco = trace_hdr2(36)
    segyfile%nsamp  = trace_hdr2(58)
    segyfile%dt     = trace_hdr2(59)
    ! print *,'DEBUG: trace header: dt,nt',segyfile%dt,segyfile%nsamp
    if (segyfile%binhdr_dt .ne. segyfile%dt .or. segyfile%binhdr_nsamp .ne. segyfile%nsamp) then
       print *,'*** SEGY: Binary and trace header mismatch'
       print *,'DEBUG: Binary: ',segyfile%binhdr_nsamp,segyfile%binhdr_dt
       print *,'DEBUG: Trace:  ',segyfile%nsamp,segyfile%dt
       stop '*** SEGY: Inconsistency detected'
    end if
    ! Find number of traces
    number_of_traces = (segyfile%size-3600)/(segyfile%nsamp*4+240)
    number_of_bytes  = 3600 + number_of_traces*(segyfile%nsamp*4+240) 
    segyfile%bytes = number_of_bytes
    segyfile%kbytes = segyfile%bytes/1024
    segyfile%Mbytes = segyfile%kbytes/1024
    segyfile%Gbytes = segyfile%Mbytes/1024
    segyfile%Tbytes = segyfile%Gbytes/1024
    write (*,'(A20)',advance="no") '*** SEGY: File size'
    write (*,'(F21.0,A6)',advance="no") segyfile%bytes,' bytes:'
    write (*,'(F15.1,A4)',advance="no") segyfile%kbytes,' kb:'
    write (*,'(F15.1,A4)',advance="no") segyfile%Mbytes,' Mb:'
    write (*,'(F15.1,A4)',advance="no") segyfile%Gbytes,' Gb:'
    write (*,'(F15.1,A4)',advance="yes") segyfile%Tbytes,' Tb:'
    
    ! set total number of traces in dataset
    segyfile%total_traces = number_of_traces

    if (number_of_bytes .ne. segyfile%size) stop '*** SEGY: Inconsistent filesize '

    ! Allocate buffers
    allocate( segyfile%trc_buffer(segyfile%nsamp,segyfile%max_ensemble_size), STAT=alloc_stat)
    if (alloc_stat .ne. 0) stop 'Unable to allocate input trace buffer'
    allocate( segyfile%hdr_buffer(60,segyfile%max_ensemble_size), STAT=alloc_stat)
    if (alloc_stat .ne. 0) stop 'Unable to allocate input header buffer'

    ! Finalize initialization
    segyfile%is_initialized = .true.
    print *,'**************************************************'
    print *,'*** SEGY: SAMPRAT       =               ', segyfile%BINHDR(9)
    print *,'*** SEGY: NUMSAMP       =               ', segyfile%BINHDR(11)
    print *,'*** SEGY: BINHDR FORMAT =               ', segyfile%BINHDR(13)
    print *,'*** SEGY: TRACES        =', segyfile%total_traces
    print *,'**************************************************'
  end subroutine initialize_input_segyfile
  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Swap bytes in an integer array
  ! 
  !! @param ARRAY 
  !! @param NWAP number of elements from ARRAY to swap
  !----------------------------------------------------------------------------
  SUBROUTINE SWAPI2 (ARRAY, NSWAP)
    ! Swaps adjacent bytes in an INTEGER*2 array.
    IMPLICIT NONE
    INTEGER*2                :: ARRAY(*)
    INTEGER                  :: NSWAP
    INTEGER                  :: I
    INTEGER*1                :: BYTE(2)
    INTEGER*1                :: TEMP
    DO I = 1, NSWAP
       BYTE     = TRANSFER(ARRAY(I), BYTE)
       TEMP     = BYTE(1)
       BYTE(1)  = BYTE(2)
       BYTE(2)  = TEMP
       ARRAY(I) = TRANSFER(BYTE, ARRAY(I))
    END DO
    RETURN
  END SUBROUTINE SWAPI2
  !----------------------------------------------------------------------------
  !> @author
  !> Are Osen, Statoil R&D Exploration Geophysics
  !
  !> @brief
  !> Check if byte swap is needed
  ! 
  !! @param[out] find_byte_swap Logical 
  !----------------------------------------------------------------------------
  function find_byte_swap()
    implicit none
    logical :: find_byte_swap
    ! local
    integer(i2b) :: I4
    integer(i1b) :: I2(2)
    equivalence(I4,I2)
    I4=1
    if (I2(1)==1) then
       find_byte_swap=.true.
    else
       find_byte_swap=.false.
    end if
  end function find_byte_swap
end module Segy
