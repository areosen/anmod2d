!
! Output source spectrum and time wavelet
! Compile: ifort prog-src.f90 four.f90
! Execute: a.out
! Plot:    a2b n1=1 < wavelet.a | suaddhead ns=1024 | suxgraph style=normal
!
!
!
!
! Input data: Ricker: suwaveform type=ricker1 dt=0.004 ns=1024 | sustrip | b2a n1=1 > ricker.asc
!
program source_output
  
  implicit none

  integer, parameter :: nt=1024, nw=513
  complex, parameter :: j=cmplx(0.,1.)
  integer :: i, it, iw
  real    :: dt=0.004
  real    :: t0=0.
  real    :: sigma=100.
  real    :: rwavelet(nt)
  real    :: Pi
  complex :: source
  complex :: omega
  complex :: cwavelet(nt)

  Pi= 2*acos(0.)
  cwavelet = cmplx(0.,0.);rwavelet = 0.

!!$  ! Make Gaussian wavelet
!!$  open (unit=20,file='spectrum.a',form='formatted')
!!$  do iw=2,nw
!!$     omega = (iw-1)*4.*ACOS(0.)/(1024*0.012)
!!$     source = exp(-omega**2/(4.*sigma))*exp(j*omega*t0)&
!!$          &*j*omega
!!$     source = sqrt(source*conjg(source))
!!$     cwavelet(iw)=source
!!$     write (20,*) real(source)
!!$     ! print *,real(source)
!!$  end do
!!$  close (20)
!!$  do iw=nw+1,nt
!!$     cwavelet(iw) = conjg(cwavelet(nt-iw+2))
!!$  end do
!!$  call four(cwavelet,nt,nt,dt,-1)
!!$  open (unit=20,file='wavelet.a',form='formatted')
!!$  do it=1,nt
!!$     write (20,*) real(cwavelet(it))
!!$  end do
!!$  close (20)
  

  ! Read external wavelet
  open (unit=20,file='ricker.asc',form='formatted')
  read (20,*) (rwavelet(it),it=1,nt)
  close (20)

  ! fft
  cwavelet=rwavelet

  ! call dlfft(cwavelet,nt) § DIMA fft ok
  ! call four(cwavelet,nt,nt,dt,1) ! The four is not working!
  call four90(nt,cwavelet,1.0)
  
  ! output spectrum
  open (unit=20,file='spec.a',form='formatted')
  do iw=1,nw
     write (20,*) real(cwavelet(iw)*conjg(cwavelet(iw)))
  end do
  close (20)

  !-------------------------- CONTAINS

contains
  !--------------------------------------------------------------------
  ! Subroutine Four90
  !--------------------------------------------------------------------
  subroutine Four90(Samples,Cvec,Signi)
    ! use types
    implicit none
    ! global variables
    integer                          :: Samples
    complex, dimension(Samples) :: Cvec
    real                             :: Signi
    ! local variables
    integer                          :: I,I2,K,M,L,Istep
    real                             :: Sc
    complex                          :: CARG,CW,Ctemp

    K=1
    SC=SQRT(1.0/REAL(Samples))
    DO 30 I=1,Samples
       IF(I.GT.K) GOTO 10
       CTEMP=Cvec(K)*SC
       Cvec(K)=Cvec(I)*SC
       Cvec(I)=CTEMP
10     M=Samples/2
20     IF(K.LE.M) GOTO 30
       K=K-M
       M=M/2
       IF(M.GE.1) GOTO 20
30     K=K+M
       L=1
40     ISTEP=2*L
       DO 50 M=1,L
          CARG=(0.,1.)*(Pi*SIGNI*(M-1))/L
          CW=CEXP(CARG)
          DO 50 I2=M,Samples,ISTEP
             CTEMP=CW*Cvec(I2+L)
             Cvec(I2+L)=Cvec(I2)-CTEMP
50           Cvec(I2)=Cvec(I2)+CTEMP
             L=ISTEP
             IF(L.LT.Samples) GOTO 40
             RETURN
           end subroutine Four90


end program source_output
