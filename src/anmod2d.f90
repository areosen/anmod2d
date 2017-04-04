!-----------------------------------------------------------------
! Program AnMod2D
!
! This is really an old program written before I understand f90.
! Please be aware of poor programming style and implementation.
!
! If you wish to output Vz, please make correct changes to code!
! If you wish output binary data, just edit the write function name.
!-----------------------------------------------------------------
Program AnMod2D
  Use Types; Use AO ! Modules (they are really not needed, but I used this)
  Implicit None
  ! Variables
  Integer, parameter :: Nx=4         ! Number of traces (offsets)
  Integer, parameter :: Nt=1024      ! Number of time samples
  Integer, parameter :: Nw=Nt/2+1    ! Number of positive frequencies
  Real, parameter    :: Dt=0.001     ! Sampling rate
  Real, parameter    :: Xs=0.        ! Source offset
  Real, parameter    :: Zs=200.        ! Source depth

  Real, parameter    :: Vel=1500.    ! Velocity
  Real, parameter    :: Rho=1000.    ! Density

  Character*60       :: datafile='file.asc'
  Character*60       :: streamerfile='streamer.asc'

  ! Variables
  Real, Dimension(Nt,Nx)    :: Pr,Vz  ! Data matrices
  Real               :: StreamerX(Nx) ! Streamer on file
  Real               :: StreamerZ(Nx) ! Streamer on file
  Integer            :: Ix

  ! Test initialize
  Pr=0.;Vz=0.

  ! Read streamer positions
  open (unit=20,file=streamerfile,form='formatted')
  do ix=1,nx
     read (20,*) StreamerX(ix),StreamerZ(ix)
  end do
  close (20)

  ! Begin program
  Call Model
  
  ! Write data to file: I used ascii to debug temporarily
  Call WriteDataAscii( Pr, datafile)

  ! The program used the internal procedures below

Contains
  !-------------------------------------------------------
  ! Main modeling routine
  !-------------------------------------------------------
  Subroutine Model
    Implicit None
    ! Locals
    Complex, Dimension(Nt,Nx) :: CPr,CVz
    Real                      :: Wnyq,Dw,Scale,Fact
    Real, parameter           :: Tau=1. ! MUST BE UNITY
    Real, Dimension(Nx)       :: Rminus,Rpluss
    Integer                   :: I,It,Ix,Iw
    Complex                   :: Warr(Nw),Karr(Nw),wavelet
    Character*60              :: OutFile

    ! Initialize
    CPr=0.;CVz=0.

    ! Set circular Nyquist frequency and sampling
    Wnyq = Pi/Dt
    Dw = 2.*Pi/(Nt*Dt)

    ! Make array of complex frequency components
    Scale = Alog(Tau)/((Nt-1)*Dt)
    Warr  = (/ (Cmplx( (Iw-1)*Dw, Scale ),Iw=1,Nw) /)

    ! Make array of complex wavenumbers components
    Karr = Warr/Vel
    
    ! Make arrays of distances, Rminus for source, Rpluss for mirror source
    Rminus(:) = Sqrt( ( StreamerX(:) - Xs )**2 + ( StreamerZ(:) - Zs)**2 )
    Rpluss(:) = Sqrt( ( StreamerX(:) - Xs )**2 + ( StreamerZ(:) + Zs)**2 )

    if (minval(abs(Rminus)).eq.0.) then
       stop '***STOP *** Must have Distance R>0'
    end if

    ! If inclination of receivers
    ! Fact = Cos( ATan( Deltaz/(Nx*Dx) ) ) 
    Fact = 1.0
    print *,'*** If you use Vz, pleasea check if Fact should be used in formula'

    ! Make data
    CPr = 0.;CVz = 0.
    Do Ix=1,Nx
       Do Iw=Nwst,Nw
          wavelet = GausSqrSource(Warr(Iw))
          ! Pressure direct wave
          
!!$          CPr(Iw,Ix)  = - (J/4.)*(Hankel(Karr(Iw)*Rminus(Ix),0))*wavelet
!!$
          ! Pressure dir+ghost
          CPr(Iw,Ix)  = - (J/4.)*(Hankel(Karr(Iw)*Rminus(Ix),0) &
               & - Hankel(Karr(Iw)*Rpluss(Ix),0))*wavelet
          ! Vertical velocity dir+ghost
          CVz(Iw,Ix) = Fact*(J*Karr(Iw)/(4.*J*Warr(Iw)*Rho))*&
               &((StreamerZ(Ix) - Zs)*Hankel(Karr(Iw)*Rminus(Ix),1)/Rminus(Ix) &
               &- (StreamerZ(Ix) + Zs)*Hankel(Karr(Iw)*Rpluss(Ix),1)/Rpluss(Ix)&
               &)*wavelet
       End Do
    End Do
 
    ! Transform over frequency, first set negative frequencies
    Do I=Nw+1,Nt
       CPr(I,:) = Conjg( CPr(Nt-I+2,:) )
       CVz(I,:) = Conjg( CVz(Nt-I+2,:) )
    End Do
    Do Ix=1,Nx
       call four90(nt,CPr(:,Ix),-1.0)
    End Do
    Pr = Real(CPr); Vz = Real(CVz)

!!$    ! Remove damping from data ! ---------------NOT WORKING
!!$    Do Ix=1,Nx
!!$       Do It=1,Nt
!!$          Pr(It,Ix) = Pr(It,Ix)*Exp(Tau*(It-1)*Dt)
!!$          Vz(It,Ix) = Vz(It,Ix)*Exp(Tau*(It-1)*Dt)
!!$       End Do
!!$    End Do

!!$    ! output spectrum
!!$    print *,'DEBUG trace.a'
!!$    open (unit=20,file='trace.a',form='formatted')
!!$    do it=1,nt
!!$       write (20,*) Pr(it,Nx)
!!$    end do
!!$    close (20)

    ! End Subroutine
  End Subroutine Model
  !-------------------------------
  ! Hankel function calculation
  !-------------------------------
  Function Hankel(Arg,Order)
    Implicit None
    Complex         :: Hankel
    Complex         :: Arg
    !Locals
    Complex         :: J0,Y0
    Integer         :: Order

    Call BSSLJ(Arg,Order,J0)
    Call BSSLY(Arg,Order,Y0)

    Hankel = J0 + J*Y0

    ! End Function
  End Function Hankel
  !-------------------------------
  ! Gaussian source function
  !-------------------------------
  Function GausOneSource(Omega)
    Implicit None
    Complex         :: GausOneSource
    Complex         :: Omega
    ! Locals
    Real, parameter :: Sigma=10000,T0=0.06
    ! Calc source
    GausOneSource = J*Omega*Sqrt(Pi)/Sigma*Exp(-Omega**2/(4*Sigma) + J*Omega*T0)    ! End Function
  End Function GausOneSource
  !-------------------------------
  ! Gaussian source function
  !-------------------------------
  Function GausSqrSource(Omega)
    Implicit None
    Complex         :: GausSqrSource
    Complex         :: Omega
    ! Locals
    Real, parameter :: Sigma=10000,T0=0.06
    ! Calc source
    GausSqrSource = J*Omega*Sqrt(Pi)/Sigma*Exp(-Omega**2/(4*Sigma) + J*Omega*T0)    ! End Function
  End Function GausSqrSource
  
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

  ! End Program
End Program AnMod2D
