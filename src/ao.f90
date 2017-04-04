module AO
  Interface
     Subroutine ReadData( Array, DataFile )
       implicit none
       real , dimension(:,:) ,intent(in) :: Array
       character*60                      :: DataFile
     End Subroutine ReadData
     !
     Subroutine WriteData( Array, DataFile )
       implicit none
       real , dimension(:,:) ,intent(in) :: Array
       character*60                      :: DataFile
     End Subroutine WriteData
     Subroutine WriteDataAscii( Array, DataFile )
       implicit none
       real , dimension(:,:) ,intent(in) :: Array
       character*60                      :: DataFile
     End Subroutine WriteDataAscii
     !
     Subroutine ReadDataVector( Vector , DataFile )
       implicit none
       real , dimension(:) ,intent(out) :: Vector
       character*60                     :: DataFile
     End Subroutine ReadDataVector
     !
     Subroutine WriteDataVector( Vector , DataFile )
       implicit none
       real , dimension(:) ,intent(in) :: Vector
       character*60                    :: DataFile
     End Subroutine WriteDataVector
     !
     Subroutine FFT(Array_real, Array_complex)
       real , dimension(:,:) ,intent(in)      :: Array_real
       complex , dimension(:,:), intent(out)  :: Array_complex
     End Subroutine FFT
     !
     Subroutine IFFT(Array_complex,Array_real)
       implicit none
       real , dimension(:,:) , intent(out)      :: Array_real
       complex , dimension(:,:) , intent(inout) :: Array_complex
     End Subroutine IFFT
     !
     Subroutine FFP(Array_real, Array_complex, Dx)
       Implicit None
       Real                  :: Dx
       Real, Intent(in)      :: Array_real(:,:)
       Complex, Intent(out)  :: Array_complex(:,:)
     End Subroutine FFP
     !
     Subroutine IFFP(Array_complex, Array_real, Dx)
       Implicit None
       Real                   :: Dx
       Real, Intent(out)      :: Array_real(:,:)
       Complex, Intent(inout) :: Array_complex(:,:)
     End Subroutine IFFP
     !
     Subroutine FHT(Array_real, Array_complex, Dx, BesselOrder)
       Implicit None
       Real                 :: Dx
       Real,Intent(in)      :: Array_real(:,:)
       Complex, Intent(out) :: Array_complex(:,:)
       Integer              :: BesselOrder
     End Subroutine FHT
     !
     Subroutine IFHT(Array_complex, Array_real, Dx, BesselOrder)
       Implicit None
       Real                   :: Dx
       Real,Intent(inout)     :: Array_real(:,:)
       Complex, Intent(inout) :: Array_complex(:,:)
       Integer              :: BesselOrder
     End Subroutine IFHT
     !
!!$     Function GausOneSource(Omega)
!!$       Use Types
!!$       Implicit None
!!$       Complex         :: GausOneSource
!!$       Complex         :: Omega
!!$     End Function GausOneSource
     !
  End Interface
  !
End module AO
