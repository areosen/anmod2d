!!$--------------------------------------------------------------------
!!$   Subroutine ReadData
!!$
!!$     Description:
!!$
!!$        Will read a dataset from file
!!$
!!$--------------------------------------------------------------------
subroutine ReadData( Array, DataFile )
  implicit none
  
  character*60          :: DataFile
  real , dimension(:,:) :: Array
  ! locals
  integer :: It, Ix, Nt, Nx
  Nt = Size( Array(:,1) )
  Nx = Size( Array(1,:) )
  
  open(unit=99,file=DataFile,form='unformatted', &
       status="old", action="read" )
  print *,'     ',DataFile
  do Ix=1,Nx
     read (99) (Array(It,Ix),It=1,Nt)
  end do
  close(99)
end subroutine ReadData
!!$--------------------------------------------------------------------
!!$   Subroutine WriteData
!!$
!!$     Description:
!!$
!!$        Will write a dataset to file
!!$
!!$--------------------------------------------------------------------
subroutine WriteData( Array, DataFile )
  implicit none
  
  character*60          :: DataFile
  real , dimension(:,:) :: Array
  ! locals
  integer :: It, Ix, Nt, Nx
  Nt = Size( Array(:,1) )
  Nx = Size( Array(1,:) )
  
  open(unit=99,file=DataFile,form='unformatted', &
       status="replace", action="write" )
  print *,'     ',DataFile
  do Ix=1,Nx
     write (99) (Array(It,Ix),It=1,Nt)
  end do
  close(99)
end subroutine WriteData
subroutine WriteDataAscii( Array, DataFile )
  implicit none
   real , dimension(:,:) :: Array
  character*60          :: DataFile
  ! locals
  integer :: It, Ix, Nt, Nx
  Nt = Size( Array(:,1) )
  Nx = Size( Array(1,:) )
  print *,'** Write to: ',DataFile
  print *,'dataio: nt,nx= ',Nt,Nx
  !
  open(unit=99,file=DataFile,form='formatted', &
       status="replace", action="write" )
  do Ix=1,Nx
     write (99,150) (Array(It,Ix),It=1,Nt)
  end do
  close(99)
150 format(E15.8)
end subroutine WriteDataAscii
!!$--------------------------------------------------------------------
!!$   Subroutine ReadDataVector
!!$
!!$     Description:
!!$
!!$        Will read a vector-dataset to file
!!$
!!$--------------------------------------------------------------------
subroutine ReadDataVector( Vector , DataFile )
  implicit none
  real , dimension(:) ,intent(out) :: Vector
  character*60                     :: DataFile
  ! locals
  integer :: It, Nt
  Nt = Size( Vector(:) )

  open(unit=99,file=DataFile,form='unformatted')
  print *,'     ',DataFile
  read (99) (Vector(It),It=1,Nt)
  close(99)
end subroutine ReadDataVector
!!$--------------------------------------------------------------------
!!$   Subroutine WriteDataVector
!!$
!!$     Description:
!!$
!!$        Will write a vector-dataset to file
!!$
!!$--------------------------------------------------------------------
subroutine WriteDataVector( Vector , DataFile )
  implicit none
  real , dimension(:) ,intent(in) :: Vector
  character*60                    :: DataFile
  ! locals
  integer :: It, Nt
  Nt = Size( Vector(:) )
  
  open(unit=99,file=DataFile,form='unformatted')
  print *,'     ',DataFile
  write (99) (Vector(It),It=1,Nt)
  close(99)
end subroutine WriteDataVector
