      SUBROUTINE DLFFT(A,N)
C
C  FAST FOURIER TRANSFORM OF COMPLEX VECTOR
C
      COMPLEX A(1),W,T
      DATA PI/3.141592653589793/
      J=N
      M=0
10    J=J/2
      M=M+1
      IF (J .GT. 1) GO TO 10
      NV2=N/2
      NM1=N-1
      J=1
      DO 40 I=1,NM1
      IF (I .GE. J) GO TO 20
      T=A(J)
      A(J)=A(I)
      A(I)=T
20    K=NV2
30    IF (K .GE. J) GO TO 40
      J=J-K
      K=K/2
      GO TO 30
40    J=J+K
      DO 50 L=1,M
      LE=2**L
      LE1=LE/2
      P=PI/LE1
      DO 50 J=1,LE1
      Q=P*(1-J)
      W=CMPLX(COS(Q),SIN(Q))
      DO 50 I=J,N,LE
      IP=I+LE1
      T=A(IP)*W
      A(IP)=A(I)-T
50    A(I)=T+A(I)
      RETURN
      END
c**********************************************************************
c     Subroutine IDLFFT
c**********************************************************************
C
C  INVERSE OF FAST FOURIER TRANSFORM OF COMPLEX VECTOR
C
      SUBROUTINE IDLFFT(A,N)
      COMPLEX A(1),T
C
      DO 10 J=1,N
         T=A(J)
         A(J)=T/N
   10 CONTINUE
      J=2
      K=N
   20 IF (J .GE. K) GO TO 30
         T=A(J)
         A(J)=A(K)
         A(K)=T
         J=J+1
         K=K-1
         GO TO 20
   30 CONTINUE
      CALL DLFFT(A,N)
      RETURN
      END

