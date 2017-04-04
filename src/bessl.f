      SUBROUTINE BSSLJ (A, IN, W)
C     ******************************************************************
C     FORTRAN SUBROUTINE FOR ORDINARY BESSEL FUNCTION OF INTEGRAL ORDER
C     ******************************************************************
C     A  = ARGUMENT (COMPLEX NUMBER)
C     IN = ORDER (INTEGER)
C     W  = FUNCTION OF FIRST KIND (COMPLEX NUMBER)
C     -------------------
      COMPLEX A, W
      DIMENSION AZ(2), FJ(2)
      DIMENSION CD(30), CE(30)
      DIMENSION QZ(2), RZ(2), SZ(2), ZR(2)
      DIMENSION TS(2), TM(2), RM(4), SM(4), AQ(2), QF(2)
      DATA CD(1) / 0.00000000000000E00/,  CD(2) /-1.64899505142212E-2/,
     1     CD(3) /-7.18621880068536E-2/,  CD(4) /-1.67086878124866E-1/,
     2     CD(5) /-3.02582250219469E-1/,  CD(6) /-4.80613945245927E-1/,
     3     CD(7) /-7.07075239357898E-1/,  CD(8) /-9.92995790539516E-1/,
     4     CD(9) /-1.35583925612592E00/,  CD(10)/-1.82105907899132E00/,
     5     CD(11)/-2.42482175310879E00/,  CD(12)/-3.21956655708750E00/,
     6     CD(13)/-4.28658077248384E00/,  CD(14)/-5.77022816798128E00/,
     7     CD(15)/-8.01371260952526E00/
      DATA CD(16)/ 0.00000000000000E00/,  CD(17)/-5.57742429879505E-3/,
     1     CD(18)/-4.99112944172476E-2/,  CD(19)/-1.37440911652397E-1/,
     2     CD(20)/-2.67233784710566E-1/,  CD(21)/-4.40380166808682E-1/,
     3     CD(22)/-6.61813614872541E-1/,  CD(23)/-9.41861077665017E-1/,
     4     CD(24)/-1.29754130468326E00/,  CD(25)/-1.75407696719816E00/,
     5     CD(26)/-2.34755299882276E00/,  CD(27)/-3.13041332689196E00/,
     6     CD(28)/-4.18397120563729E00/,  CD(29)/-5.65251799214994E00/,
     7     CD(30)/-7.87863959810677E00/
      DATA CE(1) / 0.00000000000000E00/,  CE(2) /-4.80942336387447E-3/,
     1     CE(3) /-1.31366200347759E-2/,  CE(4) /-1.94843834008458E-2/,
     2     CE(5) /-2.19948900032003E-2/,  CE(6) /-2.09396625676519E-2/,
     3     CE(7) /-1.74600268458650E-2/,  CE(8) /-1.27937813362085E-2/,
     4     CE(9) /-8.05234421796592E-3/,  CE(10)/-4.15817375002760E-3/,
     5     CE(11)/-1.64317738747922E-3/,  CE(12)/-4.49175585314709E-4/,
     6     CE(13)/-7.28594765574007E-5/,  CE(14)/-5.38265230658285E-6/,
     7     CE(15)/-9.93779048036289E-8/
      DATA CE(16)/ 0.00000000000000E00/,  CE(17)/ 7.53805779200591E-2/,
     1     CE(18)/ 7.12293537403464E-2/,  CE(19)/ 6.33116224228200E-2/,
     2     CE(20)/ 5.28240264523301E-2/,  CE(21)/ 4.13305359441492E-2/,
     3     CE(22)/ 3.01350573947510E-2/,  CE(23)/ 2.01043439592720E-2/,
     4     CE(24)/ 1.18552223068074E-2/,  CE(25)/ 5.86055510956010E-3/,
     5     CE(26)/ 2.25465148267325E-3/,  CE(27)/ 6.08173041536336E-4/,
     6     CE(28)/ 9.84215550625747E-5/,  CE(29)/ 7.32139093038089E-6/,
     7     CE(30)/ 1.37279667384666E-7/
C     -------------------
      AZ(1)=REAL(A)
      AZ(2)=AIMAG(A)
      ZS=AZ(1)*AZ(1)+AZ(2)*AZ(2)
      ZM=SQRT(ZS)
      PN=IABS(IN)
      SN=+1.0
      IF(IN)002,003,003
  002 IF(IN.EQ.IN/2*2)GO TO 003
      SN=-1.0
  003 IF(AZ(1))004,005,005
  004 QZ(1)=-AZ(1)
      QZ(2)=-AZ(2)
      IF(IN.EQ.IN/2*2)GO TO 006
      SN=-SN
      GO TO 006
  005 QZ(1)=+AZ(1)
      QZ(2)=+AZ(2)
  006 IF(ZM.LE.17.5+0.5*PN*PN)GO TO 007
      QN=PN
      GO TO 013
  007 QN=0.5*ZM-0.5*ABS(QZ(2))+0.5*ABS(0.5*ZM-ABS(QZ(2)))
      IF(PN.LE.QN)GO TO 008
      QN=+AINT(0.0625*ZS)
      IF(PN.LE.QN)GO TO 031
      QN=PN
      GO TO 031
  008 IF(ZM.LE.17.5)GO TO 009
      QN=+AINT(SQRT(2.0*(ZM-17.5)))
      GO TO 013
  009 IF(ZS-1.0)011,010,010
  010 IF(-ABS(AZ(2))+0.096*AZ(1)*AZ(1))011,012,012
  011 QN=+AINT(0.0625*ZS)
      IF(PN.LE.QN)GO TO 031
      QN=PN
      GO TO 031
  012 QN=0.0
  013 SZ(1)=QZ(1)
      SZ(2)=QZ(2)
      QM=SN*0.797884560802865
      ZR(1)=SQRT(SZ(1)+ZM)
      ZR(2)=SZ(2)/ZR(1)
      ZR(1)=0.707106781186548*ZR(1)
      ZR(2)=0.707106781186548*ZR(2)
      QF(1)=+QM*ZR(1)/ZM
      QF(2)=-QM*ZR(2)/ZM
      IF(ZM.LE.17.5)GO TO 018
  014 RZ(1)=+0.5*QZ(1)/ZS
      RZ(2)=-0.5*QZ(2)/ZS
      AN=QN*QN-0.25
      SM(1)=0.0
      SM(2)=0.0
      SM(3)=0.0
      SM(4)=0.0
      TM(1)=1.0
      TM(2)=0.0
      PM=0.0
      GO TO 016
  015 AN=AN-2.0*PM
      PM=PM+1.0
      TS(1)=TM(1)*RZ(1)-TM(2)*RZ(2)
      TS(2)=TM(1)*RZ(2)+TM(2)*RZ(1)
      TM(1)=-AN*TS(1)/PM
      TM(2)=-AN*TS(2)/PM
  016 SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      AN=AN-2.0*PM
      PM=PM+1.0
      TS(1)=TM(1)*RZ(1)-TM(2)*RZ(2)
      TS(2)=TM(1)*RZ(2)+TM(2)*RZ(1)
      TM(1)=+AN*TS(1)/PM
      TM(2)=+AN*TS(2)/PM
      IF(ABS(SM(3))+ABS(TM(1)).NE.ABS(SM(3)))GO TO 017
      IF(ABS(SM(4))+ABS(TM(2)).EQ.ABS(SM(4)))GO TO 020
  017 SM(3)=SM(3)+TM(1)
      SM(4)=SM(4)+TM(2)
      IF(PM.LT.35.0)GO TO 015
      GO TO 020
  018 SM(1)=1.0
      SM(2)=0.0
      SM(3)=1.0
      SM(4)=0.0
      M=15.0*QN+2.0
      N=15.0*QN+15.0
      DO 019 I=M,N
      TS(1)=+QZ(2)-CD(I)
      TS(2)=-QZ(1)
      SS=TS(1)*TS(1)+TS(2)*TS(2)
      TM(1)=+CE(I)*TS(1)/SS
      TM(2)=-CE(I)*TS(2)/SS
      SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      TS(1)=-QZ(2)-CD(I)
      TS(2)=+QZ(1)
      SS=TS(1)*TS(1)+TS(2)*TS(2)
      TM(1)=+CE(I)*TS(1)/SS
      TM(2)=-CE(I)*TS(2)/SS
      SM(3)=SM(3)+TM(1)
      SM(4)=SM(4)+TM(2)
  019 CONTINUE
      TS(1)=+0.5*(SM(2)-SM(4))
      TS(2)=-0.5*(SM(1)-SM(3))
      SM(1)=+0.5*(SM(1)+SM(3))
      SM(2)=+0.5*(SM(2)+SM(4))
      SM(3)=TS(1)
      SM(4)=TS(2)
  020 AQ(1)=QZ(1)-1.57079632679490*(QN+0.5)
      AQ(2)=QZ(2)
      TS(1)=+COS(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=-SIN(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      TM(1)=SM(1)*TS(1)-SM(2)*TS(2)
      TM(2)=SM(1)*TS(2)+SM(2)*TS(1)
      TS(1)=+SIN(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=+COS(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      RM(1)=TM(1)-SM(3)*TS(1)+SM(4)*TS(2)
      RM(2)=TM(2)-SM(3)*TS(2)-SM(4)*TS(1)
      IF(QN.EQ.PN)GO TO 030
      RM(3)=RM(1)
      RM(4)=RM(2)
      QN=QN+1.0
      IF(ZM.LE.17.5)GO TO 025
  021 AN=QN*QN-0.25
      SM(1)=0.0
      SM(2)=0.0
      SM(3)=0.0
      SM(4)=0.0
      TM(1)=1.0
      TM(2)=0.0
      PM=0.0
      GO TO 023
  022 AN=AN-2.0*PM
      PM=PM+1.0
      TS(1)=TM(1)*RZ(1)-TM(2)*RZ(2)
      TS(2)=TM(1)*RZ(2)+TM(2)*RZ(1)
      TM(1)=-AN*TS(1)/PM
      TM(2)=-AN*TS(2)/PM
  023 SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      AN=AN-2.0*PM
      PM=PM+1.0
      TS(1)=TM(1)*RZ(1)-TM(2)*RZ(2)
      TS(2)=TM(1)*RZ(2)+TM(2)*RZ(1)
      TM(1)=+AN*TS(1)/PM
      TM(2)=+AN*TS(2)/PM
      IF(ABS(SM(3))+ABS(TM(1)).NE.ABS(SM(3)))GO TO 024
      IF(ABS(SM(4))+ABS(TM(2)).EQ.ABS(SM(4)))GO TO 027
  024 SM(3)=SM(3)+TM(1)
      SM(4)=SM(4)+TM(2)
      IF(PM.LT.35.0)GO TO 022
      GO TO 027
  025 SM(1)=1.0
      SM(2)=0.0
      SM(3)=1.0
      SM(4)=0.0
      M=15.0*QN+2.0
      N=15.0*QN+15.0
      DO 026 I=M,N
      TS(1)=+QZ(2)-CD(I)
      TS(2)=-QZ(1)
      SS=TS(1)*TS(1)+TS(2)*TS(2)
      TM(1)=+CE(I)*TS(1)/SS
      TM(2)=-CE(I)*TS(2)/SS
      SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      TS(1)=-QZ(2)-CD(I)
      TS(2)=+QZ(1)
      SS=TS(1)*TS(1)+TS(2)*TS(2)
      TM(1)=+CE(I)*TS(1)/SS
      TM(2)=-CE(I)*TS(2)/SS
      SM(3)=SM(3)+TM(1)
      SM(4)=SM(4)+TM(2)
  026 CONTINUE
      TS(1)=+0.5*(SM(2)-SM(4))
      TS(2)=-0.5*(SM(1)-SM(3))
      SM(1)=+0.5*(SM(1)+SM(3))
      SM(2)=+0.5*(SM(2)+SM(4))
      SM(3)=TS(1)
      SM(4)=TS(2)
  027 AQ(1)=QZ(1)-1.57079632679490*(QN+0.5)
      AQ(2)=QZ(2)
      TS(1)=+COS(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=-SIN(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      TM(1)=SM(1)*TS(1)-SM(2)*TS(2)
      TM(2)=SM(1)*TS(2)+SM(2)*TS(1)
      TS(1)=+SIN(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=+COS(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      RM(1)=TM(1)-SM(3)*TS(1)+SM(4)*TS(2)
      RM(2)=TM(2)-SM(3)*TS(2)-SM(4)*TS(1)
      GO TO 029
  028 TM(1)=+2.0*QN*QZ(1)/ZS
      TM(2)=-2.0*QN*QZ(2)/ZS
      TS(1)=TM(1)*RM(1)-TM(2)*RM(2)-RM(3)
      TS(2)=TM(1)*RM(2)+TM(2)*RM(1)-RM(4)
      RM(3)=RM(1)
      RM(4)=RM(2)
      RM(1)=TS(1)
      RM(2)=TS(2)
      QN=QN+1.0
  029 IF(QN.LT.PN)GO TO 028
  030 FJ(1)=QF(1)*RM(1)-QF(2)*RM(2)
      FJ(2)=QF(1)*RM(2)+QF(2)*RM(1)
      W=CMPLX(FJ(1),FJ(2))
      RETURN
  031 SZ(1)=+0.25*(QZ(1)*QZ(1)-QZ(2)*QZ(2))
      SZ(2)=+0.5*QZ(1)*QZ(2)
      AN=QN
      SM(1)=0.0
      SM(2)=0.0
      SM(3)=0.0
      SM(4)=0.0
      TM(1)=1.0
      TM(2)=0.0
      PM=0.0
  032 AN=AN+1.0
      TS(1)=+TM(1)/AN
      TS(2)=+TM(2)/AN
      SM(3)=SM(3)+TS(1)
      SM(4)=SM(4)+TS(2)
      TM(1)=-TS(1)*SZ(1)+TS(2)*SZ(2)
      TM(2)=-TS(1)*SZ(2)-TS(2)*SZ(1)
      PM=PM+1.0
      TM(1)=TM(1)/PM
      TM(2)=TM(2)/PM
      IF(ABS(SM(1))+ABS(TM(1)).NE.ABS(SM(1)))GO TO 033
      IF(ABS(SM(2))+ABS(TM(2)).EQ.ABS(SM(2)))GO TO 034
  033 SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      GO TO 032
  034 SM(1)=SM(1)+1.0
      AN=QN+1.0
      SM(3)=AN*SM(3)
      SM(4)=AN*SM(4)
      GO TO 036
  035 AN=QN*(QN+1.0)
      TM(1)=SZ(1)/AN
      TM(2)=SZ(2)/AN
      TS(1)=-TM(1)*SM(3)+TM(2)*SM(4)
      TS(2)=-TM(1)*SM(4)-TM(2)*SM(3)
      SM(3)=SM(1)
      SM(4)=SM(2)
      SM(1)=SM(1)+TS(1)
      SM(2)=SM(2)+TS(2)
      QN=QN-1.0
  036 IF(QN.GT.PN)GO TO 035
      QF(1)=SN
      QF(2)=0.0
      QN=0.0
      GO TO 038
  037 QN=QN+1.0
      TM(1)=QF(1)*QZ(1)-QF(2)*QZ(2)
      TM(2)=QF(1)*QZ(2)+QF(2)*QZ(1)
      QF(1)=0.5*TM(1)/QN
      QF(2)=0.5*TM(2)/QN
  038 IF(QN.LT.PN)GO TO 037
      FJ(1)=QF(1)*SM(1)-QF(2)*SM(2)
      FJ(2)=QF(1)*SM(2)+QF(2)*SM(1)
      W=CMPLX(FJ(1),FJ(2))
      RETURN
      END

      SUBROUTINE BSSLY (A, IN, W)
C     ******************************************************************
C     FORTRAN SUBROUTINE FOR ORDINARY BESSEL FUNCTION OF INTEGRAL ORDER
C     ******************************************************************
C     A  = ARGUMENT (COMPLEX NUMBER)
C     IN = ORDER (INTEGER)
C     W  = FUNCTION OF SECOND KIND (COMPLEX NUMBER)
C     -------------------
      COMPLEX A, W
      DIMENSION AZ(2)
      DIMENSION CD(30), CE(30)
      DIMENSION QZ(2), RZ(2), SZ(2), ZL(2)
      DIMENSION TS(2), TM(4), SM(4), SL(2), SQ(2), SR(2), AQ(2), QF(2)
      DATA CD(1) / 0.00000000000000E00/,  CD(2) /-1.64899505142212E-2/,
     1     CD(3) /-7.18621880068536E-2/,  CD(4) /-1.67086878124866E-1/,
     2     CD(5) /-3.02582250219469E-1/,  CD(6) /-4.80613945245927E-1/,
     3     CD(7) /-7.07075239357898E-1/,  CD(8) /-9.92995790539516E-1/,
     4     CD(9) /-1.35583925612592E00/,  CD(10)/-1.82105907899132E00/,
     5     CD(11)/-2.42482175310879E00/,  CD(12)/-3.21956655708750E00/,
     6     CD(13)/-4.28658077248384E00/,  CD(14)/-5.77022816798128E00/,
     7     CD(15)/-8.01371260952526E00/
      DATA CD(16)/ 0.00000000000000E00/,  CD(17)/-5.57742429879505E-3/,
     1     CD(18)/-4.99112944172476E-2/,  CD(19)/-1.37440911652397E-1/,
     2     CD(20)/-2.67233784710566E-1/,  CD(21)/-4.40380166808682E-1/,
     3     CD(22)/-6.61813614872541E-1/,  CD(23)/-9.41861077665017E-1/,
     4     CD(24)/-1.29754130468326E00/,  CD(25)/-1.75407696719816E00/,
     5     CD(26)/-2.34755299882276E00/,  CD(27)/-3.13041332689196E00/,
     6     CD(28)/-4.18397120563729E00/,  CD(29)/-5.65251799214994E00/,
     7     CD(30)/-7.87863959810677E00/
      DATA CE(1) / 0.00000000000000E00/,  CE(2) /-4.80942336387447E-3/,
     1     CE(3) /-1.31366200347759E-2/,  CE(4) /-1.94843834008458E-2/,
     2     CE(5) /-2.19948900032003E-2/,  CE(6) /-2.09396625676519E-2/,
     3     CE(7) /-1.74600268458650E-2/,  CE(8) /-1.27937813362085E-2/,
     4     CE(9) /-8.05234421796592E-3/,  CE(10)/-4.15817375002760E-3/,
     5     CE(11)/-1.64317738747922E-3/,  CE(12)/-4.49175585314709E-4/,
     6     CE(13)/-7.28594765574007E-5/,  CE(14)/-5.38265230658285E-6/,
     7     CE(15)/-9.93779048036289E-8/
      DATA CE(16)/ 0.00000000000000E00/,  CE(17)/ 7.53805779200591E-2/,
     1     CE(18)/ 7.12293537403464E-2/,  CE(19)/ 6.33116224228200E-2/,
     2     CE(20)/ 5.28240264523301E-2/,  CE(21)/ 4.13305359441492E-2/,
     3     CE(22)/ 3.01350573947510E-2/,  CE(23)/ 2.01043439592720E-2/,
     4     CE(24)/ 1.18552223068074E-2/,  CE(25)/ 5.86055510956010E-3/,
     5     CE(26)/ 2.25465148267325E-3/,  CE(27)/ 6.08173041536336E-4/,
     6     CE(28)/ 9.84215550625747E-5/,  CE(29)/ 7.32139093038089E-6/,
     7     CE(30)/ 1.37279667384666E-7/
C     -------------------
      AZ(1)=REAL(A)
      AZ(2)=AIMAG(A)
      ZS=AZ(1)*AZ(1)+AZ(2)*AZ(2)
      ZL(1)=0.5*ALOG(ZS)
      ZL(2)=ATAN2(AZ(2),AZ(1))
      AN=IABS(IN)
      SN=+1.0
      IF(IN)002,003,003
  002 IF(IN.EQ.IN/2*2)GO TO 003
      SN=-1.0
  003 IF(AZ(1))004,005,005
  004 QZ(1)=-AZ(1)
      QZ(2)=-AZ(2)
      GO TO 006
  005 QZ(1)=+AZ(1)
      QZ(2)=+AZ(2)
  006 IF(ZS-1.0)020,020,007
  007 IF(ZS-289.0)008,010,010
  008 IF(-ABS(AZ(2))+0.096*AZ(1)*AZ(1))020,020,015
  010 QM=SN*0.797884560802865*EXP(-0.5*ZL(1))
      QF(1)=QM*COS(-0.5*ZL(2))
      QF(2)=QM*SIN(-0.5*ZL(2))
      IF(AN.GT.1.0)GO TO 012
      PN=AN
      ASSIGN 011 TO LA
      GO TO 100
  011 TS(1)=QF(1)*SM(1)-QF(2)*SM(2)
      TS(2)=QF(1)*SM(2)+QF(2)*SM(1)
      SM(1)=TS(1)
      SM(2)=TS(2)
      GO TO 029
  012 PN=1.0
      ASSIGN 013 TO LA
      GO TO 100
  013 SQ(1)=-QF(1)*SM(1)+QF(2)*SM(2)
      SQ(2)=-QF(1)*SM(2)-QF(2)*SM(1)
      PN=0.0
      ASSIGN 014 TO LA
      GO TO 100
  014 SR(1)=+QF(1)*SM(1)-QF(2)*SM(2)
      SR(2)=+QF(1)*SM(2)+QF(2)*SM(1)
      GO TO 026
  015 QM=SN*0.3989422804014327*EXP(-0.5*ZL(1))
      QF(1)=QM*COS(-0.5*ZL(2))
      QF(2)=QM*SIN(-0.5*ZL(2))
      IF(AN.GT.1.0)GO TO 017
      PN=AN
      ASSIGN 016 TO LR
      GO TO 112
  016 TS(1)=QF(1)*SM(1)-QF(2)*SM(2)
      TS(2)=QF(1)*SM(2)+QF(2)*SM(1)
      SM(1)=TS(1)
      SM(2)=TS(2)
      GO TO 029
  017 PN=1.0
      ASSIGN 018 TO LR
      GO TO 112
  018 SQ(1)=-QF(1)*SM(1)+QF(2)*SM(2)
      SQ(2)=-QF(1)*SM(2)-QF(2)*SM(1)
      PN=0.0
      ASSIGN 019 TO LR
      GO TO 112
  019 SR(1)=+QF(1)*SM(1)-QF(2)*SM(2)
      SR(2)=+QF(1)*SM(2)+QF(2)*SM(1)
      GO TO 026
  020 QF(1)=SN*0.6366197723675813
      QF(2)=0.0
  021 IF(AN.GT.1.0)GO TO 023
      PN=AN
      ASSIGN 022 TO LY
      GO TO 122
  022 TS(1)=QF(1)*SM(1)-QF(2)*SM(2)
      TS(2)=QF(1)*SM(2)+QF(2)*SM(1)
      SM(1)=TS(1)
      SM(2)=TS(2)
      GO TO 029
  023 PN=1.0
      ASSIGN 024 TO LY
      GO TO 122
  024 SQ(1)=-QF(1)*SM(1)+QF(2)*SM(2)
      SQ(2)=-QF(1)*SM(2)-QF(2)*SM(1)
      PN=0.0
      ASSIGN 025 TO LY
      GO TO 122
  025 SR(1)=+QF(1)*SM(1)-QF(2)*SM(2)
      SR(2)=+QF(1)*SM(2)+QF(2)*SM(1)
  026 RZ(1)=+AZ(1)/ZS
      RZ(2)=-AZ(2)/ZS
      PN=0.0
      GO TO 028
  027 SQ(1)=SR(1)
      SQ(2)=SR(2)
      SR(1)=SM(1)
      SR(2)=SM(2)
  028 SM(1)=2.0*PN*(RZ(1)*SR(1)-RZ(2)*SR(2))-SQ(1)
      SM(2)=2.0*PN*(RZ(1)*SR(2)+RZ(2)*SR(1))-SQ(2)
      PN=PN+1.0
      IF(PN.LT.AN)GO TO 027
  029 W=CMPLX(SM(1),SM(2))
      RETURN
  100 SM(1)=0.0
      SM(2)=0.0
      SM(3)=0.0
      SM(4)=0.0
      RZ(1)=+0.5*QZ(1)/ZS
      RZ(2)=-0.5*QZ(2)/ZS
      QN=PN*PN-0.25
      TM(1)=1.0
      TM(2)=0.0
      PM=0.0
      GO TO 102
  101 QN=QN-2.0*PM
      PM=PM+1.0
      TS(1)=TM(1)*RZ(1)-TM(2)*RZ(2)
      TS(2)=TM(1)*RZ(2)+TM(2)*RZ(1)
      TM(1)=-QN*TS(1)/PM
      TM(2)=-QN*TS(2)/PM
  102 SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      QN=QN-2.0*PM
      PM=PM+1.0
      TS(1)=TM(1)*RZ(1)-TM(2)*RZ(2)
      TS(2)=TM(1)*RZ(2)+TM(2)*RZ(1)
      TM(1)=+QN*TS(1)/PM
      TM(2)=+QN*TS(2)/PM
      IF(ABS(SM(3))+ABS(TM(1)).NE.ABS(SM(3)))GO TO 103
      IF(ABS(SM(4))+ABS(TM(2)).EQ.ABS(SM(4)))GO TO 104
  103 SM(3)=SM(3)+TM(1)
      SM(4)=SM(4)+TM(2)
      IF(PM.LT.35.0)GO TO 101
  104 AQ(1)=QZ(1)-1.57079632679490*(PN+0.5)
      AQ(2)=QZ(2)
      TS(1)=+COS(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=-SIN(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      TM(1)=SM(1)*TS(1)-SM(2)*TS(2)
      TM(2)=SM(1)*TS(2)+SM(2)*TS(1)
      TM(3)=SM(3)*TS(1)-SM(4)*TS(2)
      TM(4)=SM(3)*TS(2)+SM(4)*TS(1)
      TS(1)=+SIN(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=+COS(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      TM(1)=TM(1)-SM(3)*TS(1)+SM(4)*TS(2)
      TM(2)=TM(2)-SM(3)*TS(2)-SM(4)*TS(1)
      TM(3)=TM(3)+SM(1)*TS(1)-SM(2)*TS(2)
      TM(4)=TM(4)+SM(1)*TS(2)+SM(2)*TS(1)
  105 IF(AZ(1))106,110,110
  106 IF(AZ(2))107,108,108
  107 SM(1)=-2.0*TM(1)+TM(4)
      SM(2)=-2.0*TM(2)-TM(3)
      GO TO 109
  108 SM(1)=-2.0*TM(1)-TM(4)
      SM(2)=-2.0*TM(2)+TM(3)
  109 IF(PN.EQ.0.0)GO TO 111
      SM(1)=-SM(1)
      SM(2)=-SM(2)
      GO TO 111
  110 SM(1)=TM(3)
      SM(2)=TM(4)
  111 GO TO LA,(011,013,014)
  112 SM(1)=1.0
      SM(2)=0.0
      SM(3)=1.0
      SM(4)=0.0
      M=15.0*PN+2.0
      N=15.0*PN+15.0
      DO 113 I=M,N
      TS(1)=+QZ(2)-CD(I)
      TS(2)=-QZ(1)
      SS=TS(1)*TS(1)+TS(2)*TS(2)
      TM(1)=+CE(I)*TS(1)/SS
      TM(2)=-CE(I)*TS(2)/SS
      SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      TS(1)=-QZ(2)-CD(I)
      TS(2)=+QZ(1)
      SS=TS(1)*TS(1)+TS(2)*TS(2)
      TM(1)=+CE(I)*TS(1)/SS
      TM(2)=-CE(I)*TS(2)/SS
      SM(3)=SM(3)+TM(1)
      SM(4)=SM(4)+TM(2)
  113 CONTINUE
  114 AQ(1)=QZ(1)-1.57079632679490*(PN+0.5)
      AQ(2)=QZ(2)
      TS(1)=+COS(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=-SIN(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      TM(1)=+TS(1)*SM(1)-TS(2)*SM(2)+TS(1)*SM(3)-TS(2)*SM(4)
      TM(2)=+TS(1)*SM(2)+TS(2)*SM(1)+TS(1)*SM(4)+TS(2)*SM(3)
      TM(3)=+TS(1)*SM(2)+TS(2)*SM(1)-TS(1)*SM(4)-TS(2)*SM(3)
      TM(4)=-TS(1)*SM(1)+TS(2)*SM(2)+TS(1)*SM(3)-TS(2)*SM(4)
      TS(1)=+SIN(AQ(1))*0.5*(EXP(+AQ(2))+EXP(-AQ(2)))
      TS(2)=+COS(AQ(1))*0.5*(EXP(+AQ(2))-EXP(-AQ(2)))
      TM(1)=TM(1)-TS(1)*SM(2)-TS(2)*SM(1)+TS(1)*SM(4)+TS(2)*SM(3)
      TM(2)=TM(2)+TS(1)*SM(1)-TS(2)*SM(2)-TS(1)*SM(3)+TS(2)*SM(4)
      TM(3)=TM(3)+TS(1)*SM(1)-TS(2)*SM(2)+TS(1)*SM(3)-TS(2)*SM(4)
      TM(4)=TM(4)+TS(1)*SM(2)+TS(2)*SM(1)+TS(1)*SM(4)+TS(2)*SM(3)
  115 IF(AZ(1))116,120,120
  116 IF(AZ(2))117,118,118
  117 SM(1)=-2.0*TM(1)+TM(4)
      SM(2)=-2.0*TM(2)-TM(3)
      GO TO 119
  118 SM(1)=-2.0*TM(1)-TM(4)
      SM(2)=-2.0*TM(2)+TM(3)
  119 IF(PN.EQ.0.0)GO TO 121
      SM(1)=-SM(1)
      SM(2)=-SM(2)
      GO TO 121
  120 SM(1)=TM(3)
      SM(2)=TM(4)
  121 GO TO LR,(016,018,019)
  122 AQ(1)=1.0
      AQ(2)=0.0
      RN=0.0
      PM=0.0
      GO TO 124
  123 PM=PM+1.0
      RN=RN+0.5/PM
      TS(1)=0.5*(AZ(1)*AQ(1)-AZ(2)*AQ(2))
      TS(2)=0.5*(AZ(1)*AQ(2)+AZ(2)*AQ(1))
      AQ(1)=TS(1)/PM
      AQ(2)=TS(2)/PM
  124 IF(PM.LT.PN)GO TO 123
      SZ(1)=0.25*(AZ(1)-AZ(2))*(AZ(1)+AZ(2))
      SZ(2)=0.5*AZ(1)*AZ(2)
      SR(1)=0.0
      SR(2)=0.0
      SS=AQ(1)*AQ(1)+AQ(2)*AQ(2)
      TM(1)=+AQ(1)/SS
      TM(2)=-AQ(2)/SS
      PM=0.0
      GO TO 126
  125 TM(1)=TM(1)/(PN-PM)
      TM(2)=TM(2)/(PN-PM)
      SR(1)=SR(1)-0.5*TM(1)
      SR(2)=SR(2)-0.5*TM(2)
      PM=PM+1.0
      TS(1)=SZ(1)*TM(1)-SZ(2)*TM(2)
      TS(2)=SZ(1)*TM(2)+SZ(2)*TM(1)
      TM(1)=+TS(1)/PM
      TM(2)=+TS(2)/PM
  126 IF(PM.LT.PN)GO TO 125
      SM(1)=0.0
      SM(2)=0.0
      RM=1.0
      QM=0.0
      SL(1)=-0.115931515658412+ZL(1)-RN
      SL(2)=+ZL(2)
      PM=0.0
      GO TO 128
  127 QM=QM+RM
      PM=PM+1.0
      RM=0.25*ZS*RM/(PM*(PN+PM))
      TS(1)=SZ(1)*AQ(1)-SZ(2)*AQ(2)
      TS(2)=SZ(1)*AQ(2)+SZ(2)*AQ(1)
      AQ(1)=-TS(1)/(PM*(PN+PM))
      AQ(2)=-TS(2)/(PM*(PN+PM))
      SL(1)=SL(1)-0.5/PM-0.5/(PN+PM)
  128 TM(1)=AQ(1)*SL(1)-AQ(2)*SL(2)
      TM(2)=AQ(1)*SL(2)+AQ(2)*SL(1)
      SM(1)=SM(1)+TM(1)
      SM(2)=SM(2)+TM(2)
      IF(QM+RM.GT.QM)GO TO 127
      SM(1)=SR(1)+SM(1)
      SM(2)=SR(2)+SM(2)
      GO TO LY,(022,024,025)
      END

C---------------------------------------------------------------------------


