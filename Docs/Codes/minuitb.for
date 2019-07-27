	SUBROUTINE FUTIL
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
C
C	This is a dummy Subroutine required by the new FCN
C
	WRITE (*,100)
100	FORMAT (' ***'/' FUTIL has been called'/' ***')
	RETURN
	END
      SUBROUTINE MNEXIN(PINT)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Transforms the external parameter values U to internal     
CC        values in the dense array PINT. Subroutine MNPINT is used.     
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
      DIMENSION PINT(*)  
      LIMSET = .FALSE.   
      DO 100  IINT= 1, NPAR  
      IEXT = NEXOFI(IINT)    
      CALL MNPINT(U(IEXT),IEXT,PINTI)    
      PINT(IINT) = PINTI     
  100 CONTINUE   
      RETURN     
      END    
      SUBROUTINE MNFIXP(IINT,IERR)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        removes parameter IINT from the internal (variable) parameter  
CC        list, and arranges the rest of the list to fill the hole.  
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
      DIMENSION YY(MNI)  
C                           first see if it can be done  
      IERR = 0   
      IF (IINT.GT.NPAR .OR. IINT.LE.0)  THEN     
         IERR = 1    
         WRITE (ISYSWR,'(A,I4)')     
     +       ' MINUIT ERROR.  ARGUMENT TO MNFIXP=',IINT  
         GO TO 300   
      ENDIF  
      IEXT = NEXOFI(IINT)    
      IF (NPFIX .GE. MNI) THEN   
         IERR = 1    
         WRITE (ISYSWR,'(A,I4,A,I4)') ' MINUIT CANNOT FIX PARAMETER',    
     +   IEXT,' MAXIMUM NUMBER THAT CAN BE FIXED IS',MNI     
         GO TO 300   
      ENDIF  
C                           reduce number of variable parameters by one  
      NIOFEX(IEXT) = 0   
      NOLD = NPAR    
      NPAR = NPAR - 1    
C                       save values in case parameter is later restored  
      NPFIX = NPFIX + 1  
      IPFIX(NPFIX) = IEXT    
      LC = IINT  
      XS(NPFIX) = X(LC)  
      XTS(NPFIX) = XT(LC)    
      DIRINS(NPFIX) = WERR(LC)   
      GRDS(NPFIX) = GRD(LC)  
      G2S(NPFIX) = G2(LC)    
      GSTEPS(NPFIX) = GSTEP(LC)  
C                        shift values for other parameters to fill hole  
      DO 100  IK= IEXT+1, NU     
         IF  (NIOFEX(IK) .GT. 0)  THEN   
         LC = NIOFEX(IK) - 1     
         NIOFEX(IK) = LC     
         NEXOFI(LC) = IK     
         X(LC)     = X(LC+1)     
         XT(LC)    = XT(LC+1)    
         DIRIN(LC) = DIRIN(LC+1)     
         WERR(LC)  = WERR(LC+1)  
         GRD(LC)   = GRD(LC+1)   
         G2(LC)    = G2(LC+1)    
         GSTEP(LC) = GSTEP(LC+1)     
         ENDIF   
  100 CONTINUE   
      IF (ISW(2) .LE. 0)  GO TO 300  
C                    remove one row and one column from variance matrix  
      IF (NPAR .LE. 0)  GO TO 300    
      DO 260 I= 1, NOLD  
      M = MAX(I,IINT)    
      N = MIN(I,IINT)    
      NDEX = M*(M-1)/2 + N   
  260 YY(I)=VHMAT(NDEX)  
      YYOVER = 1.0/YY(IINT)  
      KNEW = 0   
      KOLD = 0   
      DO 294 I= 1, NOLD  
      DO 292 J= 1, I     
      KOLD = KOLD + 1    
      IF (J.EQ.IINT .OR. I.EQ.IINT)  GO TO 292   
      KNEW = KNEW + 1    
      VHMAT(KNEW) = VHMAT(KOLD) - YY(J)*YY(I)*YYOVER     
  292 CONTINUE   
  294 CONTINUE   
  300 RETURN     
      END    
      SUBROUTINE MNFREE(K)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Restores one or more fixed parameter(s) to variable status     
CC        by inserting it into the internal parameter list at the    
CC        appropriate place.     
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
C--       K = 0 means restore all parameters     
C--       K = 1 means restore the last parameter fixed   
C--       K = -I means restore external parameter I (if possible)    
C--       IQ = fix-location where internal parameters were stored    
C--       IR = external number of parameter being restored   
C--       IS = internal number of parameter being restored   
      IF (K .GT. 1)  WRITE (ISYSWR,510)  
      IF (NPFIX .LT. 1)  WRITE (ISYSWR,500)  
      IF (K.EQ.1 .OR. K.EQ.0)  GO TO 40  
C                   release parameter with specified external number     
      KA = IABS(K)   
      IF (NIOFEX(KA) .EQ. 0)  GO TO 15   
      WRITE (ISYSWR,540)     
  540 FORMAT (' IGNORED.  PARAMETER SPECIFIED IS ALREADY VARIABLE.')     
      RETURN     
   15 IF (NPFIX .LT. 1)  GO TO 21    
      DO 20 IK= 1, NPFIX     
      IF (IPFIX(IK) .EQ. KA)  GO TO 24   
   20 CONTINUE   
   21 WRITE (ISYSWR,530) KA  
  530 FORMAT (' PARAMETER',I4,' NOT FIXED.  CANNOT BE RELEASED.')    
      RETURN     
   24 IF (IK .EQ. NPFIX)  GO TO 40   
C                   move specified parameter to end of list  
      IPSAV = KA     
      XV = XS(IK)    
      XTV = XTS(IK)  
      DIRINV = DIRINS(IK)    
      GRDV = GRDS(IK)    
      G2V = G2S(IK)  
      GSTEPV = GSTEPS(IK)    
         DO 30 I= IK+1,NPFIX     
         IPFIX(I-1) = IPFIX(I)   
         XS(I-1) = XS(I)     
         XTS(I-1) = XTS(I)   
         DIRINS(I-1) = DIRINS(I)     
         GRDS(I-1) = GRDS(I)     
         G2S(I-1) = G2S(I)   
         GSTEPS(I-1) = GSTEPS(I)     
   30    CONTINUE    
      IPFIX(NPFIX) = IPSAV   
      XS(NPFIX) = XV     
      XTS(NPFIX) = XTV   
      DIRINS(NPFIX) = DIRINV     
      GRDS(NPFIX) = GRDV     
      G2S(NPFIX) = G2V   
      GSTEPS(NPFIX) = GSTEPV     
C                restore last parameter in fixed list  -- IPFIX(NPFIX)   
   40 CONTINUE   
      IF (NPFIX .LT. 1)  GO TO 300   
      IR = IPFIX(NPFIX)  
      IS = 0     
      DO 100 IK= NU, IR, -1  
        IF (NIOFEX(IK) .GT. 0) THEN  
         LC = NIOFEX(IK) + 1     
         IS = LC - 1     
         NIOFEX(IK) = LC     
         NEXOFI(LC) = IK     
         X(LC)     = X(LC-1)     
         XT(LC)    = XT(LC-1)    
         DIRIN(LC) = DIRIN(LC-1)     
         WERR(LC)  = WERR(LC-1)  
         GRD(LC)   = GRD(LC-1)   
         G2(LC)    = G2(LC-1)    
         GSTEP(LC) = GSTEP(LC-1)     
        ENDIF    
  100 CONTINUE   
      NPAR = NPAR + 1    
      IF (IS .EQ. 0)   IS = NPAR     
      NIOFEX(IR) = IS    
      NEXOFI(IS) = IR    
      IQ = NPFIX     
      X(IS) = XS(IQ)     
      XT(IS) = XTS(IQ)   
      DIRIN(IS) = DIRINS(IQ)     
      WERR(IS)  = DIRINS(IQ)     
      GRD(IS) = GRDS(IQ)     
      G2(IS) = G2S(IQ)   
      GSTEP(IS) = GSTEPS(IQ)     
      NPFIX = NPFIX - 1  
      ISW(2) = 0     
      DCOVAR = 1.    
      IF (ITAUR .LT. 1)  WRITE(ISYSWR,520) IR,CPNAM(IR)  
      IF (K.EQ.0)  GO TO 40  
  300 CONTINUE   
C         if different from internal, external values are taken  
      CALL MNEXIN(X)     
  400 RETURN     
  500 FORMAT (' CALL TO MNFREE IGNORED.  THERE ARE NO FIXED PA',     
     + 'RAMETERS'/)  
  510 FORMAT (' CALL TO MNFREE IGNORED.  ARGUMENT GREATER THAN ONE'/)    
  520 FORMAT (20X, 9HPARAMETER,I4,2H, ,A10,' RESTORED TO VARIABLE.')     
      END    
      SUBROUTINE MNGRAD   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       Called from MNSET   
CC       Interprets the SET GRAD command, which informs MINUIT whether   
CC       the first derivatives of FCN will be calculated by the user     
CC       inside FCN.  It can check the user's derivative calculation     
CC       by comparing it with a finite difference approximation.     
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
C    
     
      CHARACTER*4 CGOOD,CBAD,CNONE,CWD   
      LOGICAL LNONE  
      DIMENSION GF(MNI)  
      PARAMETER (CGOOD='GOOD',CBAD=' BAD',CNONE='NONE')  
C    
      ISW(3) = 1     
      NPARX = NPAR   
      IF (WORD7(1) .GT. ZERO)  GO TO 2000    
C                  get user-calculated first derivatives from FCN    
      DO 30 I= 1, NU     
   30 GIN(I) = UNDEFI    
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,FZERO,U,2)    
      NFCN = NFCN + 1    
      CALL MNDERI     
      DO 40 I= 1, NPAR   
   40 GF(I) = GRD(I)     
C                    get MINUIT-calculated first derivatives     
      ISW(3) = 0     
      ISTSAV = ISTRAT    
      ISTRAT = 2     
      CALL MNHES1     
      ISTRAT = ISTSAV    
      WRITE (ISYSWR,51)  
   51 FORMAT(/' CHECK OF GRADIENT CALCULATION IN FCN'/12X,'PARAMETER',   
     + 6X,9HG(IN FCN) ,3X,9HG(MINUIT) ,2X,'DG(MINUIT)',3X,9HAGREEMENT)   
      ISW(3) = 1     
      LNONE = .FALSE.    
      DO 100 LC = 1, NPAR    
      I = NEXOFI(LC)     
      CWD = CGOOD    
      ERR = DGRD(LC)     
      IF (DABS(GF(LC)-GRD(LC)) .GT. ERR)  CWD = CBAD  
      IF (GIN(I) .EQ. UNDEFI)  THEN  
          CWD = CNONE    
          LNONE = .TRUE.     
          GF(LC) = 0.    
          ENDIF  
      IF (CWD .NE. CGOOD)  ISW(3) = 0    
      WRITE (ISYSWR,99) I,CPNAM(I),GF(LC),GRD(LC),ERR,CWD    
   99 FORMAT (7X,I5,2X ,A10,3E12.4,4X ,A4)   
  100 CONTINUE   
      IF (LNONE) WRITE (ISYSWR,'(A)')    
     +  '  AGREEMENT=NONE  MEANS FCN DID NOT CALCULATE THE DERIVATIVE'   
      IF (ISW(3) .EQ. 0)  WRITE (ISYSWR,1003)    
 1003 FORMAT(/' MINUIT DOES NOT ACCEPT DERIVATIVE CALCULATIONS BY FCN'/  
     + ' TO FORCE ACCEPTANCE, ENTER "SET GRAD    1"'/)   
C    
 2000 CONTINUE   
      RETURN     
      END    
      SUBROUTINE MNHESS   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Calculates the full second-derivative matrix of FCN    
CC        by taking finite differences. When calculating diagonal    
CC        elements, it may iterate so that step size is nearly that  
CC        which gives function change= UP/10. The first derivatives  
CC        of course come as a free side effect, but with a smaller   
CC        step size in order to obtain a known accuracy.     
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
      DIMENSION YY(MNI)  
      LOGICAL LDEBUG     
      CHARACTER CBF1*22  
C    
      LDEBUG = (IDBG(3) .GE. 1)  
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      IF (ISTRAT .LE. 0) THEN    
         NCYC = 3    
         TLRSTP = 0.5    
         TLRG2  = 0.1    
      ELSE IF (ISTRAT .EQ. 1) THEN   
         NCYC = 5    
         TLRSTP = 0.3    
         TLRG2  = 0.05   
      ELSE   
         NCYC = 7    
         TLRSTP = 0.1    
         TLRG2  = 0.02   
      ENDIF  
      IF (ISW(5).GE.2 .OR. LDEBUG)  WRITE (ISYSWR,'(A)')     
     +   '   START COVARIANCE MATRIX CALCULATION.'   
      CFROM = 'HESSE   '     
      NFCNFR = NFCN  
      CSTATU= 'OK        '   
      NPARD = NPAR   
C                 make sure starting at the right place  
      CALL MNINEX(X)     
      NPARX = NPAR   
      CALL FCN(NPARX,GIN,FS1,U,4)  
      NFCN = NFCN + 1    
      IF (FS1 .NE. AMIN) THEN    
         DF = AMIN - FS1     
         WRITE (CBF1(1:12),'(G12.3)') DF     
         CALL MNWARN('D','MNHESS',   
     +       'function value differs from AMIN by '//CBF1(1:12) )    
      ENDIF  
      AMIN = FS1     
      IF (LDEBUG) WRITE (ISYSWR,'(A,A)') ' PAR D   GSTEP          ',     
     +' D          G2         GRD         SAG    '   
C                                        . . . . . . diagonal elements .     
C         ISW(2) = 1 if approx, 2 if not posdef, 3 if ok     
C         AIMSAG is the sagitta we are aiming for in second deriv calc.  
      AIMSAG = DSQRT(EPSMA2)*(DABS(AMIN)+UP)   
C         Zero the second derivative matrix  
      NPAR2 = NPAR*(NPAR+1)/2    
      DO 10 I= 1,NPAR2   
   10 VHMAT(I) = 0.  
C    
C         Loop over variable parameters for second derivatives   
      IDRV = 2   
      DO 100 ID= 1, NPARD    
      I = ID + NPAR - NPARD  
      IF (G2(I) .EQ. ZERO) THEN  
        CALL MNWARN('D','MNHESS',    
     +        'A second derivative is zero on entering.')    
        WINT = WERR(I)   
        IEXT = NEXOFI(I)     
        IF (NVARL(IEXT) .GT. 1) THEN     
           CALL MNDXDI(X(I),I,DXDI)  
           IF (DABS(DXDI) .LT. .001) THEN     
              WINT = .01     
           ELSE  
              WINT = WINT/DABS(DXDI)  
           ENDIF     
        ENDIF    
        G2(I) = UP/WINT**2   
      ENDIF  
      XTF = X(I)     
      DMIN = 8.*EPSMA2*DABS(XTF)  
C    
C                               find step which gives sagitta = AIMSAG   
      D = DABS(GSTEP(I))  
      DO 40 ICYC= 1, NCYC    
C                               loop here only if SAG=0.     
      DO 25 MULTPY= 1, 5     
C           take two steps   
         X(I) = XTF + D  
         CALL MNINEX(X)  
         NPARX = NPAR    
         CALL FCN(NPARX,GIN,FS1,U,4)   
         NFCN = NFCN + 1     
         X(I) = XTF - D  
         CALL MNINEX(X)  
         CALL FCN(NPARX,GIN,FS2,U,4)   
         NFCN = NFCN + 1     
         X(I) = XTF  
         SAG = 0.5*(FS1+FS2-2.0*AMIN)    
         IF (SAG .NE. ZERO) GO TO 30     
         IF (GSTEP(I) .LT. ZERO) THEN    
           IF (D .GE. .5)  GO TO 26  
           D = 10.*D     
           IF (D .GT. 0.5)  D = 0.51     
           GO TO 25  
         ENDIF   
         D = 10.*D   
   25 CONTINUE   
   26      WRITE (CBF1(1:4),'(I4)') IEXT     
           CALL MNWARN('W','HESSE',  
     +      'Second derivative zero for parameter'//CBF1(1:4) )  
           GO TO 390     
C                             SAG is not zero    
   30 G2BFOR = G2(I)     
      G2(I) = 2.*SAG/D**2    
      GRD(I) = (FS1-FS2)/(2.*D)  
      IF (LDEBUG) WRITE (ISYSWR,31) I,IDRV,GSTEP(I),D,G2(I),GRD(I),SAG   
   31 FORMAT (I4,I2,6G12.5)  
      GSTEP(I) = DSIGN(D,GSTEP(I))    
      DIRIN(I) = D   
      YY(I) = FS1    
      DLAST = D  
      D = DSQRT(2.0*AIMSAG/DABS(G2(I)))    
C         if parameter has limits, max int step size = 0.5   
      STPINM = 0.5   
      IF (GSTEP(I) .LT. ZERO)  D = DMIN1(D,STPINM)     
      IF (D .LT. DMIN)  D = DMIN     
C           see if converged     
      IF (DABS((D-DLAST)/D)          .LT. TLRSTP)  GO TO 50   
      IF (DABS((G2(I)-G2BFOR)/G2(I)) .LT. TLRG2 )  GO TO 50   
      D = DMIN1(D, 10.*DLAST)  
      D = DMAX1(D, 0.1*DLAST)  
   40 CONTINUE   
C                       end of step size loop    
      WRITE (CBF1,'(I2,2E10.2)') IEXT,SAG,AIMSAG     
      CALL MNWARN('D','MNHESS','Second Deriv. SAG,AIM= '//CBF1)  
C    
   50 CONTINUE   
      NDEX = I*(I+1)/2   
      VHMAT(NDEX) = G2(I)    
  100 CONTINUE   
C                              end of diagonal second derivative loop    
      CALL MNINEX(X)     
C                                     refine the first derivatives   
      IF (ISTRAT .GT. 0) CALL MNHES1  
      ISW(2) = 3     
      DCOVAR = 0.    
C                                        . . . .  off-diagonal elements  
      IF (NPAR .EQ. 1)  GO TO 214    
      DO 200 I= 1, NPAR  
      DO 180 J= 1, I-1   
      XTI = X(I)     
      XTJ = X(J)     
      X(I) = XTI + DIRIN(I)  
      X(J) = XTJ + DIRIN(J)  
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,FS1,U,4)  
      NFCN = NFCN + 1    
      X(I) = XTI     
      X(J) = XTJ     
      ELEM = (FS1+AMIN-YY(I)-YY(J)) / (DIRIN(I)*DIRIN(J))    
      NDEX = I*(I-1)/2 + J   
      VHMAT(NDEX) = ELEM     
  180 CONTINUE   
  200 CONTINUE   
  214 CALL MNINEX(X)     
C                  verify matrix positive-definite   
      CALL MNPSDF    
      DO 220 I= 1, NPAR  
      DO 219 J= 1, I     
      NDEX = I*(I-1)/2 + J   
      P(I,J) = VHMAT(NDEX)   
  219 P(J,I) = P(I,J)    
  220 CONTINUE   
      CALL MNVERT(P,MAXINT,MAXINT,NPAR,IFAIL)    
      IF (IFAIL .GT. 0)  THEN    
        CALL MNWARN('W','HESSE', 'Matrix inversion fails.')  
        GO TO 390    
      ENDIF  
C                                        . . . . . . .  calculate  e d m     
      EDM = 0.   
        DO 230 I= 1, NPAR    
C                              off-diagonal elements     
        NDEX = I*(I-1)/2     
          DO 225 J= 1, I-1   
          NDEX = NDEX + 1    
          ZTEMP = 2.0 * P(I,J)   
          EDM = EDM + GRD(I)*ZTEMP*GRD(J)    
  225     VHMAT(NDEX) = ZTEMP    
C                              diagonal elements     
        NDEX = NDEX + 1  
        VHMAT(NDEX) = 2.0 * P(I,I)   
        EDM = EDM  + P(I,I) * GRD(I)**2  
  230   CONTINUE     
      IF (ISW(5).GE.1 .AND. ISW(2).EQ.3 .AND. ITAUR.EQ.0)    
     + WRITE(ISYSWR,'(A)')' COVARIANCE MATRIX CALCULATED SUCCESSFULLY'   
      GO TO 900  
C                              failure to invert 2nd deriv matrix    
  390 ISW(2) = 1     
      DCOVAR = 1.    
      CSTATU = 'FAILED    '  
      IF (ISW(5) .GE. 0) WRITE (ISYSWR,'(A)')    
     +        '  MNHESS FAILS AND WILL RETURN DIAGONAL MATRIX. '     
      DO 395 I= 1, NPAR  
      NDEX = I*(I-1)/2   
      DO 394 J= 1, I-1   
      NDEX = NDEX + 1    
  394 VHMAT(NDEX) = 0.0  
      NDEX = NDEX +1     
      G2I = G2(I)    
      IF (G2I .LE. ZERO)  G2I = 1.0  
  395 VHMAT(NDEX) = 2.0/G2I  
  900 RETURN     
      END    
      SUBROUTINE MNHES1   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC      Called from MNHESS and MNGRAD    
CC      Calculate first derivatives (GRD) and uncertainties (DGRD)   
CC         and appropriate step sizes GSTEP  
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
      LOGICAL LDEBUG     
      CHARACTER CBF1*22  
      LDEBUG = (IDBG(5) .GE. 1)  
      IF (ISTRAT .LE. 0) NCYC = 1    
      IF (ISTRAT .EQ. 1) NCYC = 2    
      IF (ISTRAT .GT. 1) NCYC = 6    
      IDRV = 1   
      NPARX = NPAR   
      DFMIN = 4.*EPSMA2*(DABS(AMIN)+UP)   
C                                     main loop over parameters  
      DO 100 I= 1, NPAR  
      XTF = X(I)     
      DMIN = 4.*EPSMA2*DABS(XTF)  
      EPSPRI = EPSMA2 + DABS(GRD(I)*EPSMA2)   
      OPTSTP = DSQRT(DFMIN/(DABS(G2(I))+EPSPRI))   
      D = 0.2 * DABS(GSTEP(I))    
      IF (D .GT. OPTSTP)  D = OPTSTP     
      IF (D .LT. DMIN)  D = DMIN     














      CHGOLD = 10000.    


C                                       iterate reducing step size   


      DO 50 ICYC= 1, NCYC    
      X(I) = XTF + D     
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,FS1,U,4)  
      NFCN = NFCN + 1    


      X(I) = XTF - D     
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,FS2,U,4)  
      NFCN = NFCN + 1    
      X(I) = XTF     
C                                       check if step sizes appropriate  
      SAG = 0.5*(FS1+FS2-2.0*AMIN)   
      GRDOLD = GRD(I)    
      GRDNEW = (FS1-FS2)/(2.0*D)     
      DGMIN = EPSMAC*(DABS(FS1)+DABS(FS2))/D   
      IF (LDEBUG) WRITE (ISYSWR,11) I,IDRV,GSTEP(I),D,G2(I),GRDNEW,SAG   
   11 FORMAT (I4,I2,6G12.5)  
      IF (GRDNEW .EQ. ZERO)  GO TO 60    
      CHANGE = DABS((GRDOLD-GRDNEW)/GRDNEW)   
      IF (CHANGE.GT.CHGOLD .AND. ICYC.GT.1)  GO TO 60    
      CHGOLD = CHANGE    
      GRD(I) = GRDNEW    
      GSTEP(I) = DSIGN(D,GSTEP(I))    
C                  decrease step until first derivative changes by <5%   
      IF (CHANGE .LT. 0.05) GO TO 60     
      IF (DABS(GRDOLD-GRDNEW) .LT. DGMIN)  GO TO 60   
      IF (D .LT. DMIN)  THEN     
         CALL MNWARN('D','MNHES1','Step size too small for 1st drv.')    
         GO TO 60    
      ENDIF  
      D = 0.2*D  
   50 CONTINUE   
C                                       loop satisfied = too many iter   
      WRITE (CBF1,'(2G11.3)') GRDOLD,GRDNEW  
      CALL MNWARN('D','MNHES1','Too many iterations on D1.'//CBF1)   
   60 CONTINUE   
      DGRD(I) = DMAX1(DGMIN,DABS(GRDOLD-GRDNEW))    
  100 CONTINUE   
C                                        end of first deriv. loop    
      CALL MNINEX(X)     
      RETURN     
      END    
      SUBROUTINE MNIMPR   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Attempts to improve on a good local minimum by finding a   
CC        better one.   The quadratic part of FCN is removed by MNCALF   
CC        and this transformed function is minimized using the simplex   
CC        method from several random starting points.    
CC        ref. -- Goldstein and Price, Math.Comp. 25, 569 (1971)     
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
      DIMENSION DSAV(MNI), Y(MNI+1)  
      PARAMETER (ALPHA=1.,BETA=0.5,GAMMA=2.0)    
      DATA RNUM/0./  
      IF (NPAR .LE. 0)  RETURN   
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      CSTATU = 'UNCHANGED '  
      ITAUR = 1  
      EPSI = 0.1*UP  
      NPFN=NFCN  
      NLOOP = WORD7(2)   
      IF (NLOOP .LE. 0)  NLOOP = NPAR + 4    
      NPARX = NPAR   
      NPARP1=NPAR+1  
      WG = 1.0/FLOAT(NPAR)   
      SIGSAV = EDM   
      APSI = AMIN    
         DO 2 I= 1, NPAR     
         XT(I) = X(I)    
         DSAV(I) = WERR(I)   
           DO 2 J = 1, I     
           NDEX = I*(I-1)/2 + J  
           P(I,J) = VHMAT(NDEX)  
    2      P(J,I) = P(I,J)   
      CALL MNVERT(P,MAXINT,MAXINT,NPAR,IFAIL)    
      IF (IFAIL .GE. 1)  GO TO 280   
C               Save inverted matrix in VT   
         DO 12 I= 1, NPAR    
         NDEX = I*(I-1)/2    
           DO 12 J= 1, I     
           NDEX = NDEX + 1   
   12      VTHMAT(NDEX) = P(I,J)     
      LOOP = 0   
C    
   20 CONTINUE   
         DO 25 I= 1, NPAR    
         DIRIN(I) = 2.0*DSAV(I)  
         CALL MNRN15(RNUM,ISEED)     
   25    X(I) = XT(I) + 2.0*DIRIN(I)*(RNUM-0.5)  
      LOOP = LOOP + 1    
      REG = 2.0  
      IF (ISW(5) .GE. 0)   WRITE (ISYSWR, 1040) LOOP     
   30 CALL  MNCALF(X,YCALF)    
      AMIN = YCALF   
C                                        . . . . set up  random simplex  
      JL = NPARP1    
      JH = NPARP1    
      Y(NPARP1) = AMIN   
      AMAX = AMIN    
         DO 45 I= 1, NPAR    
         XI = X(I)   
         CALL MNRN15(RNUM,ISEED)     
         X(I) = XI - DIRIN(I) *(RNUM-0.5)    
         CALL MNCALF(X,YCALF)  
         Y(I) = YCALF    
         IF (Y(I) .LT. AMIN)  THEN   
            AMIN = Y(I)  
            JL = I   
         ELSE IF (Y(I) .GT. AMAX)  THEN  
            AMAX = Y(I)  
            JH = I   
         ENDIF   
            DO 40 J= 1, NPAR     
   40       P(J,I) = X(J)    
         P(I,NPARP1) = XI    
         X(I) = XI   
   45    CONTINUE    
C    
      EDM = AMIN     
      SIG2 = EDM     
C                                        . . . . . . .  start main loop  
   50 CONTINUE   
      IF (AMIN .LT. ZERO)  GO TO 95  
      IF (ISW(2) .LE. 2)  GO TO 280  
      EP = 0.1*AMIN  
      IF (SIG2 .LT. EP   .AND. EDM.LT.EP  )     GO TO 100    
      SIG2 = EDM     
      IF ((NFCN-NPFN) .GT. NFCNMX)  GO TO 300    
C         calculate new point * by reflection    
      DO 60 I= 1, NPAR   
      PB = 0.    
      DO 59 J= 1, NPARP1     
   59 PB = PB + WG * P(I,J)  
      PBAR(I) = PB - WG * P(I,JH)    
   60 PSTAR(I)=(1.+ALPHA)*PBAR(I)-ALPHA*P(I,JH)  
      CALL MNCALF(PSTAR,YCALF)     
      YSTAR = YCALF  
      IF(YSTAR.GE.AMIN) GO TO 70     
C         point * better than jl, calculate new point **     
      DO 61 I=1,NPAR     
   61 PSTST(I)=GAMMA*PSTAR(I)+(1.-GAMMA)*PBAR(I)     
      CALL MNCALF(PSTST,YCALF)     
      YSTST = YCALF  
   66 IF (YSTST .LT. Y(JL))  GO TO 67    
      CALL MNRAZZ(YSTAR,PSTAR,Y,JH,JL)   
      GO TO 50   
   67 CALL MNRAZZ(YSTST,PSTST,Y,JH,JL)   
      GO TO 50   
C         point * is not as good as jl   
   70 IF (YSTAR .GE. Y(JH))  GO TO 73    
      JHOLD = JH     
      CALL MNRAZZ(YSTAR,PSTAR,Y,JH,JL)   
      IF (JHOLD .NE. JH)  GO TO 50   
C         calculate new point **     
   73 DO 74 I=1,NPAR     
   74 PSTST(I)=BETA*P(I,JH)+(1.-BETA)*PBAR(I)    
      CALL MNCALF(PSTST,YCALF)     
      YSTST = YCALF  
      IF(YSTST.GT.Y(JH)) GO TO 30    
C     point ** is better than jh     
      IF (YSTST .LT. AMIN)  GO TO 67     
      CALL MNRAZZ(YSTST,PSTST,Y,JH,JL)   
      GO TO 50   
C                                        . . . . . .  end main loop  
   95 IF (ISW(5) .GE. 0)  WRITE (ISYSWR,1000)    
      REG = 0.1  
C                                        . . . . . ask if point is new   
  100 CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,AMIN,U,4)     
      NFCN = NFCN + 1    
      DO 120 I= 1, NPAR  
      DIRIN(I) = REG*DSAV(I)     
      IF (DABS(X(I)-XT(I)) .GT. DIRIN(I)) GO TO 150   
  120 CONTINUE   
      GO TO 230  
  150 NFCNMX = NFCNMX + NPFN - NFCN  
      NPFN = NFCN    
      CALL MNSIMP     
      IF (AMIN .GE. APSI)  GO TO 325     
      DO 220 I= 1, NPAR  
      DIRIN(I) = 0.1 *DSAV(I)    
      IF (DABS(X(I)-XT(I)) .GT. DIRIN(I)) GO TO 250   
  220 CONTINUE   
  230 IF (AMIN .LT. APSI)  GO TO 350     
      GO TO 325  
C                                        . . . . . . truly new minimum   
  250 LNEWMN = .TRUE.    
      IF (ISW(2) .GE. 1) THEN    
          ISW(2) = 1     
          DCOVAR = DMAX1(DCOVAR,HALF)  
      ELSE   
          DCOVAR = 1.    
      ENDIF  
      ITAUR = 0  
      NFCNMX = NFCNMX + NPFN - NFCN  
      CSTATU = 'NEW MINIMU'  
      IF (ISW(5) .GE. 0)      WRITE (ISYSWR,1030)    
      RETURN     
C                                        . . . return to previous region     
  280 IF (ISW(5) .GT. 0) WRITE (ISYSWR,1020)     
      GO TO 325  
  300 ISW(1) = 1     
  325 DO 330 I= 1, NPAR  
      DIRIN(I) = 0.01*DSAV(I)    
  330 X(I) = XT(I)   
      AMIN = APSI    
      EDM = SIGSAV   
  350 CALL MNINEX(X)     
      IF (ISW(5) .GT. 0)    WRITE (ISYSWR,1010)  
      CSTATU= 'UNCHANGED '   
      CALL MNRSET(0)     
      IF (ISW(2) .LT. 2)  GO TO 380  
      IF (LOOP .LT. NLOOP .AND. ISW(1) .LT. 1)  GO TO 20     
  380 CALL MNPRIN (5,AMIN)   
      ITAUR = 0  
      RETURN     
 1000 FORMAT (54H AN IMPROVEMENT ON THE PREVIOUS MINIMUM HAS BEEN FOUND)     
 1010 FORMAT (51H IMPROVE HAS RETURNED TO REGION OF ORIGINAL MINIMUM)    
 1020 FORMAT (/44H COVARIANCE MATRIX WAS NOT POSITIVE-DEFINITE)  
 1030 FORMAT (/38H IMPROVE HAS FOUND A TRULY NEW MINIMUM/1H ,37(1H*)/)   
 1040 FORMAT (/18H START ATTEMPT NO.,I2,  20H TO FIND NEW MINIMUM)   
      END    
      SUBROUTINE MNINEX(PINT)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Transforms from internal coordinates (PINT) to external    
CC        parameters (U).   The minimizing routines which work in    
CC        internal coordinates call this routine before calling FCN.     
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
      DIMENSION PINT(*)  
      DO 100 J= 1, NPAR  
      I = NEXOFI(J)  
      IF (NVARL(I) .EQ. 1) THEN  
         U(I) = PINT(J)  
      ELSE   
         U(I) = ALIM(I) + 0.5*(DSIN(PINT(J)) +1.0) * (BLIM(I)-ALIM(I))    
      ENDIF  
  100 CONTINUE   
      RETURN     
      END    
      SUBROUTINE MNINIT (I1,I2,I3)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        This is the main initialization subroutine for MINUIT  
CC     It initializes some constants in common   
CC                (including the logical I/O unit nos.),     
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
C    
C      EXTERNAL INTRAC    
C      LOGICAL  INTRAC    
C            I/O unit numbers    
      ISYSRD = I1    
      ISYSWR = I2    
        ISTKWR(1) = ISYSWR   
        NSTKWR = 1   
      ISYSSA = I3    
      NSTKRD = 0     
C               version identifier   
      CVRSN = '90.10 '   
C               some CONSTANT constants in COMMON    
      MAXINT=MNI     
      MAXEXT=MNE     
      UNDEFI = -54321.   
      BIGEDM = 123456.   
      CUNDEF = ')UNDEFINED'  
      COVMES(0) = 'NO ERROR MATRIX       '   
      COVMES(1) = 'ERR MATRIX APPROXIMATE'   
      COVMES(2) = 'ERR MATRIX NOT POS-DEF'   
      COVMES(3) = 'ERROR MATRIX ACCURATE '   
C                some starting values in COMMON  
      NBLOCK = 0     
      ICOMND = 0     
      CTITL = CUNDEF     
      CFROM = 'INPUT   '     
      NFCNFR = NFCN  
      CSTATU= 'INITIALIZE'   
      ISW(3) = 0     
      ISW(4) = 0     
      ISW(5) = 1     
C         ISW(6)=0 for batch jobs,  =1 for interactive jobs  
      ISW(6) = 0     
C      IF (INTRAC(DUMMY))  ISW(6) = 1     
C        DEBUG options set to default values     
      DO 10 IDB= 0, MAXDBG   
   10 IDBG(IDB) = 0  
      LREPOR = .FALSE.   
      LWARN  = .TRUE.    
      LIMSET = .FALSE.   
      LNEWMN = .FALSE.   
      ISTRAT = 1     
      ITAUR = 0  
C        default page dimensions and 'new page' carriage control integer     
      NPAGWD = 120   
      NPAGLN = 56    
      NEWPAG = 1     
      IF (ISW(6) .GT. 0) THEN    
         NPAGWD = 80     
         NPAGLN = 30     
         NEWPAG = 0  
      ENDIF  
      UP = 1.0   
      UPDFLT = UP    
C                   determine machine accuracy epsmac    
      EPSTRY = 0.5   
      DO 33 I= 1, 100    
      EPSTRY = EPSTRY * 0.5  
      EPSP1 = ONE + EPSTRY   
      CALL MNTINY(EPSP1, EPSBAK)     
      IF (EPSBAK .LT. EPSTRY)  GO TO 35  
   33 CONTINUE   
      EPSTRY = 1.0E-7    
      EPSMAC = 4.0*EPSTRY    
      WRITE (ISYSWR,'(A,A,E10.2)') ' MNINIT UNABLE TO DETERMINE',    
     + ' ARITHMETIC PRECISION. WILL ASSUME:',EPSMAC  
   35 EPSMAC = 8.0 * EPSTRY  
      EPSMA2 = 2.0 * DSQRT(EPSMAC)    
C                 the vlims are a non-negligible distance from pi/2  
C         used by MNPINT to set variables "near" the physical limits     
      PIBY2 = 2.0*ATAN(1.0)  
      DISTNN = 8.0*DSQRT(EPSMA2)  
      VLIMHI =  PIBY2 - DISTNN   
      VLIMLO = -PIBY2 + DISTNN   
      CALL MNCLER    
      WRITE (ISYSWR,'(3A,I3,A,I3,A,E10.2)')  '  MINUIT RELEASE ',CVRSN,  
     +' INITIALIZED.   DIMENSIONS ',MNE,'/',MNI,'  EPSMAC=',EPSMAC   
      RETURN     
      END    
      SUBROUTINE MNINTR   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       Called by user. Interfaces to MNREAD to allow user to change    
CC       easily from Fortran-callable to interactive mode.   
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
      IFLGIN = 3     
      CALL MNREAD(IFLGIN,IFLGUT)   
      WRITE (ISYSWR,'(2A/)')  ' END OF MINUIT COMMAND INPUT. ',  
     +      '   RETURN TO USER PROGRAM.'     
      RETURN     
      END    
      SUBROUTINE MNLIMS   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       Called from MNSET   
CC       Interprets the SET LIM command, to reset the parameter limits   
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
C    
      CFROM = 'SET LIM '     
      NFCNFR = NFCN  
      CSTATU= 'NO CHANGE '   
      I2 = WORD7(1)  
      IF (I2 .GT. MAXEXT .OR. I2 .LT. 0)  GO TO 900  
      IF (I2 .GT. 0)  GO TO 30   
C                                     set limits on all parameters   
      NEWCOD = 4     
      IF (WORD7(2) .EQ. WORD7(3))  NEWCOD = 1    
      DO 20 INU= 1, NU   
      IF (NVARL(INU) .LE. 0)  GO TO 20   
      IF (NVARL(INU).EQ.1 .AND. NEWCOD.EQ.1)  GO TO 20   
      KINT = NIOFEX(INU)     
C             see if parameter has been fixed    
      IF (KINT .LE. 0)  THEN     
         IF (ISW(5) .GE. 0)  WRITE (ISYSWR,'(11X,A,I3)')     
     +      ' LIMITS NOT CHANGED FOR FIXED PARAMETER:',INU   
         GO TO 20    
      ENDIF  
      IF (NEWCOD .EQ. 1)  THEN   
C            remove limits from parameter    
         IF (ISW(5) .GT. 0)     WRITE (ISYSWR,134)  INU  
         CSTATU = 'NEW LIMITS'   
         CALL MNDXDI(X(KINT),KINT,DXDI)  
         SNEW = GSTEP(KINT)*DXDI     
         GSTEP(KINT) = DABS(SNEW)     
         NVARL(INU) = 1  
      ELSE   
C             put limits on parameter    
         ALIM(INU) = DMIN1(WORD7(2),WORD7(3))  
         BLIM(INU) = DMAX1(WORD7(2),WORD7(3))  
         IF (ISW(5) .GT. 0) WRITE (ISYSWR,237)  INU,ALIM(INU),BLIM(INU)  
         NVARL(INU) = 4  
         CSTATU = 'NEW LIMITS'   
         GSTEP(KINT) = -0.1  
      ENDIF  
   20 CONTINUE   
      GO TO 900  
C                                       set limits on one parameter  
   30 IF (NVARL(I2) .LE. 0)  THEN    
        WRITE (ISYSWR,'(A,I3,A)') ' PARAMETER ',I2,' IS NOT VARIABLE.'   
        GO TO 900    
      ENDIF  
      KINT = NIOFEX(I2)  
C                                       see if parameter was fixed   
      IF (KINT .EQ. 0)  THEN     
         WRITE (ISYSWR,'(A,I3)')     
     +     ' REQUEST TO CHANGE LIMITS ON FIXED PARAMETER:',I2    
         DO 82 IFX= 1, NPFIX     
         IF (I2 .EQ. IPFIX(IFX)) GO TO 92    
   82    CONTINUE    
         WRITE (ISYSWR,'(A)') ' MINUIT BUG IN MNLIMS. SEE F. JAMES'  
   92    CONTINUE    
      ENDIF  
      IF (WORD7(2) .NE. WORD7(3))  GO TO 235     
C                                       remove limits    
      IF (NVARL(I2) .NE. 1)  THEN    
         IF (ISW(5) .GT. 0)  WRITE (ISYSWR,134)  I2  
  134    FORMAT (30H LIMITS REMOVED FROM PARAMETER  ,I4)     
         CSTATU = 'NEW LIMITS'   
         IF (KINT .LE. 0)  THEN  
            GSTEPS(IFX) = DABS(GSTEPS(IFX))   
         ELSE    
            CALL MNDXDI(X(KINT),KINT,DXDI)   
            IF (DABS(DXDI) .LT. 0.01)  DXDI=0.01  
            GSTEP(KINT) = DABS(GSTEP(KINT)*DXDI)  
            GRD(KINT) = GRD(KINT)*DXDI   
         ENDIF   
         NVARL(I2) = 1   
      ELSE   
         WRITE (ISYSWR,'(A,I3)') ' NO LIMITS SPECIFIED.  PARAMETER ',    
     +        I2,' IS ALREADY UNLIMITED.  NO CHANGE.'    
      ENDIF  
      GO TO 900  
C                                        put on limits   
  235 ALIM(I2) = DMIN1(WORD7(2),WORD7(3))  
      BLIM(I2) = DMAX1(WORD7(2),WORD7(3))  
      NVARL(I2) = 4  
      IF (ISW(5) .GT. 0)   WRITE (ISYSWR,237)  I2,ALIM(I2),BLIM(I2)  
  237 FORMAT (10H PARAMETER ,I3, 14H LIMITS SET TO  ,2G15.5)     
      CSTATU = 'NEW LIMITS'  
      IF (KINT .LE. 0)  THEN     
         GSTEPS(IFX) = -0.1  
      ELSE   
         GSTEP(KINT) = -0.1  
      ENDIF  
C    
  900 CONTINUE   
      IF (CSTATU .NE. 'NO CHANGE ')  THEN    
        CALL MNEXIN(X)   
        CALL MNRSET(1)   
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNLINE(START,FSTART,STEP,SLOPE,TOLER)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Perform a line search from position START  
CC        along direction STEP, where the length of vector STEP  
CC                   gives the expected position of minimum.     
CC        FSTART is value of function at START   
CC        SLOPE (if non-zero) is df/dx along STEP at START   
CC        TOLER is initial tolerance of minimum in direction STEP    
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
      DIMENSION START(*), STEP(*)    
      PARAMETER (MAXPT=12)   
      DIMENSION XPQ(MAXPT),YPQ(MAXPT)    
      CHARACTER*1 CHPQ(MAXPT)    
      DIMENSION XVALS(3),FVALS(3),COEFF(3)   
      CHARACTER*26 CHARAL    
      CHARACTER*60 CMESS     
      PARAMETER (SLAMBG=5.,ALPHA=2.)     
C SLAMBG and ALPHA control the maximum individual steps allowed.     
C The first step is always =1. The max length of second step is SLAMBG.  
C The max size of subsequent steps is the maximum previous successful    
C   step multiplied by ALPHA + the size of most recent successful step,  
C   but cannot be smaller than SLAMBG.   
      LOGICAL LDEBUG     
      DATA CHARAL / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /   
      LDEBUG = (IDBG(1).GE.1)    
C                  starting values for overall limits on total step SLAM     
      OVERAL = 1000.     
      UNDRAL = -100.     
C                              debug check if start is ok    
      IF (LDEBUG)  THEN  
         CALL MNINEX(START)  
         CALL FCN(NPARX,GIN,F1,U,4)    
         NFCN=NFCN+1     
         IF (F1 .NE. FSTART) THEN    
             WRITE (ISYSWR,'(A/2E14.5/2X,10F10.5)')  
     + ' MNLINE start point not consistent, F values, parameters=',  






     +  (X(KK),KK=1,NPAR)    




         ENDIF   




      ENDIF  
C                                      . set up linear search along STEP     
     
      FVMIN = FSTART     


      XVMIN = 0.     
      NXYPT = 1  
      CHPQ(1) = CHARAL(1:1)  
      XPQ(1) = 0.    


      YPQ(1) = FSTART    


C               SLAMIN = smallest possible value of DABS(SLAM)    
      SLAMIN = 0.    
      DO 20 I= 1, NPAR   
      IF (STEP(I) .EQ. ZERO)  GO TO 20   
      RATIO = DABS(START(I)/STEP(I))  
      IF (SLAMIN .EQ. ZERO)     SLAMIN = RATIO   
      IF (RATIO .LT. SLAMIN)  SLAMIN = RATIO     
   20 X(I) = START(I) + STEP(I)  
      IF (SLAMIN .EQ. ZERO)  SLAMIN = EPSMAC     
      SLAMIN = SLAMIN*EPSMA2     
      NPARX = NPAR   
C    
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,F1,U,4)   
      NFCN=NFCN+1    
      NXYPT = NXYPT + 1  
      CHPQ(NXYPT) = CHARAL(NXYPT:NXYPT)  
      XPQ(NXYPT) = 1.    
      YPQ(NXYPT) = F1    
      IF (F1 .LT. FSTART) THEN   
         FVMIN = F1  
         XVMIN = 1.0     
      ENDIF  
C                         . quadr interp using slope GDEL and two points     
      SLAM = 1.  
      TOLER8 = TOLER     
      SLAMAX = SLAMBG    
      FLAST = F1     
C                         can iterate on two-points (cut) if no imprvmnt     
   25 CONTINUE   
      DENOM = 2.0*(FLAST-FSTART-SLOPE*SLAM)/SLAM**2  
C     IF (DENOM .EQ. ZERO)  DENOM = -0.1*SLOPE   
                            SLAM  = 1.   
      IF (DENOM .NE. ZERO)  SLAM = -SLOPE/DENOM  
      IF (SLAM  .LT. ZERO)  SLAM = SLAMAX    
      IF (SLAM .GT. SLAMAX)  SLAM = SLAMAX   
      IF (SLAM .LT. TOLER8)  SLAM = TOLER8   
      IF (SLAM .LT. SLAMIN)  GO TO 80    
      IF (DABS(SLAM-1.0).LT.TOLER8 .AND. F1.LT.FSTART)  GO TO 70  
      IF (DABS(SLAM-1.0).LT.TOLER8) SLAM = 1.0+TOLER8     
      IF (NXYPT .GE. MAXPT) GO TO 65     
      DO 30 I= 1, NPAR   
   30 X(I) = START(I) + SLAM*STEP(I)     
      CALL MNINEX(X)     
      CALL FCN(NPAR,GIN,F2,U,4)    
      NFCN = NFCN + 1    
      NXYPT = NXYPT + 1  
      CHPQ(NXYPT) = CHARAL(NXYPT:NXYPT)  
      XPQ(NXYPT) = SLAM  
      YPQ(NXYPT) = F2    
      IF (F2 .LT. FVMIN)  THEN   
         FVMIN = F2  
         XVMIN = SLAM    
      ENDIF  
      IF (FSTART .EQ. FVMIN) THEN    
         FLAST = F2  
         TOLER8 = TOLER*SLAM     
         OVERAL = SLAM-TOLER8    
         SLAMAX = OVERAL     
         GO TO 25    
      ENDIF  
C                                        . quadr interp using 3 points   
      XVALS(1) = XPQ(1)  
      FVALS(1) = YPQ(1)  
      XVALS(2) = XPQ(NXYPT-1)    
      FVALS(2) = YPQ(NXYPT-1)    
      XVALS(3) = XPQ(NXYPT)  
      FVALS(3) = YPQ(NXYPT)  
C                             begin iteration, calculate desired step    
   50 CONTINUE   
      SLAMAX = DMAX1(SLAMAX,ALPHA*DABS(XVMIN))  
      CALL MNPFIT(XVALS,FVALS,3,COEFF,SDEV)  
      IF (COEFF(3) .LE. ZERO)  THEN  
         SLOPEM = 2.0*COEFF(3)*XVMIN + COEFF(2)  
         IF (SLOPEM .LE. ZERO) THEN  
            SLAM = XVMIN + SLAMAX    
         ELSE    
            SLAM = XVMIN - SLAMAX    
         ENDIF   
      ELSE   
         SLAM = -COEFF(2)/(2.0*COEFF(3))     
         IF (SLAM .GT. XVMIN+SLAMAX)  SLAM = XVMIN+SLAMAX    
         IF (SLAM .LT. XVMIN-SLAMAX)  SLAM = XVMIN-SLAMAX    
      ENDIF  
      IF (SLAM .GT. ZERO) THEN   
          IF (SLAM .GT. OVERAL) SLAM = OVERAL    
      ELSE   
          IF (SLAM .LT. UNDRAL) SLAM = UNDRAL    
      ENDIF  
C               come here if step was cut below  
   52 CONTINUE   
      TOLER9 = DMAX1(TOLER8,DABS(TOLER8*SLAM))  
      DO 55 IPT= 1, 3    
      IF (DABS(SLAM-XVALS(IPT)) .LT. TOLER9)  GO TO 70    
   55 CONTINUE   
C                take the step   
      DO 60 I= 1, NPAR   
   60 X(I) = START(I)+SLAM*STEP(I)   
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,F3,U,4)   
      NFCN = NFCN + 1    
      NXYPT = NXYPT + 1  
      CHPQ(NXYPT) = CHARAL(NXYPT:NXYPT)  
      XPQ(NXYPT) = SLAM  
      YPQ(NXYPT) = F3    
C             find worst previous point out of three     
      FVMAX = FVALS(1)   
      NVMAX = 1  
      IF (FVALS(2) .GT. FVMAX) THEN  
         FVMAX = FVALS(2)    
         NVMAX = 2   
      ENDIF  
      IF (FVALS(3) .GT. FVMAX) THEN  
         FVMAX = FVALS(3)    
         NVMAX = 3   
      ENDIF  
C              if latest point worse than all three previous, cut step   
      IF (F3 .GE. FVMAX)  THEN   
          IF (NXYPT .GE. MAXPT) GO TO 65     
          IF (SLAM .GT. XVMIN) OVERAL = DMIN1(OVERAL,SLAM-TOLER8)  
          IF (SLAM .LT. XVMIN) UNDRAL = DMAX1(UNDRAL,SLAM+TOLER8)  
          SLAM = 0.5*(SLAM+XVMIN)    
          GO TO 52   
      ENDIF  
C              prepare another iteration, replace worst previous point   
      XVALS(NVMAX) = SLAM    
      FVALS(NVMAX) = F3  
      IF (F3 .LT. FVMIN)  THEN   
         FVMIN = F3  
         XVMIN = SLAM    
      ELSE   
         IF (SLAM .GT. XVMIN) OVERAL = DMIN1(OVERAL,SLAM-TOLER8)   
         IF (SLAM .LT. XVMIN) UNDRAL = DMAX1(UNDRAL,SLAM+TOLER8)   
      ENDIF  
      IF (NXYPT .LT. MAXPT)  GO TO 50    
C                                            . . end of iteration . . .  
C            stop because too many iterations    
   65 CMESS = ' LINE SEARCH HAS EXHAUSTED THE LIMIT OF FUNCTION CALLS '  
      IF (LDEBUG) THEN   
        WRITE (ISYSWR,'(A/(2X,6G12.4))') ' MNLINE DEBUG: steps=',    
     +    (STEP(KK),KK=1,NPAR)   
      ENDIF  
      GO TO 100  
C            stop because within tolerance   
   70 CONTINUE   
      CMESS = ' LINE SEARCH HAS ATTAINED TOLERANCE '     
      GO TO 100  
   80 CONTINUE   
      CMESS = ' STEP SIZE AT ARITHMETICALLY ALLOWED MINIMUM'     
  100 CONTINUE   
      AMIN = FVMIN   
      DO 120 I= 1, NPAR  
      DIRIN(I) = STEP(I)*XVMIN   
  120 X(I) = START(I) + DIRIN(I)     
      CALL MNINEX(X)     
      IF (XVMIN .LT. 0.)      CALL MNWARN('D','MNLINE',  
     +                   ' LINE MINIMUM IN BACKWARDS DIRECTION')     
      IF (FVMIN .EQ. FSTART)  CALL MNWARN('D','MNLINE',  
     +                     ' LINE SEARCH FINDS NO IMPROVEMENT ')     
      IF (LDEBUG)  THEN  
         WRITE (ISYSWR,'('' AFTER'',I3,'' POINTS,'',A)') NXYPT,CMESS     
         CALL MNPLOT(XPQ,YPQ,CHPQ,NXYPT,ISYSWR,NPAGWD,NPAGLN)    
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNMATU(KODE)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        prints the covariance matrix v when KODE=1.    
CC        always prints the global correlations, and     
CC        calculates and prints the individual correlation coefficients  
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
      DIMENSION VLINE(MNI)   
      ISW2 = ISW(2)  
      IF (ISW2 .LT. 1)  THEN     
          WRITE (ISYSWR,'(1X,A)')  COVMES(ISW2)  
          GO TO 500  
      ENDIF  
      IF (NPAR .EQ. 0)  THEN     
          WRITE (ISYSWR,'('' MNMATU: NPAR=0'')')     
          GO TO 500  
          ENDIF  
C                                       . . . . .external error matrix   
      IF (KODE .EQ. 1)  THEN     
         ISW5 = ISW(5)   
         ISW(5) = 2  
         CALL MNEMAT(P,MAXINT)   
           IF (ISW2.LT.3)  WRITE (ISYSWR,'(1X,A)')  COVMES(ISW2)     
         ISW(5) = ISW5   
      ENDIF  
C                                       . . . . . correlation coeffs. .  
      IF (NPAR .LE. 1)   GO TO 500   
      CALL MNWERR    
C     NCOEF is number of coeff. that fit on one line, not to exceed 20   
      NCOEF = (NPAGWD-19)/6  
      NCOEF = MIN(NCOEF,20)  
      NPARM = MIN(NPAR,NCOEF)    
      WRITE (ISYSWR, 150) (NEXOFI(ID),ID=1,NPARM)    
  150 FORMAT (/36H PARAMETER  CORRELATION COEFFICIENTS  /    
     +         18H       NO.  GLOBAL   ,20I6)    
      DO 200 I= 1, NPAR  
         IX = NEXOFI(I)  
         NDI = I*(I+1)/2     
           DO 170 J= 1, NPAR     
           M = MAX(I,J)  
           N = MIN(I,J)  
           NDEX = M*(M-1)/2 + N  
           NDJ = J*(J+1)/2   
  170      VLINE(J) = VHMAT(NDEX)/DSQRT(DABS(VHMAT(NDI)*VHMAT(NDJ)))   
         NPARM = MIN(NPAR,NCOEF)     
         WRITE (ISYSWR,171)   IX, GLOBCC(I), (VLINE(IT),IT=1,NPARM)  
  171    FORMAT (6X,I3,2X,F7.5,1X,20F6.3)    
         IF (I.LE.NPARM) GO TO 200   
            DO 190 ISO= 1, 10    
            NSOFAR = NPARM   
            NPARM = MIN(NPAR,NSOFAR+NCOEF)   
            WRITE (ISYSWR,181)  (VLINE(IT),IT=NSOFAR+1,NPARM)    
  181       FORMAT (19X,20F6.3)  
            IF (I .LE. NPARM) GO TO 192  
  190       CONTINUE     
  192    CONTINUE    
  200 CONTINUE   
      IF (ISW2.LT.3)  WRITE (ISYSWR,'(1X,A)')  COVMES(ISW2)  
  500 RETURN     
      END    
      SUBROUTINE MNMIGR   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Performs a local function minimization using basically the     
CC        method of Davidon-Fletcher-Powell as modified by Fletcher  
CC        ref. -- Fletcher, Comp.J. 13,317 (1970)   "switching method"   
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
      DIMENSION GS(MNI), STEP(MNI),  XXS(MNI), FLNU(MNI), VG(MNI)    
      LOGICAL LDEBUG     
      PARAMETER (TOLER=0.05)     
      IF (NPAR .LE. 0)  RETURN   
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      LDEBUG = (IDBG(4) .GE. 1)  
      CFROM = 'MIGRAD  '     
      NFCNFR = NFCN  
      NFCNMG = NFCN  
      CSTATU= 'INITIATE  '   
      ISWTR = ISW(5) - 2*ITAUR   
      NPFN = NFCN    
      NPARX = NPAR   
      VLEN = NPAR*(NPAR+1)/2     
      NRSTRT = 0     
      NPSDF = 0  
      LINED2 = 0     
      ISW(4) = -1    
      RHOTOL = 1.0E-3*APSI   
      IF (ISWTR .GE. 1)  WRITE (ISYSWR,470) ISTRAT,RHOTOL    
  470 FORMAT (' START MIGRAD MINIMIZATION.  STRATEGY',I2,    
     +'.  CONVERGENCE WHEN EDM .LT.',E9.2)   
C                                           initialization strategy  
      IF (ISTRAT.LT.2 .OR. ISW(2).GE.3)  GO TO 2     
C                                come (back) here to restart completely  
    1 CONTINUE   
      IF (NRSTRT .GT. ISTRAT)  THEN  
         CSTATU= 'FAILED    '    
         ISW(4) = -1     
         GO TO 230   
         ENDIF   
C                                      . get full covariance and gradient    
      CALL MNHESS     
      CALL MNWERR    
      NPSDF = 0  
      IF (ISW(2) .GE. 1)  GO TO 10   
C                                        . get gradient at start point   
    2 CONTINUE   
      CALL MNINEX(X)     
      IF (ISW(3) .EQ. 1) THEN    
          CALL FCN(NPARX,GIN,FZERO,U,2)    
          NFCN = NFCN + 1    
      ENDIF  
      CALL MNDERI     
      IF (ISW(2) .GE. 1)  GO TO 10   
C                                   sometimes start with diagonal matrix     
      DO 3 I= 1, NPAR    
         XXS(I) = X(I)   
         STEP(I) = ZERO  
    3 CONTINUE   
C                           do line search if second derivative negative     
      LINED2 = LINED2 + 1    
      IF (LINED2 .LT. 2*NPAR) THEN   
      DO 5 I= 1, NPAR    
         IF (G2(I) .GT. 0.)  GO TO 5     
         STEP(I) = -DSIGN(GSTEP(I),GRD(I))    
         GDEL = STEP(I)*GRD(I)   
         FS = AMIN   
         CALL MNLINE(XXS,FS,STEP,GDEL,TOLER)   
         CALL MNWARN('D','MNMIGR','Negative G2 line search')     
         IEXT = NEXOFI(I)    
         IF (LDEBUG) WRITE (ISYSWR,'(A,I3,2G13.3)')  
     +    ' Negative G2 line search, param ',IEXT,FS,AMIN    
         GO TO 2     
    5 CONTINUE   
      ENDIF  
C                           make diagonal error matrix   
      DO 8 I=1,NPAR  
         NDEX = I*(I-1)/2    
           DO 7 J=1,I-1  
           NDEX = NDEX + 1   
    7      VHMAT(NDEX) = 0.  
         NDEX = NDEX + 1     
         IF (G2(I) .LE. ZERO)  G2(I) = 1.    
         VHMAT(NDEX) = 2./G2(I)  
    8 CONTINUE   
      DCOVAR = 1.    
      IF (LDEBUG) WRITE (ISYSWR,'(A,A/(1X,10G10.2))') ' DEBUG MNMIGR,',  
     +  ' STARTING MATRIX DIAGONAL,  VHMAT=', (VHMAT(KK),KK=1,VLEN)  
C                                         ready to start first iteration     
   10 CONTINUE   
      IMPRUV = 0     
      NRSTRT = NRSTRT + 1    
      IF (NRSTRT .GT. ISTRAT+1)  THEN    
         CSTATU= 'FAILED    '    
         GO TO 230   
         ENDIF   
      FS = AMIN  
C                                        . . . get EDM and set up loop   
      EDM = 0.   
         DO 18 I= 1, NPAR    
         GS(I) = GRD(I)  
         XXS(I) = X(I)   
         NDEX = I*(I-1)/2    
           DO 17 J= 1, I-1   
           NDEX = NDEX + 1   
   17      EDM = EDM + GS(I)*VHMAT(NDEX)*GS(J)   
         NDEX = NDEX + 1     
   18    EDM = EDM + 0.5 * GS(I)**2 *VHMAT(NDEX)     
      EDM = EDM * 0.5 * (1.0+3.0*DCOVAR)     
        IF (EDM .LT. ZERO)  THEN     
        CALL MNWARN('W','MIGRAD','STARTING MATRIX NOT POS-DEFINITE.')    
        ISW(2) = 0   
        DCOVAR = 1.  
        GO TO 2  
        ENDIF    
      IF (ISW(2) .EQ. 0)  EDM=BIGEDM     
      ITER = 0   
      CALL MNINEX(X)     
      CALL MNWERR    
      IF (ISWTR .GE. 1)  CALL MNPRIN(3,AMIN)     
      IF (ISWTR .GE. 2)  CALL MNMATU(0)  
C                                        . . . . .  start main loop  
   24 CONTINUE   
      IF (NFCN-NPFN .GE. NFCNMX)  GO TO 190  
      GDEL = 0.  
      GSSQ = 0.  
         DO 30  I=1,NPAR     
         RI = 0.     
         GSSQ = GSSQ + GS(I)**2  
           DO 25 J=1,NPAR    
           M = MAX(I,J)  
           N = MIN(I,J)  
           NDEX = M*(M-1)/2 + N  
   25      RI = RI + VHMAT(NDEX) *GS(J)  
         STEP(I) = -0.5*RI   
   30    GDEL = GDEL + STEP(I)*GS(I)     
      IF (GSSQ .EQ. ZERO)  THEN  
          CALL MNWARN('D','MIGRAD',  
     +             ' FIRST DERIVATIVES OF FCN ARE ALL ZERO')     
          GO TO 300  
      ENDIF  
C                 if gdel positive, V not posdef     
      IF (GDEL .GE. ZERO)  THEN  
         CALL MNWARN('D','MIGRAD',' NEWTON STEP NOT DESCENT.')   
         IF (NPSDF .EQ. 1)  GO TO 1  
         CALL MNPSDF     
         NPSDF = 1   
         GO TO 24    
         ENDIF   
C                                        . . . . do line search  
      CALL MNLINE(XXS,FS,STEP,GDEL,TOLER)  
      IF (AMIN .EQ. FS) GO TO 200    
      CFROM  = 'MIGRAD  '    
      NFCNFR = NFCNMG    
      CSTATU= 'PROGRESS  '   
C                                        . get gradient at new point     
      CALL MNINEX(X)     
      IF (ISW(3) .EQ. 1) THEN    
          CALL FCN(NPARX,GIN,FZERO,U,2)    
          NFCN = NFCN + 1    
      ENDIF  
      CALL MNDERI     
C                                         . calculate new EDM    
      NPSDF = 0  
   81 EDM = 0.   
      GVG = 0.   
      DELGAM = 0.    
      GDGSSQ = 0.    
         DO 100 I= 1, NPAR   
         RI = 0.     
         VGI = 0.    
           DO 90 J= 1, NPAR  
           M = MAX(I,J)  
           N = MIN(I,J)  
           NDEX = M*(M-1)/2 + N  
           VGI = VGI + VHMAT(NDEX)*(GRD(J)-GS(J))    
   90      RI  =  RI + VHMAT(NDEX)* GRD(J)   
      VG(I) = VGI*0.5    
      GAMI = GRD(I) - GS(I)  
      GDGSSQ = GDGSSQ + GAMI**2  
      GVG = GVG + GAMI*VG(I)     
      DELGAM = DELGAM + DIRIN(I)*GAMI    
  100 EDM = EDM + GRD(I)*RI*0.5  
      EDM = EDM * 0.5 * (1.0 + 3.0*DCOVAR)   
C                          . if EDM negative,  not positive-definite     
      IF (EDM .LT. ZERO .OR. GVG .LE. ZERO)  THEN    
         CALL MNWARN('D','MIGRAD','NOT POS-DEF. EDM OR GVG NEGATIVE.')   
         CSTATU = 'NOT POSDEF'   
         IF (NPSDF .EQ. 1)  GO TO 230    
         CALL MNPSDF     
         NPSDF = 1   
         GO TO 81    
      ENDIF  
C                            print information about this iteration  
      ITER = ITER + 1    
      IF (ISWTR.GE.3 .OR. (ISWTR.EQ.2.AND.MOD(ITER,10).EQ.1)) THEN   
         CALL MNWERR     
         CALL MNPRIN(3,AMIN)     
      ENDIF  
      IF (GDGSSQ .EQ. ZERO)  CALL MNWARN('D','MIGRAD',   
     +           'NO CHANGE IN FIRST DERIVATIVES OVER LAST STEP')    
      IF (DELGAM .LT. ZERO) CALL MNWARN('D','MIGRAD',    
     +          'FIRST DERIVATIVES INCREASING ALONG SEARCH LINE')    
C                                        .  update covariance matrix     
      CSTATU = 'IMPROVEMNT'  
        IF (LDEBUG) WRITE (ISYSWR,'(A,(1X,10G10.3))') ' VHMAT 1 =',  
     +             (VHMAT(KK),KK=1,10)   
      DSUM = 0.  
      VSUM = 0.  
         DO  120  I=1, NPAR  
           DO  120  J=1, I   
           D = DIRIN(I)*DIRIN(J)/DELGAM - VG(I)*VG(J)/GVG    
           DSUM = DSUM + DABS(D)  
           NDEX = I*(I-1)/2 + J  
           VHMAT(NDEX) = VHMAT(NDEX) + 2.0*D     
           VSUM = VSUM + DABS(VHMAT(NDEX))    
  120      CONTINUE  
C                smooth local fluctuations by averaging DCOVAR   
      DCOVAR = 0.5*(DCOVAR + DSUM/VSUM)  
      IF (ISWTR.GE.3 .OR. LDEBUG) WRITE (ISYSWR,'(A,F5.1,A)')    
     +      ' RELATIVE CHANGE IN COV. MATRIX=',DCOVAR*100.,'%'   
      IF (LDEBUG) WRITE (ISYSWR,'(A,(1X,10G10.3))') ' VHMAT 2 =',    
     +             (VHMAT(KK),KK=1,10)   
      IF (DELGAM .LE. GVG)  GO TO 135    
      DO 125 I= 1, NPAR  
  125 FLNU(I) = DIRIN(I)/DELGAM - VG(I)/GVG  
      DO 130 I= 1, NPAR  
      DO 130 J= 1, I     
      NDEX = I*(I-1)/2 + J   
  130 VHMAT(NDEX) = VHMAT(NDEX) + 2.0*GVG*FLNU(I)*FLNU(J)    
  135 CONTINUE   
C                                              and see if converged  
      IF (EDM .LT. 0.1*RHOTOL)  GO TO 300    
C                                    if not, prepare next iteration  
      DO 140 I= 1, NPAR  
      XXS(I) = X(I)  
      GS(I) = GRD(I)     
  140 CONTINUE   
      FS = AMIN  
      IMPRUV = IMPRUV + 1    
      IF (ISW(2) .EQ. 0  .AND. DCOVAR.LT. 0.5 )  ISW(2) = 1  
      IF (ISW(2) .EQ. 3  .AND. DCOVAR.GT. 0.1 )  ISW(2) = 1  
      IF (ISW(2) .EQ. 1  .AND. DCOVAR.LT. 0.05)  ISW(2) = 3  
      GO TO 24   
C                                        . . . . .  end main loop    
C                                         . . call limit in MNMIGR   
  190 ISW(1) = 1     
      IF (ISW(5) .GE. 0)     
     +     WRITE (ISYSWR,'(A)')  ' CALL LIMIT EXCEEDED IN MIGRAD.'   
      CSTATU = 'CALL LIMIT'  
      GO TO 230  
C                                         . . fails to improve . .   
  200 IF (ISWTR .GE. 1)  WRITE (ISYSWR,'(A)')    
     +           ' MIGRAD FAILS TO FIND IMPROVEMENT'     
      DO 210 I= 1, NPAR  
  210 X(I) = XXS(I)  
      IF (EDM .LT. RHOTOL)  GO TO 300    
      IF (EDM .LT. DABS(EPSMA2*AMIN))  THEN   
         IF (ISWTR .GE. 0)  WRITE (ISYSWR, '(A)')    
     +      ' MACHINE ACCURACY LIMITS FURTHER IMPROVEMENT.'  
         GO TO 300   
         ENDIF   
      IF (ISTRAT .LT. 1)  THEN   
         IF (ISW(5) .GE. 0) WRITE (ISYSWR, '(A)')    
     +    ' MIGRAD FAILS WITH STRATEGY=0.   WILL TRY WITH STRATEGY=1.'   
         ISTRAT = 1  
      ENDIF  
         GO TO 1     
C                                         . . fails to converge  
  230 IF (ISWTR .GE. 0)  WRITE (ISYSWR,'(A)')    
     +    ' MIGRAD TERMINATED WITHOUT CONVERGENCE.'  
      IF (ISW(2) .EQ. 3)  ISW(2) = 1     
      ISW(4) = -1    
      GO TO 400  
C                                         . . apparent convergence   
  300 IF (ISWTR .GE. 0) WRITE(ISYSWR,'(/A)')     
     +   ' MIGRAD MINIMIZATION HAS CONVERGED.'   
      IF (ITAUR .EQ. 0) THEN     
        IF (ISTRAT .GE. 2 .OR. (ISTRAT.EQ.1.AND.ISW(2).LT.3)) THEN   
           IF (ISW(5) .GE. 0)  WRITE (ISYSWR, '(/A)')    
     +      ' MIGRAD WILL VERIFY CONVERGENCE AND ERROR MATRIX.'  
           CALL MNHESS    
           CALL MNWERR   
           NPSDF = 0     
           IF (EDM .GT. RHOTOL) GO TO 10     
        ENDIF    
      ENDIF  
      CSTATU='CONVERGED '    
      ISW(4) = 1     
C                                           come here in any case    
  400 CONTINUE   
      CFROM = 'MIGRAD  '     
      NFCNFR = NFCNMG    
      CALL  MNINEX(X)    
      CALL MNWERR    
      IF (ISWTR .GE. 0)  CALL MNPRIN (3,AMIN)    
      IF (ISWTR .GE. 1)  CALL MNMATU(1)  
      RETURN     
      END    
      SUBROUTINE MNMNOS   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Performs a MINOS error analysis on those parameters for    
CC        which it is requested on the MINOS command.    
CC   
      PARAMETER (MNE=100 , MNI=50)   
      PARAMETER (MNIHL=MNI*(MNI+1)/2)    
      CHARACTER*10 CPNAM     
      COMMON     
     1/MN7NAM/ CPNAM(MNE)    
     2/MN7EXT/ U(MNE)     ,ALIM(MNE)  ,BLIM(MNE)     
     3/MN7ERR/ ERP(MNI)   ,ERN(MNI)   ,WERR(MNI)  ,GLOBCC(MNI)   
     4/MN7INX/ NVARL(MNE) ,NIOFEX(MNE),NEXOFI(MNI)   
     5/MN7INT/ X(MNI)     ,XT(MNI)    ,DIRIN(MNI)    
     6/MN7FX2/ XS(MNI)    ,XTS(MNI)   ,DIRINS(MNI)   
     7/MN7DER/ GRD(MNI)   ,G2(MNI)    ,GSTEP(MNI) ,GIN(MNE) ,DGRD(MNI)   
     8/MN7FX3/ GRDS(MNI)  ,G2S(MNI)   ,GSTEPS(MNI)   
     9/MN7FX1/ IPFIX(MNI) ,NPFIX     
     A/MN7VAR/ VHMAT(MNIHL)  
     B/MN7VAT/ VTHMAT(MNIHL)     
     C/MN7SIM/ P(MNI,MNI+1),PSTAR(MNI),PSTST(MNI) ,PBAR(MNI),PRHO(MNI)   
C    
      PARAMETER (MAXDBG=10, MAXSTK=10, MAXCWD=20, MAXP=30, MAXCPT=101)   
      PARAMETER (ZERO=0.0,  ONE=1.0,   HALF=0.5)     
      COMMON     
     D/MN7NPR/ MAXINT ,NPAR   ,MAXEXT ,NU    
     E/MN7IOU/ ISYSRD ,ISYSWR ,ISYSSA ,NPAGWD ,NPAGLN ,NEWPAG    
     E/MN7IO2/ ISTKRD(MAXSTK) ,NSTKRD ,ISTKWR(MAXSTK) ,NSTKWR    
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD  ,CUNDEF ,CVRSN ,COVMES     
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND    
     H/MN7MIN/ AMIN   ,UP     ,EDM    ,FVAL3  ,EPSI   ,APSI  ,DCOVAR     
     I/MN7CNV/ NFCN   ,NFCNMX ,NFCNLC ,NFCNFR ,ITAUR,ISTRAT,NWRMES(2)    
     J/MN7ARG/ WORD7(MAXP)   
     K/MN7LOG/ LWARN  ,LREPOR ,LIMSET ,LNOLIM ,LNEWMN ,LPHEAD    
     L/MN7CNS/ EPSMAC ,EPSMA2 ,VLIMLO ,VLIMHI ,UNDEFI ,BIGEDM,UPDFLT     
     M/MN7RPT/ XPT(MAXCPT)    ,YPT(MAXCPT)   
     N/MN7CPT/ CHPT(MAXCPT)  
     o/MN7XCR/ XMIDCR ,YMIDCR ,XDIRCR ,YDIRCR ,KE1CR  ,KE2CR     
      CHARACTER CTITL*50, CWORD*(MAXCWD), CUNDEF*10, CFROM*8,    
     +          CVRSN*6,  COVMES(0:3)*22, CSTATU*10, CHPT*1  
      LOGICAL   LWARN, LREPOR, LIMSET, LNOLIM, LNEWMN, LPHEAD    
     
      IF (NPAR .LE. 0)  GO TO 700    
      NGOOD = 0  
      NBAD = 0   
      NFCNMI = NFCN  
C                                      . loop over parameters requested  
      DO 570 KNT= 1, NPAR    
      IF (INT(WORD7(2)) .EQ. 0) THEN     
          ILAX = NEXOFI(KNT)     
      ELSE   
          IF (KNT .GE. 7)  GO TO 580     
          ILAX = INT(WORD7(KNT+1))   
          IF (ILAX .EQ. 0)  GO TO 580    
          IF (ILAX .GT. 0 .AND. ILAX .LE. NU) THEN   
             IF (NIOFEX(ILAX) .GT. 0)  GO TO 565     
          ENDIF  
          WRITE (ISYSWR,564) ILAX    
  564     FORMAT (' PARAMETER NUMBER ',I5,' NOT VARIABLE. IGNORED.')     
          GO TO 570  
      ENDIF  
  565 CONTINUE   
C                                         calculate one pair of M E's    
      ILAX2 = 0  
      CALL MNMNOT(ILAX,ILAX2,VAL2PL,VAL2MI)    
      IF (LNEWMN)  GO TO 650     
C                                          update NGOOD and NBAD     
      IIN = NIOFEX(ILAX)     
      IF (ERP(IIN) .GT. ZERO) THEN   
         NGOOD=NGOOD+1   
      ELSE   
         NBAD=NBAD+1     
      ENDIF  
      IF (ERN(IIN) .LT. ZERO) THEN   
         NGOOD=NGOOD+1   
      ELSE   
         NBAD=NBAD+1     
      ENDIF  
  570 CONTINUE   
C                                           end of loop . . . . . . .    
  580 CONTINUE   
C                                        . . . . printout final values .     
      CFROM = 'MINOS   '     
      NFCNFR = NFCNMI    
      CSTATU= 'UNCHANGED '   
      IF (NGOOD.EQ.0.AND.NBAD.EQ.0) GO TO 700    
      IF (NGOOD.GT.0.AND.NBAD.EQ.0) CSTATU='SUCCESSFUL'  
      IF (NGOOD.EQ.0.AND.NBAD.GT.0) CSTATU='FAILURE   '  
      IF (NGOOD.GT.0.AND.NBAD.GT.0) CSTATU='PROBLEMS  '  
      IF (ISW(5) .GE. 0) CALL MNPRIN(4,AMIN)     
      IF (ISW(5) .GE. 2) CALL MNMATU(0)  
      GO TO 900  
C                                        . . . new minimum found . . . .     
  650 CONTINUE   
      CFROM = 'MINOS   '     
      NFCNFR = NFCNMI    
      CSTATU= 'NEW MINIMU'   
      IF (ISW(5) .GE. 0) CALL MNPRIN(4,AMIN)     
      WRITE (ISYSWR,675)     
  675 FORMAT(/50H NEW MINIMUM FOUND.  GO BACK TO MINIMIZATION STEP./1H ,     
     +60(1H=)/60X,1HV/60X,1HV/60X,1HV/57X,7HVVVVVVV/58X,5HVVVVV/59X,     
     +3HVVV/60X,1HV//)   
      GO TO 900  
  700 WRITE (ISYSWR,'(A)') ' THERE ARE NO MINOS ERRORS TO CALCULATE.'    
  900 RETURN     
      END    
