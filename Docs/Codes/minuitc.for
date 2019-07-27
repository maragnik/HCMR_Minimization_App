

      SUBROUTINE MNMNOT(ILAX,ILAX2,VAL2PL,VAL2MI)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Performs a MINOS error analysis on one parameter.  
CC        The parameter ILAX is varied, and the minimum of the   
CC        function with respect to the other parameters is followed  
CC        until it crosses the value FMIN+UP.    
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
     
      DIMENSION XDEV(MNI),W(MNI),GCC(MNI)    
      CHARACTER*4 CPOS,CNEG,CSIG     
      CHARACTER*1 CDOT,CSTAR,CBLANK  
      PARAMETER (CPOS='POSI',CNEG='NEGA',CDOT='.',CSTAR='*',CBLANK=' ')  
      LOGICAL  LOVFLO,  LRIGHT, LLEFT    
C                                        . . save and prepare start vals     
      ISW2 = ISW(2)  
      ISW4 = ISW(4)  
      SIGSAV = EDM   
      ISTRAV = ISTRAT    
      DC = DCOVAR    
      LOVFLO = .FALSE.   
      LNEWMN = .FALSE.   
      TOLER = EPSI*0.5   
      APSI  = EPSI*0.5   
      ABEST=AMIN     
      AIM = AMIN + UP    
      MPAR=NPAR  
      NFMXIN = NFCNMX    
      DO 125 I= 1, MPAR  
  125 XT(I) = X(I)   
      DO 130 J= 1, MPAR*(MPAR+1)/2   
  130 VTHMAT(J) = VHMAT(J)   
      DO 135 I= 1, MPAR  
      GCC(I) = GLOBCC(I)     
  135 W(I) = WERR(I)     
      IT = NIOFEX(ILAX)  
      ERP(IT) = 0.   
      ERN(IT) = 0.   
      CALL MNINEX(XT)    
      UT = U(ILAX)   
      IF (NVARL(ILAX) .EQ. 1) THEN   
         ALIM(ILAX) = UT -100.*W(IT)     
         BLIM(ILAX) = UT +100.*W(IT)     
         ENDIF   
      NDEX = IT*(IT+1)/2     
      XUNIT = DSQRT(UP/VTHMAT(NDEX))  
      MARC = 0   
      DO 162 I= 1, MPAR  
      IF (I .EQ. IT)  GO TO 162  
      MARC = MARC + 1    
         IMAX = MAX(IT,I)    
         INDX = IMAX*(IMAX-1)/2 + MIN(IT,I)  
      XDEV(MARC) = XUNIT*VTHMAT(INDX)    
  162 CONTINUE   
C                           fix the parameter in question    
      CALL MNFIXP (IT,IERR)  
      IF (IERR .GT. 0)  THEN     
         WRITE (ISYSWR,'(A,I5,A,I5)')    
     +    ' MINUIT ERROR. CANNOT FIX PARAMETER',ILAX,'    INTERNAL',IT   
         GO TO 700   
      ENDIF  
C                       . . . . . Nota Bene: from here on, NPAR=MPAR-1   
C      Remember: MNFIXP squeezes IT out of X, XT, WERR, and VHMAT,   
C                                                    not W, VTHMAT   
      DO 500 ISIG= 1,2   
      IF (ISIG .EQ. 1) THEN  
         SIG = 1.0   
         CSIG = CPOS     
      ELSE   
         SIG = -1.0  
         CSIG = CNEG     
      ENDIF  
C                                        . sig=sign of error being calcd     
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,806)  CSIG,ILAX,CPNAM(ILAX)   
  806 FORMAT (/' DETERMINATION OF ',A4,'TIVE MINOS ERROR FOR PARAMETER',     
     +    I3, 2X ,A)     
      IF (ISW(2).LE.0) CALL MNWARN('D','MINOS','NO COVARIANCE MATRIX.')  
      NLIMIT = NFCN + NFMXIN     
      ISTRAT = MAX(ISTRAV-1,0)   
      DU1 = W(IT)    
      U(ILAX) = UT + SIG*DU1     
         FAC = SIG*DU1/W(IT)     
         DO 185 I= 1, NPAR   
  185    X(I) = XT(I) + FAC*XDEV(I)  
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,801)  ILAX,UT,SIG*DU1,U(ILAX)     
  801 FORMAT (/' PARAMETER',I4,' SET TO',E11.3,' + ',E10.3,' = ',E12.3)  
C                                        loop to hit AIM     
      KE1CR = ILAX   
      KE2CR = 0  
      XMIDCR = UT + SIG*DU1  
      XDIRCR = SIG*DU1   
C    
      AMIN = ABEST   
      NFCNMX = NLIMIT - NFCN     
      CALL MNCROS(AOPT,IERCR)  
      IF (ABEST-AMIN .GT. 0.01*UP)  GO TO 650    
      IF (IERCR .EQ. 1)  GO TO 440   
      IF (IERCR .EQ. 2)  GO TO 450   
      IF (IERCR .EQ. 3)  GO TO 460   
C                                        . error successfully calculated     
      EROS = SIG*DU1 + AOPT*XDIRCR   
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,808)  CSIG,ILAX,CPNAM(ILAX),EROS  
  808 FORMAT (/9X,4HTHE ,A4,  29HTIVE MINOS ERROR OF PARAMETER,I3,   2H  
     +, ,A10,      4H, IS ,E12.4)    
      GO TO 480  
C                                        . . . . . . . . failure returns     
  440 IF (ISW(5) .GE. 1) WRITE(ISYSWR,807)  CSIG,ILAX,CPNAM(ILAX)    
  807 FORMAT (5X,'THE ',A4,'TIVE MINOS ERROR OF PARAMETER',I3,', ',A,    
     +', EXCEEDS ITS LIMIT.'/)   
      EROS = UNDEFI  
      GO TO 480  
  450 IF (ISW(5) .GE. 1) WRITE (ISYSWR, 802)  CSIG,ILAX,NFMXIN   
  802 FORMAT (9X,'THE ',A,'TIVE MINOS ERROR',I4,' REQUIRES MORE THAN',   
     +   I5,' FUNCTION CALLS.'/)     
      EROS = 0.  
      GO TO 480  
  460 IF (ISW(5) .GE. 1) WRITE (ISYSWR, 805) CSIG,ILAX   
  805 FORMAT (25X,A,'TIVE MINOS ERROR NOT CALCULATED FOR PARAMETER',I4/)     
      EROS = 0.  
C    
  480 IF (ISW(5) .GT. 1) WRITE (ISYSWR,'(5X, 74(1H*))')  
      IF (SIG .LT. ZERO)  THEN   
         ERN(IT) = EROS  
         IF (ILAX2.GT.0 .AND. ILAX2.LE.NU)  VAL2MI = U(ILAX2)    
      ELSE   
         ERP(IT) = EROS  
         IF (ILAX2.GT.0 .AND. ILAX2.LE.NU)  VAL2PL = U(ILAX2)    
      ENDIF  
  500 CONTINUE   
C                                        . . parameter finished. reset v     
C                       normal termination   
      ITAUR = 1  
      CALL MNFREE(1)     
      DO 550 J= 1, MPAR*(MPAR+1)/2   
  550 VHMAT(J) = VTHMAT(J)   
      DO 595 I= 1, MPAR  
      WERR(I) = W(I)     
      GLOBCC(I) = GCC(I)     
  595 X(I) = XT(I)   
      CALL MNINEX (X)    
      EDM = SIGSAV   
      AMIN = ABEST   
      ISW(2) = ISW2  
      ISW(4) = ISW4  
      DCOVAR = DC    
      GO TO 700  
C                       new minimum  
  650 LNEWMN = .TRUE.    
      ISW(2) = 0     
      DCOVAR = 1.    
      ISW(4) = 0     
      SAV = U(ILAX)  
      ITAUR = 1  
      CALL MNFREE(1)     
      U(ILAX) = SAV  
      CALL MNEXIN(X)     
      EDM = BIGEDM   
C                       in any case  
  700 CONTINUE   
      ITAUR = 0  
      NFCNMX = NFMXIN    
      ISTRAT = ISTRAV    
      RETURN     
      END    
      SUBROUTINE MNPARM(K,CNAMJ,UK,WK,A,B,IERFLG)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called from MNREAD and user-callable   
CC    Implements one parameter definition, that is:  
CC          K     (external) parameter number    
CC          CNAMK parameter name     
CC          UK    starting value     
CC          WK    starting step size or uncertainty  
CC          A, B  lower and upper physical parameter limits  
CC    and sets up (updates) the parameter lists.     
CC    Output: IERFLG=0 if no problems    
CC                  >0 if MNPARM unable to implement definition  
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
      CHARACTER*(*) CNAMJ    
      CHARACTER  CNAMK*10, CHBUFI*4  
C    
      CNAMK = CNAMJ  
      KINT = NPAR    
      IF (K.LT.1 .OR. K.GT.MAXEXT) THEN  
C                     parameter number exceeds allowed maximum value     
        WRITE (ISYSWR,9)  K,MAXEXT   
    9   FORMAT (/' MINUIT USER ERROR.  PARAMETER NUMBER IS',I11/     
     +         ',  ALLOWED RANGE IS ONE TO',I4/)     
        GO TO 800    
      ENDIF  
C                     normal parameter request   
      KTOFIX = 0     
      IF (NVARL(K) .LT. 0) GO TO 50  
C         previously defined parameter is being redefined    
C                                     find if parameter was fixed    
      DO 40 IX= 1, NPFIX     
      IF (IPFIX(IX) .EQ. K)  KTOFIX = K  
   40 CONTINUE   
      IF (KTOFIX .GT. 0)  THEN   
         CALL MNWARN('W','PARAM DEF','REDEFINING A FIXED PARAMETER.')    
         IF (KINT .GE. MAXINT)  THEN     
            WRITE (ISYSWR,'(A)') ' CANNOT RELEASE. MAX NPAR EXCEEDED.'   
            GO TO 800    
            ENDIF    
         CALL MNFREE(-K)     
         ENDIF   
C                       if redefining previously variable parameter  
      IF(NIOFEX(K) .GT. 0) KINT = NPAR-1     
   50 CONTINUE   
C    
C                                      . . .print heading    
      IF (LPHEAD .AND. ISW(5).GE.0) THEN     
        WRITE (ISYSWR,61)    
        LPHEAD = .FALSE.     
      ENDIF  
   61 FORMAT(/' PARAMETER DEFINITIONS:'/     
     +        '    NO.   NAME         VALUE      STEP SIZE      LIMITS')     
      IF (WK .GT. ZERO)  GO TO 122   
C                                        . . .constant parameter . . . .     
      IF (ISW(5) .GE. 0)  WRITE (ISYSWR, 82)  K,CNAMK,UK     
   82 FORMAT (1X,I5,1X,1H',A10,1H',1X,G13.5, '  constant')   
      NVL = 0    
      GO TO 200  
  122 IF (A.EQ.ZERO .AND. B.EQ.ZERO) THEN    
C                                      variable parameter without limits     
      NVL = 1    
      IF (ISW(5) .GE. 0)  WRITE (ISYSWR, 127)  K,CNAMK,UK,WK     
  127 FORMAT (1X,I5,1X,1H',A10,1H',1X,2G13.5, '     no limits')  
      ELSE   
C                                         variable parameter with limits     
      NVL = 4    
      LNOLIM = .FALSE.   
      IF (ISW(5) .GE. 0)  WRITE (ISYSWR, 132)  K,CNAMK,UK,WK,A,B     
  132 FORMAT(1X,I5,1X,1H',A10,1H',1X,2G13.5,2X,2G13.5)   
      ENDIF  
C                             . . request for another variable parameter     
      KINT = KINT + 1    
      IF (KINT .GT. MAXINT)  THEN    
         WRITE (ISYSWR,135)  MAXINT  
  135    FORMAT (/' MINUIT USER ERROR.   TOO MANY VARIABLE PARAMETERS.'/     
     +   ' THIS VERSION OF MINUIT DIMENSIONED FOR',I4//)     
         GO TO 800   
         ENDIF   
      IF (NVL .EQ. 1)  GO TO 200     
      IF (A .EQ. B)  THEN    
        WRITE (ISYSWR,'(/A,A/A/)') ' USER ERROR IN MINUIT PARAMETER',    
     +   ' DEFINITION',' UPPER AND LOWER LIMITS EQUAL.'  
        GO TO 800    
        ENDIF    
      IF (B .LT. A) THEN     
         SAV = B     
         B = A   
         A = SAV     
         CALL MNWARN('W','PARAM DEF','PARAMETER LIMITS WERE REVERSED.')  
         IF (LWARN) LPHEAD=.TRUE.    
         ENDIF   
      IF ((B-A) .GT. 1.0E7)  THEN    
         WRITE (CHBUFI,'(I4)') K     
         CALL MNWARN('W','PARAM DEF',    
     +               'LIMITS ON PARAM'//CHBUFI//' TOO FAR APART.')   
         IF (LWARN) LPHEAD=.TRUE.    
      ENDIF  
      DANGER = (B-UK)*(UK-A)     
      IF (DANGER .LT. 0.)    
     +     CALL MNWARN('W','PARAM DEF','STARTING VALUE OUTSIDE LIMITS.')     
      IF (DANGER .EQ. 0.)    
     +     CALL MNWARN('W','PARAM DEF','STARTING VALUE IS AT LIMIT.')    
  200 CONTINUE   
C                           . . . input OK, set values, arrange lists,   
C                                    calculate step sizes GSTEP, DIRIN   
      CFROM = 'PARAMETR'     
      NFCNFR = NFCN  
      CSTATU= 'NEW VALUES'   
      NU = MAX(NU,K)     
      CPNAM(K) = CNAMK   
      U(K) = UK  
      ALIM(K) = A    
      BLIM(K) = B    
      NVARL(K) = NVL     
      CALL MNRSET(1)     
C                             K is external number of new parameter  
C           LASTIN is the number of var. params with ext. param. no.< K  
      LASTIN = 0     
      DO 240 IX= 1, K-1  
      IF (NIOFEX(IX) .GT. 0)  LASTIN=LASTIN+1    
  240 CONTINUE   
C                 KINT is new number of variable params, NPAR is old     
      IF (KINT .EQ. NPAR)  GO TO 280     
      IF (KINT .GT. NPAR) THEN   
C                          insert new variable parameter in list     
         DO 260 IN= NPAR,LASTIN+1,-1     
         IX = NEXOFI(IN)     
         NIOFEX(IX) = IN+1   
         NEXOFI(IN+1)= IX    
         X    (IN+1) = X    (IN)     
         XT   (IN+1) = XT   (IN)     
         DIRIN(IN+1) = DIRIN(IN)     
         G2   (IN+1) = G2   (IN)     
         GSTEP(IN+1) = GSTEP(IN)     
  260    CONTINUE    
      ELSE   
C                          remove variable parameter from list   
         DO 270 IN= LASTIN+1,KINT    
         IX = NEXOFI(IN+1)   
         NIOFEX(IX) = IN     
         NEXOFI(IN)= IX  
         X     (IN)= X    (IN+1)     
         XT    (IN)= XT   (IN+1)     
         DIRIN (IN)= DIRIN(IN+1)     
         G2    (IN)= G2   (IN+1)     
         GSTEP (IN)= GSTEP(IN+1)     
  270    CONTINUE    
      ENDIF  
  280 CONTINUE   
      IX = K     
      NIOFEX(IX) = 0     
      NPAR = KINT    
C                                       lists are now arranged . . . .   
      IF (NVL .GT. 0)  THEN  
         IN = LASTIN+1   
         NEXOFI(IN) = IX     
         NIOFEX(IX) = IN     
         SAV = U(IX)     
         CALL MNPINT(SAV,IX,PINTI)   
         X(IN) = PINTI   
         XT(IN) = X(IN)  
         WERR(IN) = WK   
         SAV2 = SAV + WK     
         CALL MNPINT(SAV2,IX,PINTI)  
         VPLU = PINTI - X(IN)    
         SAV2 = SAV - WK     
         CALL MNPINT(SAV2,IX,PINTI)  
         VMINU = PINTI - X(IN)   
         DIRIN(IN) = 0.5 * (DABS(VPLU) +DABS(VMINU))   
         G2(IN) = 2.0*UP / DIRIN(IN)**2  
         GSMIN = 8.*EPSMA2*DABS(X(IN))    
         GSTEP(IN) = MAX (GSMIN, 0.1*DIRIN(IN))  
         IF (AMIN .NE. UNDEFI) THEN  
             SMALL = DSQRT(EPSMA2*(AMIN+UP)/UP)   
             GSTEP(IN) = DMAX1(GSMIN, SMALL*DIRIN(IN))     
         ENDIF   
         GRD  (IN) = G2(IN)*DIRIN(IN)    
C                   if parameter has limits  
         IF (NVARL(K) .GT. 1) THEN   
            IF (GSTEP(IN).GT. 0.5)  GSTEP(IN)=0.5    
            GSTEP(IN) = -GSTEP(IN)   
         ENDIF   
      ENDIF  
      IF (KTOFIX .GT. 0)  THEN   
         KINFIX = NIOFEX(KTOFIX)     
         IF (KINFIX .GT. 0)  CALL MNFIXP(KINFIX,IERR)    
         IF (IERR .GT. 0)  GO TO 800     
      ENDIF  
      IERFLG = 0     
      RETURN     
C                   error on input, unable to implement request  . . . .     
  800 CONTINUE   
      IERFLG = 1     
      RETURN     
      END    
      SUBROUTINE MNPFIT(PARX2P,PARY2P,NPAR2P,COEF2P,SDEV2P)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
C    
C     to fit a parabola to npar2p points     
C    
C   npar2p   no. of points   
C   parx2p(i)   x value of point i   
C   pary2p(i)   y value of point i   
C    
C   coef2p(1...3)  coefficients of the fitted parabola   
C   y=coef2p(1) + coef2p(2)*x + coef2p(3)*x**2   
C   sdev2p= variance     
C   method : chi**2 = min equation solved explicitly     
      DIMENSION PARX2P(NPAR2P),PARY2P(NPAR2P),COEF2P(NPAR2P)     
      DIMENSION CZ(3)    
C    
      DO 3  I=1,3    
    3 CZ(I)=0.   
      SDEV2P=0.  
      IF(NPAR2P.LT.3) GO TO 10   
      F=NPAR2P   
C--- center x values for reasons of machine precision    
      XM=0.  
      DO 2  I=1,NPAR2P   
    2 XM=XM+PARX2P(I)    
      XM=XM/F    
      X2=0.  
      X3=0.  
      X4=0.  
      Y=0.   
      Y2=0.  
      XY=0.  
      X2Y=0.     
      DO 1  I=1,NPAR2P   
      S=PARX2P(I)-XM     
      T=PARY2P(I)    
      S2=S*S     
      X2=X2+S2   
      X3=X3+S*S2     
      X4=X4+S2*S2    
      Y=Y+T  
      Y2=Y2+T*T  
      XY=XY+S*T  
      X2Y=X2Y+S2*T   
    1 CONTINUE   
      A=(F*X4-X2**2)*X2-F*X3**2  
      IF(A.EQ.0.)  GOTO 10   
      CZ(3)=(X2*(F*X2Y-X2*Y)-F*X3*XY)/A  
      CZ(2)=(XY-X3*CZ(3))/X2     
      CZ(1)=(Y-X2*CZ(3))/F   
      IF(NPAR2P.EQ.3)  GOTO 6    
      SDEV2P=Y2-(CZ(1)*Y+CZ(2)*XY+CZ(3)*X2Y)     
      IF(SDEV2P.LT.0.)  SDEV2P=0.    
      SDEV2P=SDEV2P/(F-3.)   
    6 CZ(1)=CZ(1)+XM*(XM*CZ(3)-CZ(2))    
      CZ(2)=CZ(2)-2.*XM*CZ(3)    
   10 CONTINUE   
      DO 11  I=1,3   
   11 COEF2P(I)=CZ(I)    
      RETURN     
      END    
      SUBROUTINE MNPINT(PEXTI,I,PINTI)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Calculates the internal parameter value PINTI corresponding    
CC        to the external value PEXTI for parameter I.   
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
      LOGICAL LIMLOC     
      CHARACTER CHBUFI*4, CHBUF2*30  
      LIMLOC = .FALSE.   
      PINTI = PEXTI  
      IGO = NVARL(I)     
      IF (IGO .EQ. 4)  THEN  
C--                          there are two limits    
        ALIMI = ALIM(I)  
        BLIMI = BLIM(I)  
        YY=2.0*(PEXTI-ALIMI)/(BLIMI-ALIMI) - 1.0     
        YY2 = YY**2  
        IF (YY2 .GE. (1.0- EPSMA2))  THEN    
           IF (YY .LT. 0.) THEN  
               A = VLIMLO    
               CHBUF2 = ' IS AT ITS LOWER ALLOWED LIMIT.'    
           ELSE  
               A = VLIMHI    
               CHBUF2 = ' IS AT ITS UPPER ALLOWED LIMIT.'    
           ENDIF     
           PINTI = A     
           PEXTI = ALIMI + 0.5* (BLIMI-ALIMI) *(DSIN(A) +1.0)     
           LIMSET = .TRUE.   
           WRITE (CHBUFI,'(I4)') I   
           IF (YY2 .GT. 1.0) CHBUF2 = ' BROUGHT BACK INSIDE LIMITS.'     
           CALL MNWARN('W',CFROM,'VARIABLE'//CHBUFI//CHBUF2)     
         ELSE    
           PINTI = DASIN(YY)  
         ENDIF   
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNPLOT(XPT,YPT,CHPT,NXYPT,NUNIT,NPAGWD,NPAGLN)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        plots points in array xypt onto one page with labelled axes    
CC        NXYPT is the number of points to be plotted    
CC        XPT(I) = x-coord. of ith point     
CC        YPT(I) = y-coord. of ith point     
CC        CHPT(I) = character to be plotted at this position     
CC        the input point arrays XPT, YPT, CHPT are destroyed.   
CC   
      DIMENSION   XPT(*), YPT(*), SAV(2)     
      CHARACTER*1 CHPT(*) ,  CHSAV,  CHBEST, CDOT, CSLASH, CBLANK    
      PARAMETER (MAXWID=100)     
      CHARACTER CLINE*100, CHMESS*30     
      DIMENSION XVALUS(12)   
      LOGICAL OVERPR     
      DATA CDOT,CSLASH,CBLANK/ '.' , '/' , ' '/  
      MAXNX = MIN(NPAGWD-20,MAXWID)  
      IF (MAXNX .LT. 10)  MAXNX = 10     
      MAXNY = NPAGLN     
      IF (MAXNY .LT. 10)  MAXNY = 10     
      IF (NXYPT .LE. 1)  RETURN  
      XBEST = XPT(1)     
      YBEST = YPT(1)     
      CHBEST = CHPT(1)   
C         order the points by decreasing y   
      KM1 = NXYPT - 1    
      DO 150 I= 1, KM1   
      IQUIT = 0  
      NI = NXYPT - I     
      DO 140 J= 1, NI    
      IF (YPT(J) .GT. YPT(J+1)) GO TO 140    
        SAVX = XPT(J)    
        XPT(J) = XPT(J+1)    
        XPT(J+1) = SAVX  
        SAVY = YPT(J)    
        YPT(J) = YPT(J+1)    
        YPT(J+1) = SAVY  
        CHSAV = CHPT(J)  
        CHPT(J) = CHPT(J+1)  
        CHPT(J+1) = CHSAV    
      IQUIT = 1  
  140 CONTINUE   
      IF (IQUIT .EQ. 0) GO TO 160    
  150 CONTINUE   
  160 CONTINUE   
C         find extreme values    
      XMAX = XPT(1)  
      XMIN = XMAX    
      DO 200 I= 1, NXYPT     
        IF (XPT(I) .GT. XMAX)  XMAX = XPT(I)     
        IF (XPT(I) .LT. XMIN)  XMIN = XPT(I)     
  200 CONTINUE   
      DXX = 0.001*(XMAX-XMIN)    
      XMAX = XMAX + DXX  
      XMIN = XMIN - DXX  
      CALL MNBINS(XMIN,XMAX,MAXNX,XMIN,XMAX,NX,BWIDX)    
      YMAX = YPT(1)  
      YMIN = YPT(NXYPT)  
      IF (YMAX .EQ. YMIN)  YMAX=YMIN+1.0     
      DYY = 0.001*(YMAX-YMIN)    
      YMAX = YMAX + DYY  
      YMIN = YMIN - DYY  
      CALL MNBINS(YMIN,YMAX,MAXNY,YMIN,YMAX,NY,BWIDY)    
      ANY = NY   
C         if first point is blank, it is an 'origin'     
      IF (CHBEST .EQ. CBLANK)  GO TO 50  
      XBEST = 0.5 * (XMAX+XMIN)  
      YBEST = 0.5 * (YMAX+YMIN)  
   50 CONTINUE   
C         find scale constants   
      AX = 1.0/BWIDX     
      AY = 1.0/BWIDY     
      BX = -AX*XMIN + 2.0    
      BY = -AY*YMIN - 2.0    
C         convert points to grid positions   
      DO 300 I= 1, NXYPT     
      XPT(I) = AX*XPT(I) + BX    
  300 YPT(I) = ANY-AY*YPT(I) - BY    
      NXBEST = AX*XBEST + BX     
      NYBEST = ANY  - AY*YBEST - BY  
C         print the points   
      NY = NY + 2    
      NX = NX + 2    
      ISP1 = 1   
      LINODD = 1     
      OVERPR=.FALSE.     
      DO 400 I= 1, NY    
      DO 310 IBK= 1, NX  
  310 CLINE (IBK:IBK) = CBLANK   
      CLINE(1:1) = CDOT  
      CLINE(NX:NX) = CDOT    
      CLINE(NXBEST:NXBEST) = CDOT    
      IF (I.NE.1 .AND. I.NE.NYBEST .AND. I.NE.NY)  GO TO 320     
      DO 315 J= 1, NX    
  315 CLINE(J:J) = CDOT  
  320 CONTINUE   
      YPRT = YMAX - FLOAT(I-1)*BWIDY     
      IF (ISP1 .GT. NXYPT)  GO TO 350    
C         find the points to be plotted on this line     
        DO 341 K= ISP1,NXYPT     
      KS = YPT(K)    
      IF (KS .GT. I)  GO TO 345  
      IX = XPT(K)    
      IF (CLINE(IX:IX) .EQ.   CDOT)  GO TO 340   
      IF (CLINE(IX:IX) .EQ. CBLANK)  GO TO 340   
      IF (CLINE(IX:IX) .EQ.CHPT(K))  GO TO 341   
      OVERPR = .TRUE.    
C         OVERPR is true if one or more positions contains more than     
C            one point   
      CLINE(IX:IX) = '&'     
      GO TO 341  
  340 CLINE(IX:IX) = CHPT(K)     
  341 CONTINUE   
        ISP1 = NXYPT + 1     
        GO TO 350    
  345   ISP1 = K     
  350 CONTINUE   
      IF (LINODD .EQ. 1 .OR. I .EQ. NY)  GO TO 380   
      LINODD = 1     
      WRITE (NUNIT, '(18X,A)')       CLINE(:NX)  
      GO TO 400  
  380 WRITE (NUNIT,'(1X,G14.7,A,A)') YPRT, ' ..', CLINE(:NX)     
      LINODD = 0     
  400 CONTINUE   
C         print labels on x-axis every ten columns   
      DO 410 IBK= 1, NX  
      CLINE(IBK:IBK) = CBLANK    
      IF (MOD(IBK,10) .EQ. 1)  CLINE(IBK:IBK) = CSLASH   
  410 CONTINUE   
      WRITE (NUNIT, '(18X,A)')       CLINE(:NX)  
C    
      DO 430 IBK= 1, 12  
  430 XVALUS(IBK) = XMIN + FLOAT(IBK-1)*10.*BWIDX    
      ITEN = (NX+9) / 10     
      WRITE (NUNIT,'(12X,12G10.4)')  (XVALUS(IBK), IBK=1,ITEN)   
      CHMESS = ' '   
      IF (OVERPR) CHMESS='   Overprint character is &'   
      WRITE (NUNIT,'(25X,A,G13.7,A)') 'ONE COLUMN=',BWIDX, CHMESS    
  500 RETURN     
      END    
      SUBROUTINE MNPOUT(IUEXT,CHNAM,VAL,ERR,XLOLIM,XUPLIM,IUINT)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC     User-called   
CC   Provides the user with information concerning the current status    
CC          of parameter number IUEXT. Namely, it returns:   
CC        CHNAM: the name of the parameter   
CC        VAL: the current (external) value of the parameter     
CC        ERR: the current estimate of the parameter uncertainty     
CC        XLOLIM: the lower bound (or zero if no limits)     
CC        XUPLIM: the upper bound (or zero if no limits)     
CC        IUINT: the internal parameter number (or zero if not variable,     
CC           or negative if undefined).  
CC  Note also:  If IUEXT is negative, then it is -internal parameter     
CC           number, and IUINT is returned as the EXTERNAL number.   
CC     Except for IUINT, this is exactly the inverse of MNPARM   
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
      CHARACTER*(*) CHNAM    
      XLOLIM = 0.    
      XUPLIM = 0.    
      ERR = 0.   
      IF (IUEXT .EQ. 0)  GO TO 100   
      IF (IUEXT .LT. 0)  THEN    
C                   internal parameter number specified  
         IINT = -IUEXT   
         IF (IINT .GT. NPAR) GO TO 100   
         IEXT = NEXOFI(IINT)     
         IUINT = IEXT    
      ELSE   
C                    external parameter number specified     
         IEXT = IUEXT    
         IF (IEXT .EQ. 0)   GO TO 100    
         IF (IEXT .GT. NU)  GO TO 100    
         IINT = NIOFEX(IEXT)     
         IUINT = IINT    
      ENDIF  
C                     in both cases  
         NVL = NVARL(IEXT)   
         IF (NVL .LT. 0) GO TO 100   
      CHNAM = CPNAM(IEXT)    
      VAL = U(IEXT)  
      IF (IINT .GT. 0)  ERR = WERR(IINT)     
      IF (NVL .EQ. 4) THEN   
         XLOLIM = ALIM(IEXT)     
         XUPLIM = BLIM(IEXT)     
      ENDIF  
      RETURN     
C                parameter is undefined  
  100 IUINT = -1     
      CHNAM = 'undefined'    
      VAL = 0.   
      RETURN     
      END    
      SUBROUTINE MNPRIN  (INKODE,FVAL)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Prints the values of the parameters at the time of the call.   
CC        also prints other relevant information such as function value,     
CC        estimated distance to minimum, parameter errors, step sizes.   
CC   
C         According to the value of IKODE, the printout is:  
C    IKODE=INKODE= 0    only info about function value   
C                  1    parameter values, errors, limits     
C                  2    values, errors, step sizes, internal values  
C                  3    values, errors, step sizes, first derivs.    
C                  4    values, parabolic errors, MINOS errors   
C    when INKODE=5, MNPRIN chooses IKODE=1,2, or 3, according to ISW(2)  
C    
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
      CHARACTER*14 COLHDU(6),COLHDL(6), CX2,CX3,CGETX    
      CHARACTER*11 CNAMBF, CBLANK    
      CHARACTER  CHEDM*10, CHEVAL*15     
      PARAMETER (CGETX='PLEASE GET X..')     
      DATA CBLANK/'          '/  
C    
      IF (NU .EQ. 0)  THEN   
       WRITE (ISYSWR,'(A)') ' THERE ARE CURRENTLY NO PARAMETERS DEFINED'     
       GO TO 700     
      ENDIF  
C                  get value of IKODE based in INKODE, ISW(2)    
      IKODE = INKODE     
      IF (INKODE .EQ. 5) THEN    
         IKODE = ISW(2)+1    
         IF (IKODE .GT. 3)  IKODE=3  
      ENDIF  
C                  set 'default' column headings     
      DO 5 K= 1, 6   
      COLHDU(K) = 'UNDEFINED'    
    5 COLHDL(K) = 'COLUMN HEAD'  
C              print title if Minos errors, and title exists.    
      IF (IKODE.EQ.4 .AND. CTITL.NE.CUNDEF)  
     +            WRITE (ISYSWR,'(/A,A)')  ' MINUIT TASK: ',CTITL    
C              report function value and status  
      IF (FVAL .EQ. UNDEFI) THEN     
         CHEVAL = ' unknown       '  
      ELSE   
         WRITE (CHEVAL,'(G15.7)') FVAL   
      ENDIF  
         IF (EDM .EQ. BIGEDM) THEN   
            CHEDM = ' unknown  '     
         ELSE    
            WRITE (CHEDM, '(E10.2)') EDM     
         ENDIF   
      NC = NFCN-NFCNFR   
      WRITE (ISYSWR,905)  CHEVAL,CFROM,CSTATU,NC,NFCN    
  905 FORMAT (/' FCN=',A,' FROM ',A8,'  STATUS=',A10,I6,' CALLS',    
     +         I9,' TOTAL')  
      M = ISW(2)     
      IF (M.EQ.0 .OR. M.EQ.2 .OR. DCOVAR.EQ.ZERO) THEN   
        WRITE (ISYSWR,907) CHEDM,ISTRAT,COVMES(M)    
  907   FORMAT (21X,'EDM=',A,'    STRATEGY=',I2,6X,A)    
      ELSE   
        DCMAX = 1.   
        DC = DMIN1(DCOVAR,DCMAX) * 100.    
        WRITE (ISYSWR,908) CHEDM,ISTRAT,DC   
  908   FORMAT (21X,'EDM=',A,'  STRATEGY=',I1,'  ERROR MATRIX',  
     +     ' UNCERTAINTY=',F5.1,'%')     
      ENDIF  
C    
      IF (IKODE .EQ. 0)  GO TO 700   
C               find longest name (for Rene!)    
      NTRAIL = 10    
      DO 20 I= 1, NU     
         IF (NVARL(I) .LT. 0)  GO TO 20  
         DO 15 IC= 10,1,-1   
            IF (CPNAM(I)(IC:IC) .NE. ' ') GO TO 16   
   15    CONTINUE    
         IC = 1  
   16    LBL = 10-IC     
         IF (LBL .LT. NTRAIL)  NTRAIL=LBL    
   20 CONTINUE   
      NADD = NTRAIL/2 + 1    
      IF (IKODE .EQ. 1)  THEN    
         COLHDU(1) = '              '    
         COLHDL(1) = '      ERROR   '    
         COLHDU(2) = '      PHYSICAL'    
         COLHDU(3) = ' LIMITS       '    
         COLHDL(2) = '    NEGATIVE  '    
         COLHDL(3) = '    POSITIVE  '    
      ENDIF  
      IF (IKODE .EQ. 2)  THEN    
         COLHDU(1) = '              '    
         COLHDL(1) = '      ERROR   '    
         COLHDU(2) = '    INTERNAL  '    
         COLHDL(2) = '    STEP SIZE '    
         COLHDU(3) = '    INTERNAL  '    
         COLHDL(3) = '      VALUE   '    
      ENDIF  
      IF (IKODE .EQ. 3)  THEN    
         COLHDU(1) = '              '    
         COLHDL(1) = '      ERROR   '    
         COLHDU(2) = '       STEP   '    
         COLHDL(2) = '       SIZE   '    
         COLHDU(3) = '      FIRST   '    
         COLHDL(3) = '   DERIVATIVE '    
      ENDIF  
      IF (IKODE .EQ. 4)  THEN    
         COLHDU(1) = '    PARABOLIC '    
         COLHDL(1) = '      ERROR   '    
         COLHDU(2) = '        MINOS '    
         COLHDU(3) = 'ERRORS        '    
         COLHDL(2) = '   NEGATIVE   '    
         COLHDL(3) = '   POSITIVE   '    
      ENDIF  
C    
      IF (IKODE .NE. 4)  THEN    
         IF (ISW(2) .LT. 3) COLHDU(1)='  APPROXIMATE '   
         IF (ISW(2) .LT. 1) COLHDU(1)=' CURRENT GUESS'   
      ENDIF  
      NCOL = 3   
      WRITE (ISYSWR, 910) (COLHDU(KK),KK=1,NCOL)     
      WRITE (ISYSWR, 911) (COLHDL(KK),KK=1,NCOL)     
  910 FORMAT (/'  EXT PARAMETER ',     13X       ,6A14)  
  911 FORMAT ( '  NO.   NAME    ','    VALUE    ',6A14)  
C    
C                                        . . . loop over parameters . .  
      DO 200 I= 1, NU    
      IF (NVARL(I) .LT. 0)  GO TO 200    
      L = NIOFEX(I)  
      CNAMBF = CBLANK(1:NADD)//CPNAM(I)  
      IF (L .EQ. 0)  GO TO 55    
C              variable parameter.   
      X1 = WERR(L)   
      CX2 = CGETX    
      CX3 = CGETX    
      IF (IKODE .EQ. 1) THEN     
         IF (NVARL(I) .LE. 1) THEN   
            WRITE (ISYSWR, 952)  I,CNAMBF,U(I),X1    
            GO TO 200    
         ELSE    
         X2 = ALIM(I)    
         X3 = BLIM(I)    
         ENDIF   
      ENDIF  
      IF (IKODE .EQ. 2) THEN     
         X2 = DIRIN(L)   
         X3 = X(L)   
      ENDIF  
      IF (IKODE .EQ. 3) THEN     
         X2 = DIRIN(L)   
         X3 = GRD(L)     
         IF (NVARL(I).GT.1 .AND. DABS(DCOS(X(L))) .LT. 0.001)  
     +      CX3 = '** at limit **'   
      ENDIF  
      IF (IKODE .EQ. 4) THEN     
         X2 = ERN(L)     
           IF (X2.EQ.ZERO)   CX2=' '     
           IF (X2.EQ.UNDEFI) CX2='   at limit   '    
         X3 = ERP(L)     
           IF (X3.EQ.ZERO)   CX3=' '     
           IF (X3.EQ.UNDEFI) CX3='   at limit   '    
      ENDIF  
      IF (CX2.EQ.CGETX) WRITE (CX2,'(G14.5)') X2     
      IF (CX3.EQ.CGETX) WRITE (CX3,'(G14.5)') X3     
      WRITE (ISYSWR,952)   I,CNAMBF,U(I),X1,CX2,CX3  
  952 FORMAT (I4,1X,A11,2G14.5,2A)   
C               check if parameter is at limit   
      IF (NVARL(I) .LE. 1 .OR. IKODE .EQ. 3)  GO TO 200  
      IF (DABS(DCOS(X(L))) .LT. 0.001)  WRITE (ISYSWR,1004)    
 1004 FORMAT (1H ,32X,42HWARNING -   - ABOVE PARAMETER IS AT LIMIT.)     
      GO TO 200  
C    
C                                print constant or fixed parameter.  
   55 CONTINUE   
                          COLHDU(1) = '   constant   '   
      IF (NVARL(I).GT.0)  COLHDU(1) = '     fixed    '   
      IF (NVARL(I).EQ.4 .AND. IKODE.EQ.1) THEN   
        WRITE (ISYSWR,'(I4,1X,A11,G14.5,A,2G14.5)')  
     +     I,CNAMBF,U(I),COLHDU(1),ALIM(I),BLIM(I)   
      ELSE   
        WRITE (ISYSWR,'(I4,1X,A11,G14.5,A)')  I,CNAMBF,U(I),COLHDU(1)    
      ENDIF  
  200 CONTINUE   
C    
      IF (UP.NE.UPDFLT)  WRITE (ISYSWR,'(31X,A,G10.2)') 'ERR DEF=',UP    
  700 CONTINUE   
      RETURN     
      END    
      SUBROUTINE MNPSDF  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        calculates the eigenvalues of v to see if positive-def.    
CC        if not, adds constant along diagonal to make positive.     
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
      CHARACTER CHBUFF*12    
      DIMENSION S(MNI)   
      EPSMIN = 1.0E-6    
      EPSPDF = DMAX1(EPSMIN, EPSMA2)   
      DGMIN = VHMAT(1)   
C                        Check if negative or zero on diagonal   
      DO 200 I= 1, NPAR  
      NDEX = I*(I+1)/2   
      IF (VHMAT(NDEX) .LE. ZERO) THEN    
          WRITE (CHBUFF(1:3),'(I3)') I   
          CALL MNWARN('W',CFROM,     
     +'Negative diagonal element'//CHBUFF(1:3)//' in Error Matrix')  
      ENDIF  
      IF (VHMAT(NDEX) .LT. DGMIN)  DGMIN = VHMAT(NDEX)   
  200 CONTINUE   
      IF (DGMIN .LE. 0.) THEN    
         DG = 1.0 - DGMIN    
         WRITE (CHBUFF,'(E12.2)') DG     
         CALL MNWARN('W',CFROM,  
     +     CHBUFF//' added to diagonal of error matrix')     
      ELSE   
         DG = 0.     
      ENDIF  
C                    Store VHMAT in P, make sure diagonal pos.   
      DO 213 I= 1, NPAR  
      NDEX = I*(I-1)/2   
      NDEXD = NDEX + I   
      VHMAT(NDEXD) = VHMAT(NDEXD) + DG   
      S(I) = 1.0/DSQRT(VHMAT(NDEXD))  
      DO 213 J= 1, I     
      NDEX =  NDEX + 1   
  213 P(I,J) = VHMAT(NDEX) * S(I)*S(J)   
C      call eigen (p,p,maxint,npar,pstar,-npar)  
      CALL MNEIG(P,MAXINT,NPAR,MAXINT,PSTAR,EPSPDF,IFAULT)   
      PMIN = PSTAR(1)    
      PMAX = PSTAR(1)    
      DO 215 IP= 2, NPAR     
      IF (PSTAR(IP) .LT. PMIN)  PMIN = PSTAR(IP)     
      IF (PSTAR(IP) .GT. PMAX)  PMAX = PSTAR(IP)     
  215 CONTINUE   
      PMAX = DMAX1(DABS(PMAX), ONE)     
      IF ((PMIN .LE. ZERO .AND. LWARN) .OR.  ISW(5) .GE. 2) THEN     
         WRITE (ISYSWR,550)  
         WRITE (ISYSWR,551) (PSTAR(IP),IP=1,NPAR)    
      ENDIF  
      IF (PMIN .GT. EPSPDF*PMAX)  GO TO 217  
      IF (ISW(2) .EQ. 3)  ISW(2)=2   
      PADD = 1.0E-3*PMAX - PMIN  
      DO 216 IP= 1, NPAR     
      NDEX = IP*(IP+1)/2     
  216 VHMAT(NDEX) = VHMAT(NDEX) *(1.0 + PADD)    
      CSTATU= 'NOT POSDEF'   
      WRITE (CHBUFF,'(G12.5)') PADD  
      CALL MNWARN('W',CFROM,     
     +   'MATRIX FORCED POS-DEF BY ADDING '//CHBUFF//' TO DIAGONAL.')    
  217 CONTINUE   
C    
  550 FORMAT (' EIGENVALUES OF SECOND-DERIVATIVE MATRIX:' )  
  551 FORMAT (7X,6E12.4)     
      RETURN     
      END    
      SUBROUTINE MNRAZZ(YNEW,PNEW,Y,JH,JL)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called only by MNSIMP (and MNIMPR) to add a new point  
CC        and remove an old one from the current simplex, and get the    
CC        estimated distance to minimum.     
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
      DIMENSION PNEW(*), Y(*)    
      DO 10 I=1,NPAR     
   10 P(I,JH) = PNEW(I)  
      Y(JH)=YNEW     
      IF(YNEW .LT. AMIN) THEN    
        DO 15 I=1,NPAR   
   15   X(I) = PNEW(I)   
        CALL MNINEX(X)   
        AMIN = YNEW  
        CSTATU = 'PROGRESS  '    
        JL=JH    
      ENDIF  
      JH = 1     
      NPARP1 = NPAR+1    
   20 DO 25 J=2,NPARP1   
      IF (Y(J) .GT. Y(JH))  JH = J   
   25 CONTINUE   
      EDM = Y(JH) - Y(JL)    
      IF (EDM .LE. ZERO)  GO TO 45   
      US = 1.0/EDM   
      DO 35 I= 1, NPAR   
      PBIG = P(I,1)  
      PLIT = PBIG    
      DO 30 J= 2, NPARP1     
      IF (P(I,J) .GT. PBIG)  PBIG = P(I,J)   
      IF (P(I,J) .LT. PLIT)  PLIT = P(I,J)   
   30 CONTINUE   
      DIRIN(I) = PBIG - PLIT     
   35 CONTINUE   
   40 RETURN     
   45 WRITE (ISYSWR, 1000)  NPAR     
      GO TO 40   
 1000 FORMAT ('   FUNCTION VALUE DOES NOT SEEM TO DEPEND ON ANY OF THE',     
     +    I3,' VARIABLE PARAMETERS.' /10X,'VERIFY THAT STEP SIZES ARE',  
     +    ' BIG ENOUGH AND CHECK FCN LOGIC.'/1X,79(1H*)/1X,79(1H*)/)     
      END    
      SUBROUTINE MNREAD(IFLGIN,IFLGUT)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called from MINUIT.  Reads all user input to MINUIT.   
CC     This routine is highly unstructured and defies normal logic.  
CC   
CC     IFLGIN indicates the function originally requested:   
CC           = 1: read one-line title    
CC             2: read parameter definitions     
CC             3: read MINUIT commands   
CC   
CC     IFLGUT= 1: reading terminated normally    
CC             2: end-of-data on input   
CC             3: unrecoverable read error   
CC             4: unable to process parameter requests   
CC internally,   
CC     IFLGDO indicates the subfunction to be performed on the next  
CC         input record: 1: read a one-line title    
CC                       2: read a parameter definition  
CC                       3: read a command   
CC                       4: read in covariance matrix    
CC     for example, when IFLGIN=3, but IFLGDO=1, then it should read     
CC       a title, but this was requested by a command, not by MINUIT.    
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
     
      DIMENSION PLIST(MAXP)  
      CHARACTER CNAMK*10, CRDBUF*80, CELMNT*20   
      CHARACTER COMAND*(MAXCWD)  
      CHARACTER CPROMT(3)*40, CLOWER*26, CUPPER*26   
      LOGICAL LEOF   
      DATA CPROMT/' ENTER MINUIT TITLE, or "SET INPUT n" : ',    
     +            ' ENTER MINUIT PARAMETER DEFINITION:     ',    
     +            ' ENTER MINUIT COMMAND:                  '/    
C    
      DATA CLOWER/'abcdefghijklmnopqrstuvwxyz'/  
      DATA CUPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/  
C    
      IFLGUT = 1     
      IFLGDO = IFLGIN    
      IFATAL = 0     
      LEOF = .FALSE.     
C                                           . . . . read next record     
   10 CONTINUE   
      IF (ISW(6) .EQ. 1) WRITE (ISYSWR,'(A)') CPROMT(IFLGDO)     
      CRDBUF = '   '     
      READ (ISYSRD,'(A)',ERR=500,END=45)  CRDBUF     
C                                           . .   preemptive commands    
      LEOF = .FALSE.     
      IF (INDEX(CRDBUF,'*EOF') .EQ. 1 .OR.   
     +    INDEX(CRDBUF,'*eof') .EQ. 1)    THEN   
         WRITE (ISYSWR,'(A,I3)') ' *EOF ENCOUNTERED ON UNIT NO.',ISYSRD  
         LPHEAD = .TRUE.     
         GO TO 50    
         ENDIF   
      IF (INDEX(CRDBUF,'SET INP') .EQ. 1 .OR.    
     +    INDEX(CRDBUF,'set inp') .EQ. 1)    THEN    
         ICOMND = ICOMND + 1     
         WRITE (ISYSWR, 21) ICOMND,CRDBUF(1:50)  
   21    FORMAT (' **********'/' **',I5,' **',A/' **********')   
         LPHEAD = .TRUE.     
         GO TO 50    
         ENDIF   
      GO TO 80   
C                                    . . hardware EOF on current ISYSRD  
   45 CRDBUF = '*EOF '   
      WRITE (ISYSWR,'(A,I3)') ' END OF DATA ON UNIT NO.',ISYSRD  
C                                     or SET INPUT command   
   50 CONTINUE   
         CALL MNSTIN(CRDBUF,IERR)    
         IF (IERR .EQ. 0)  GO TO 10  
         IF (IERR .EQ. 2)  THEN  
            IF (.NOT. LEOF) THEN     
               WRITE (ISYSWR,'(A,A/)') ' TWO CONSECUTIVE EOFs ON ',  
     +              'PRIMARY INPUT FILE WILL TERMINATE EXECUTION.'   
               LEOF = .TRUE.     
               GO TO 10  
            ENDIF    
         ENDIF   
         IFLGUT = IERR   
         GO TO 900   
   80 IF (IFLGDO .GT. 1) GO TO 100   
C                            read title        . . . . .   IFLGDO = 1    
C              if title is 'SET TITLE', skip and read again  
      IF (INDEX(CRDBUF,'SET TIT') .EQ. 1)  GO TO 10  
      IF (INDEX(CRDBUF,'set tit') .EQ. 1)  GO TO 10  
      CTITL = CRDBUF(1:50)   
      WRITE (ISYSWR,'(1X,A50)')  CTITL   
      WRITE (ISYSWR,'(1X,78(1H*))')  
         LPHEAD = .TRUE.     
      IF (IFLGIN .EQ. IFLGDO)  GO TO 900     
      IFLGDO = IFLGIN    
      GO TO 10   
C                            data record is not a title. get upper case  
  100 CONTINUE   
      DO 110 I= 1, MAXCWD    
      IF (CRDBUF(I:I) .EQ. '''') GO TO 111   
         DO 108 IC= 1, 26    
         IF (CRDBUF(I:I) .EQ. CLOWER(IC:IC)) CRDBUF(I:I)=CUPPER(IC:IC)   
  108    CONTINUE    
     
     
  110 CONTINUE   
  111 CONTINUE   
C                            read parameter definitions.   IFLGDO = 2    
      IF (IFLGDO .GT. 2)  GO TO 300  
C              if parameter def is 'PARAMETER', skip and read again  
      IF (INDEX(CRDBUF,'PAR') .EQ. 1)  GO TO 10  
C              if line starts with SET TITLE, read a title first     
      IF (INDEX(CRDBUF,'SET TIT') .EQ. 1)  THEN  
         IFLGDO = 1  
         GO TO 10    
         ENDIF   
C              find out whether fixed or free-field format   
      KAPO1 = INDEX(CRDBUF,'''')     
      IF (KAPO1 .EQ. 0)  GO TO 150   
      KAPO2 = INDEX(CRDBUF(KAPO1+1:),'''')   
      IF (KAPO2 .EQ. 0)  GO TO 150   
C          new (free-field) format   
      KAPO2 = KAPO2 + KAPO1  
C                             skip leading blanks if any     
         DO 115 ISTART=1, KAPO1-1    
         IF (CRDBUF(ISTART:ISTART) .NE. ' ')  GO TO 120  
  115    CONTINUE    
         ISTART = KAPO1-1    
  120 CONTINUE   
C                               parameter number integer     
      IF (ISTART .LT. 1)  GO TO 210  
      CELMNT = CRDBUF(ISTART:KAPO1-1)    
      READ (CELMNT,'(BN,F20.0)',ERR=180) FK  
      K = FK     
      IF (K .EQ. 0)  GO TO 210   
      CNAMK = 'PARAM '//CELMNT   
      IF (KAPO2-KAPO1 .GT. 1) CNAMK = CRDBUF(KAPO1+1:KAPO2-1)    
      CALL MNCRCK(CRDBUF(KAPO2+1:),MAXCWD,COMAND,LNC,    
     +                             MAXP,PLIST,LLIST, IERR,ISYSWR)    
      IF (IERR .GT. 0)  GO TO 180    
      UK = PLIST(1)  
      WK = 0.    
      IF (LLIST .GE. 2)  WK = PLIST(2)   
      A = 0.     
      IF (LLIST .GE. 3)  A = PLIST(3)    
      B = 0.     
      IF (LLIST .GE. 4)  B = PLIST(4)    
      GO TO 170  
C          old (fixed-field) format  
  150 CONTINUE   
      READ (CRDBUF, 158,ERR=180)  XK,CNAMK,UK,WK,A,B     
  158 FORMAT (BN,F10.0, A10, 4F10.0)     
      K = XK     
      IF (K .EQ. 0)  GO TO 210   
C          parameter format cracked, implement parameter definition  
  170 CALL MNPARM(K,CNAMK,UK,WK,A,B,IERR)    
      IF (IERR .EQ. 0)  GO TO 10     
C          format error  
  180 CONTINUE   
      IF (ISW(6) .EQ. 1)  THEN   
          WRITE (ISYSWR,'(A)') ' FORMAT ERROR.  IGNORED.  ENTER AGAIN.'  
      ELSE   
          WRITE (ISYSWR,'(A)') ' ERROR IN PARAMETER DEFINITION'  
          IFATAL = IFATAL + 1    
      ENDIF  
      GO TO 10   
C                                       . . . end parameter requests     
  210 WRITE (ISYSWR,'(4X,75(1H*))')  
      IF (IFATAL.GT.0 .AND. ISW(6).NE.1)  THEN   
         IFLGUT = 4  
         GO TO 900   
      ENDIF  
      IF (IFLGIN .EQ. IFLGDO)  GO TO 900     
      IFLGDO = IFLGIN    
      GO TO 10   
C                                              . . . . .   IFLGDO = 3    
C                                           read commands    
  300 CONTINUE   
C               crack the next command . . . . . . . . . . . . . . . .   
         DO 350 IPOS= 1, 80  
         IF (CRDBUF(IPOS:IPOS) .NE. ' ') GO TO 355   
  350    CONTINUE    
      WRITE (ISYSWR,'(A)') ' BLANK COMMAND IGNORED.'     
      GO TO 10   
  355 IBEGIN = IPOS  
      CALL MNCRCK(CRDBUF(IBEGIN:),MAXCWD,COMAND,LNC,     
     +                            MAXP,  PLIST, LLIST, IERR,ISYSWR)  
      IF (IERR .GT. 0) THEN  
         IF (ISW(6) .EQ. 1) THEN     
            WRITE (ISYSWR,'(A)') ' COMMAND IGNORED '     
            GO TO 10     
         ELSE    
            WRITE (ISYSWR,'(A)') ' COMMAND CANNOT BE INTERPRETED'    
            GO TO 500    
         ENDIF   
      ENDIF  
C                    certain commands are trapped here already   
         LPHEAD = .TRUE.     
         IF (INDEX(COMAND,'PAR' ) .EQ. 1)  GO TO 440     
             IF (INDEX(COMAND,'SET') .NE. 1)  GO TO 370  
             IF (INDEX(COMAND,'COV') .EQ. 5)  GO TO 400  
             IF (INDEX(COMAND,'TIT') .EQ. 5)  GO TO 460  
  370 CONTINUE   
      CALL MNEXCM(COMAND(1:LNC),PLIST,LLIST,IERR)  
         IF (COMAND(1:3).EQ.'END')  GO TO 900    
         IF (COMAND(1:3).EQ.'EXI')  GO TO 900    
         IF (COMAND(1:3).EQ.'RET')  GO TO 900    
         IF (COMAND(1:3).EQ.'STO')  GO TO 900    
      GO TO 10   






C                                        . . . . . . . . . . set covar   




  400 NRAPE = PLIST(1)   
      ICOMND = ICOMND + 1    
      WRITE (ISYSWR,405) ICOMND,COMAND(1:LNC),(PLIST(I),I=1,LLIST)   
  405 FORMAT (1H ,10(1H*)/' **',I5,' **',A,4G12.4/20X,5G12.4)    
      WRITE (ISYSWR, '(1H ,10(1H*))' )   
      IF (NRAPE .NE. NPAR)  GO TO 425    
      NPAR2 = NPAR*(NPAR+1)/2    
      READ (ISYSRD,420,ERR=500,END=45)  (VHMAT(I),I=1,NPAR2)     
  420 FORMAT (BN,7E11.4,3X)  
      ISW(2) = 3     
      DCOVAR = 0.0   
      IF (ISW(5) .GE. 0)  CALL MNMATU(1)     
      IF (ISW(5) .GE. 1)  CALL MNPRIN(2,AMIN)    
      GO TO 10   
  425 CONTINUE   
      WRITE (ISYSWR,428)     
  428 FORMAT(' SIZE OF COVARIANCE MATRIX TO BE READ DOES NOT',   
     + ' CORRESPOND TO'/' NUMBER OF CURRENTLY VARIABLE PARAMETERS.',     
     + '    COMMAND IGNORED.'/)  
      READ (ISYSRD,420,ERR=500,END=45)  ((DUMMY,I=1,J),J=1,NRAPE)    
      GO TO 10   
C                                           . . . . . PARAMETER command  
  440 CONTINUE   
      IFLGDO = 2     
      IFATAL = 0     
C        go and read parameter definitions   
      GO TO 10   
C                                              . . . . set title     
  460 CONTINUE   
      IFLGDO = 1     
      GO TO 10   
C                                              . . . . error conditions  
  500 IFLGUT = 3     
  900 RETURN     
      END    
      SUBROUTINE MNRN15(VAL,INSEED)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
C         This is a super-portable random number generator.  
C         It should not overflow on any 32-bit machine.  
C         The cycle is only ~10**9, so use with care!    
C         Note especially that VAL must not be undefined on input.   
C                    Set Default Starting Seed   
      PARAMETER (THREE=3.0)  
      DATA ISEED/12345/  
      IF (VAL .EQ. THREE)  GO TO 100     
C    
      INSEED = ISEED     
      K = ISEED/53668    
      ISEED = 40014*(ISEED-K*53668) - K*12211    
      IF (ISEED .LT. 0) ISEED = ISEED + 2147483563   
      VAL = REAL(ISEED) * 4.656613E-10   
      RETURN     
C               "entry" to set seed, flag is VAL=3.  
  100 ISEED = INSEED     
      RETURN     
      END    
      SUBROUTINE MNRSET(IOPT)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called from MNCLER and whenever problem changes, for example   
CC        after SET LIMITS, SET PARAM, CALL FCN 6    
CC    If IOPT=1,     
CC        Resets function value and errors to UNDEFINED  
CC    If IOPT=0, sets only MINOS errors to undefined     
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
      CSTATU = 'RESET     '  
      IF (IOPT .GE. 1)  THEN     
        AMIN = UNDEFI    
        FVAL3 = 2.0*DABS(AMIN) + 1.   
        EDM = BIGEDM     
        ISW(4) = 0   
        ISW(2) = 0   
        DCOVAR = 1.  
        ISW(1) = 0   
      ENDIF  
      LNOLIM = .TRUE.    
      DO 10 I= 1, NPAR   
      IEXT = NEXOFI(I)   
      IF (NVARL(IEXT) .GE. 4) LNOLIM=.FALSE.     
      ERP(I) = ZERO  
      ERN(I) = ZERO  
      GLOBCC(I) = ZERO   
   10 CONTINUE   
      IF (ISW(2) .GE. 1)  THEN   
         ISW(2) = 1  
         DCOVAR = DMAX1(DCOVAR,HALF)   
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNSAVE  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       Writes current parameter values and step sizes onto file ISYSSA     
CC          in format which can be reread by Minuit for restarting.  
CC       The covariance matrix is also output if it exists.  
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
      DIMENSION VC(7)    
      LOGICAL LOPEN,LNAME    
      CHARACTER CGNAME*64, CFNAME*64, CANSWR*1   
C    
      INQUIRE(UNIT=ISYSSA,OPENED=LOPEN,NAMED=LNAME,NAME=CGNAME)  
      IF (LOPEN) THEN    
         IF (.NOT.LNAME) CGNAME='UNNAMED FILE'   
         WRITE (ISYSWR,32) ISYSSA,CGNAME     
   32    FORMAT (' CURRENT VALUES WILL BE SAVED ON UNIT',I3,': ',A/)     
      ELSE   
C                new file, open it   
         WRITE (ISYSWR,35) ISYSSA    
   35    FORMAT (' UNIT',I3,' IS NOT OPENED.')   
         IF (ISW(6) .EQ. 1) THEN     
            WRITE (ISYSWR,'(A)') ' PLEASE GIVE FILE NAME:'   
            READ (ISYSRD,'(A)') CFNAME   
            OPEN (UNIT=ISYSSA,FILE=CFNAME,STATUS='NEW',ERR=600)  
            CGNAME = CFNAME  
         ELSE    
            GO TO 650    
         ENDIF   
      ENDIF  
C                               file is now correctly opened     
      IF (ISW(6) .EQ. 1)  THEN   
         WRITE (ISYSWR,37)  ISYSSA   
   37    FORMAT (' SHOULD UNIT',I3,' BE REWOUND BEFORE WRITING TO IT?' )     
         READ  (ISYSRD,'(A)')  CANSWR    
         IF (CANSWR.EQ.'Y' .OR. CANSWR.EQ.'y') REWIND ISYSSA     
      ENDIF  
C                               and rewound if requested     
      WRITE (ISYSSA,'(10HSET TITLE )',ERR=700)   
      WRITE (ISYSSA,'(A)')  CTITL    
      WRITE (ISYSSA,'(10HPARAMETERS)')   
      NLINES = 3     
C                                write out parameter values  
      DO 200 I= 1, NU    
      IF (NVARL(I) .LT. 0)  GO TO 200    
      NLINES = NLINES + 1    
      IINT = NIOFEX(I)   
      IF (NVARL(I) .GT. 1)  GO TO 100    
C         parameter without limits   
      WRITE (ISYSSA,1001)  I,CPNAM(I),U(I),WERR(IINT)    
      GO TO 200  
C         parameter with limits  
  100 CONTINUE   
      WRITE (ISYSSA,1001) I,CPNAM(I),U(I),WERR(IINT),ALIM(I),BLIM(I)     
 1001 FORMAT (1X,I5,1H',A10,1H',4E13.5)  
  200 CONTINUE   
      WRITE (ISYSSA,'(A)')  ' '  
      NLINES = NLINES + 1    
C                                  write out covariance matrix, if any   
      IF (ISW(2) .LT. 1)  GO TO 750  
      WRITE (ISYSSA,1003,ERR=700)  NPAR  
 1003 FORMAT ('SET COVARIANCE',I6)   
      NPAR2 = NPAR*(NPAR+1)/2    
      WRITE (ISYSSA,1004) (VHMAT(I),I=1,NPAR2)   
 1004 FORMAT (BN,7E11.4,3X)  
      NCOVAR = NPAR2/7 + 1   
      IF (MOD(NPAR2,7) .GT. 0)  NCOVAR = NCOVAR + 1  
      NLINES = NLINES + NCOVAR   
      WRITE (ISYSWR, 501) NLINES,ISYSSA,CGNAME(1:45)     
  501 FORMAT (1X,I5,' RECORDS WRITTEN TO UNIT',I4,':',A)     
      IF (NCOVAR .GT. 0) WRITE (ISYSWR, 502) NCOVAR  
  502 FORMAT (' INCLUDING',I5,' RECORDS FOR THE COVARIANCE MATRIX.'/)    
      GO TO 900  
C                                           some error conditions    
  600 WRITE (ISYSWR,'(A,I4)') ' I/O ERROR: UNABLE TO OPEN UNIT',ISYSSA   
      GO TO 900  
  650 WRITE (ISYSWR,'(A,I4,A)') ' UNIT',ISYSSA,' IS NOT OPENED.'     
      GO TO 900  
  700 WRITE (ISYSWR,'(A,I4)') ' ERROR: UNABLE TO WRITE TO UNIT',ISYSSA   
      GO TO 900  
  750 WRITE (ISYSWR,'(A)') ' THERE IS NO COVARIANCE MATRIX TO SAVE.'     
C    
  900 RETURN     
      END    
      SUBROUTINE MNSCAN   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Scans the values of FCN as a function of one parameter     
CC        and plots the resulting values as a curve using MNPLOT.    
CC        It may be called to scan one parameter or all parameters.  
CC        retains the best function and parameter values found.  
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
     
      XLREQ = DMIN1(WORD7(3),WORD7(4))     
      XHREQ = DMAX1(WORD7(3),WORD7(4))     
      NCALL = WORD7(2) + 0.01    
      IF (NCALL .LE. 1)  NCALL = 41  
      IF (NCALL .GT. MAXCPT)  NCALL = MAXCPT     
      NCCALL = NCALL     
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      IPARWD = WORD7(1) + 0.1    
      IPAR = MAX(IPARWD, 0)  
      IINT = NIOFEX(IPAR)    
      CSTATU = 'NO CHANGE'   
      IF (IPARWD .GT. 0)  GO TO 200  
C    
C         equivalent to a loop over parameters requested     
  100 IPAR = IPAR + 1    
      IF (IPAR .GT. NU)  GO TO 900   
      IINT = NIOFEX(IPAR)    
      IF (IINT .LE. 0)  GO TO 100    
C         set up range for parameter IPAR    
  200 CONTINUE   
      UBEST = U(IPAR)    
      XPT(1) = UBEST     
      YPT(1) = AMIN  
      CHPT(1)= ' '   
      XPT(2) = UBEST     
      YPT(2) = AMIN  
      CHPT(2)= 'X'   
      NXYPT = 2  
      IF (NVARL(IPAR) .GT. 1)  GO TO 300     
C         no limits on parameter     
      IF (XLREQ .EQ. XHREQ)  GO TO 250   
      UNEXT = XLREQ  
      STEP = (XHREQ-XLREQ)/FLOAT(NCALL-1)    
      GO TO 500  
  250 CONTINUE   
      XL = UBEST - WERR(IINT)    
      XH = UBEST+  WERR(IINT)    
      CALL MNBINS(XL,XH,NCALL, UNEXT,UHIGH,NBINS,STEP)   
      NCCALL = NBINS + 1     
      GO TO 500  
C         limits on parameter    
  300 CONTINUE   
      IF (XLREQ .EQ. XHREQ)  GO TO 350   
      XL = DMAX1(XLREQ,ALIM(IPAR))     
      XH = DMIN1(XHREQ,BLIM(IPAR))     
      IF (XL .GE. XH)  GO TO 700     
      UNEXT = XL     
      STEP = (XH-XL)/FLOAT(NCALL-1)  
      GO TO 500  
  350 CONTINUE   
      UNEXT = ALIM(IPAR)     
      STEP = (BLIM(IPAR)-ALIM(IPAR))/FLOAT(NCALL-1)  
C         main scanning loop over parameter IPAR     
  500 CONTINUE   
      DO 600 ICALL = 1, NCCALL   
      U(IPAR) = UNEXT    
      NPARX = NPAR   
      CALL FCN(NPARX,GIN,FNEXT,U,4)    
      NFCN = NFCN + 1    
      NXYPT = NXYPT + 1  
      XPT(NXYPT) = UNEXT     
      YPT(NXYPT) = FNEXT     
      CHPT(NXYPT) = '*'  
      IF (FNEXT .LT. AMIN)  THEN     
        AMIN = FNEXT     
        UBEST = UNEXT    
        CSTATU= 'IMPROVED  '     
        ENDIF    
  530 CONTINUE   
      UNEXT = UNEXT + STEP   
  600 CONTINUE   
C         finished with scan of parameter IPAR   
      U(IPAR) = UBEST    
      CALL MNEXIN(X)     
      WRITE (ISYSWR,1001)  NEWPAG,IPAR,CPNAM(IPAR)   
      NUNIT = ISYSWR     
      CALL MNPLOT(XPT,YPT,CHPT,NXYPT,NUNIT,NPAGWD,NPAGLN)    
      GO TO 800  
  700 CONTINUE   
      WRITE (ISYSWR,1000) IPAR   
  800 CONTINUE   
      IF (IPARWD .LE. 0)  GO TO 100  
C         finished with all parameters   
  900 CONTINUE   
      CALL MNPRIN(5,AMIN)    
      RETURN     
 1000 FORMAT (46H REQUESTED RANGE OUTSIDE LIMITS FOR PARAMETER  ,I3/)    
 1001 FORMAT (I1,'SCAN OF PARAMETER NO.',I3,3H,   ,A10)  
      END    
      SUBROUTINE MNSEEK   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC   Performs a rough (but global) minimization by monte carlo search.   
CC        Each time a new minimum is found, the search area is shifted   
CC        to be centered at the best value.  Random points are chosen    
CC        uniformly over a hypercube determined by current step sizes.   
CC   The Metropolis algorithm accepts a worse point with probability     
CC      exp(-d/UP), where d is the degradation.  Improved points     
CC      are of course always accepted.  Actual steps are random  
CC      multiples of the nominal steps (DIRIN).  
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
     
      PARAMETER (TWOPI=2.0*3.141593)     
      DIMENSION STEP(MNI), XBEST(MNI), XMID(MNI)     
      MXFAIL = WORD7(1)  
      IF (MXFAIL .LE. 0)  MXFAIL=100+20*NPAR     
      MXSTEP = 10*MXFAIL     
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      ALPHA = WORD7(2)   
      IF (ALPHA .LE. ZERO)  ALPHA=3.     
      IF (ISW(5) .GE. 1)  WRITE (ISYSWR, 3) MXFAIL,MXSTEP,ALPHA  
    3 FORMAT (' MNSEEK: MONTE CARLO MINIMIZATION USING METROPOLIS',  
     + ' ALGORITHM'/' TO STOP AFTER',I6,' SUCCESSIVE FAILURES, OR',  
     + I7,' STEPS'/' MAXIMUM STEP SIZE IS',F9.3,' ERROR BARS.')  
      CSTATU= 'INITIAL  '    
      IF (ISW(5) .GE. 2)  CALL MNPRIN(2,AMIN)    
      CSTATU = 'UNCHANGED '  
      IFAIL = 0  
      RNUM = ZERO    
      RNUM1 = ZERO   
      RNUM2 = ZERO   
      NPARX = NPAR   
      FLAST = AMIN   
C              set up step sizes, starting values    
      DO 10 IPAR =  1, NPAR  
      IEXT = NEXOFI(IPAR)    
      DIRIN(IPAR) = 2.0*ALPHA*WERR(IPAR)     
      IF (NVARL(IEXT) .GT. 1)  THEN  
C              parameter with limits     
         CALL MNDXDI(X(IPAR),IPAR,DXDI)  
         IF (DXDI .EQ. ZERO)  DXDI=1.    
         DIRIN(IPAR) = 2.0*ALPHA*WERR(IPAR)/DXDI     
         IF (DABS(DIRIN(IPAR)).GT.TWOPI)  DIRIN(IPAR)=TWOPI   
         ENDIF   
      XMID(IPAR) = X(IPAR)   
   10 XBEST(IPAR) = X(IPAR)  
C                              search loop   
      DO 500 ISTEP= 1, MXSTEP    
      IF (IFAIL .GE. MXFAIL)  GO TO 600  
        DO 100 IPAR= 1, NPAR     
        CALL MNRN15(RNUM1,ISEED)     
        CALL MNRN15(RNUM2,ISEED)     
  100   X(IPAR) = XMID(IPAR) + 0.5*(RNUM1+RNUM2-1.)*DIRIN(IPAR)  
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,FTRY,U,4)     
      NFCN = NFCN + 1    
      IF (FTRY .LT. FLAST)  THEN     
         IF (FTRY .LT. AMIN)  THEN   
            CSTATU = 'IMPROVEMNT'    
            AMIN = FTRY  
            DO 200 IB= 1, NPAR   
  200       XBEST(IB) = X(IB)    
            IFAIL = 0    
            IF (ISW(5) .GE. 2) CALL MNPRIN(2,AMIN)   
            ENDIF    
         GO TO 300   
      ELSE   
         IFAIL = IFAIL + 1   
C                   Metropolis algorithm     
         BAR = DEXP((AMIN-FTRY)/UP)   
         CALL MNRN15(RNUM,ISEED)     
         IF (BAR .LT. RNUM)  GO TO 500   
      ENDIF  
C                    Accept new point, move there    
  300 CONTINUE   
      DO 350 J= 1, NPAR  
      XMID(J) = X(J)     
  350 CONTINUE   
      FLAST = FTRY   
  500 CONTINUE   
C                               end search loop  
  600 CONTINUE   
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,601) IFAIL    
  601 FORMAT(' MNSEEK:',I5,' SUCCESSIVE UNSUCCESSFUL TRIALS.')   
      DO 700 IB= 1, NPAR     
  700 X(IB) = XBEST(IB)  
      CALL MNINEX(X)     
      IF (ISW(5) .GE. 1)  CALL MNPRIN(2,AMIN)    
      IF (ISW(5) .EQ. 0)  CALL MNPRIN(0,AMIN)    
      RETURN     
      END    
