
       SUBROUTINE MINUIT   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
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
C  CPNAM   Parameter name (10 characters)    
C  U       External (visible to user in FCN) value of parameter  
C  ALIM, BLIM Lower and upper parameter limits. If both zero, no limits.     
C  ERP,ERN Positive and negative MINOS errors, if calculated.    
C  WERR    External parameter error (standard deviation, defined by UP)  
C  GLOBCC  Global Correlation Coefficient    
C  NVARL   =-1 if parameter undefined,      =0 if constant,  
C          = 1 if variable without limits,  =4 if variable with limits   
C   (Note that if parameter has been fixed, NVARL=1 or =4, and NIOFEX=0)     
C  NIOFEX  Internal parameter number, or zero if not currently variable  
C  NEXOFI  External parameter number for currently variable parameters   
C  X, XT   Internal parameter values (X are sometimes saved in XT)   
C  DIRIN   (Internal) step sizes for current step    
C  variables with names ending in ..S are saved values for fixed params  
C  VHMAT   (Internal) error matrix stored as Half MATrix, since  
C                it is symmetric     
C  VTHMAT  VHMAT is sometimes saved in VTHMAT, especially in MNMNOT  
C    
C  ISW definitions:  
C      ISW(1) =0 normally, =1 means CALL LIMIT EXCEEDED  
C      ISW(2) =0 means no error matrix   
C             =1 means only approximate error matrix     
C             =2 means full error matrix, but forced pos-def.    
C             =3 means good normal full error matrix exists  
C      ISW(3) =0 if Minuit is calculating the first derivatives  
C             =1 if first derivatives calculated inside FCN  
C      ISW(4) =-1 if most recent minimization did not converge.  
C             = 0 if problem redefined since most recent minimization.   
C             =+1 if most recent minimization did converge.  
C      ISW(5) is the PRInt level.  See SHO PRIntlevel    
C      ISW(6) = 0 for batch mode, =1 for interactive mode    
C    
C  LWARN is true if warning messges are to be put out (default=true)     
C            SET WARN turns it on, set NOWarn turns it off   
C  LREPOR is true if exceptional conditions are put out (default=false)  
C            SET DEBUG turns it on, SET NODebug turns it off     
C  LIMSET is true if a parameter is up against limits (for MINOS)    
C  LNOLIM is true if there are no limits on any parameters (not yet used)    
C  LNEWMN is true if the previous process has unexpectedly improved FCN  
C  LPHEAD is true if a heading should be put out for the next parameter  
C        definition, false if a parameter has just been defined  
C    
     
      CHARACTER*40 CWHYXT    
      DATA CWHYXT/'FOR UNKNOWN REASONS                     '/    
      DATA JSYSRD,JSYSWR,JSYSSA/5,6,7/   
C                                 . . . . . . . . . . initialize minuit  
      WRITE (JSYSWR,'(1X,75(1H*))')  
      CALL MNINIT (JSYSRD,JSYSWR,JSYSSA)     
C                                      . . . . initialize new data block     
  100 CONTINUE   
      WRITE (ISYSWR,'(1X,75(1H*))')  
      NBLOCK = NBLOCK + 1    
      WRITE (ISYSWR,'(26X,A,I4)')  'MINUIT DATA BLOCK NO.',NBLOCK    
      WRITE (ISYSWR,'(1X,75(1H*))')  
C               . . . . . . . . . . .   set parameter lists to undefined     
      CALL MNCLER    
C                                             . . . . . . . . read title     
      CALL MNREAD(1,IFLGUT)    
      IF (IFLGUT .EQ. 2)  GO TO 500  
      IF (IFLGUT .EQ. 3)  GO TO 600  
C                                        . . . . . . . . read parameters     
      CALL MNREAD(2,IFLGUT)    
      IF (IFLGUT .EQ. 2)  GO TO 500  
      IF (IFLGUT .EQ. 3)  GO TO 600  
      IF (IFLGUT .EQ. 4)  GO TO 700  
C                              . . . . . . verify FCN not time-dependent     
      WRITE (ISYSWR,'(/A,A)') ' MINUIT: FIRST CALL TO USER FUNCTION,',   
     +    ' WITH IFLAG=1'    
      NPARX = NPAR   
      CALL MNINEX(X)     
      FZERO = UNDEFI     
      CALL FCN(NPARX,GIN,FZERO,U,1)    
      FIRST = UNDEFI     
      CALL FCN(NPARX,GIN,FIRST,U,4)    
      NFCN = 2   
      IF (FZERO.EQ.UNDEFI .AND. FIRST.EQ.UNDEFI)  THEN   
          CWHYXT = 'BY ERROR IN USER FUNCTION.  '    
          WRITE (ISYSWR,'(/A,A/)') ' USER HAS NOT CALCULATED FUNCTION',  
     +    ' VALUE WHEN IFLAG=1 OR 4'     
          GO TO 800  
      ENDIF  
      AMIN = FIRST   
      IF (FIRST .EQ. UNDEFI) AMIN=FZERO  
      CALL MNPRIN(1,AMIN)    
      NFCN = 2   
      IF (FIRST .EQ. FZERO)  GO TO 300   
      FNEW = 0.0     
      CALL FCN(NPARX,GIN,FNEW,U,4)     
      IF  (FNEW .NE. AMIN) WRITE (ISYSWR,280) AMIN, FNEW     
  280 FORMAT (/' MINUIT WARNING: PROBABLE ERROR IN USER FUNCTION.'/  
     +         ' FOR FIXED VALUES OF PARAMETERS, FCN IS TIME-DEPENDENT'/     
     +         ' F =',E22.14,' FOR FIRST CALL'/  
     +         ' F =',E22.14,' FOR SECOND CALL.'/)   
      NFCN = 3   
  300 FVAL3 = 2.0*AMIN+1.0   
C                                   . . . . . . . . . . . read commands  
      CALL MNREAD(3,IFLGUT)    
      IF (IFLGUT .EQ. 2)  GO TO 500  
      IF (IFLGUT .EQ. 3)  GO TO 600  
      IF (IFLGUT .EQ. 4)  GO TO 700  
      CWHYXT = 'BY MINUIT COMMAND: '//CWORD  
      IF (INDEX(CWORD,'STOP').GT. 0)  GO TO 800  
      IF (INDEX(CWORD,'EXI') .GT. 0)  GO TO 800  
      IF (INDEX(CWORD,'RET') .EQ. 0)  GO TO 100  
      CWHYXT = 'AND RETURNS TO USER PROGRAM.    '    
      WRITE (ISYSWR,'(A,A)')  ' ..........MINUIT TERMINATED ',CWHYXT     
      RETURN     
C                                           . . . . . . stop conditions  
  500 CONTINUE   
      CWHYXT = 'BY END-OF-DATA ON PRIMARY INPUT FILE.   '    
      GO TO 800  
  600 CONTINUE   
      CWHYXT = 'BY UNRECOVERABLE READ ERROR ON INPUT.   '    
      GO TO 800  
  700 CONTINUE   
      CWHYXT = ': FATAL ERROR IN PARAMETER DEFINITIONS. '    
  800 WRITE (ISYSWR,'(A,A)')  ' ..........MINUIT TERMINATED ',CWHYXT     
      STOP   
C    
C  ......................entry to set unit numbers  - - - - - - - - - -  
      ENTRY MINTIO(I1,I2,I3)     
      JSYSRD = I1    
      JSYSWR = I2    
      JSYSSA = I3    
      RETURN     
      END    
      SUBROUTINE MNAMIN   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called  from many places.  Initializes the value of AMIN by    
CC        calling the user function. Prints out the function value and   
CC        parameter values if Print Flag value is high enough.   
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
     
      NPARX = NPAR   
      IF (ISW(5) .GE. 1) WRITE (ISYSWR,'(/A,A)') ' FIRST CALL TO ',  
     + 'USER FUNCTION AT NEW START POINT, WITH IFLAG=4.'     
      CALL MNEXIN(X)     
      CALL FCN(NPARX,GIN,FNEW,U,4)     
      NFCN = NFCN + 1    
      AMIN = FNEW    
      EDM = BIGEDM   
      RETURN     
      END    
      SUBROUTINE MNBINS(A1,A2,NAA,BL,BH,NB,BWID)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
C         SUBROUTINE TO DETERMINE REASONABLE HISTOGRAM INTERVALS     
C         GIVEN ABSOLUTE UPPER AND LOWER BOUNDS  A1 AND A2   
C         AND DESIRED MAXIMUM NUMBER OF BINS NAA     
C         PROGRAM MAKES REASONABLE BINNING FROM BL TO BH OF WIDTH BWID   
C         F. JAMES,   AUGUST, 1974 , stolen for Minuit, 1988     
      PARAMETER (ZERO=0.0)   
      AL = DMIN1(A1,A2)    
      AH = DMAX1(A1,A2)    
      IF (AL.EQ.AH)  AH = AL + 1.    
C         IF NAA .EQ. -1 , PROGRAM USES BWID INPUT FROM CALLING ROUTINE  
      IF (NAA .EQ. -1)  GO TO 150    
   10 NA = NAA - 1   
      IF (NA .LT. 1)  NA = 1     
C          GET NOMINAL BIN WIDTH IN EXPON FORM   
   20 AWID = (AH-AL)/FLOAT(NA)   
      LOG = INT(LOG10(AWID))     
      IF (AWID .LE. 1.0)  LOG=LOG-1  
      SIGFIG = AWID * (10.00 **(-LOG))   
C         ROUND MANTISSA UP TO 2, 2.5, 5, OR 10  
      IF(SIGFIG .GT. 2.0)  GO TO 40  
      SIGRND = 2.0   
      GO TO 100  
   40 IF (SIGFIG .GT. 2.5)  GO TO 50     
      SIGRND = 2.5   
      GO TO 100  
   50 IF(SIGFIG .GT. 5.0)  GO TO 60  
      SIGRND =5.0    
      GO TO 100  
   60 SIGRND = 1.0   
      LOG = LOG + 1  
  100 CONTINUE   
      BWID = SIGRND*10.0**LOG    
      GO TO 200  
C         GET NEW BOUNDS FROM NEW WIDTH BWID     
  150 IF (BWID .LE. ZERO)  GO TO 10  
  200 CONTINUE   
      ALB = AL/BWID  
      LWID=ALB   
      IF (ALB .LT. ZERO)  LWID=LWID-1    
      BL = BWID*FLOAT(LWID)  
      ALB = AH/BWID + 1.0    
      KWID = ALB     
      IF (ALB .LT. ZERO)  KWID=KWID-1    
      BH = BWID*FLOAT(KWID)  
      NB = KWID-LWID     
      IF (NAA .GT. 5)  GO TO 240     
      IF (NAA .EQ. -1)  RETURN   
C          REQUEST FOR ONE BIN IS DIFFICULT CASE     
      IF (NAA .GT. 1 .OR. NB .EQ. 1)  RETURN     
      BWID =  BWID*2.0   
       NB  = 1   
       RETURN    
  240 IF (2*NB .NE. NAA)  RETURN     
      NA = NA + 1    
      GO TO 20   
      END    
      SUBROUTINE MNCALF(PVEC,YCALF)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called only from MNIMPR.  Transforms the function FCN  
CC        by dividing out the quadratic part in order to find further    
CC        minima.    Calculates  ycalf = (f-fmin)/(x-xmin)*v*(x-xmin)    
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
     
      DIMENSION PVEC(15)     
      NPARX = NPAR   
      CALL MNINEX(PVEC)  
      CALL FCN(NPARX,GIN,F,U,4)    
      NFCN = NFCN + 1    
      DO 200 I= 1, NPAR  
      GRD(I) = 0.    
         DO 200 J= 1, NPAR   
         M = MAX(I,J)    
         N = MIN(I,J)    
         NDEX = M*(M-1)/2 + N    
  200    GRD(I) = GRD(I) + VTHMAT(NDEX) * (XT(J)-PVEC(J))    
      DENOM = 0.     
      DO 210 I= 1, NPAR  
  210 DENOM = DENOM + GRD(I) * (XT(I)-PVEC(I))   
      IF (DENOM .LE. ZERO)  THEN     
         DCOVAR = 1.     
         ISW(2) = 0  
         DENOM = 1.0     
      ENDIF  
      YCALF = (F-APSI) / DENOM   
      RETURN     
      END    
      SUBROUTINE MNCLER  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called from MINUIT and by option from MNEXCM   
CC        Resets the parameter list to UNDEFINED     
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
      NPFIX = 0  
      NU = 0     
      NPAR = 0   
      NFCN = 0   
      NWRMES(1) = 0  
      NWRMES(2) = 0  
      DO 10 I= 1, MAXEXT     
      U(I) = 0.0     
      CPNAM(I) = CUNDEF  
      NVARL(I) = -1  
   10 NIOFEX(I) = 0  
      CALL MNRSET(1)     
      CFROM = 'CLEAR   '     
      NFCNFR = NFCN  
      CSTATU ='UNDEFINED '   
      LNOLIM = .TRUE.    
      LPHEAD = .TRUE.    
      RETURN     
      END    
      SUBROUTINE MNCNTR(KE1,KE2,IERRF)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       to print function contours in two variables, on line printer    
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
     
      PARAMETER (NUMBCS=20,NXMAX=115)    
      DIMENSION CONTUR(NUMBCS), FCNA(NXMAX),FCNB(NXMAX)  
      CHARACTER CLABEL*(NUMBCS)  
      CHARACTER CHLN*(NXMAX),CHMID*(NXMAX),CHZERO*(NXMAX)    
      DATA CLABEL/'0123456789ABCDEFGHIJ'/    
C                 input arguments: parx, pary, devs, ngrid   
      IF (KE1.LE.0 .OR. KE2.LE.0)  GO TO 1350    
      IF (KE1.GT.NU .OR. KE2.GT.NU)  GO TO 1350  
      KI1 = NIOFEX(KE1)  
      KI2 = NIOFEX(KE2)  
      IF (KI1.LE.0 .OR. KI2.LE.0)  GO TO 1350    
      IF (KI1 .EQ. KI2)  GO TO 1350  
C    
      IF (ISW(2) .LT. 1)  THEN   
          CALL MNHESS     
          CALL MNWERR    
          ENDIF  
      NPARX = NPAR   
      XSAV = U(KE1)  
      YSAV = U(KE2)  
      DEVS = WORD7(3)    
      IF (DEVS .LE. ZERO)  DEVS=2.   
      XLO = U(KE1) - DEVS*WERR(KI1)  
      XUP = U(KE1) + DEVS*WERR(KI1)  
      YLO = U(KE2) - DEVS*WERR(KI2)  
      YUP = U(KE2) + DEVS*WERR(KI2)  
      NGRID = WORD7(4)   
      IF (NGRID .LE. 0)  THEN    
          NGRID=25   
          NX = MIN(NPAGWD-15,NGRID)  
          NY = MIN(NPAGLN-7, NGRID)  
      ELSE   
          NX = NGRID     
          NY = NGRID     
      ENDIF  
      IF (NX .LT. 11) NX=11  
      IF (NY .LT. 11) NY=11  
      IF (NX .GE. NXMAX)  NX=NXMAX-1     
C         ask if parameter outside limits    
      IF (NVARL(KE1) .GT. 1)  THEN   
         IF (XLO .LT. ALIM(KE1))  XLO = ALIM(KE1)    
         IF (XUP .GT. BLIM(KE1))  XUP = BLIM(KE1)    
      ENDIF  
      IF (NVARL(KE2) .GT. 1)   THEN  
         IF (YLO .LT. ALIM(KE2))  YLO = ALIM(KE2)    
         IF (YUP .GT. BLIM(KE2))  YUP = BLIM(KE2)    
      ENDIF  
      BWIDX = (XUP-XLO)/REAL(NX)     
      BWIDY = (YUP-YLO)/REAL(NY)     
      IXMID = INT((XSAV-XLO)*REAL(NX)/(XUP-XLO)) + 1     
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      DO 185 I= 1, NUMBCS    
      CONTUR(I) = AMIN + UP*FLOAT(I-1)**2    
  185 CONTINUE   
      CONTUR(1) = CONTUR(1) + 0.01*UP    
C                fill FCNB to prepare first row, and find column zero    
      U(KE2) = YUP   
      IXZERO = 0     
      XB4 = ONE  
      DO 200 IX= 1, NX+1     
      U(KE1) = XLO + REAL(IX-1)*BWIDX    
      CALL FCN(NPARX,GIN,FF,U,4)   
      FCNB(IX) = FF  
      IF (XB4.LT.ZERO .AND. U(KE1).GT.ZERO)  IXZERO = IX-1   
      XB4 = U(KE1)   
      CHMID(IX:IX) = '*'     
      CHZERO(IX:IX)= '-'     
  200 CONTINUE   
      WRITE (ISYSWR,'(A,I3,A,A)') ' Y-AXIS: PARAMETER ',     
     +      KE2,': ',CPNAM(KE2)  
      IF (IXZERO .GT. 0)  THEN   
         CHZERO(IXZERO:IXZERO) = '+'     
         CHLN = ' '  
         WRITE (ISYSWR,'(12X,A,A)') CHLN(1:IXZERO),'X=0'     
      ENDIF  
C                 loop over rows     
      DO 280 IY= 1, NY   
      UNEXT = U(KE2) - BWIDY     
C                 prepare this line's background pattern for contour     
      CHLN = ' '     
      CHLN(IXMID:IXMID) = '*'    
      IF (IXZERO .NE. 0) CHLN(IXZERO:IXZERO) = ':'   
      IF (U(KE2).GT.YSAV .AND. UNEXT.LT.YSAV) CHLN=CHMID     
      IF (U(KE2).GT.ZERO .AND. UNEXT.LT.ZERO) CHLN=CHZERO    
      U(KE2) = UNEXT     
      YLABEL = U(KE2) + 0.5*BWIDY    
C                 move FCNB to FCNA and fill FCNB with next row  
      DO 220 IX= 1, NX+1     
      FCNA(IX) = FCNB(IX)    
      U(KE1) = XLO + REAL(IX-1)*BWIDX    
      CALL FCN(NPARX,GIN,FF,U,4)   
      FCNB(IX) = FF  
  220 CONTINUE   
C                 look for contours crossing the FCNxy squares   
      DO 250 IX= 1, NX   
      FMX = DMAX1(FCNA(IX),FCNB(IX),FCNA(IX+1),FCNB(IX+1))     
      FMN = DMIN1(FCNA(IX),FCNB(IX),FCNA(IX+1),FCNB(IX+1))     
      DO 230 ICS= 1, NUMBCS  
      IF (CONTUR(ICS) .GT. FMN)  GO TO 240   
  230 CONTINUE   
      GO TO 250  
  240 IF (CONTUR(ICS) .LT. FMX) CHLN(IX:IX)=CLABEL(ICS:ICS)  
  250 CONTINUE   
C                 print a row of the contour plot    
      WRITE (ISYSWR,'(1X,G12.4,1X,A)') YLABEL,CHLN(1:NX)     
  280 CONTINUE   
C                 contours printed, label x-axis     
      CHLN = ' '     
      CHLN( 1: 1) = 'I'  
      CHLN(IXMID:IXMID) = 'I'    
      CHLN(NX:NX) = 'I'  
      WRITE (ISYSWR,'(14X,A)') CHLN(1:NX)    
C                the hardest of all: print x-axis scale!     
      CHLN = ' '     
      IF (NX .LE. 26) THEN   
          NL = MAX(NX-12,2)  
          NL2 = NL/2     
          WRITE (ISYSWR,'(8X,G12.4,A,G12.4)') XLO,CHLN(1:NL),XUP     
          WRITE (ISYSWR,'(14X,A,G12.4)')   CHLN(1:NL2),XSAV  
      ELSE   
          NL = MAX(NX-24,2)/2    
          NL2 = NL   
          IF (NL .GT. 10) NL2=NL-6   
          WRITE (ISYSWR,'(8X,G12.4,A,G12.4,A,G12.4)')  XLO,  
     +      CHLN(1:NL),XSAV,CHLN(1:NL2),XUP  
      ENDIF  
      WRITE (ISYSWR,'(6X,A,I3,A,A,A,G12.4)') ' X-AXIS: PARAMETER',   
     +    KE1,': ',CPNAM(KE1),'  ONE COLUMN=',BWIDX  
      WRITE (ISYSWR,'(A,G12.4,A,G12.4,A)') ' FUNCTION VALUES: F(I)=',    
     +    AMIN,' +',UP,' *I**2'  
C                 finished.  reset input values  
      U(KE1) = XSAV  
      U(KE2) = YSAV  
      IERRF = 0  
      RETURN     
 1350 WRITE (ISYSWR,1351)    
 1351 FORMAT (' INVALID PARAMETER NUMBER(S) REQUESTED.  IGNORED.' /)     
      IERRF = 1  
      RETURN     
      END    
      SUBROUTINE MNCONT(KE1,KE2,NPTU,XPTU,YPTU,IERRF)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       Find NPTU points along a contour where the function     
CC             FMIN (X(KE1),X(KE2)) =  AMIN+UP   
CC       where FMIN is the minimum of FCN with respect to all    
CC       the other NPAR-2 variable parameters (if any).  
CC   IERRF on return will be equal to the number of points found:    
CC     NPTU if normal termination with NPTU points found     
CC     -1   if errors in the calling sequence (KE1, KE2 not variable)    
CC      0   if less than four points can be found (using MNMNOT)     
CC     n>3  if only n points can be found (n < NPTU)     
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
      DIMENSION XPTU(NPTU), YPTU(NPTU), W(MNI),GCC(MNI)  
      CHARACTER CHERE*10     
      PARAMETER (CHERE='MNContour ')     
      LOGICAL LDEBUG     
     
C                 input arguments: parx, pary, devs, ngrid   
      LDEBUG = (IDBG(6) .GE. 1)  
      IF (KE1.LE.0 .OR. KE2.LE.0)  GO TO 1350    
      IF (KE1.GT.NU .OR. KE2.GT.NU)  GO TO 1350  
      KI1 = NIOFEX(KE1)  
      KI2 = NIOFEX(KE2)  
      IF (KI1.LE.0 .OR. KI2.LE.0)  GO TO 1350    
      IF (KI1 .EQ. KI2)  GO TO 1350  
      IF (NPTU .LT. 4)  GO TO 1400   
C    
      NFCNCO = NFCN  
      NFCNMX = 100*(NPTU+5)*(NPAR+1)     
C           The minimum  
      CALL MNCUVE     
      U1MIN = U(KE1)     
      U2MIN = U(KE2)     
      IERRF = 0  
      CFROM = CHERE  




      NFCNFR = NFCNCO    
      IF (ISW(5) .GE. 0)  THEN   
         WRITE (ISYSWR,'(1X,A,I4,A)')    


     +   'START MNCONTOUR CALCULATION OF',NPTU,' POINTS ON CONTOUR.'     


         IF (NPAR .GT. 2) THEN   




            IF (NPAR .EQ. 3) THEN    
              KI3 = 6 - KI1 - KI2    
              KE3 = NEXOFI(KI3)  
              WRITE (ISYSWR,'(1X,A,I3,2X,A)')    




     +        'EACH POINT IS A MINIMUM WITH RESPECT TO PARAMETER ',  
     +        KE3, CPNAM(KE3)    
            ELSE     
              WRITE (ISYSWR,'(1X,A,I3,A)')   
     +        'EACH POINT IS A MINIMUM WITH RESPECT TO THE OTHER',   
     +        NPAR-2, ' VARIABLE PARAMETERS.'    
            ENDIF    
         ENDIF   
      ENDIF  
C    
C           Find the first four points using MNMNOT  
C              ........................ first two points     
      CALL MNMNOT(KE1,KE2,VAL2PL,VAL2MI)   
      IF (ERN(KI1) .EQ. UNDEFI)  THEN    
         XPTU(1) = ALIM(KE1)     
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')  
      ELSE   
         IF (ERN(KI1) .GE. ZERO)  GO TO 1500     
         XPTU(1) = U1MIN+ERN(KI1)    
      ENDIF  
      YPTU(1) = VAL2MI   
C    
      IF (ERP(KI1) .EQ. UNDEFI)  THEN    
         XPTU(3) = BLIM(KE1)     
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')  
      ELSE   
         IF (ERP(KI1) .LE. ZERO)  GO TO 1500     
         XPTU(3) = U1MIN+ERP(KI1)    
      ENDIF  
      YPTU(3) = VAL2PL   
      SCALX = 1.0/(XPTU(3) - XPTU(1))    
C              ........................... next two points   
      CALL MNMNOT(KE2,KE1,VAL2PL,VAL2MI)   
      IF (ERN(KI2) .EQ. UNDEFI)  THEN    
         YPTU(2) = ALIM(KE2)     
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')  
      ELSE   
         IF (ERN(KI2) .GE. ZERO)  GO TO 1500     
         YPTU(2) = U2MIN+ERN(KI2)    
      ENDIF  
      XPTU(2) = VAL2MI   
      IF (ERP(KI2) .EQ. UNDEFI)  THEN    
         YPTU(4) = BLIM(KE2)     
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')  
      ELSE   
         IF (ERP(KI2) .LE. ZERO)  GO TO 1500     
         YPTU(4) = U2MIN+ERP(KI2)    
      ENDIF  
      XPTU(4) = VAL2PL   
      SCALY = 1.0/(YPTU(4) - YPTU(2))    
      NOWPTS = 4     
      NEXT = 5   
      IF (LDEBUG) THEN   
         WRITE (ISYSWR,'(A)') ' Plot of four points found by MINOS'  
         XPT(1) = U1MIN  
         YPT(1) = U2MIN  
         CHPT(1) = ' '   
         NALL = MIN(NOWPTS+1,MAXCPT)     
         DO 85 I= 2, NALL    
           XPT(I) = XPTU(I-1)    
           YPT(I) = YPTU(I-1)    
   85    CONTINUE    
           CHPT(2)= 'A'  
           CHPT(3)= 'B'  
           CHPT(4)= 'C'  
           CHPT(5)= 'D'  
         CALL MNPLOT(XPT,YPT,CHPT,NALL,ISYSWR,NPAGWD,NPAGLN)     
      ENDIF  
C    
C               ..................... save some values before fixing     
      ISW2 = ISW(2)  
      ISW4 = ISW(4)  
      SIGSAV = EDM   
      ISTRAV = ISTRAT    
      DC = DCOVAR    
      APSI  = EPSI*0.5   
      ABEST=AMIN     
      MPAR=NPAR  
      NFMXIN = NFCNMX    
      DO 125 I= 1, MPAR  
  125 XT(I) = X(I)   
      DO 130 J= 1, MPAR*(MPAR+1)/2   
  130 VTHMAT(J) = VHMAT(J)   
      DO 135 I= 1, MPAR  
      GCC(I) = GLOBCC(I)     
  135 W(I) = WERR(I)     
C                           fix the two parameters in question   
      KINTS = NIOFEX(KE1)    
      CALL MNFIXP (KINTS,IERR)   
      KINTS = NIOFEX(KE2)    
      CALL MNFIXP (KINTS,IERR)   
C               ......................Fill in the rest of the points     
      DO 900 INEW= NEXT, NPTU    
C            find the two neighbouring points with largest separation    
      BIGDIS = 0.    
         DO 200  IOLD = 1, INEW-1    
         I2 = IOLD + 1   
         IF (I2 .EQ. INEW) I2 = 1    
         DIST = (SCALX*(XPTU(IOLD)-XPTU(I2)))**2 +   
     +          (SCALY*(YPTU(IOLD)-YPTU(I2)))**2     
         IF (DIST .GT. BIGDIS) THEN  
            BIGDIS = DIST    
            IDIST = IOLD     
         ENDIF   
  200    CONTINUE    
      I1 = IDIST     
      I2 = I1 + 1    
      IF (I2 .EQ. INEW) I2 = 1   
C                   next point goes between I1 and I2    
      A1 = HALF  
      A2 = HALF  
  300 XMIDCR = A1*XPTU(I1) + A2*XPTU(I2)     
      YMIDCR = A1*YPTU(I1) + A2*YPTU(I2)     
      XDIR = YPTU(I2) - YPTU(I1)     
      YDIR = XPTU(I1) - XPTU(I2)     
      SCLFAC = DMAX1(DABS(XDIR*SCALX), DABS(YDIR*SCALY))     
      XDIRCR = XDIR/SCLFAC   
      YDIRCR = YDIR/SCLFAC   
      KE1CR = KE1    
      KE2CR = KE2    
C                Find the contour crossing point along DIR   
      AMIN = ABEST   
      CALL MNCROS(AOPT,IERCR)  
      IF (IERCR .GT. 1)  THEN    
C              If cannot find mid-point, try closer to point 1   
         IF (A1 .GT. HALF) THEN  
            WRITE (ISYSWR,'(A,A,I3,A)') ' MNCONT CANNOT FIND NEXT',  
     +           ' POINT ON CONTOUR.  ONLY ',NOWPTS,' POINTS FOUND.'     
            GO TO 950    
         ENDIF   
         CALL MNWARN('W',CHERE,'Cannot find midpoint, try closer.')  
         A1 = 0.75   
         A2 = 0.25   
         GO TO 300   
      ENDIF  
C                Contour has been located, insert new point in list  
         DO 830 MOVE= NOWPTS,I1+1,-1     
         XPTU(MOVE+1) = XPTU(MOVE)   
         YPTU(MOVE+1) = YPTU(MOVE)   
  830    CONTINUE    
      NOWPTS = NOWPTS + 1    
      XPTU(I1+1) = XMIDCR + XDIRCR*AOPT  
      YPTU(I1+1) = YMIDCR + YDIRCR*AOPT  
  900 CONTINUE   
  950 CONTINUE   
C     IERRF = NOWPTS     
      CSTATU = 'SUCCESSFUL'  
      IF (NOWPTS .LT. NPTU)  CSTATU = 'INCOMPLETE'   
C                make a lineprinter plot of the contour  
      IF (ISW(5) .GE. 0) THEN    
         XPT(1) = U1MIN  
         YPT(1) = U2MIN  
         CHPT(1) = ' '   
         NALL = MIN(NOWPTS+1,MAXCPT)     
         DO 1000 I= 2, NALL  
           XPT(I) = XPTU(I-1)    
           YPT(I) = YPTU(I-1)    
           CHPT(I)= 'X'  
 1000    CONTINUE    
         WRITE (ISYSWR,'(A,I3,2X,A)') ' Y-AXIS: PARAMETER ',KE2,     
     +        CPNAM(KE2)     
         CALL MNPLOT(XPT,YPT,CHPT,NALL,ISYSWR,NPAGWD,NPAGLN)     
         WRITE (ISYSWR,'(25X,A,I3,2X,A)') 'X-AXIS: PARAMETER ',  
     +         KE1,CPNAM(KE1)    
      ENDIF  
C                 print out the coordinates around the contour   
      IF (ISW(5) .GE. 1)  THEN   
         NPCOL = (NOWPTS+1)/2    
         NFCOL = NOWPTS/2    
         WRITE (ISYSWR,'(/I5,A,G13.5,A,G11.3)') NOWPTS,  
     +    ' POINTS ON CONTOUR.   FMIN=',ABEST,'   ERRDEF=',UP    
         WRITE (ISYSWR,'(9X,A,3X,A,18X,A,3X,A)')     
     +         CPNAM(KE1),CPNAM(KE2),CPNAM(KE1),CPNAM(KE2)   
         DO 1050 LINE = 1, NFCOL     
           LR = LINE + NPCOL     
           WRITE (ISYSWR,'(1X,I5,2G13.5,10X,I5,2G13.5)')     
     +     LINE,XPTU(LINE),YPTU(LINE),LR,XPTU(LR),YPTU(LR)   
 1050    CONTINUE    
         IF (NFCOL .LT. NPCOL) WRITE (ISYSWR,'(1X,I5,2G13.5)')   
     +                         NPCOL,XPTU(NPCOL),YPTU(NPCOL)     
      ENDIF  
C                                    . . contour finished. reset v   
      ITAUR = 1  
      CALL MNFREE(1)     
      CALL MNFREE(1)     
      DO 1100 J= 1, MPAR*(MPAR+1)/2  
 1100 VHMAT(J) = VTHMAT(J)   
      DO 1120 I= 1, MPAR     
      GLOBCC(I) = GCC(I)     
      WERR(I) = W(I)     
 1120 X(I) = XT(I)   
      CALL MNINEX (X)    
      EDM = SIGSAV   
      AMIN = ABEST   
      ISW(2) = ISW2  
      ISW(4) = ISW4  
      DCOVAR = DC    
      ITAUR = 0  
      NFCNMX = NFMXIN    
      ISTRAT = ISTRAV    
      U(KE1) = U1MIN     
      U(KE2) = U2MIN     
      GO TO 2000     
C                                     Error returns  
 1350 WRITE (ISYSWR,'(A)') ' INVALID PARAMETER NUMBERS.'     
      GO TO 1450     
 1400 WRITE (ISYSWR,'(A)') ' LESS THAN FOUR POINTS REQUESTED.'   
 1450 IERRF = -1     
      CSTATU = 'USER ERROR'  
      GO TO 2000     
 1500 WRITE (ISYSWR,'(A)') ' MNCONT UNABLE TO FIND FOUR POINTS.'     
      U(KE1) = U1MIN     
      U(KE2) = U2MIN     
      IERRF = 0  
      CSTATU = 'FAILED'  
 2000 CONTINUE   
      CFROM = CHERE  
      NFCNFR = NFCNCO    
      RETURN     
      END    
      SUBROUTINE MNCRCK(CRDBUF,MAXCWD,COMAND,LNC,    
     +                         MXP,   PLIST, LLIST,IERR,ISYSWR)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC   
CC       Called from MNREAD.     
CC       Cracks the free-format input, expecting zero or more    
CC         alphanumeric fields (which it joins into COMAND(1:LNC))   
CC         followed by one or more numeric fields separated by   
CC         blanks and/or one comma.  The numeric fields are put into     
CC         the LLIST (but at most MXP) elements of PLIST.    
CC      IERR = 0 if no errors,   
CC           = 1 if error(s).    
CC      Diagnostic messages are written to ISYSWR    
CC   
      PARAMETER (MAXELM=25, MXLNEL=19)   
      CHARACTER*(*) COMAND, CRDBUF   
      CHARACTER CNUMER*13, CELMNT(MAXELM)*(MXLNEL), CNULL*15     
      DIMENSION LELMNT(MAXELM),PLIST(MXP)    
      DATA CNULL /')NULL STRING   '/     
      DATA CNUMER/'123456789-.0+'/   
      IELMNT = 0     
      LEND = LEN(CRDBUF)     
      NEXTB = 1  
      IERR = 0   
C                                   . . . .  loop over words CELMNT  
   10 CONTINUE   
      DO 100 IPOS= NEXTB,LEND    
         IBEGIN = IPOS   
         IF (CRDBUF(IPOS:IPOS).EQ.' ')  GO TO 100    
         IF (CRDBUF(IPOS:IPOS).EQ.',')  GO TO 250    
         GO TO 150   
  100 CONTINUE   
         GO TO 300   
  150 CONTINUE   
C               found beginning of word, look for end    
         DO 180 IPOS = IBEGIN+1,LEND     
         IF (CRDBUF(IPOS:IPOS).EQ.' ')  GO TO 250    
         IF (CRDBUF(IPOS:IPOS).EQ.',')  GO TO 250    
  180    CONTINUE    
      IPOS = LEND+1  
  250 IEND = IPOS-1  
      IELMNT = IELMNT + 1    
      IF (IEND .GE. IBEGIN) THEN     
         CELMNT(IELMNT) = CRDBUF(IBEGIN:IEND)    
      ELSE   
         CELMNT(IELMNT) = CNULL  
      ENDIF  
      LELMNT(IELMNT) = IEND-IBEGIN+1     
      IF (LELMNT(IELMNT) .GT. MXLNEL)  THEN  
         WRITE (ISYSWR, 253) CRDBUF(IBEGIN:IEND),CELMNT(IELMNT)  
  253    FORMAT (' MINUIT WARNING: INPUT DATA WORD TOO LONG.'    
     +   /'     ORIGINAL:',A     
     +   /' TRUNCATED TO:',A)    
         LELMNT(IELMNT) = MXLNEL     
         ENDIF   
      IF (IPOS .GE. LEND) GO TO 300  
      IF (IELMNT .GE. MAXELM)  GO TO 300     
C                     look for comma or beginning of next word   
         DO 280 IPOS= IEND+1,LEND    
         IF (CRDBUF(IPOS:IPOS) .EQ. ' ') GO TO 280   
         NEXTB = IPOS    
         IF (CRDBUF(IPOS:IPOS) .EQ. ',') NEXTB = IPOS+1  
         GO TO 10    
  280    CONTINUE    
C                 All elements found, join the alphabetic ones to    
C                                form a command  
  300 CONTINUE   
      NELMNT = IELMNT    
      COMAND = ' '   
      LNC = 1    
      PLIST(1) = 0.  
      LLIST = 0  
      IF (IELMNT .EQ. 0)  GO TO 900  
      KCMND = 0  
         DO 400 IELMNT = 1, NELMNT   
         IF (CELMNT(IELMNT) .EQ. CNULL)  GO TO 450   
            DO 350 IC= 1, 13     
            IF (CELMNT(IELMNT)(1:1) .EQ. CNUMER(IC:IC)) GO TO 450    
  350       CONTINUE     
         IF (KCMND .GE. MAXCWD) GO TO 400    
         LEFT = MAXCWD-KCMND     
         LTOADD = LELMNT(IELMNT)     
         IF (LTOADD .GT. LEFT) LTOADD=LEFT   
         COMAND(KCMND+1:KCMND+LTOADD) = CELMNT(IELMNT)(1:LTOADD)     
         KCMND = KCMND + LTOADD  
         IF (KCMND .EQ. MAXCWD)  GO TO 400   
         KCMND = KCMND + 1   
         COMAND(KCMND:KCMND) = ' '   
  400    CONTINUE    
      LNC = KCMND    
      GO TO 900  
  450 CONTINUE   
      LNC = KCMND    
C                      . . . .  we have come to a numeric field  
      LLIST = 0  
      DO 600 IFLD= IELMNT,NELMNT     
      LLIST = LLIST + 1  
      IF (LLIST .GT. MXP) THEN   
         NREQ = NELMNT-IELMNT+1  
         WRITE (ISYSWR,511) NREQ,MXP     
  511 FORMAT (/' MINUIT WARNING IN MNCRCK: '/ ' COMMAND HAS INPUT',I5,   
     + ' NUMERIC FIELDS, BUT MINUIT CAN ACCEPT ONLY',I3)     
         GO TO 900   
      ENDIF  
      IF (CELMNT(IFLD) .EQ. CNULL)  THEN     
          PLIST(LLIST) = 0.  
        ELSE     
          READ (CELMNT(IFLD), '(BN,F19.0)',ERR=575) PLIST(LLIST)     
      ENDIF  
      GO TO 600  
  575 WRITE (ISYSWR,'(A,A,A)') ' FORMAT ERROR IN NUMERIC FIELD: "',  
     + CELMNT(IFLD)(1:LELMNT(IFLD)),'"'  
      IERR = 1   
      PLIST(LLIST) = 0.  
  600 CONTINUE   
C                                  end loop over numeric fields  
  900 CONTINUE   
      IF (LNC .LE. 0)  LNC=1     
      RETURN     
      END    
      SUBROUTINE MNCROS(AOPT,IERCR)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       Find point where MNEVAL=AMIN+UP, along the line through     
CC       XMID,YMID with direction XDIR,YDIR,   where X and Y are     
CC       parameters KE1 and KE2.  If KE2=0 (from MINOS), then    
CC       only KE1 is varied.  From MNCONT, both are varied.  
CC       Crossing point is at    
CC        (U(KE1),U(KE2)) = (XMID,YMID) + AOPT*(XDIR,YDIR)   
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
      CHARACTER CHERE*10, CHARAL*28, CHSIGN*4    
      PARAMETER (CHERE='MNCONTour ', MLSB=3, MAXITR=15, TLR=0.01)    
      DIMENSION FLSB(MLSB),ALSB(MLSB), COEFF(3)  
      LOGICAL LDEBUG     
     
      DATA  CHARAL/' .ABCDEFGHIJKLMNOPQRSTUVWXYZ'/   
      LDEBUG = (IDBG(6) .GE. 1)  
      AMINSV = AMIN  
      AIM = AMIN + UP    
      TLF = TLR*UP   
      TLA = TLR*0.1  
      XPT(1) = 0.0   
      YPT(1) = AIM   
      CHPT(1) = ' '  
      XPT(2) = -1.0  
      YPT(2) = AMIN  
      CHPT(2) = '.'  
      IPT = 2    
C                    find the largest allowed A  
      AULIM = 100.   
      DO 100 IK= 1, 2    
         IF (IK .EQ. 1)  THEN    
            KEX = KE1CR  
            ZMID = XMIDCR    
            ZDIR = XDIRCR    
         ELSE    
            IF (KE2CR .EQ. 0)  GO TO 100     
            KEX = KE2CR  
            ZMID = YMIDCR    
            ZDIR = YDIRCR    
         ENDIF   
         IF (NVARL(KEX) .LE. 1) GO TO 100    
         IF (ZDIR .EQ. ZERO)      GO TO 100  
         ZLIM = ALIM(KEX)    
         IF (ZDIR .GT. ZERO) ZLIM = BLIM(KEX)    
         AULIM = MIN(AULIM,(ZLIM-ZMID)/ZDIR)     
  100 CONTINUE   
C                  LSB = Line Search Buffer  
C          first point   
      ANEXT = 0.     
      AOPT = ANEXT   
      LIMSET = .FALSE.   
        IF (AULIM .LT. AOPT+TLA)  LIMSET = .TRUE.    
      CALL MNEVAL(ANEXT,FNEXT,IEREV)   
C debug printout:    
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')   
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT  
      IF (IEREV .GT. 0)  GO TO 900   
      IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930    
      IPT = IPT + 1  
      XPT(IPT) = ANEXT   
      YPT(IPT) = FNEXT   
      CHPT(IPT)= CHARAL(IPT:IPT)     
      ALSB(1) = ANEXT    
      FLSB(1) = FNEXT    
      FNEXT = MAX(FNEXT,AMINSV+0.1*UP)   
      AOPT =  DSQRT((UP)/(FNEXT-AMINSV)) - 1.0    
      IF (DABS(FNEXT-AIM) .LT. TLF)  GO TO 800    
C    
      IF (AOPT .LT. -0.5)  AOPT = -0.5   
      LIMSET = .FALSE.   
      IF (AOPT .GT. AULIM)  THEN     
              AOPT = AULIM   
              LIMSET = .TRUE.    
      ENDIF  
      CALL MNEVAL(AOPT,FNEXT,IEREV)    
C debug printout:    
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')   
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT  
      IF (IEREV .GT. 0)  GO TO 900   
      IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930    
      ALSB(2) = AOPT     
      IPT = IPT + 1  
      XPT(IPT) = ALSB(2)     
      YPT(IPT) = FNEXT   
      CHPT(IPT)= CHARAL(IPT:IPT)     
      FLSB(2) = FNEXT    
      DFDA = (FLSB(2)-FLSB(1))/ (ALSB(2)-ALSB(1))    
      ILSB = 2   
C                   DFDA must be positive on the contour     
      IF (DFDA .GT. ZERO)  GO TO 460     
  300    CALL MNWARN('D',CHERE,'Looking for slope of the right sign')    
         MAXLK = MAXITR - IPT    
         DO 400 IT= 1, MAXLK     
            ALSB(1) = ALSB(2)    
            FLSB(1) = FLSB(2)    
            AOPT = ALSB(1) + 0.2*REAL(IT)    
            LIMSET = .FALSE.     
            IF (AOPT .GT. AULIM)  THEN   
              AOPT = AULIM   
              LIMSET = .TRUE.    
            ENDIF    
            CALL MNEVAL(AOPT,FNEXT,IEREV)  
C debug printout:    
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')   
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT  
            IF (IEREV .GT. 0)  GO TO 900     
            IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930  
               ALSB(2) = AOPT    
               IPT = IPT + 1     
               XPT(IPT) = ALSB(2)    
               YPT(IPT) = FNEXT  
               CHPT(IPT)= CHARAL(IPT:IPT)    
            FLSB(2) = FNEXT  
            DFDA = (FLSB(2)-FLSB(1))/ (ALSB(2)-ALSB(1))  
            IF (DFDA .GT. ZERO)  GO TO 450   
  400    CONTINUE    
         CALL MNWARN('W',CHERE,'Cannot find slope of the right sign')    
         GO TO 950   
  450    CONTINUE    
C                    we have two points with the right slope     
  460 AOPT = ALSB(2) + (AIM-FLSB(2))/DFDA    
      IF (DMIN1(DABS(AOPT-ALSB(1)),DABS(AOPT-ALSB(2))).LT. TLA)GO TO 800    
      IF (IPT .GE. MAXITR)  GO TO 950    
      BMIN = MIN(ALSB(1),ALSB(2)) - 1.0  
      IF (AOPT .LT. BMIN)  AOPT = BMIN   
      BMAX = MAX(ALSB(1),ALSB(2)) + 1.0  
      IF (AOPT .GT. BMAX)  AOPT = BMAX   
C                    Try a third point   
      CALL MNEVAL(AOPT,FNEXT,IEREV)    
C debug printout:    
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')   
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT  
      IF (IEREV .GT. 0)  GO TO 900   
      ALSB(3) = AOPT     
      IPT = IPT + 1  
      XPT(IPT) = ALSB(3)     
      YPT(IPT) = FNEXT   
      CHPT(IPT)= CHARAL(IPT:IPT)     
      FLSB(3) = FNEXT    
      INEW = 3   
C                now we have three points, ask how many <AIM     
      ECARMN = DABS(FNEXT-AIM)    
      IBEST = 3  
      ECARMX = 0.    
      NOLESS = 0     
      DO 480 I= 1, 3     
         ECART = DABS(FLSB(I) - AIM)  
         IF (ECART .GT. ECARMX) THEN     
            ECARMX = ECART   
            IWORST = I   
         ENDIF   
         IF (ECART .LT. ECARMN) THEN     
            ECARMN = ECART   
            IBEST = I    
         ENDIF   
         IF (FLSB(I) .LT. AIM) NOLESS = NOLESS + 1   
  480 CONTINUE   
C           if at least one on each side of AIM, fit a parabola  
      IF (NOLESS.EQ.1 .OR. NOLESS.EQ.2) GO TO 500    
C           if all three are above AIM, third must be closest to AIM     
      IF (NOLESS .EQ. 0 .AND. IBEST .NE. 3)  GO TO 950   
C           if all three below, and third is not best, then slope    
C             has again gone negative, look for positive slope.  
      IF (NOLESS .EQ. 3 .AND. IBEST .NE. 3) THEN     
          ALSB(2) = ALSB(3)  
          FLSB(2) = FLSB(3)  
          GO TO 300  
      ENDIF  
C           in other cases, new straight line thru last two points   
      ALSB(IWORST) = ALSB(3)     
      FLSB(IWORST) = FLSB(3)     
      DFDA = (FLSB(2)-FLSB(1))/ (ALSB(2)-ALSB(1))    
      GO TO 460  
C                parabola fit    
  500 CALL MNPFIT(ALSB,FLSB,3,COEFF,SDEV)    
      IF (COEFF(3) .LE. ZERO)  CALL MNWARN ('D',CHERE,   
     +             'Curvature is negative near contour line.')   
      DETERM =  COEFF(2)**2 - 4.*COEFF(3)*(COEFF(1)-AIM)     
      IF (DETERM .LE. ZERO)   THEN   
          CALL MNWARN('D',CHERE,'Problem 2, impossible determinant')     
          GO TO 950  
      ENDIF  
C                Find which root is the right one    
      RT = DSQRT(DETERM)  
      X1 = (-COEFF(2) + RT)/(2.*COEFF(3))    
      X2 = (-COEFF(2) - RT)/(2.*COEFF(3))    
      S1 = COEFF(2) + 2.*X1*COEFF(3)     
      S2 = COEFF(2) + 2.*X2*COEFF(3)     
      IF (S1*S2 .GT. ZERO) WRITE (ISYSWR,'(A)') ' MNCONTour problem 1'   
      AOPT = X1  
      IF (S2 .GT. ZERO)  AOPT = X2   
      IF (DABS(AOPT-ALSB(INEW)) .LT. TLA)  GO TO 800  
C                  Evaluate function at parabolic optimum    
      IF (IPT .GE. MAXITR)  GO TO 950    
      LIMSET = .FALSE.   
      IF (AOPT .GT. AULIM)  THEN     
              AOPT = AULIM   
              LIMSET = .TRUE.    
      ENDIF  
      CALL MNEVAL(AOPT,FNEXT,IEREV)    
C debug printout:    
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')   
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT  
      IF (IEREV .GT. 0)  GO TO 900   
      IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930    
      IPT = IPT + 1  
      XPT(IPT) = AOPT    
      YPT(IPT) = FNEXT   
      CHPT(IPT)= CHARAL(IPT:IPT)     
C                Replace unneeded point by new one   
C            find nearest, farthest, (and hence middle) points,  
      INEAR = 1  
      ANEAR = ALSB(1)    
      IFAR = 1   
      AFAR = ALSB(1)     
      DO 620 I= 1, 3     
      IF (ALSB(I) .LT. ANEAR) THEN   
         ANEAR = ALSB(I)     
         INEAR = I   
      ENDIF  
      IF (ALSB(I) .GT. AFAR)  THEN   
         AFAR = ALSB(I)  
         IFAR = I    
      ENDIF  
  620 CONTINUE   
      IMID = 6 - INEAR - IFAR    
      FDIST = FLSB(IMID)-AIM     
      IF (FDIST*(FLSB(INEAR)-AIM) .GT. ZERO) THEN    
         INEW = INEAR    
      ELSE   
         INEW = IFAR     
      ENDIF  
      ALSB(INEW) = AOPT  
      FLSB(INEW) = FNEXT     
      GO TO 500  
C       Contour has been located, return point to MNCONT OR MINOS    
  800 CONTINUE   
      IERCR = 0  
      GO TO 1000     
C                error in the minimization   
  900 IF (IEREV .EQ. 1)  GO TO 940   
      GO TO 950  
C                parameter up against limit  
  930 IERCR = 1  
      GO TO 1000     
C                too many calls to FCN   
  940 IERCR = 2  
      GO TO 1000     
C                cannot find next point  
  950 IERCR = 3  
C                in any case     
 1000 CONTINUE   
      IF (LDEBUG) THEN   
         ITOOHI = 0  
         DO 1100 I= 1, IPT   
         IF (YPT(I) .GT. AIM+UP) THEN    
            YPT(I) = AIM+UP  
            CHPT(I) = '+'    
            ITOOHI = 1   
         ENDIF   
 1100    CONTINUE    
         CHSIGN = 'POSI'     
         IF (XDIRCR .LT. ZERO)  CHSIGN = 'NEGA'  
         IF (KE2CR .EQ. 0)  WRITE (ISYSWR, '(2X,A,A,I3)')    
     +            CHSIGN,'TIVE MINOS ERROR, PARAMETER ',KE1CR    
         IF (ITOOHI .EQ. 1)  WRITE (ISYSWR, '(10X,A)')   
     +            'POINTS LABELLED "+" WERE TOO HIGH TO PLOT.'   
         IF (IERCR .EQ. 1) WRITE (ISYSWR,'(10X,A)')  
     +            'RIGHTMOST POINT IS UP AGAINST LIMIT.'     
         CALL MNPLOT(XPT,YPT,CHPT,IPT,ISYSWR,NPAGWD,NPAGLN)  
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNCUVE   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Makes sure that the current point is a local   
CC        minimum and that the error matrix exists,  
CC        or at least something good enough for MINOS and MNCONT     
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
     
      IF (ISW(4) .LT. 1) THEN    
          WRITE (ISYSWR,'(/A,A)')    
     +    ' FUNCTION MUST BE MINIMIZED BEFORE CALLING ',CFROM    
          APSI = EPSI    
          CALL MNMIGR     
      ENDIF  
      IF (ISW(2) .LT. 3)  THEN   
         CALL MNHESS  
         IF (ISW(2) .LT. 1)  THEN    
            CALL MNWARN('W',CFROM,'NO ERROR MATRIX.  WILL IMPROVISE.')   
            DO 555 I=1,NPAR  
              NDEX = I*(I-1)/2   
              DO 554 J=1,I-1     
              NDEX = NDEX + 1    
  554         VHMAT(NDEX) = 0.   
            NDEX = NDEX + 1  
            IF (G2(I) .LE. ZERO)  THEN   
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
            VHMAT(NDEX) = 2./G2(I)   
  555       CONTINUE     
            ISW(2) = 1   
            DCOVAR = 1.  
         ELSE    
           CALL MNWERR   
         ENDIF   
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNDERI   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Calculates the first derivatives of FCN (GRD),     
CC        either by finite differences or by transforming the user-  
CC        supplied derivatives to internal coordinates,  
CC        according to whether ISW(3) is zero or one.    




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
     
      LOGICAL LDEBUG     
      CHARACTER CBF1*22  
      NPARX = NPAR   
      LDEBUG = (IDBG(2) .GE. 1)  
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      IF (ISW(3) .EQ. 1)  GO TO 100  
      IF (LDEBUG) THEN   
C                       make sure starting at the right place    
        CALL MNINEX(X)   
        NPARX = NPAR     
        CALL FCN(NPARX,GIN,FS1,U,4)    
        NFCN = NFCN + 1  
        IF (FS1 .NE. AMIN) THEN  
           DF = AMIN - FS1   
           WRITE (CBF1(1:12),'(G12.3)') DF   
           CALL MNWARN('D','MNDERI',     
     +         'function value differs from AMIN by '//CBF1(1:12) )  
           AMIN = FS1    
        ENDIF    
          WRITE  
     +   (ISYSWR,'(/''  FIRST DERIVATIVE DEBUG PRINTOUT.  MNDERI''/  
     +   '' PAR    DERIV     STEP      MINSTEP   OPTSTEP '',     
     +   '' D1-D2    2ND DRV'')')    
      ENDIF  
      DFMIN = 8. * EPSMA2*(DABS(AMIN)+UP)     
      IF (ISTRAT .LE. 0) THEN    
         NCYC = 2    
         TLRSTP = 0.5    
         TLRGRD = 0.1    
      ELSE IF (ISTRAT .EQ. 1) THEN   
         NCYC = 3    
         TLRSTP = 0.3    
         TLRGRD = 0.05   
      ELSE   
         NCYC = 5    
         TLRSTP = 0.1    
         TLRGRD = 0.02   
      ENDIF  
C                                loop over variable parameters   
      DO 60  I=1,NPAR    
      EPSPRI = EPSMA2 + DABS(GRD(I)*EPSMA2)   
C         two-point derivatives always assumed necessary     
C         maximum number of cycles over step size depends on strategy    
      XTF = X(I)     
      STEPB4 = 0.    
C                               loop as little as possible here!     
      DO 45 ICYC= 1, NCYC    
C                 ........ theoretically best step   
      OPTSTP = DSQRT(DFMIN/(DABS(G2(I))+EPSPRI))   
C                     step cannot decrease by more than a factor of ten  
      STEP = MAX(OPTSTP, DABS(0.1*GSTEP(I)))  
C                 but if parameter has limits, max step size = 0.5   
      IF (GSTEP(I).LT.ZERO .AND. STEP.GT.0.5)  STEP=0.5  
C                 and not more than ten times the previous step  
      STPMAX = 10.*DABS(GSTEP(I))     
      IF (STEP .GT. STPMAX)  STEP = STPMAX   
C                 minimum step size allowed by machine precision     
      STPMIN = 8. * DABS(EPSMA2*X(I))     
      IF (STEP .LT. STPMIN)  STEP = STPMIN   
C                 end of iterations if step change less than factor 2    
      IF (DABS((STEP-STEPB4)/STEP) .LT. TLRSTP)  GO TO 50     
C         take step positive     
      GSTEP(I) = DSIGN(STEP, GSTEP(I))    
      STEPB4 = STEP  
      X(I) = XTF + STEP  
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,FS1,U,4)  
      NFCN=NFCN+1    
C         take step negative     
      X(I) = XTF - STEP  
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN,FS2,U,4)  
      NFCN=NFCN+1    
      GRBFOR = GRD(I)    
      GRD(I) = (FS1-FS2)/(2.0*STEP)  
      G2(I) = (FS1+FS2-2.0*AMIN)/(STEP**2)   
      X(I) = XTF     
      IF (LDEBUG) THEN   
         D1D2 = (FS1+FS2-2.0*AMIN)/STEP  
         WRITE (ISYSWR,41) I,GRD(I),STEP,STPMIN,OPTSTP,D1D2,G2(I)    
   41    FORMAT (I4,2G11.3,5G10.2)   
      ENDIF  
C         see if another iteration is necessary  
      IF (DABS(GRBFOR-GRD(I))/(DABS(GRD(I))+DFMIN/STEP) .LT. TLRGRD)   
     +        GO TO 50   
   45 CONTINUE   
C                           end of ICYC loop. too many iterations    
      IF (NCYC .EQ. 1)  GO TO 50     
         WRITE (CBF1,'(2E11.3)')  GRD(I),GRBFOR  
         CALL MNWARN('D','MNDERI',   
     +         'First derivative not converged. '//CBF1)     
   50 CONTINUE   
C    
   60 CONTINUE   
      CALL MNINEX(X)     
      RETURN     
C                                        .  derivatives calc by fcn  
  100 DO 150 IINT= 1, NPAR   
      IEXT = NEXOFI(IINT)    
      IF (NVARL(IEXT) .GT. 1)  GO TO 120     
      GRD(IINT) = GIN(IEXT)  
      GO TO 150  
  120 DD = (BLIM(IEXT)-ALIM(IEXT))*0.5 *DCOS(X(IINT))     
      GRD(IINT) = GIN(IEXT)*DD   
  150 CONTINUE   
  200 RETURN     
      END    
      SUBROUTINE MNDXDI(PINT,IPAR,DXDI)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        calculates the transformation factor between external and  
CC        internal parameter values.     this factor is one for  
CC        parameters which are not limited.     called from MNEMAT.  
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
      I = NEXOFI(IPAR)   
      DXDI = 1.0     
      IF (NVARL(I) .GT. 1)   
     +      DXDI = 0.5 *DABS((BLIM(I)-ALIM(I)) * DCOS(PINT))   
      RETURN     
      END    
      SUBROUTINE MNEIG(A,NDIMA,N,MITS,WORK,PRECIS,IFAULT)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
C    
      DIMENSION A(NDIMA,*),WORK(*)   
      DATA ZERO,ONE,TWO/0.0,1.0,2.0/     
      DATA TOL/1.0E-35/  
C          PRECIS is the machine precision EPSMAC    
      IFAULT = 1     
C    
      I = N  
      DO 70 I1 = 2,N     
      L = I-2    
      F = A(I,I-1)   
      GL = ZERO  
C    
      IF(L .LT. 1) GO TO 25  
C    
      DO 20 K = 1,L  
   20 GL = GL+A(I,K)**2  
   25 H = GL + F**2  
C    
      IF(GL .GT. TOL) GO TO 30   
C    
      WORK(I) = ZERO     
      WORK(N+I) = F  
      GO TO 65   
   30 L = L+1    
C    
      GL = DSQRT(H)   
C    
      IF(F .GE. ZERO) GL = -GL   
C    
      WORK(N+I) = GL     
      H = H-F*GL     
      A(I,I-1) = F-GL    
      F = ZERO   
      DO 50 J = 1,L  
      A(J,I) = A(I,J)/H  
      GL = ZERO  
      DO 40 K = 1,J  
   40 GL = GL+A(J,K)*A(I,K)  
C    
      IF(J .GE. L) GO TO 47  
C    
      J1 = J+1   
      DO 45 K = J1,L     
   45 GL = GL+A(K,J)*A(I,K)  
   47 WORK(N+J) = GL/H   
      F = F+GL*A(J,I)    
   50 CONTINUE   
      HH = F/(H+H)   
      DO 60 J = 1,L  
      F = A(I,J)     
      GL = WORK(N+J)-HH*F    
      WORK(N+J) = GL     
      DO 60 K = 1,J  
      A(J,K) = A(J,K)-F*WORK(N+K)-GL*A(I,K)  
   60 CONTINUE   
      WORK(I) = H    
   65 I = I-1    
   70 CONTINUE   
      WORK(1) = ZERO     
      WORK(N+1) = ZERO   
      DO 110 I = 1,N     
      L = I-1    
C    
      IF(WORK(I) .EQ. ZERO .OR. L .EQ. 0) GO TO 100  
C    
      DO 90 J = 1,L  
      GL = ZERO  
      DO 80 K = 1,L  
   80 GL = GL+A(I,K)*A(K,J)  
      DO 90 K = 1,L  
      A(K,J) = A(K,J)-GL*A(K,I)  
   90 CONTINUE   
  100 WORK(I) = A(I,I)   
      A(I,I) = ONE   
C    
      IF(L .EQ. 0) GO TO 110     
C    
      DO 105 J = 1,L     
      A(I,J) = ZERO  
      A(J,I) = ZERO  
  105 CONTINUE   
  110 CONTINUE   
C    
C    
      N1 = N-1   
      DO 130 I = 2,N     
      I0 = N+I-1     
  130 WORK(I0) = WORK(I0+1)  
      WORK(N+N) = ZERO   
      B = ZERO   
      F = ZERO   
      DO 210 L = 1,N     
      J = 0  
      H = PRECIS*(DABS(WORK(L))+DABS(WORK(N+L)))   
C    
      IF(B .LT. H) B = H     
C    
      DO 140 M1 = L,N    
      M = M1     
C    
      IF(DABS(WORK(N+M)) .LE. B) GO TO 150    
C    
  140 CONTINUE   
C    
  150 IF(M .EQ. L) GO TO 205     
C    
  160 IF(J .EQ. MITS) RETURN     
C    
      J = J+1    
      PT = (WORK(L+1)-WORK(L))/(TWO*WORK(N+L))   
      R = DSQRT(PT*PT+ONE)    
      PR = PT+R  
C    
      IF(PT .LT. ZERO) PR=PT-R   
C    
      H = WORK(L)-WORK(N+L)/PR   
      DO 170 I=L,N   
  170 WORK(I) = WORK(I)-H    
      F = F+H    
      PT = WORK(M)   
      C = ONE    
      S = ZERO   
      M1 = M-1   
      I = M  
      DO 200 I1 = L,M1   
      J = I  
      I = I-1    
      GL = C*WORK(N+I)   
      H = C*PT   
C    
      IF(DABS(PT) .GE. DABS(WORK(N+I))) GO TO 180  
C    
      C = PT/WORK(N+I)   
      R = DSQRT(C*C+ONE)  
      WORK(N+J) = S*WORK(N+I)*R  
      S = ONE/R  
      C = C/R    
      GO TO 190  
  180 C = WORK(N+I)/PT   
      R = DSQRT(C*C+ONE)  
      WORK(N+J) = S*PT*R     
      S = C/R    
      C = ONE/R  
  190 PT = C*WORK(I)-S*GL    
      WORK(J) = H+S*(C*GL+S*WORK(I))     
      DO 200 K = 1,N     
      H = A(K,J)     
      A(K,J) = S*A(K,I)+C*H  
      A(K,I) = C*A(K,I)-S*H  
  200 CONTINUE   
      WORK(N+L) = S*PT   
      WORK(L) = C*PT     
C    
      IF(DABS(WORK(N+L)) .GT. B) GO TO 160    
C    
  205 WORK(L) = WORK(L)+F    
  210 CONTINUE   
      DO 240 I=1,N1  
      K = I  
      PT = WORK(I)   
      I1 = I+1   
      DO 220 J = I1,N    
C    
      IF(WORK(J) .GE. PT) GO TO 220  
C    
      K = J  
      PT = WORK(J)   
  220 CONTINUE   
C    
      IF(K .EQ. I) GO TO 240     
C    
      WORK(K) = WORK(I)  
      WORK(I) = PT   
      DO 230 J=1,N   
      PT = A(J,I)    
      A(J,I) = A(J,K)    
      A(J,K) = PT    
  230 CONTINUE   
  240 CONTINUE   
      IFAULT = 0     
C    
      RETURN     
      END    
      SUBROUTINE MNEMAT(EMAT,NDIM)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION EMAT(NDIM,NDIM)  
CC        Calculates the external error matrix from the internal     
CC        to be called by user, who must dimension EMAT at (NDIM,NDIM)   
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
      IF (ISW(2) .LT. 1)  RETURN     
      IF (ISW(5) .GE. 2)  WRITE (ISYSWR,'(/A,I4,A,I3,A,G10.2)')  
     +    ' EXTERNAL ERROR MATRIX.    NDIM=',NDIM,'    NPAR=',NPAR,  
     +    '    ERR DEF=',UP  
C                    size of matrix to be printed    
      NPARD = NPAR   
      IF (NDIM .LT. NPAR)  THEN  
        NPARD = NDIM     
        IF (ISW(5) .GE. 0) WRITE (ISYSWR,'(A,A)') ' USER-DIMENSIONED ',  
     +      ' ARRAY EMAT NOT BIG ENOUGH. REDUCED MATRIX CALCULATED.'     
      ENDIF  
C                 NPERLN is the number of elements that fit on one line  
      NPERLN = (NPAGWD-5)/10     
      NPERLN = MIN(NPERLN,13)    
      IF (ISW(5).GE. 1 .AND. NPARD.GT.NPERLN)  WRITE (ISYSWR,'(A)')  
     +     ' ELEMENTS ABOVE DIAGONAL ARE NOT PRINTED.'   
C                 I counts the rows of the matrix    
      DO 110 I= 1, NPARD     
         CALL MNDXDI(X(I),I,DXDI)    
         KGA = I*(I-1)/2     
         DO 100 J= 1, I  
            CALL MNDXDI(X(J),J,DXDJ)     
            KGB = KGA + J    
            EMAT(I,J) = DXDI * VHMAT(KGB) * DXDJ * UP    
            EMAT(J,I) = EMAT(I,J)    
  100    CONTINUE    
  110 CONTINUE   
C                    IZ is number of columns to be printed in row I  
      IF (ISW(5) .GE. 2)  THEN   
      DO 160 I= 1, NPARD     
         IZ = NPARD  
         IF (NPARD .GE. NPERLN)  IZ = I  
         DO 150 K= 1, IZ, NPERLN     
           K2 = K + NPERLN - 1   
           IF (K2 .GT. IZ)  K2=IZ    
           WRITE (ISYSWR,'(1X,13E10.3)')  (EMAT(I,KK),KK=K,K2)   
  150    CONTINUE    
  160 CONTINUE   
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNERRS(NUMBER,EPLUS,EMINUS,EPARAB,GCC)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC    Called by user, utility routine to get MINOS errors    
CC    If NUMBER is positive, then it is external parameter number,   
CC                  if negative, it is -internal number.     
CC    values returned by MNERRS:     
CC       EPLUS, EMINUS are MINOS errors of parameter NUMBER,     
CC       EPARAB is 'parabolic' error (from error matrix).    
CC                 (Errors not calculated are set = 0.)  
CC       GCC is global correlation coefficient from error matrix     
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
      IEX = NUMBER   
      IF (NUMBER .LT. 0)  THEN   
         IIN = -NUMBER   
         IF (IIN .GT. NPAR)  GO TO 900   
         IEX = NEXOFI(IIN)   
      ENDIF  
      IF (IEX .GT. NU .OR. IEX .LE. 0)  GO TO 900    
      IIN = NIOFEX(IEX)  
      IF (IIN .LE. 0)  GO TO 900     
C             IEX is external number, IIN is internal number     
      EPLUS = ERP(IIN)   
        IF (EPLUS.EQ.UNDEFI)  EPLUS=0.   
      EMINUS= ERN(IIN)   
        IF (EMINUS.EQ.UNDEFI) EMINUS=0.  
      CALL MNDXDI(X(IIN),IIN,DXDI)   
      NDIAG = IIN*(IIN+1)/2  
      EPARAB = DABS(DXDI*DSQRT(DABS(UP*VHMAT(NDIAG))))  
C              global correlation coefficient    
      GCC = 0.   
      IF (ISW(2) .LT. 2)  GO TO 990  
      GCC = GLOBCC(IIN)  
      GO TO 990  
C                  ERROR.  parameter number not valid    
  900 EPLUS = 0.     
      EMINUS = 0.    
      EPARAB = 0.    
      GCC = 0.   
  990 RETURN     
      END    
      SUBROUTINE MNEVAL(ANEXT,FNEXT,IEREV)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC      Evaluates the function being analyzed by MNCROS, which is    
CC      generally the minimum of FCN with respect to all remaining   
CC      variable parameters.  Common block /MN7XCR/ contains the     
CC      data necessary to know the values of U(KE1CR) and U(KE2CR)   
CC      to be used, namely     U(KE1CR) = XMIDCR + ANEXT*XDIRCR  
CC      and (if KE2CR .NE. 0)  U(KE2CR) = YMIDCR + ANEXT*YDIRCR  
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
CC   
     
                          U(KE1CR) = XMIDCR + ANEXT*XDIRCR   
      IF ( KE2CR .NE. 0)  U(KE2CR) = YMIDCR + ANEXT*YDIRCR   
      CALL MNINEX(X)     
      NPARX = NPAR   
      CALL FCN(NPARX,GIN,FNEXT,U,4)    
      NFCN = NFCN + 1    
      IEREV = 0  
      IF (NPAR .GT. 0)  THEN     
         ITAUR = 1   
         AMIN = FNEXT    
         ISW(1) = 0  
         CALL MNMIGR  
         ITAUR = 0   
         FNEXT = AMIN    
         IF (ISW(1) .GE. 1)  IEREV = 1   
         IF (ISW(4) .LT. 1)  IEREV = 2   
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNEXCM(COMAND,PLIST,LLIST,IERFLG)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Interprets a command and takes appropriate action,     
CC        either directly by skipping to the corresponding code in   
CC        MNEXCM, or by setting up a call to a subroutine    
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
     
      CHARACTER*(*) COMAND   
C   Cannot say DIMENSION PLIST(LLIST) since LLIST can be =0.     
      DIMENSION PLIST(*)     
      PARAMETER (MXPT=101)   
      DIMENSION XPTU(MXPT), YPTU(MXPT)   
C  alphabetical order of command names!  
      DIMENSION    ISORT(40)     
      CHARACTER*10 CNAME(40), CNEWAY, CHWHY*18, C26*30, CVBLNK*2     
      LOGICAL LTOFIX, LFIXED, LFREED     
C  recognized MINUIT commands:   
      DATA CNAME( 1) / 'MINImize  ' /    
      DATA CNAME( 2) / 'SEEk      ' /    
      DATA CNAME( 3) / 'SIMplex   ' /    
      DATA CNAME( 4) / 'MIGrad    ' /    
      DATA CNAME( 5) / 'MINOs     ' /    
      DATA CNAME( 6) / 'SET xxx   ' /    
      DATA CNAME( 7) / 'SHOw xxx  ' /    
      DATA CNAME( 8) / 'TOP of pag' /    
      DATA CNAME( 9) / 'FIX       ' /    
      DATA CNAME(10) / 'REStore   ' /    
      DATA CNAME(11) / 'RELease   ' /    
      DATA CNAME(12) / 'SCAn      ' /    
      DATA CNAME(13) / 'CONtour   ' /    
      DATA CNAME(14) / 'HESse     ' /    
      DATA CNAME(15) / 'SAVe      ' /    
      DATA CNAME(16) / 'IMProve   ' /    
      DATA CNAME(17) / 'CALl fcn  ' /    
      DATA CNAME(18) / 'STAndard  ' /    
      DATA CNAME(19) / 'END       ' /    
      DATA CNAME(20) / 'EXIt      ' /    
      DATA CNAME(21) / 'RETurn    ' /    
      DATA CNAME(22) / 'CLEar     ' /    
      DATA CNAME(23) / 'HELP      ' /    
      DATA CNAME(24) / 'MNContour ' /    
      DATA CNAME(25) / 'STOp      ' /    
      DATA CNAME(26) / 'JUMp      ' /    
       DATA NNAME/26/    
      DATA CNAME(27) / '          ' /    
      DATA CNAME(28) / '          ' /    
      DATA CNAME(29) / '          ' /    
      DATA CNAME(30) / '          ' /    
      DATA CNAME(31) / '          ' /    
      DATA CNAME(32) / '          ' /    
      DATA CNAME(33) / '          ' /    
C  obsolete commands:    
      DATA CNAME(34) / 'COVARIANCE' /    
      DATA CNAME(35) / 'PRINTOUT  ' /    
      DATA CNAME(36) / 'GRADIENT  ' /    
      DATA CNAME(37) / 'MATOUT    ' /    
      DATA CNAME(38) / 'ERROR DEF ' /    
      DATA CNAME(39) / 'LIMITS    ' /    
      DATA CNAME(40) / 'PUNCH     ' /    
      DATA NNTOT/40/     
C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15   
      DATA ISORT/ 17,22,13,19,20, 9,23,14,16,26, 4, 1, 5,24,11,  
     +            10,21,15,12, 2, 6, 3, 7,18,25, 8, 1, 1, 1, 1,  
     + 1,1,1,1,1,1,1,1,1,1/  
C    
      LK = LEN(COMAND)   
      IF (LK .GT. MAXCWD) LK=MAXCWD  
      CWORD = COMAND(1:LK)   
C           Copy the first MAXP arguments into COMMON (WORD7), making    
C           sure that WORD7(1)=0. if LLIST=0     
      DO 20 IW= 1, MAXP  
      WORD7(IW) = ZERO   
      IF (IW .LE. LLIST) WORD7(IW) = PLIST(IW)   
   20 CONTINUE   
      ICOMND = ICOMND + 1    
      NFCNLC = NFCN  
      IF (CWORD(1:7).NE.'SET PRI' .OR. WORD7(1).GE.0.)  THEN     
        IF (ISW(5) .GE. 0) THEN  
         LNOW = LLIST    
         IF (LNOW .GT. 4)  LNOW=4    
         WRITE (ISYSWR,25) ICOMND,CWORD(1:LK),(PLIST(I),I=1,LNOW)    
   25    FORMAT (1H ,10(1H*)/' **',I5,' **',A,4G12.4)    
         IF (LLIST .GT. LNOW) THEN   
           WRITE (CVBLNK,'(I2)') LK  
           C26 = '(11H **********,'//CVBLNK//'X,4G12.4)'     
           WRITE (ISYSWR,C26) (PLIST(I),I=LNOW+1,LLIST)  
         ENDIF   
         WRITE (ISYSWR, '(1H ,10(1H*))' )    
        ENDIF    
      ENDIF  
      NFCNMX = WORD7(1)  
      IF (NFCNMX .LE. 0)  NFCNMX = 200 + 100*NPAR + 5*NPAR**2    
      EPSI = WORD7(2)    
      IF (EPSI .LE. ZERO)  EPSI = 0.1 * UP   
      LNEWMN = .FALSE.   
      LPHEAD = .TRUE.    
      ISW(1) = 0     
      IERFLG = 0     
C                look for command in list CNAME . . . . . . . . . .  
      DO 80 I= 1, NNTOT  




      IF (CWORD(1:3) .EQ. CNAME(I)(1:3))  GO TO 90   
   80 CONTINUE   
      WRITE (ISYSWR,'(11X,''UNKNOWN COMMAND IGNORED:'',A)') COMAND   
      IERFLG = 2     
      GO TO 5000     
C                normal case: recognized MINUIT command . . . . . . .    




   90 CONTINUE   
      IF (CWORD(1:4) .EQ. 'MINO') I = 5  
      IF (I.NE.6 .AND. I.NE.7 .AND. I.NE.8 .AND. I.NE.23)  THEN  
         CFROM = CNAME(I)    
         NFCNFR = NFCN   




      ENDIF  
C              1    2    3    4    5    6    7    8    9   10    
      GO TO ( 400, 200, 300, 400, 500, 700, 700, 800, 900,1000,  




     1       1100,1200,1300,1400,1500,1600,1700,1800,1900,1900,  
     2       1900,2200,2300,2400,1900,2600,3300,3300,3300,3300,  
     3       3300,3300,3300,3400,3500,3600,3700,3800,3900,4000) , I  
C                                        . . . . . . . . . . seek    
  200 CALL MNSEEK     
      GO TO 5000     
C                                        . . . . . . . . . . simplex     
  300 CALL MNSIMP     
      GO TO 5000     
C                                        . . . . . . migrad, minimize    
  400 CONTINUE   
      NF = NFCN  
      APSI = EPSI    
      CALL MNMIGR     
      CALL MNWERR    
      IF (ISW(4) .GE. 1)         GO TO 5000  
      IF (ISW(1) .EQ. 1)         GO TO 5000  
      IF (CWORD(1:3) .EQ. 'MIG') GO TO 5000  
      NFCNMX = NFCNMX + NF - NFCN    
      NF = NFCN  
      CALL MNSIMP     
      IF (ISW(1) .EQ. 1)  GO TO 5000     
      NFCNMX = NFCNMX + NF - NFCN    
      CALL MNMIGR     
      CALL MNWERR    
      GO TO 5000     
C                                        . . . . . . . . . . minos   
  500 CONTINUE   
      NSUPER = NFCN + 2*(NPAR+1)*NFCNMX  
C          possible loop over new minima     
      EPSI = 0.1 * UP    
  510 CONTINUE   
      CALL MNCUVE     
      CALL MNMNOS     
      IF (.NOT. LNEWMN)  GO TO 5000  
      CALL MNRSET(0)     
      CALL MNMIGR     
      CALL MNWERR    
      IF (NFCN .LT. NSUPER)  GO TO 510   
      WRITE (ISYSWR,'(/'' TOO MANY FUNCTION CALLS. MINOS GIVES UP''/)')  
      IERFLG = 1     
      GO TO 5000     
C                                        . . . . . . . . . .set, show    
  700 CALL MNSET  
      GO TO 5000     
C                                        . . . . . . . . . . top of page     
  800 CONTINUE   
      WRITE (ISYSWR,'(1H1)')     
      GO TO 5000     
C                                        . . . . . . . . . . fix     
  900 LTOFIX = .TRUE.    
C                                        . . (also release) ....     
  901 CONTINUE   
      LFREED = .FALSE.   
      LFIXED = .FALSE.   
      IF (LLIST .EQ. 0)  THEN    
         WRITE (ISYSWR,'(A,A)') CWORD,':  NO PARAMETERS REQUESTED '  
         GO TO 5000  
      ENDIF  
      DO 950 ILIST= 1, LLIST     
      IEXT = PLIST(ILIST)    
      CHWHY = ' IS UNDEFINED.'   
      IF (IEXT .LE. 0)         GO TO 930     
      IF (IEXT .GT. NU)        GO TO 930     
      IF (NVARL(IEXT) .LT. 0)  GO TO 930     
      CHWHY = ' IS CONSTANT.  '  
      IF (NVARL(IEXT) .EQ. 0)  GO TO 930     
      IINT = NIOFEX(IEXT)    
      IF (LTOFIX) THEN   
         CHWHY = ' ALREADY FIXED.'   
         IF (IINT .EQ. 0)      GO TO 930     
         CALL MNFIXP(IINT,IERR)  
         IF (IERR .EQ. 0) THEN   
            LFIXED = .TRUE.  
         ELSE    
            IERFLG = 1   
         ENDIF   
      ELSE   
         CHWHY = ' ALREADY VARIABLE.'    
         IF (IINT .GT. 0)      GO TO 930     
         KRL = -IABS(IEXT)   
         CALL MNFREE(KRL)    
         LFREED = .TRUE.     
      ENDIF  
      GO TO 950  
  930 WRITE (ISYSWR,'(A,I4,A,A)') ' PARAMETER',IEXT,CHWHY,' IGNORED.'    
  950 CONTINUE   
      IF (LFREED .OR. LFIXED)  CALL MNRSET(0)    
      IF (LFREED)  THEN  
          ISW(2) = 0     
          DCOVAR = 1.    
          EDM = BIGEDM   
          ISW(4) = 0     
      ENDIF  
      CALL MNWERR    
      IF (ISW(5) .GT. 1)  CALL MNPRIN(5,AMIN)    
      GO TO 5000     
C                                        . . . . . . . . . . restore     
 1000 IT = WORD7(1)  
      IF (IT.GT.1 .OR. IT.LT.0)  GO TO 1005  
      LFREED = (NPFIX .GT. 0)    
      CALL MNFREE(IT)    
      IF (LFREED) THEN   
         CALL MNRSET(0)  
         ISW(2) = 0  
         DCOVAR = 1.     
         EDM = BIGEDM    
      ENDIF  
      GO TO 5000     
 1005 WRITE (ISYSWR,'(A,I4)') ' IGNORED.  UNKNOWN ARGUMENT:',IT  
      GO TO 5000     
C                                        . . . . . . . . . . release     
 1100 LTOFIX = .FALSE.   
      GO TO 901  
C                                       . . . . . . . . . . scan . . .   
 1200 CONTINUE   
      IEXT = WORD7(1)    
      IF (IEXT .LE. 0)  GO TO 1210   
      IT2 = 0    
      IF (IEXT .LE. NU)  IT2 = NIOFEX(IEXT)  
      IF (IT2 .LE. 0)  GO TO 1250    
 1210 CALL MNSCAN     
      GO TO 5000     
 1250 WRITE (ISYSWR,'(A,I4,A)') ' PARAMETER',IEXT,' NOT VARIABLE.'   
      GO TO 5000     
C                                        . . . . . . . . . . contour     
 1300 CONTINUE   
      KE1 = WORD7(1)     
      KE2 = WORD7(2)     
      IF (KE1 .EQ. 0)  THEN  
         IF (NPAR .EQ. 2)  THEN  
            KE1 = NEXOFI(1)  
            KE2 = NEXOFI(2)  
         ELSE    
            WRITE (ISYSWR,'(A,A)') CWORD,':  NO PARAMETERS REQUESTED '   
            GO TO 5000   
         ENDIF   
      ENDIF  
      NFCNMX = 1000  
      CALL MNCNTR(KE1,KE2,IERRF)   
      IERFLG = IERRF     
      GO TO 5000     
C                                        . . . . . . . . . . hesse   
 1400 CONTINUE   
      CALL MNHESS     
      CALL MNWERR    
      IF (ISW(5) .GE. 0)  CALL MNPRIN(2, AMIN)   
      IF (ISW(5) .GE. 1)  CALL MNMATU(1)     
      GO TO 5000     
C                                        . . . . . . . . . . save    
 1500 CONTINUE   
      CALL MNSAVE    
      GO TO 5000     
C                                        . . . . . . . . . . improve     
 1600 CONTINUE   
      CALL MNCUVE     
      CALL MNIMPR     
      IF (LNEWMN)  GO TO 400     
      GO TO 5000     
C                                        . . . . . . . . . . call fcn    
 1700 IFLAG = WORD7(1)   
      NPARX = NPAR   
      F = UNDEFI     
      CALL FCN(NPARX,GIN,F,U,IFLAG)    
      NFCN = NFCN + 1    
      NOWPRT = 0     
      IF (F .NE. UNDEFI)  THEN   
         IF (AMIN .EQ. UNDEFI)  THEN     
             AMIN = F    
             NOWPRT = 1  
         ELSE IF (F .LT. AMIN)  THEN     
             AMIN = F    
             NOWPRT = 1  
         ENDIF   
         IF (ISW(5).GE.0 .AND. IFLAG.LE.5 .AND. NOWPRT.EQ.1)     
     +          CALL MNPRIN(5,AMIN)  
         IF (IFLAG .EQ. 3)  FVAL3=F  
      ENDIF  
      IF (IFLAG .GT. 5)  CALL MNRSET(1)  
      GO TO 5000     
C                                        . . . . . . . . . . standard    
 1800 CALL STAND     
      GO TO 5000     
C                                       . . . . . . . stop, end, exit    
 1900 IT = PLIST(1)  
      IF (FVAL3 .EQ. AMIN .OR. IT .GT. 0)  GO TO 5000    
      IFLAG = 3  
      WRITE (ISYSWR,'(/A/)') ' CALL TO USER FUNCTION WITH IFLAG = 3'     
      NPARX = NPAR   
      CALL FCN(NPARX,GIN,F,U,IFLAG)    
      NFCN = NFCN + 1    
      GO TO 5000     
C                                        . . . . . . . . . . clear   
 2200 CONTINUE   
      CALL MNCLER    
      IF (ISW(5) .GE. 1)  WRITE (ISYSWR,'(A)')   
     + ' MINUIT MEMORY CLEARED. NO PARAMETERS NOW DEFINED.'  
      GO TO 5000     
C                                        . . . . . . . . . . help    
 2300 CONTINUE   
      IF (INDEX(CWORD,'SHO') .GT. 0)  GO TO 700  
      IF (INDEX(CWORD,'SET') .GT. 0)  GO TO 700  
      WRITE (ISYSWR,2301)  (CNAME(ISORT(I)),I=1,NNAME),'PARameters'  
 2301 FORMAT (' THE COMMANDS RECOGNIZED BY MINUIT ARE:'/6(2X,A10))   
      WRITE (ISYSWR,'(A)') ' see also: HELP SET and HELP SHOw'   
      GO TO 5000     
C                                       . . . . . . . . . . MNContour    
 2400 CONTINUE   
      EPSI = 0.05 * UP   
      KE1 = WORD7(1)     
      KE2 = WORD7(2)     
      IF (KE1.EQ.0 .AND. NPAR.EQ.2) THEN     
         KE1 = NEXOFI(1)     
         KE2 = NEXOFI(2)     
         ENDIF   
      NPTU = WORD7(3)    
      IF (NPTU .LE. 0)  NPTU=20  
      IF (NPTU .GT. MXPT)  NPTU = MXPT   
      NFCNMX =  100*(NPTU+5)*(NPAR+1)    
      CALL MNCONT(KE1,KE2,NPTU,XPTU,YPTU,IERRF)    
      GO TO 5000     
C                                      . . . . . . . . . . jump  
 2600 CONTINUE   
      STEP = WORD7(1)    
      IF (STEP .LE. ZERO)  STEP = 2.     
      RNO = 0.   
      IZERO = 0  
      DO 2620 I= 1, NPAR     
        CALL MNRN15(RNO,IZERO)   
        RNO = 2.0*RNO - 1.0  
 2620   X(I) = X(I) + RNO*STEP*WERR(I)   
      CALL MNINEX(X)     
      CALL MNAMIN     
      CALL MNRSET(0)     
      GO TO 5000     
C                                      . . . . . . . . . . blank line    
 3300 CONTINUE   
      WRITE (ISYSWR,'(10X,A)') ' BLANK COMMAND IGNORED.'     
      GO TO 5000     
C  . . . . . . . . obsolete commands     . . . . . . . . . . . . . .     
C                                      . . . . . . . . . . covariance    
 3400 CONTINUE   
      WRITE (ISYSWR, '(A)') ' THE "COVARIANCE" COMMAND IS OSBSOLETE.',   
     + ' THE COVARIANCE MATRIX IS NOW SAVED IN A DIFFERENT FORMAT',  
     + ' WITH THE "SAVE" COMMAND AND READ IN WITH:"SET COVARIANCE"'  
      GO TO 5000     
C                                        . . . . . . . . . . printout    
 3500 CONTINUE   
      CNEWAY = 'SET PRInt '  
      GO TO 3100     
C                                        . . . . . . . . . . gradient    
 3600 CONTINUE   
      CNEWAY = 'SET GRAd  '  
      GO TO 3100     
C                                        . . . . . . . . . . matout  
 3700 CONTINUE   
      CNEWAY = 'SHOW COVar'  
      GO TO 3100     
C                                        . . . . . . . . . error def     
 3800 CONTINUE   
      CNEWAY = 'SET ERRdef'  
      GO TO 3100     
C                                        . . . . . . . . . . limits  
 3900 CONTINUE   
      CNEWAY = 'SET LIMits'  
      GO TO 3100     
C                                        . . . . . . . . . . punch   
 4000 CONTINUE   
      CNEWAY = 'SAVE      '  
C                                ....... come from obsolete commands     
 3100 WRITE (ISYSWR, 3101) CWORD,CNEWAY  
 3101 FORMAT (' OBSOLETE COMMAND:',1X,A10,5X,'PLEASE USE:',1X,A10)   
      CWORD = CNEWAY     
      IF (CWORD .EQ. 'SAVE      ') GO TO 1500    
      GO TO 700  
C                                 . . . . . . . . . . . . . . . . . .    
 5000 RETURN     
      END    
