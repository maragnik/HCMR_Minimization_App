      SUBROUTINE MNSET    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Called from MNEXCM     
CC        Interprets the commands that start with SET and SHOW   
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
C        file characteristics for SET INPUT  
      LOGICAL LOPEN,LNAME    
      CHARACTER*1 CANSWR     
      CHARACTER CFNAME*64, CMODE*16  
C       'SET ' or 'SHOW',  'ON ' or 'OFF', 'SUPPRESSED' or 'REPORTED  '  
      CHARACTER CKIND*4,    COPT*3,         CWARN*10     
C        explanation of print level numbers -1:3  and strategies 0:2     
      CHARACTER CPRLEV(-1:3)*34 ,CSTRAT(0:2)*44  
C        identification of debug options     
      PARAMETER (NUMDBG = 6)     
      CHARACTER*40 CDBOPT(0:NUMDBG)  
C        things that can be set or shown     
      CHARACTER*10 CNAME(30)     
      DATA CNAME( 1)/'FCN value '/   
      DATA CNAME( 2)/'PARameters'/   
      DATA CNAME( 3)/'LIMits    '/   
      DATA CNAME( 4)/'COVariance'/   
      DATA CNAME( 5)/'CORrelatio'/   
      DATA CNAME( 6)/'PRInt levl'/   
      DATA CNAME( 7)/'NOGradient'/   
      DATA CNAME( 8)/'GRAdient  '/   
      DATA CNAME( 9)/'ERRor def '/   
      DATA CNAME(10)/'INPut file'/   
      DATA CNAME(11)/'WIDth page'/   
      DATA CNAME(12)/'LINes page'/   
      DATA CNAME(13)/'NOWarnings'/   
      DATA CNAME(14)/'WARnings  '/   
      DATA CNAME(15)/'RANdom gen'/   
      DATA CNAME(16)/'TITle     '/   
      DATA CNAME(17)/'STRategy  '/   
      DATA CNAME(18)/'EIGenvalue'/   
      DATA CNAME(19)/'PAGe throw'/   
      DATA CNAME(20)/'MINos errs'/   
      DATA CNAME(21)/'EPSmachine'/   
      DATA CNAME(22)/'OUTputfile'/   
      DATA CNAME(23)/'BATch     '/   
      DATA CNAME(24)/'INTeractiv'/   
          DATA NNAME/24/     
C        options not intended for normal users   
      DATA CNAME(25)/'reserve   '/   
      DATA CNAME(26)/'reserve   '/   
      DATA CNAME(27)/'NODebug   '/   
      DATA CNAME(28)/'DEBug     '/   
      DATA CNAME(29)/'SHOw      '/   
      DATA CNAME(30)/'SET       '/   
          DATA NNTOT/30/     
C    
      DATA CPRLEV(-1)/'-1: NO OUTPUT EXCEPT FROM "SHOW"  '/  
      DATA CPRLEV( 0)/' 0: REDUCED OUTPUT                '/  
      DATA CPRLEV( 1)/' 1: NORMAL OUTPUT                 '/  
      DATA CPRLEV( 2)/' 2: EXTRA OUTPUT FOR PROBLEM CASES'/  
      DATA CPRLEV( 3)/' 3: MAXIMUM OUTPUT                '/  
C    
      DATA CSTRAT( 0)/' 0: MINIMIZE THE NUMBER OF CALLS TO FUNCTION'/    
      DATA CSTRAT( 1)/' 1: TRY TO BALANCE SPEED AGAINST RELIABILITY'/    
      DATA CSTRAT( 2)/' 2: MAKE SURE MINIMUM TRUE, ERRORS CORRECT  '/    
C    
      DATA CDBOPT(0)/'REPORT ALL EXCEPTIONAL CONDITIONS      '/  
      DATA CDBOPT(1)/'MNLINE: LINE SEARCH MINIMIZATION       '/  
      DATA CDBOPT(2)/'MNDERI: FIRST DERIVATIVE CALCULATIONS  '/  
      DATA CDBOPT(3)/'MNHESS: SECOND DERIVATIVE CALCULATIONS '/  
      DATA CDBOPT(4)/'MNMIGR: COVARIANCE MATRIX UPDATES      '/  
      DATA CDBOPT(5)/'MNHES1: FIRST DERIVATIVE UNCERTAINTIES '/  
      DATA CDBOPT(6)/'MNCONT: MNCONTOUR PLOT (MNCROS SEARCH) '/  
C    
C    
      DO 2 I= 1, NNTOT   
      IF (INDEX(CWORD(4:10),CNAME(I)(1:3)) .GT. 0)  GO TO 5  
    2 CONTINUE   
      I = 0  
    5 KNAME = I  
C    
C           Command could be SET xxx, SHOW xxx,  HELP SET or HELP SHOW   
      IF (INDEX(CWORD(1:4),'HEL') .GT. 0)  GO TO 2000    
      IF (INDEX(CWORD(1:4),'SHO') .GT. 0)  GO TO 1000    
      IF (INDEX(CWORD(1:4),'SET') .EQ. 0)  GO TO 1900    
C                           ---  
      CKIND = 'SET '     
C                                        . . . . . . . . . . set unknown     
      IF (KNAME .LE. 0)  GO TO 1900  
C                                        . . . . . . . . . . set known   
      GO TO(3000,  20,  30,  40,3000,  60,  70,  80,  90, 100,   
     +       110, 120, 130, 140, 150, 160, 170,3000, 190,3000,   
     +       210, 220, 230, 240,1900,1900, 270, 280, 290, 300) , KNAME   
C    
C                                        . . . . . . . . . . set param   
   20 CONTINUE   
      IPRM = WORD7(1)    
      IF (IPRM .GT. NU)  GO TO 25    
      IF (IPRM .LE. 0)   GO TO 25    
      IF (NVARL(IPRM) .LT. 0)  GO TO 25  
      U(IPRM) = WORD7(2)     
      CALL MNEXIN(X)     
      ISW2 = ISW(2)  
      CALL MNRSET(1)     
C        Keep approximate covariance matrix, even if new param value     
      ISW(2) = MIN(ISW2,1)   
      CFROM = 'SET PARM'     
      NFCNFR = NFCN  
      CSTATU = 'NEW VALUES'  
      GO TO 4000     
   25 WRITE (ISYSWR,'(A/)') ' UNDEFINED PARAMETER NUMBER.  IGNORED.'     
      GO TO 4000     
C                                        . . . . . . . . . . set limits  
   30 CALL MNLIMS     
      GO TO 4000     
C                                        . . . . . . . . . . set covar   
   40 CONTINUE   
C   this command must be handled by MNREAD, and is not Fortran-callable  
      GO TO 3000     
C                                        . . . . . . . . . . set print   
   60 ISW(5) = WORD7(1)  
      GO TO 4000     
C                                        . . . . . . . . . . set nograd  
   70 ISW(3) = 0     
      GO TO 4000     
C                                        . . . . . . . . . . set grad    
   80 CALL MNGRAD     
      GO TO 4000     
C                                        . . . . . . . . . . set errdef  
   90 IF (WORD7(1) .EQ. UP)  GO TO 4000  
      IF (WORD7(1) .LE. ZERO)  THEN  
         IF (UP .EQ. UPDFLT)  GO TO 4000     
         UP = UPDFLT     
      ELSE   
         UP = WORD7(1)   
      ENDIF  
      DO 95 I= 1, NPAR   
      ERN(I) = 0.    
   95 ERP(I) = 0.    
      CALL MNWERR    
      GO TO 4000     
C                                        . . . . . . . . . . set input   
C This command must be handled by MNREAD. If it gets this far,   
C         it is illegal.     
  100 CONTINUE   
      GO TO 3000     
C                                        . . . . . . . . . . set width   
  110 NPAGWD = WORD7(1)  
      NPAGWD = MAX(NPAGWD,50)    
      GO TO 4000     


C                                        . . . . . . . . . . set lines   
  120 NPAGLN = WORD7(1)  
      GO TO 4000     
C                                        . . . . . . . . . . set nowarn  
  130 LWARN = .FALSE.    
      GO TO 4000     
C                                        . . . . . . . . . . set warn    
  140 LWARN = .TRUE.     
      CALL MNWARN('W','SHO','SHO')   
      GO TO 4000     
C                                        . . . . . . . . . . set random  
  150 JSEED = INT(WORD7(1))  
      VAL = 3.   
      CALL MNRN15(VAL, JSEED)    
      IF (ISW(5) .GT. 0) WRITE (ISYSWR, 151) JSEED   
  151 FORMAT (' MINUIT RANDOM NUMBER SEED SET TO ',I10)  
      GO TO 4000     
C                                        . . . . . . . . . . set title   
  160 CONTINUE   
C   this command must be handled by MNREAD, and is not Fortran-callable  
      GO TO 3000     
C                                        . . . . . . . . . set strategy  
  170 ISTRAT = WORD7(1)  
      ISTRAT = MAX(ISTRAT,0)     
      ISTRAT = MIN(ISTRAT,2)     
      IF (ISW(5) .GT. 0)  GO TO 1172     
      GO TO 4000     
C                                       . . . . . . . . . set page throw     
  190 NEWPAG = WORD7(1)  
      GO TO 1190     
C                                        . . . . . . . . . . set epsmac  
  210 IF (WORD7(1).GT.ZERO .AND. WORD7(1).LT.0.1) EPSMAC = WORD7(1)  
      EPSMA2 = DSQRT(EPSMAC)  
      GO TO 1210     
C                                        . . . . . . . . . . set outputfile  
  220 CONTINUE   
      IUNIT = WORD7(1)   
      ISYSWR = IUNIT     
      ISTKWR(1) = IUNIT  
      IF (ISW(5) .GE. 0) GO TO 1220  
      GO TO 4000     
C                                        . . . . . . . . . . set batch   
  230 ISW(6) = 0     
      IF (ISW(5) .GE. 0)  GO TO 1100     
      GO TO 4000     
C                                        . . . . . . . . . . set interactive  
  
  240 ISW(6) = 1     
      IF (ISW(5) .GE. 0)  GO TO 1100     
      GO TO 4000     
C                                        . . . . . . . . . . set nodebug     
  270 ISET = 0   
      GO TO 281  
C                                        . . . . . . . . . . set debug   
  280 ISET = 1   
  281 CONTINUE   
      IDBOPT = WORD7(1)  
      IF (IDBOPT .GT. NUMDBG) GO TO 288  
      IF (IDBOPT .GE. 0) THEN    
          IDBG(IDBOPT) = ISET    
          IF (ISET .EQ. 1)  IDBG(0) = 1  
      ELSE   
C             SET DEBUG -1  sets all debug options   
          DO 285 ID= 0, NUMDBG   
  285     IDBG(ID) = ISET    
      ENDIF  
      LREPOR = (IDBG(0) .GE. 1)  
      CALL MNWARN('D','SHO','SHO')   
      GO TO 4000     
  288 WRITE (ISYSWR,289) IDBOPT  
  289 FORMAT (' UNKNOWN DEBUG OPTION',I6,' REQUESTED. IGNORED')  
      GO TO 4000     
C                                        . . . . . . . . . . set show    
  290 CONTINUE   
C                                        . . . . . . . . . . set set     
  300 CONTINUE   
      GO TO 3000     
C                -----------------------------------------------------   
 1000 CONTINUE   
C               at this point, CWORD must be 'SHOW'  
      CKIND = 'SHOW'     
      IF (KNAME .LE. 0)  GO TO 1900  
      GO TO (1010,1020,1030,1040,1050,1060,1070,1070,1090,1100,  
     +       1110,1120,1130,1130,1150,1160,1170,1180,1190,1200,  
     +       1210,1220,1100,1100,1900,1900,1270,1270,1290,1300),KNAME    
C    
C                                        . . . . . . . . . . show fcn    
 1010 CONTINUE   
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      CALL MNPRIN (0,AMIN)   
      GO TO 4000     
C                                        . . . . . . . . . . show param  
 1020 CONTINUE   
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      CALL MNPRIN (5,AMIN)   
      GO TO 4000     
C                                        . . . . . . . . . . show limits     
 1030 CONTINUE   
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      CALL MNPRIN (1,AMIN)   
      GO TO 4000     
C                                        . . . . . . . . . . show covar  
 1040 CALL MNMATU(1)     
      GO TO 4000     
C                                        . . . . . . . . . . show corre  
 1050 CALL MNMATU(0)     
      GO TO 4000     
C                                        . . . . . . . . . . show print  
 1060 CONTINUE   
      IF (ISW(5) .LT.-1)  ISW(5) = -1    
      IF (ISW(5) .GT. 3)  ISW(5) = 3     
      WRITE (ISYSWR,'(A)') ' ALLOWED PRINT LEVELS ARE:'  
      WRITE (ISYSWR,'(27X,A)') CPRLEV    
      WRITE (ISYSWR,1061)  CPRLEV(ISW(5))    
 1061 FORMAT (/' CURRENT PRINTOUT LEVEL IS ',A)  
      GO TO 4000     
C                                        . . . . . . . show nograd, grad     
 1070 CONTINUE   
      IF (ISW(3) .LE. 0) THEN    
         WRITE (ISYSWR, 1081)    
 1081    FORMAT(' NOGRAD IS SET.  DERIVATIVES NOT COMPUTED IN FCN.')     
      ELSE   
         WRITE (ISYSWR, 1082)    
 1082    FORMAT('   GRAD IS SET.  USER COMPUTES DERIVATIVES IN FCN.')    
      ENDIF  
      GO TO 4000     
C                                       . . . . . . . . . . show errdef  
 1090 WRITE (ISYSWR, 1091)  UP   
 1091 FORMAT (' ERRORS CORRESPOND TO FUNCTION CHANGE OF',G13.5)  
      GO TO 4000     
C                                       . . . . . . . . . . show input,  
C                                                batch, or interactive   
 1100 CONTINUE   
      INQUIRE(UNIT=ISYSRD,OPENED=LOPEN,NAMED=LNAME,NAME=CFNAME)  
      CMODE = 'BATCH MODE      '     
      IF (ISW(6) .EQ. 1)  CMODE = 'INTERACTIVE MODE'     
      IF (.NOT. LNAME)  CFNAME='unknown'     
      WRITE (ISYSWR,1002) CMODE,ISYSRD,CFNAME    
 1002 FORMAT (' INPUT NOW BEING READ IN ',A,' FROM UNIT NO.',I3/     
     + ' FILENAME: ',A)  
      GO TO 4000     
C                                       . . . . . . . . . . show width   
 1110 WRITE (ISYSWR,1111) NPAGWD     
 1111 FORMAT (10X,'PAGE WIDTH IS SET TO',I4,' COLUMNS')  
      GO TO 4000     
C                                       . . . . . . . . . . show lines   
 1120 WRITE (ISYSWR,1121) NPAGLN     
 1121 FORMAT (10X,'PAGE LENGTH IS SET TO',I4,' LINES')   
      GO TO 4000     
C                                       . . . . . . .show nowarn, warn   
 1130 CONTINUE   
                 CWARN = 'SUPPRESSED'    
      IF (LWARN) CWARN = 'REPORTED  '    
      WRITE (ISYSWR,1141) CWARN  
 1141 FORMAT (' MINUIT WARNING MESSAGES ARE ',A)     
      IF (.NOT. LWARN) CALL MNWARN('W','SHO','SHO')  
      GO TO 4000     
C                                      . . . . . . . . . . show random   
 1150 VAL = 0.   
      CALL MNRN15(VAL,IGRAIN)    
      IKSEED = IGRAIN    
      WRITE (ISYSWR, 1151)  IKSEED   
 1151 FORMAT (' MINUIT RNDM SEED IS CURRENTLY=',I10/)    
      VAL = 3.0  
      ISEED = IKSEED     
      CALL MNRN15(VAL,ISEED)     
      GO TO 4000     
C                                        . . . . . . . . . show title    
 1160 WRITE (ISYSWR,'(A,A)') ' TITLE OF CURRENT TASK IS:',CTITL  
      GO TO 4000     
C                                        . . . . . . . show strategy     
 1170 WRITE (ISYSWR, '(A)') ' ALLOWED STRATEGIES ARE:'   
      WRITE (ISYSWR, '(20X,A)') CSTRAT   
 1172 WRITE (ISYSWR, 1175) CSTRAT(ISTRAT)    
 1175 FORMAT (/' NOW USING STRATEGY ',A/)    
      GO TO 4000     
C                                          . . . . . show eigenvalues    
 1180 CONTINUE   
      ISWSAV = ISW(5)    
      ISW(5) = 3     
      IF (ISW(2) .LT. 1)  THEN   
         WRITE (ISYSWR,'(1X,A)') COVMES(0)   
      ELSE   
         CALL MNPSDF     
      ENDIF  
      ISW(5) = ISWSAV    
      GO TO 4000     
C                                            . . . . . show page throw   
 1190 WRITE (ISYSWR,'(A,I3)') ' PAGE THROW CARRIAGE CONTROL =',NEWPAG    
      IF (NEWPAG .EQ. 0)     
     +    WRITE (ISYSWR,'(A)') ' NO PAGE THROWS IN MINUIT OUTPUT'    
      GO TO 4000     
C                                        . . . . . . show minos errors   
 1200 CONTINUE   
      DO 1202 II= 1, NPAR    
      IF (ERP(II).GT.ZERO .OR. ERN(II).LT.ZERO)  GO TO 1204  
 1202 CONTINUE   
      WRITE (ISYSWR,'(A)')   
     +   '       THERE ARE NO MINOS ERRORS CURRENTLY VALID.'     
      GO TO 4000     
 1204 CONTINUE   
      CALL MNPRIN(4,AMIN)    
      GO TO 4000     
C                                        . . . . . . . . . show epsmac   
 1210 WRITE (ISYSWR,'(A,E12.3)')     
     +  ' FLOATING-POINT NUMBERS ASSUMED ACCURATE TO',EPSMAC     
      GO TO 4000     
C                                        . . . . . . show outputfiles    
 1220 CONTINUE   
      WRITE (ISYSWR,'(A,I4)') '  MINUIT PRIMARY OUTPUT TO UNIT',ISYSWR   
      GO TO 4000     
C                                        . . . . . . show nodebug, debug     
 1270 CONTINUE   
      DO 1285 ID= 0, NUMDBG  
      COPT = 'OFF'   
      IF (IDBG(ID) .GE. 1)  COPT = 'ON '     
 1285 WRITE (ISYSWR,1286) ID, COPT, CDBOPT(ID)   
 1286 FORMAT (10X,'DEBUG OPTION',I3,' IS ',A3,' :',A)    
      IF (.NOT. LREPOR) CALL MNWARN('D','SHO','SHO')     
      GO TO 4000     
C                                        . . . . . . . . . . show show   
 1290 CKIND = 'SHOW'     
      GO TO 2100     
C                                        . . . . . . . . . . show set    
 1300 CKIND = 'SET '     
      GO TO 2100     
     
C                -----------------------------------------------------   
C                              UNKNOWN COMMAND   
 1900 WRITE (ISYSWR, 1901) CWORD     
 1901 FORMAT (' THE COMMAND:',A10,' IS UNKNOWN.'/)   
      GO TO 2100     
C                -----------------------------------------------------   
C                    HELP SHOW,  HELP SET,  SHOW SET, or SHOW SHOW   
 2000 CKIND = 'SET '     
      IF (INDEX(CWORD(4:10),'SHO') .GT. 0)  CKIND = 'SHOW'   
 2100 WRITE (ISYSWR, 2101)  CKIND,CKIND, (CNAME(KK),KK=1,NNAME)  
 2101 FORMAT (' THE FORMAT OF THE ',A4,' COMMAND IS:'//  
     +   1X,A4,' xxx    [numerical arguments if any]'//  
     +   ' WHERE xxx MAY BE ONE OF THE FOLLOWING:'/  
     +   (7X,6A12))  
      GO TO 4000     
C                -----------------------------------------------------   
C                               ILLEGAL COMMAND  
 3000 WRITE (ISYSWR,'('' ABOVE COMMAND IS ILLEGAL.   IGNORED'')')    
 4000 RETURN     
      END    
      SUBROUTINE MNSETI(TIT)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       Called by user to set or change title of current task.  
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
      CHARACTER*(*) TIT  
      CTITL = TIT    
      RETURN     
      END    
      SUBROUTINE MNSIMP   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Performs a minimization using the simplex method of Nelder     
CC        and Mead (ref. -- Comp. J. 7,308 (1965)).  
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
      DIMENSION Y(MNI+1)     
      DATA ALPHA,BETA,GAMMA,RHOMIN,RHOMAX / 1.0, 0.5, 2.0, 4.0, 8.0/     
      IF (NPAR .LE. 0)  RETURN   
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN  
      CFROM = 'SIMPLEX '     
      NFCNFR = NFCN  
      CSTATU= 'UNCHANGED '   
      NPFN=NFCN  
      NPARP1=NPAR+1  
      NPARX = NPAR   
      RHO1 = 1.0 + ALPHA     
      RHO2 = RHO1 + ALPHA*GAMMA  
      WG = 1.0/FLOAT(NPAR)   
      IF (ISW(5) .GE. 0) WRITE(ISYSWR,100) EPSI  
  100 FORMAT(' START SIMPLEX MINIMIZATION.    CONVERGENCE WHEN EDM .LT.'     
     +,E10.2 )   
         DO 2 I= 1, NPAR     
         DIRIN(I) = WERR(I)  
           CALL MNDXDI(X(I),I,DXDI)  
           IF (DXDI .NE. ZERO) DIRIN(I)=WERR(I)/DXDI     
         DMIN = EPSMA2*DABS(X(I))     
         IF (DIRIN(I) .LT. DMIN)  DIRIN(I)=DMIN  
    2    CONTINUE    
C**       choose the initial simplex using single-parameter searches     
    1 CONTINUE   
      YNPP1 = AMIN   
      JL = NPARP1    
      Y(NPARP1) = AMIN   
      ABSMIN = AMIN  
      DO 10 I= 1, NPAR   
      AMING = AMIN   
      PBAR(I) = X(I)     
      BESTX = X(I)   
      KG = 0     
      NS = 0     
      NF = 0     
    4 X(I) = BESTX + DIRIN(I)    
      CALL MNINEX(X)     
      CALL FCN(NPARX,GIN, F, U, 4)    
      NFCN = NFCN + 1    
      IF (F .LE. AMING)  GO TO 6     
C         failure    
      IF (KG .EQ. 1)  GO TO 8    
      KG = -1    
      NF = NF + 1    
      DIRIN(I) = DIRIN(I) * (-0.4)   
      IF (NF .LT. 3)  GO TO 4    
      NS = 6     
C         success    
    6 BESTX = X(I)   
      DIRIN(I) = DIRIN(I) * 3.0  
      AMING = F  
      CSTATU= 'PROGRESS  '   
      KG = 1     
      NS = NS + 1    
      IF (NS .LT. 6)  GO TO 4    
C         local minimum found in ith direction   
    8 Y(I) = AMING   
      IF (AMING .LT. ABSMIN)  JL = I     
      IF (AMING .LT. ABSMIN)  ABSMIN = AMING     
      X(I) = BESTX   
      DO 9 K= 1, NPAR    
    9 P(K,I) = X(K)  
   10 CONTINUE   
      JH = NPARP1    
      AMIN=Y(JL)     
      CALL MNRAZZ(YNPP1,PBAR,Y,JH,JL)    
      DO 20 I= 1, NPAR   
   20 X(I) = P(I,JL)     
      CALL MNINEX(X)     
      CSTATU = 'PROGRESS  '  
      IF (ISW(5) .GE. 1)  CALL MNPRIN(5,AMIN)    
      EDM = BIGEDM   
      SIG2 = EDM     
      NCYCL=0    
C                                        . . . . .  start main loop  
   50 CONTINUE   
      IF (SIG2 .LT. EPSI .AND. EDM.LT.EPSI)     GO TO 76     
      SIG2 = EDM     
      IF ((NFCN-NPFN) .GT. NFCNMX)  GO TO 78     
C         calculate new point * by reflection    
      DO 60 I= 1, NPAR   
      PB = 0.    
      DO 59 J= 1, NPARP1     
   59 PB = PB + WG * P(I,J)  
      PBAR(I) = PB - WG * P(I,JH)    
   60 PSTAR(I)=(1.+ALPHA)*PBAR(I)-ALPHA*P(I,JH)  
      CALL MNINEX(PSTAR)     
      CALL FCN(NPARX,GIN,YSTAR,U,4)    
      NFCN=NFCN+1    
      IF(YSTAR.GE.AMIN) GO TO 70     
C         point * better than jl, calculate new point **     
      DO 61 I=1,NPAR     
   61 PSTST(I)=GAMMA*PSTAR(I)+(1.-GAMMA)*PBAR(I)     
      CALL MNINEX(PSTST)     
      CALL FCN(NPARX,GIN,YSTST,U,4)    
      NFCN=NFCN+1    
C         try a parabola through ph, pstar, pstst.  min = prho   
      Y1 = (YSTAR-Y(JH)) * RHO2  
      Y2 = (YSTST-Y(JH)) * RHO1  
      RHO = 0.5 * (RHO2*Y1 -RHO1*Y2) / (Y1 -Y2)  
      IF (RHO .LT. RHOMIN)  GO TO 66     
      IF (RHO .GT. RHOMAX)  RHO = RHOMAX     
      DO 64 I= 1, NPAR   
   64 PRHO(I) = RHO*PBAR(I) + (1.0-RHO)*P(I,JH)  
      CALL MNINEX(PRHO)  
      CALL FCN(NPARX,GIN,YRHO, U,4)    
      NFCN = NFCN + 1    
      IF (YRHO .LT. Y(JL) .AND. YRHO .LT. YSTST)  GO TO 65   
      IF (YSTST .LT. Y(JL))  GO TO 67    
      IF (YRHO .GT. Y(JL))  GO TO 66     
C         accept minimum point of parabola, PRHO     
   65 CALL MNRAZZ (YRHO,PRHO,Y,JH,JL)    
      GO TO 68   
   66 IF (YSTST .LT. Y(JL))  GO TO 67    
      CALL MNRAZZ(YSTAR,PSTAR,Y,JH,JL)   
      GO TO 68   
   67 CALL MNRAZZ(YSTST,PSTST,Y,JH,JL)   
   68 NCYCL=NCYCL+1  
      IF (ISW(5) .LT. 2)  GO TO 50   
      IF (ISW(5) .GE. 3 .OR. MOD(NCYCL, 10) .EQ. 0) CALL MNPRIN(5,AMIN)  
      GO TO 50   
C         point * is not as good as jl   
   70 IF (YSTAR .GE. Y(JH))  GO TO 73    
      JHOLD = JH     
      CALL MNRAZZ(YSTAR,PSTAR,Y,JH,JL)   
      IF (JHOLD .NE. JH)  GO TO 50   
C         calculate new point **     
   73 DO 74 I=1,NPAR     
   74 PSTST(I)=BETA*P(I,JH)+(1.-BETA)*PBAR(I)    
      CALL MNINEX (PSTST)    
      CALL FCN(NPARX,GIN,YSTST,U,4)    
      NFCN=NFCN+1    
      IF(YSTST.GT.Y(JH)) GO TO 1     
C     point ** is better than jh     
      IF (YSTST .LT. AMIN)  GO TO 67     
      CALL MNRAZZ(YSTST,PSTST,Y,JH,JL)   
      GO TO 50   
C                                        . . . . . .  end main loop  
   76 IF (ISW(5) .GE. 0)  WRITE(ISYSWR,'(A)')    
     +                    ' SIMPLEX MINIMIZATION HAS CONVERGED.'     
      ISW(4) = 1     
      GO TO 80   
   78 IF (ISW(5) .GE. 0)  WRITE(ISYSWR,'(A)')    
     +                    ' SIMPLEX TERMINATES WITHOUT CONVERGENCE.'     
      CSTATU= 'CALL LIMIT'   
      ISW(4) = -1    
      ISW(1) = 1     
   80 DO 82 I=1,NPAR     
      PB = 0.    
      DO 81 J=1,NPARP1   
   81 PB = PB + WG * P(I,J)  
   82 PBAR(I) = PB - WG * P(I,JH)    
      CALL MNINEX(PBAR)  
      CALL FCN(NPARX,GIN,YPBAR,U,4)    
      NFCN=NFCN+1    
      IF (YPBAR .LT. AMIN)  CALL MNRAZZ(YPBAR,PBAR,Y,JH,JL)  
      CALL MNINEX(X)     
      IF (NFCNMX+NPFN-NFCN .LT. 3*NPAR)  GO TO 90    
      IF (EDM .GT. 2.0*EPSI)  GO TO 1    
   90 IF (ISW(5) .GE. 0)  CALL MNPRIN(5, AMIN)   
      RETURN     
      END    
      SUBROUTINE MNSTAT(FMIN,FEDM,ERRDEF,NPARI,NPARX,ISTAT)  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC       User-called     
CC       Provides the user with information concerning the current status    
CC          of the current minimization. Namely, it returns:     
CC        FMIN: the best function value found so far     
CC        FEDM: the estimated vertical distance remaining to minimum     
CC        ERRDEF: the value of UP defining parameter uncertainties   
CC        NPARI: the number of currently variable parameters     
CC        NPARX: the highest (external) parameter number defined by user     
CC        ISTAT: a status integer indicating how good is the covariance  
CC           matrix:  0= not calculated at all   


CC                    1= approximation only, not accurate    




CC                    2= full matrix, but forced positive-definite   




CC                    3= full accurate covariance matrix     
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
      FMIN = AMIN    
      FEDM = EDM     
      ERRDEF = UP    
      NPARI = NPAR   
      NPARX = NU     
      ISTAT = ISW(2)     
        IF (EDM  .EQ. BIGEDM)  THEN  
            FEDM = UP    
        ENDIF    
        IF (AMIN .EQ. UNDEFI)  THEN  
            FMIN = 0.0   
            FEDM = UP    
            ISTAT= 0     
        ENDIF    
      RETURN     
      END    
      SUBROUTINE MNSTIN(CRDBUF,IERR)     
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC Called from MNREAD.   
CC Implements the SET INPUT command to change input units.   
CC If command is: 'SET INPUT'   'SET INPUT 0'   or  '*EOF',  
CC                 or 'SET INPUT , ,  ',     
CC                reverts to previous input unit number,if any.  
CC   
CC      If it is: 'SET INPUT n'  or  'SET INPUT n filename',     
CC                changes to new input file, added to stack  
CC   
CC      IERR = 0: reading terminated normally    
CC             2: end-of-data on primary input file  
CC             3: unrecoverable read error   
CC             4: unable to process request  
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
      CHARACTER CRDBUF*(*),CUNIT*10,CFNAME*64,CGNAME*64,CANSWR*1     
      CHARACTER CMODE*16     
      LOGICAL LOPEN,LREWIN,NONAME,LNAME,MNUNPT   
      NONAME = .TRUE.    
      IERR = 0   
      IF (INDEX(CRDBUF,'*EOF') .EQ. 1) GO TO 190     
      IF (INDEX(CRDBUF,'*eof') .EQ. 1) GO TO 190     
      LEND = LEN(CRDBUF)     
C                               look for end of SET INPUT command    
        DO 20 IC= 8,LEND     
        IF (CRDBUF(IC:IC) .EQ. ' ') GO TO 25     
        IF (CRDBUF(IC:IC) .EQ. ',') GO TO 53     
   20   CONTINUE     
      GO TO 200  
   25 CONTINUE   
C         look for end of separator between command and first argument   
      ICOL = IC+1    
         DO 50 IC= ICOL,LEND     
         IF (CRDBUF(IC:IC) .EQ. ' ') GO TO 50    
         IF (CRDBUF(IC:IC) .EQ. ',') GO TO 53    
         GO TO 55    
   50 CONTINUE   
      GO TO 200  
   53 IC = IC + 1    
   55 IC1 = IC   
C                      see if "REWIND" was requested in command  
      LREWIN = .FALSE.   
      IF (INDEX(CRDBUF(1:IC1),'REW') .GT. 5)  LREWIN=.TRUE.  
      IF (INDEX(CRDBUF(1:IC1),'rew') .GT. 5)  LREWIN=.TRUE.  
C                      first argument begins in or after col IC1     
      DO 75 IC= IC1,LEND     
      IF (CRDBUF(IC:IC) .EQ. ' ') GO TO 75   
      IF (CRDBUF(IC:IC) .EQ. ',') GO TO 200  
      GO TO 80   
   75 CONTINUE   
      GO TO 200  
   80 IC1 = IC   
C                        first argument really begins in col IC1     
      DO 100 IC= IC1+1,LEND  
      IF (CRDBUF(IC:IC) .EQ. ' ') GO TO 108  
      IF (CRDBUF(IC:IC) .EQ. ',') GO TO 108  
  100 CONTINUE   
      IC = LEND + 1  
  108 IC2 = IC-1     
C                            end of first argument is in col IC2     
  110 CONTINUE   
      CUNIT = CRDBUF(IC1:IC2)    
      WRITE (ISYSWR,'(A,A)') ' UNIT NO. :',CUNIT     
      READ (CUNIT,'(BN,F10.0)',ERR=500) FUNIT    
      IUNIT = FUNIT  
      IF (IUNIT .EQ. 0)  GO TO 200   
C                             skip blanks and commas, find file name     
      DO 120 IC= IC2+1,LEND  
      IF (CRDBUF(IC:IC) .EQ. ' ') GO TO 120  
      IF (CRDBUF(IC:IC) .EQ. ',') GO TO 120  
      GO TO 130  
  120 CONTINUE   
      GO TO 131  
  130 CONTINUE   
      CFNAME = CRDBUF(IC:LEND)   
      NONAME = .FALSE.   
      WRITE (ISYSWR, '(A,A)') ' FILE NAME IS:',CFNAME    
C              ask if file exists, if not ask for name and open it   
  131 CONTINUE   
      INQUIRE(UNIT=IUNIT,OPENED=LOPEN,NAMED=LNAME,NAME=CGNAME)   
      IF (LOPEN) THEN    
         IF (NONAME) THEN    
             GO TO 136   
         ELSE    
             IF (.NOT.LNAME) CGNAME='unknown'    
             WRITE (ISYSWR,132) IUNIT,CGNAME,CFNAME  
  132        FORMAT (' UNIT',I3,' ALREADY OPENED WITH NAME:',A/  
     +                  '                 NEW NAME IGNORED:',A)  
         ENDIF   
      ELSE   
C                new file, open it   


         WRITE (ISYSWR,135) IUNIT    
  135    FORMAT (' UNIT',I3,' IS NOT OPENED.')   
         IF (NONAME) THEN    
            WRITE (ISYSWR,'(A)') ' NO FILE NAME GIVEN IN COMMAND.'   
            IF (ISW(6) .NE. 1)  GO TO 800    
            WRITE (ISYSWR,'(A)') ' PLEASE GIVE FILE NAME:'   
            READ (ISYSRD,'(A)') CFNAME   
         ENDIF   
         OPEN (UNIT=IUNIT,FILE=CFNAME,STATUS='OLD',ERR=600)  
         WRITE (ISYSWR,'(A)') ' FILE OPENED SUCCESSFULLY.'   
      ENDIF  
C                                     . .   file is correctly opened     
  136 IF (LREWIN) GO TO 150  
      IF (ISW(6) .NE. 1)  GO TO 300  
      WRITE (ISYSWR,137)  IUNIT  
  137 FORMAT (' SHOULD UNIT',I3,' BE REWOUND?' )     
      READ  (ISYSRD,'(A)')  CANSWR   
      IF (CANSWR.NE.'Y' .AND. CANSWR.NE.'y') GO TO 300   
  150 REWIND IUNIT   
      GO TO 300  
C                      *EOF  
  190 CONTINUE   
      IF (NSTKRD .EQ. 0)  THEN   
         IERR = 2    
         GO TO 900   
         ENDIF   
C                      revert to previous input file     
  200 CONTINUE   
      IF (NSTKRD .EQ. 0)  THEN   
          WRITE (ISYSWR, '(A,A)') ' COMMAND IGNORED:',CRDBUF     
          WRITE (ISYSWR, '(A)') ' ALREADY READING FROM PRIMARY INPUT'    
      ELSE   
        ISYSRD = ISTKRD(NSTKRD)  
        NSTKRD = NSTKRD - 1  
        IF (NSTKRD .EQ. 0)  ISW(6) = IABS(ISW(6))    
        IF (ISW(5) .GE. 0)  THEN     
          INQUIRE(UNIT=ISYSRD,NAMED=LNAME,NAME=CFNAME)   
          CMODE = 'BATCH MODE      '     
          IF (ISW(6) .EQ. 1)  CMODE = 'INTERACTIVE MODE'     
          IF (.NOT.LNAME) CFNAME='unknown'   
          IF (MNUNPT(CFNAME))  CFNAME='unprintable'  
          WRITE (ISYSWR,290) CMODE,ISYSRD,CFNAME     
  290     FORMAT (' INPUT WILL NOW BE READ IN ',A,' FROM UNIT NO.',I3/   
     +    ' FILENAME: ',A)   
        ENDIF    
      ENDIF  
      GO TO 900  
C                      switch to new input file, add to stack    
  300 CONTINUE   
      IF (NSTKRD .GE. MAXSTK)  THEN  
          WRITE (ISYSWR, '(A)') ' INPUT FILE STACK SIZE EXCEEDED.'   
          GO TO 800  
          ENDIF  
      NSTKRD = NSTKRD + 1    
      ISTKRD(NSTKRD) = ISYSRD    
      ISYSRD = IUNIT     
C                   ISW(6) = 0 for batch, =1 for interactive, and    
C                      =-1 for originally interactive temporarily batch  
      IF (ISW(6) .EQ. 1)  ISW(6) = -1    
      GO TO 900  
C                      format error  
  500 CONTINUE   
      WRITE (ISYSWR,'(A,A)') ' CANNOT READ FOLLOWING AS INTEGER:',CUNIT  
      GO TO 800  
  600 CONTINUE   
      WRITE (ISYSWR, 601) CFNAME     
  601 FORMAT (' SYSTEM IS UNABLE TO OPEN FILE:',A)   
C                      serious error     
  800 CONTINUE   
      IERR = 3   
  900 CONTINUE   
      RETURN     
      END    
      SUBROUTINE MNTINY(EPSP1,EPSBAK)    
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        Compares its argument with the value 1.0, and returns  
CC        the value .TRUE. if they are equal.  To find EPSMAC    
CC        safely by foiling the Fortran optimizer    
CC   
      PARAMETER (ONE=1.0)    
      EPSBAK =  EPSP1  - ONE     
      RETURN     
      END    
      LOGICAL FUNCTION MNUNPT(CFNAME)    
C           is .TRUE. if CFNAME contains unprintable characters.     
      CHARACTER CFNAME*(*)   
      CHARACTER CPT*80, CP1*40,CP2*40    
      PARAMETER (CP1=' ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm')     
      PARAMETER (CP2='nopqrstuvwxyz1234567890./;:[]$%*_!@#&+()')     
      CPT=CP1//CP2   
      MNUNPT = .FALSE.   
      L = LEN(CFNAME)    
      DO 100 I= 1, L     
         DO 50 IC= 1, 80     
         IF (CFNAME(I:I) .EQ. CPT(IC:IC))  GO TO 100     
   50    CONTINUE    
      MNUNPT = .TRUE.    
      GO TO 150  
  100 CONTINUE   
  150 CONTINUE   
      RETURN     
      END    
      SUBROUTINE MNVERT(A,L,M,N,IFAIL)   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        inverts a symmetric matrix.   matrix is first scaled to    
CC        have all ones on the diagonal (equivalent to change of units)  
CC        but no pivoting is done since matrix is positive-definite.     
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
      DIMENSION A(L,M) ,PP(MNI), Q(MNI),  S(MNI)     
      IFAIL=0    
      IF (N .LT. 1)  GO TO 100   
      IF (N .GT. MAXINT)  GO TO 100  
C                   scale matrix by sqrt of diag elements    
      DO 8  I=1,N    
      SI = A(I,I)    
      IF (SI) 100,100,8  
    8 S(I) = 1.0/DSQRT(SI)    
      DO 20 I= 1, N  
      DO 20 J= 1, N  
   20 A(I,J) = A(I,J) *S(I)*S(J)     
C                                        . . . start main loop . . . .   
      DO 65 I=1,N    
      K = I  
C                   preparation for elimination step1    
      Q(K)=1./A(K,K)     
      PP(K) = 1.0    
      A(K,K)=0.0     
      KP1=K+1    
      KM1=K-1    
      IF(KM1)100,50,40   
   40 DO 49 J=1,KM1  
      PP(J)=A(J,K)   
      Q(J)=A(J,K)*Q(K)   
   49 A(J,K)=0.  
   50 IF(K-N)51,60,100   
   51 DO 59 J=KP1,N  
      PP(J)=A(K,J)   
      Q(J)=-A(K,J)*Q(K)  
   59 A(K,J)=0.0     
C                   elimination proper   
   60 DO 65 J=1,N    
      DO 65 K=J,N    
   65 A(J,K)=A(J,K)+PP(J)*Q(K)   
C                   elements of left diagonal and unscaling  
      DO 70 J= 1, N  
      DO 70 K= 1, J  
      A(K,J) = A(K,J) *S(K)*S(J)     
   70 A(J,K) = A(K,J)    
      RETURN     
C                   failure return   
  100 IFAIL=1    
      RETURN     
      END    
      SUBROUTINE MNWARN(COPT,CORG,CMES)  
C     If COPT='W', CMES is a WARning message from CORG.  
C     If COPT='D', CMES is a DEBug message from CORG.    
C         If SET WARnings is in effect (the default), this routine   
C             prints the warning message CMES coming from CORG.  
C         If SET NOWarnings is in effect, the warning message is     
C             stored in a circular buffer of length MAXMES.  
C         If called with CORG=CMES='SHO', it prints the messages in  
C             the circular buffer, FIFO, and empties the buffer.     
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
      CHARACTER COPT*1, CORG*(*), CMES*(*), CTYP*7   
      PARAMETER (MAXMES=10)  
      CHARACTER     ORIGIN(MAXMES,2)*10, WARMES(MAXMES,2)*60     
      COMMON/MN7WRC/ORIGIN,              WARMES  
      COMMON/MN7WRI/NFCWAR(MAXMES,2),ICIRC(2)    
      CHARACTER ENGLSH*20    
C    
      IF (CORG(1:3).EQ.'SHO' .AND. CMES(1:3).EQ.'SHO')  GO TO 200    
C             Either print warning or put in buffer  
      IF (COPT .EQ. 'W')  THEN   
        ITYP = 1     
        IF (LWARN) THEN  
          WRITE (ISYSWR,'(A,A/A,A)') ' MINUIT WARNING IN ',CORG,     
     +              ' ============== ',CMES  
          RETURN     
        ENDIF    
      ELSE   
        ITYP = 2     
        IF (LREPOR) THEN     
          WRITE (ISYSWR,'(A,A/A,A)') ' MINUIT DEBUG FOR  ',CORG,     
     +              ' ============== ',CMES  
          RETURN     
        ENDIF    
      ENDIF  
C                 if appropriate flag is off, fill circular buffer   
         IF (NWRMES(ITYP) .EQ. 0)  ICIRC(ITYP) = 0   
         NWRMES(ITYP) = NWRMES(ITYP) + 1     
         ICIRC(ITYP) = ICIRC(ITYP) + 1   
         IF (ICIRC(ITYP) .GT. MAXMES) ICIRC(ITYP) = 1    
         IC = ICIRC(ITYP)    
         ORIGIN(IC,ITYP) = CORG  
         WARMES(IC,ITYP) = CMES  
         NFCWAR(IC,ITYP) = NFCN  
      RETURN     
C    
C             'SHO WARnings', ask if any suppressed mess in buffer   
  200 CONTINUE   
      IF (COPT .EQ. 'W') THEN    
        ITYP = 1     
        CTYP = 'WARNING'     
      ELSE   
        ITYP = 2     
        CTYP = '*DEBUG*'     
      ENDIF  
      IF (NWRMES(ITYP) .GT. 0) THEN  
         ENGLSH = ' WAS SUPPRESSED.  '   
         IF (NWRMES(ITYP) .GT. 1) ENGLSH = 'S WERE SUPPRESSED.'  
         WRITE (ISYSWR,'(/1X,I5,A,A,A,A/)') NWRMES(ITYP),    
     +    ' MINUIT ',CTYP,' MESSAGE', ENGLSH     
         NM = NWRMES(ITYP)   
         IC = 0  
         IF (NM .GT. MAXMES) THEN    
              WRITE (ISYSWR,'(A,I2,A)')  ' ONLY THE MOST RECENT ',   
     +          MAXMES,' WILL BE LISTED BELOW.'  
              NM = MAXMES    
              IC = ICIRC(ITYP)   
         ENDIF   
         WRITE (ISYSWR,'(A)') '  CALLS  ORIGIN         MESSAGE'  
           DO 300 I= 1, NM   
           IC = IC + 1   
           IF (IC .GT. MAXMES)  IC = 1   
           WRITE (ISYSWR,'(1X,I6,1X,A,1X,A)')    
     +           NFCWAR(IC,ITYP),ORIGIN(IC,ITYP),WARMES(IC,ITYP)     
 300       CONTINUE  
         NWRMES(ITYP) = 0    
         WRITE (ISYSWR,'(1H )')  
      ENDIF  
      RETURN     
      END    
      SUBROUTINE MNWERR  
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC          Calculates the WERR, external parameter errors,  
CC      and the global correlation coefficients, to be called    
CC      whenever a new covariance matrix is available.   
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
C                         calculate external error if v exists   
      IF (ISW(2) .GE. 1) THEN    
      DO 100 L= 1, NPAR  
        NDEX = L*(L+1)/2     
        DX = DSQRT(DABS(VHMAT(NDEX)*UP))   
        I = NEXOFI(L)    
        IF (NVARL(I) .GT. 1)  THEN   
          AL = ALIM(I)   
          BA = BLIM(I) - AL  
          DU1 = AL + 0.5 *(DSIN(X(L)+DX) +1.0) * BA - U(I)    
          DU2 = AL + 0.5 *(DSIN(X(L)-DX) +1.0) * BA - U(I)    
          IF (DX .GT. 1.0)  DU1 = BA     
          DX = 0.5 * (DABS(DU1) + DABS(DU2))   
        ENDIF    
        WERR(L) = DX     
  100 CONTINUE   
      ENDIF  
C                          global correlation coefficients   
      IF (ISW(2) .GE. 1) THEN    
         DO 130 I= 1, NPAR   
            GLOBCC(I) = 0.   
            K1 = I*(I-1)/2   
            DO 130 J= 1, I   
               K = K1 + J    
               P(I,J) = VHMAT(K)     
  130          P(J,I) = P(I,J)   
         CALL MNVERT(P,MAXINT,MAXINT,NPAR,IERR)  
         IF (IERR .EQ. 0)   THEN     
            DO 150 IIN= 1, NPAR  
               NDIAG = IIN*(IIN+1)/2     
               DENOM = P(IIN,IIN)*VHMAT(NDIAG)   
               IF (DENOM.LE.ONE .AND. DENOM.GE.ZERO)  THEN   
                   GLOBCC(IIN) = 0.  
               ELSE  
                   GLOBCC(IIN) = DSQRT(1.0-1.0/DENOM)     
               ENDIF     
  150       CONTINUE     
         ENDIF   
      ENDIF  
      RETURN     
      END    
      SUBROUTINE STAND   
C ************ DOUBLE PRECISION VERSION *************    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
CC        optional user-supplied subroutine is called whenever the   
CC        command "standard" appears.    
CC   
      RETURN     
      END   
