	  SUBROUTINE FCN(NPAR,GG,F,AA,IFLAG)
C
C     FCN FOR FSA
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXDBG=10, MAXCWD=20)
      CHARACTER CWORD*(MAXCWD),CFROM*8,CSTATU*10,CTITL*50
c      CHARACTER*2 TARGET,BEAM 
c      REAL*8 KEVCH
c      INTEGER Z1,Z2,A1,A2,ZBEAM
       Double precision AA(4)
c                 A(1024),B(1024),C(1024),D(1024)
	 Double precision expERROR(1024),expr(1024)
       Double precision FSASpe(1024)
       REAL*8 chan(1024)
       CHARACTER TARGE*20,FILENAME*30
c      DIMENSION ISPEC(1040) 
c      COMMON /DAT/ NPTS,ICH(1040),DSPEC(1040),DERR(1040),FSPEC(1040)
      common/VAL2/ TARGE,FILENAME
c	common/valV/ Ebeam,scatTHETA
      common/VALY/ G,OFFSET,s
      COMMON /XSQ/ XSQ
c      COMMON /EXPDAT/Z1,A1,Z2,A2,EN,Q,OMEGA,DRES,ZBEAM,RDENS
c     1,ENOT,KEVCH,THETA,PHI,TARGET,BEAM!
      COMMON /FITLIM/ LLIM,MLIM
      COMMON
     F/MN7TIT/ CFROM  ,CSTATU ,CTITL  ,CWORD
     G/MN7FLG/ ISW(7) ,IDBG(0:MAXDBG) ,NBLOCK ,ICOMND
     H/MN7MIN/ AMIN
     I/MN7CNV/ NFCN
	common/vals/ SIM
      common/valspe/ expr,expERROR
c	common/valchan/ chan
      GO TO (10,20,40,40,50,60) IFLAG
10    CONTINUE
c      WRITE (*,92)
c92    FORMAT (' Do you wish to take into account experimental errors? (Y
c     1/N)')
c      READ (*,'(A1)') XSQ
C
C     READ EXPERIMENTAL DATA
C
c      CALL ZREAD1
c      write(*,*)'check 1'
c      CALL READIN
c
      PRINT *,'FILENAME (WITH.DAT):'
      READ *,FILENAME
      PRINT *,'Low limit (channel) minimization (llim):'
      READ *,llim
	PRINT *,'Max limit (channel) minimization (mlim):'
      READ *,mlim
c	PRINT *,'Beam energy (keV):'
c     READ *,Ebeam
c	PRINT *,'scattering angle theta (deg):'
c      READ *,scatTHETA
c	PRINT *,'TARGET(CAPS)-> CAF2 or NABR or MOS2 or ZNF2 or CACO c:'
c      READ *,TARGE

c     Ebeam=1230.3
c	scatTHETA=120.6
c
c      PRINT *,'offset (keV):'
c      READ *,offset
c	
c
C------FROSSO--initialize--anoigo oti arxeia xreiazomai=exp,sim,output
c      OPEN(11,FILE=SUMM,STATUS='UNKNOWN')
c      DO 60, K=1,1024
c     Edw grafoume ta nea athroismena counts kai ta arxiko/teliko olokliroma
c      WRITE(11,*) FSAspe(K),A(K),B(K),C(K),D(K)
c  60  END DO
c      WRITE(11,*) J,G
c     Kleinoume to arxeio eksodou kai teleiwnoume to programma!
c      CLOSE(11)      
C-------------------        
      OPEN (UNIT=10,FILE=filename)
      Do i=1,1024
       READ (10,*) CHAN(I),expr(I)
         expERROR(I)=SQRT(expr(I))
      enddo
      CLOSE (10)
c     low limit (llim) k max limit (mlim) minimization
c      LLIM=265
c      MLIM=301
20    CONTINUE
40    CONTINUE
c      write(*,*)'check 2'
      CALL sumup(AA,FSASpe)
C A(4),simK(1024),simPb(1024),simBi(1024),simTl(1024),expr(1024)
      F=FCHISQ(expr,expERROR,FSASpe,LLIM,MLIM,NPAR)
      WRITE (*,31) ICOMND,CWORD,NFCN,F,AMIN,aa(1),aa(2),AA(3),AA(4)
31    FORMAT (I3,', ',A10,' NFCN = ', I5,', CHI-SQUARE = ',G12.4
     1,', MIN =',G12.4/' Ak = ',G10.4,' Apb = ',G10.4, ' Abi = ',G10.4,
     1' Atl = ',G10.4)
      IF (IFLAG.EQ.3) GO TO 30
      RETURN
30    CONTINUE
C
C     WRITE RESULTS
C
      WRITE (6,250)
250   FORMAT (//' ****'/' RESULTS'/' ****')
      WRITE (6,255) (chan(I),expr(I),expERROR(I),FSASpe(I),I=1,1024)
255   FORMAT (' CHAN ','      expDATA     expERROR       result'/(I6,
     13F12.4))   
   
      OPEN(UNIT=37,FILE='OUTPUT.TXT',STATUS='UNKNOWN')
      DO I=1,1024
      WRITE(37,*) FSASpe(I)
      END DO
      CLOSE(37)

      RETURN
50    RETURN
60    RETURN
      END
C
      FUNCTION FCHISQ(Y,SY,YF,LLIM,MLIM,NPAR)
c      F=FCHISQ(expr,expERROR,FSASpe,LLIM,MLIM,NPAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      double precision Y(1024),SY(1024),YF(1024)
      COMMON /XSQ/ XSQ
      NFREE=MLIM-LLIM+1-NPAR
12    IF (NFREE) 13,13,20
13    FCHISQ=0.
      GO TO 40
20    CHISQ=0.
      DO 30 K=LLIM,MLIM
      IF (SY(K).LE.0.) THEN
      NFREE=NFREE-1
	xsq='y'
      ELSE
      IF (XSQ.EQ.'Y'.OR.XSQ.EQ.'y') THEN
      WEIGHT=1./SY(K)**2
      ELSE
      WEIGHT=1.
      ENDIF
      IF (CHISQ.GT.0.1D+99) GO TO 30
         CHISQ=CHISQ+WEIGHT*(Y(K)-YF(K))**2
      ENDIF
30    CONTINUE
c     free: double prec, enw to nfree einai "fisika" akeraios
      FREE=NFREE
      FCHISQ=CHISQ/FREE
40    RETURN
      END
C
c     -------------------------------------------------------------------    
c
      SUBROUTINE sumup(AA,FSASpe)
       implicit none
C     PROGRAM SUM
c     Programma pou pairnei ena istogramma apo p.x. MC kai to kanei fasma
c     Ksekiname me ton orismo twn metablitwn
C     -------------------------------orismos metavliton pou ipirxan apo prin
      DOUBLE PRECISION FSASpe(1024),F,PATHX,YIELD,GAMMA1,EXPR(1024)
      DOUBLE PRECISION Ekinematics,ST
c      AA: number of parameters in Minuit 
      DOUBLE PRECISION K0,K1,K2,K3,K4,K5,K6,K7,PI,Yo,PATH,AA(4),Ech
      DOUBLE PRECISION L0,L1,L2,L3,L4,L5,L6,L7,X,yieldr,expERROR(1024)
      DOUBLE PRECISION M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,Ek,MASS2,gg,g
c      DOUBLE PRECISION THETA,P,PHI_N,Z_N,N,A,B,E,PHI_A,PHI_B,Z_A,Z_B
      DOUBLE PRECISION A2,B2,Er,g1,g2,g3
      CHARACTER TARGE*20,FILENAME*30
      DOUBLE PRECISION EJ,SPATH,GAIN,SYIELD,OFFSET,SIM(1024),sigma
      DOUBLE PRECISION Ak,Apb, Abi, Atl 
      REAL*8 CHANN
      INTEGER r,ch_EMAX,ch_Ezero,i,l
c  
c
      REAL*8 A(1024),B(1024),C(1024),D(1024)
c
      EXTERNAL PATHX,YIELD,F,ST,GAMMA1,PATH,Yo
	COMMON/VAL/ A,B,Er
	COMMON/VAL1/ CHANN
	COMMON/VAL2/ TARGE,FILENAME
	COMMON/VAL4/ SPATH
c	COMMON/VALF/ N
	COMMON/VALG/ PI
	COMMON/VALY/ GAIN,OFFSET,Ak,Apb,Abi,Atl
	common/vals/ SIM
      common/valspe/ expr,expERROR
c	common/valchan/ chan
      CHARACTER*50 K,Pb,Bi,Tl,SUMM
c     Gia kompsotita akolouthei o orismos twn parametrwn kai arxikwn timwn
c     twn metablitwn
c	  G=0.
c	  J=0.
c	List of parameters to give values in MINUIT
	Ak=AA(1)
	Apb=AA(2)
      Abi=AA(3)
      Atl=AA(4)
c      WRITE(*,*)'PLEASE GIVE ME THE FIRST INPUT FILES TO SUM UP'
c      READ(*,2)Kl
c      WRITE(*,*)'PLEASE GIVE ME THE NEXT INPUT FILES TO SUM UP'
c      READ(*,2)Pb
c      WRITE(*,*)'PLEASE GIVE ME THE NEXT INPUT FILES TO SUM UP'
c      READ(*,2)Bi
c      WRITE(*,*)'PLEASE GIVE ME THE NEXT INPUT FILES TO SUM UP'
c      READ(*,2)Tl
c      WRITE(*,*)'PLEASE GIVE ME THE NAME OF THE OUTPUT FILE'
c      READ(*,2)SUMM
c  2   FORMAT(A50)
      OPEN(31,FILE='K',STATUS='OLD')
      DO 21, I=1,1024
      READ(31,*) A(I)
c     To G einai to oloklirwma tou arxikou fasmatos - tha to xreiastoume! 
  21  END DO
      CLOSE(31)
      OPEN(32,FILE='Pb',STATUS='OLD')
      DO 80, I=1,1024
      READ(32,*) B(I)
c     To 
  80  END DO
      CLOSE(32)
      OPEN(34,FILE='Bi',STATUS='OLD')
      DO 90, I=1,1024
      READ(34,*) C(I)
c     To  
  90  END DO
      CLOSE(34)
      OPEN(36,FILE='Tl',STATUS='OLD')
      DO 70, I=1,1024
      READ(36,*) D(I)
c     To 
  70  END DO
      CLOSE(36)
c     Edw midenizoume ti metabliti 'eksodou' pou tha xreiastoume meta kai
c     kai pou einai bebaia 'pinakas', omoios me tous pinakes eisodou.
      DO 53, I=1,1024
      FSAspe(I)=0.
  53  END DO          
c      DO 56, N=1,1024
c      G=G+A(N)+B(N)+C(N)+D(N)
c  56  END DO
      DO 52, L=1,1024
      FSAspe(L)=FSAspe(L)+B(L)*Apb+A(L)*Ak+C(L)*Abi+D(L)*Atl
c     Edw upologizoume to oloklirwma tou convoluted fasmatos
c     gia na kanonikopoiisoume kai na min exoume dusarestes ekplikseis	 	
  52  END DO
C      ENDIF
c     Edw anoigoume to arxeio eksodou gia na grapsoume to summed
c     istogramma  
c      DO 59, M=1,1024
c          J=J+FSAspe(M) 
c  59  END DO

      return
      END
