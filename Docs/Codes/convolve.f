C     PROGRAM CONVOLUTION
c     Programma pou pairnei ena istogramma apo p.x. MC kai to kanei fasma
c     Ksekiname me ton orismo twn metablitwn
      REAL*8 A(3050),C(3050),M(3050),W,D,F,S,G
      REAL*8 B(3050)
      CHARACTER*50 FROSO,MIKE
c     Gia kompsotita akolouthei o orismos twn parametrwn kai arxikwn timwn
c     twn metablitwn
	  D=0.
	  F=0.
          S=0.
c     Anoigoume to palio arxeio to opoio edw einai distilo, p.x. 
c     kanalia i energeia kai periexomeno. Antistoixa ruthmizoume kai to eidos
c     twn metablitwn pou diabazoume (akeraioi i oxi). To diabasma ginetai seiriaka,
c     grammi-grammi apo to arxeio eisodou kai oi metablites mas einai stoixeia 
c     pinaka.
      WRITE(*,*)'PLEASE GIVE ME THE NAME OF THE INPUT FILE'
      READ(*,2)MIKE
      WRITE(*,*)'PLEASE GIVE ME THE NAME OF THE OUTPUT FILE'
      READ(*,2)FROSO
  2   FORMAT(A50)
      OPEN(10,FILE=MIKE,STATUS='OLD')
      DO 20, I=1,2800
      READ(10,*) A(I),B(I)
c     To D einai to oloklirwma tou arxikou fasmatos - tha to xreiastoume! 
  20  END DO
      CLOSE(10)
c     Edw midenizoume ti metabliti 'eksodou' pou tha xreiastoume meta kai
c     kai pou einai bebaia 'pinakas', omoios me tous pinakes eisodou.
      DO 30, I=1,3050
      C(I)=0.
  30  END DO
c     Edw ksekinaei to kurio meros tou convolution opou to periexomeno
c     kathe kanaliou/energeias efoson einai mi mideniko 'moirazetai' 
c     se ola ta kanalia/energeies tou convoluted fasmatos
c     sumfwna me mia gaussian me metablito euros w. Gi'auto emfanizetai
c     loop mesa sto loop, mia pou praktika i diadikasia ginetai p.x.
c     1024x1024 fores, diladi to periexomeno tou prwtou kanaliou/energeias
c     'aplwnetai' pantou, meta to deutero k.o.k. 
      DO 40, I=1,2800
      W=-0.021+0.0862*SQRT(A(I)-0.0765*A(I)**2)
      S=0.60056120439322*W
c     W=SQRT(A(I)*3.29-239.5)
      IF(S.GT.0.001)THEN
      D=D+B(I)
      DO 50, K=1,2800
      C(K)=C(K)+B(I)*(1/(S*1.7724538509)*EXP(-((A(K)-A(I))**2/S**2)))
c     Edw upologizoume to oloklirwma tou convoluted fasmatos
c     gia na kanonikopoiisoume kai na min exoume dusarestes ekplikseis	 	
  50  END DO
      ENDIF
  40  END DO
c     Edw anoigoume to arxeio eksodou gia na grapsoume to convoluted
c     istogramma  
      DO 70, J=1,2800
          F=F+C(J) 
  70  END DO
      OPEN(11,FILE=FROSO,STATUS='UNKNOWN')
      DO 60, L=1,2800
c     Edw kanonikopoioume tin eksodo wste na einai omoia me tin eisodo
	  M(L)=C(L)*D/F
          G=G+M(L)
      WRITE(11,*) A(L),M(L),B(L)
  60  END DO
      WRITE(11,*) D,G
c     Kleinoume to arxeio eksodou kai teleiwnoume to programma!
      CLOSE(11)
      END
