Program FFT
Implicit none

!INPUT DATA & INITIALIZATION
INTEGER,PARAMETER :: S=16      !THE NUMBER OF SAMPLE POINT
INTEGER           :: N=S        !THE NUMBER OF SAMPLE POINT FOR DO LOOP   
REAL*8, PARAMETER :: pi=Acos(-1.0)
INTEGER           :: K        !SAMPLE POINT

!REAL, DIMENSION(S) :: X
REAL*8, DIMENSION(S) :: XREAL
REAL*8, DIMENSION(S) :: XIMAG

!COMPLEX,Dimension(N) :: x  !DATA VECTOR. IMAGINARTY PART SHOULD BE SET TO  ZERO.


INTEGER :: N2                 !THE SPACING BETWEEN DUAL NODES

INTEGER :: NU                 !GAMMA 
INTEGER :: L=1                !GAMMA FOR DO LOOP (ARRAY NUMBER BEING CONSIDERED)                 

INTEGER :: P
INTEGER :: NU1                !THE NU1 IS THE RIGHT SHIFT REQUIRED WHEN DETERMINING THE VALUE OF P.
INTEGER :: I                  !THIS COUNTER MONITORS THE NUMBER OF DUAL
!NODE PAIRS THAT HAVE BEEN CONSIDERED.                
!THE COUNTER I IS THE CONTROL FOR DETERMINING WHEN THE PROGRAM MUST SKIP.

COMPLEX :: W=EXP(-2*pi/S)

INTEGER :: M 
INTEGER :: T1                 !TEMPORARY VALUE
INTEGER :: T3                 !TEMPORARY VALUE
INTEGER :: t
REAL*8 :: f=1.0d+1

OPEN(10, FILE="sine_testFFT.txt", status='replace')
OPEN(11, FILE="output_testFFT.txt",status='replace')

!=======INPUTDATA=============
f=1

Do t=1, S

   XREAL(t)=cos(2*pi*f*(t-1)/(S-1))
!   Print *, "XREAL(",t,")=",XREAL(t)
   WRITE(10,*) XREAL(t)

END DO 

!=============================


!RELATIONSHIPS & INITIALIZATION 
K = 0
NU = 0
!======DEFINE NU (GAMMA)=======
!NU = 0
    
DO 

IF (N==1) EXIT
!  PRINT*, "N=",N
!  PRINT*, "N/2=",N/2
  N=INT(N/2)
  NU=NU+1
  
!  PRINT*, "N=",N
!  PRINT*, "NU(GAMMA)=",NU  

END DO
 
!==============================


PRINT*, "NU(GAMMA)=",NU

  DO L=1,NU

     N2 = INT(S /2**(L))

    
110   DO I=1,N2
       
!        PRINT *, "I=",I, "N2=",N2
        !J=I/(2**(L))            
         NU1=NU-L
!        PRINT *, "NU1=",NU1 

!100 IF (L < NU)    !TO SEE IF ALL ARRAYS HAVE BEEN COMPUTED

                 
120      M=INT(K/(2**NU1)) 
         PRINT*, "M=",M
         CALL bit_reversing(m,NU,P)      
         !P=br
         print *, "P=",P
         !P=bit_reversing(m)

         T1=(W**P)*XREAL(K+N2) 

         XREAL(K+N2) = XREAL(K) - T1
         XREAL(K) = XREAL(K) + T1

         K=K+1

           IF (I /= N2) THEN

             GO TO 120
       
           ELSE

             K=K+N2
               IF (K < S -1) THEN
                  GO TO 110
               ELSE 
                  N2=INT(N2/2)
                  NU1=NU1-1
                  K=0
                  GO TO 130
               END IF
            END IF    

         END DO
!    ELSE IF (L > NU) !WE PROCEED TO UNSCRAMBLE THE FINAL RESULTS
   END DO

!==============UNSCRAMBLE THE COMPUTED RESULTS BY BIT INVERSION=================   
        
130     CALL bit_reversing(k,NU,i)
        print *, "i=",i 
         IF (i<K) THEN  

!THIS STEP IS NECESSARY TO PROHIBIT THE ALTERING OF PREVIOUSLY UNSCRAMBLED
!NODES.
 
         GO TO 140

         ELSE 
             T3=XREAL(K)
             XREAL(K) = XREAL(i)
             XREAL(i)=T3
             GO TO 140
         END IF    


140      IF (K==S-1) THEN
             STOP
         ELSE
             K = K + 1
             GO TO 130
         END IF
!================================================================================


CONTAINS
            
 SUBROUTINE  bit_reversing(m,NU,br)
   IMPLICIT NONE
   INTEGER              :: m,NU
   INTEGER              :: j2, i1
   INTEGER, INTENT(OUT) :: br

   !INITIALIZATION
     i1 = 1
     br = 0
     PRINT*, "m=",m
     PRINT*, "NU=",NU
     PRINT*, "br1=",br

150  IF (i1>NU) THEN
         RETURN 
    
     ELSE 
         j2 = INT(m/2)
         br = 2*br + (m-2*j2)
         !PRINT*, "br2=",br
         m  = j2

         i1=i1+1

         GO TO 150
     END IF 
    
    PRINT*, "br3=",br   
 END SUBROUTINE bit_reversing

END PROGRAM FFT
