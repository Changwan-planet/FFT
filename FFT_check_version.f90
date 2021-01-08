Program FFT
IMPLICIT NONE

!INPUT DATA & INITIALIZATION
INTEGER,PARAMETER :: S=256!THE NUMBER OF SAMPLE POINT
INTEGER           :: N=S        !THE NUMBER OF SAMPLE POINT FOR DO LOOP   
REAL*8, PARAMETER :: pi=Acos(-1.0)
INTEGER           :: K        !SAMPLE POINT

INTEGER :: N2                 !THE SPACING BETWEEN DUAL NODES

INTEGER :: NU                 !GAMMA 
INTEGER :: L                  !GAMMA FOR DO LOOP (ARRAY NUMBER BEING CONSIDERED)                 

INTEGER :: P
INTEGER :: P_INTEGER
INTEGER :: NU1                !THE NU1 IS THE RIGHT SHIFT REQUIRED WHEN DETERMINING THE VALUE OF P.
INTEGER :: I                  !THIS COUNTER MONITORS THE NUMBER OF DUAL NODE PAIRS THAT HAVE BEEN CONSIDERED.                                            !THE COUNTER I IS THE CONTROL FOR DETERMINING WHEN THE PROGRAM MUST SKIP.
INTEGER :: I_INTEGER
 

!REAL*8, PARAMETER :: JEX=-2*pi/S
REAL*8 :: ARG
REAL*8, DIMENSION(0:S-1) :: XREAL
REAL*8, DIMENSION(0:S-1) :: XIMAG
REAL*8 :: TREAL
REAL*8 :: TIMAG
REAL*8 :: CC
REAL*8 :: SS


REAL*8 :: T1  !TEMPORARY VALUE
REAL*8 :: T3  !TEMPORARY VALUE
INTEGER :: T4 !TEMPORARY VALUE

!INTEGER :: BD !BINARY DIGIT
INTEGER :: t, br, CT, Y, Z, ALLOCATESTATUS
INTEGER, DIMENSION(:), ALLOCATABLE :: K_BINARY
INTEGER, DIMENSION(:), ALLOCATABLE :: K_BINARY_SCALED
INTEGER, DIMENSION(:), ALLOCATABLE :: K_BINARY_SCALED_REVERSED
INTEGER, DIMENSION(:), ALLOCATABLE :: I_BINARY
INTEGER, DIMENSION(:), ALLOCATABLE :: I_BINARY_REVERSED

INTEGER :: f

OPEN(10, FILE="sine_testFFT.txt", status='replace')
OPEN(11, FILE="output_testFFT.txt",status='replace')

!=======INPUTDATA=============
!Trigonometric signal
f=20
XREAL=0

Do t=0, S-1
   XREAL(t)=sin(2*pi*f*(t)/(S-1))
   Print "(a,i4,a,f20.16)", "XREAL(",t,")=",XREAL(t)
   WRITE(10,*) XREAL(t)
END DO 

!=============================
!SIMPLIE SIGNAL
!constant-amplitude waveform
!Do t=0, S-1
!   XREAL(t)=1
!   WRITE(10,*) XREAL(t)

!END DO 

!symmetrical-puls waveform
!Do t=0, S-1
      
!     XREAL(t)=1
   
!   WRITE(10,*) XREAL(t)

!END DO 



!RELATIONSHIPS & INITIALIZATION 
NU = 0
T1 = 0
T3 = 0 
K = 0
L = 1
!BD = 0
T4 = 0
!======DEFINE NU (GAMMA)=======
    
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

!PRINT*, "NU(GAMMA)=",NU

NU1 = NU - L
N2 = S / 2**(L)

T4 = S-1   


!THE LAST VALUE OF THE K! I NEED THIS VALUE TO GET THE DIGIT OF 
!THE MOST LARGE BINARY VALUE IN THIS CALCUATION.

   !BD=BINARY_DIGIT(T4)
   
   PRINT *, ""
   PRINT *, "**********************************************************"
   PRINT *, "T4 MEANS THE LAST VALUE OF THE K"
   PRINT "(a,i4)", "T4=",T4
   PRINT *, "BD MENAS THE DIGIT WHEN THE K TURN INTO BINDARY NUMBER."
   PRINT "(a,i4)", "BD=",NU
   PRINT *, "**********************************************************"
   PRINT *, ""

   !ALLOCATE(K_BINARY(NU), STAT=ALLOCATESTATUS)
    !IF(ALLOCATESTATUS /= 0) STOP "***NOT ENOUGH MEMORY***"

150 IF(L<=NU) THEN
  
       !DO I=1,N2
140      I=1
    !     K=4 
!=====W**P DETERPEMATION==========================================
130      PRINT *,""
         PRINT *, "**********************************************************************"
         PRINT "(a,i4,4X,a,i2)", "K=",K,"NU-L=NU1=",NU1 
         PRINT "(a,i4,4X,a,i2,4X,a,i2,4X,a,i2)", &
               "L=",L,"NU(GAMMA)=",NU,"I=",I, "N2=(S/2**L)=",N2
         PRINT *, "**********************************************************************"
         PRINT *, "" 
         
         CALL INTEGER2BINARY(K,NU,K_BINARY)
         !PRINT *, "CT(THE DIGIT OF BINARY NUMBER)=",CT 
         
         !PRINT *,"K_BINARY=",K_BINARY  
         !PRINT *, "SIZE=",SIZE(K_BINARY)

         CALL SCALE2RIGHT(K_BINARY,NU1,K_BINARY_SCALED)

         !PRINT *, "K_BINARY_SCALED=",K_BINARY_SCALED
         !PRINT *, "SIZE=",SIZE(K_BINARY_SCALED)

         CALL REVERSED_BINARY(K_BINARY_SCALED,K_BINARY_SCALED_REVERSED) 
         
         !PRINT *, "K_BINARY_SCALED_REVERSED=",K_BINARY_SCALED_REVERSED
         !PRINT *, "SIZE=",SIZE(K_BINARY_SCALED_REVERSED)

         CALL BINARY2INTEGER(K_BINARY_SCALED_REVERSED,P_INTEGER)
         
         !PRINT *, "P_INTEGER=",P_INTEGER

         ARG = 2*pi*P_INTEGER/S
         !ARG=pi
         PRINT "(a,i4)", "K=",K
         PRINT "(a,i4)", "K+N2=",K+N2
         PRINT *, ""
         PRINT "(a,i4,a,f20.16)", "XREAL(K+N2)=XREAL(",K+N2,")=",XREAL(K+N2)
         PRINT "(a,i4,a,f20.16)", "XIMAG(K+N2)=XIMAG(",K+N2,")=",XIMAG(K+N2)
         PRINT "(a,i4,a,f20.16)", "XREAL(K)=XREAL(",K,")=",XREAL(K)
         PRINT "(a,i4,a,f20.16)", "XIMAG(K)=IMAG(",K,")=",XIMAG(K)
         PRINT *, ""
          
         PRINT *,"K_BINARY=",K_BINARY  
         PRINT *, "K_BINARY_SCALED=",K_BINARY_SCALED
         PRINT *, "K_BINARY_SCALED_REVERSED=",K_BINARY_SCALED_REVERSED     
         PRINT "(a,i4)", "P_INTEGER=",P_INTEGER 
!==================================================================
         CC = COS(ARG)
         SS = SIN(ARG)

         PRINT *, ""
         PRINT *, "ARG=",ARG
         PRINT *, "COS(ARG)=",CC
         PRINT *, "SIN(ARG)=",SS
         PRINT *, ""
         
         TREAL = XREAL(K+N2) * CC + XIMAG(K+N2) * SS
         TIMAG = XIMAG(K+N2) * CC - XREAL(K+N2) * SS
     
         PRINT "(a,f20.16)", "TREAL=",TREAL
         PRINT "(a,f20.16)", "TIMAG=",TIMAG


         XREAL(K+N2) = XREAL(K) - TREAL
         XIMAG(K+N2) = XIMAG(K) - TIMAG
         XREAL(K) = XREAL(K) + TREAL
         XIMAG(K) = XIMAG(K) + TIMAG
                
         PRINT *,""
         
         PRINT "(a,i4,a,i4,a,f20.16,4X,a,f20.16)",& 
                  "XREAL(",K+N2,")_2=XREAL(",K,")-",TREAL,"=",XREAL(K+N2)
         PRINT "(a,i4,a,i4,a,f20.16,4X,a,f20.16)",& 
                  "XREAL(",K+N2,")_2=XIMAG(",K,")-",TIMAG,"=",XIMAG(K+N2)   
                
         PRINT  "(a,i4,a,i4,a,f20.16,4X,a,f20.16)",& 
                 "XREAL(",K,")_2=XREAL(",K,")+",TREAL,"=",XREAL(K)  
         PRINT  "(a,i4,a,i4,a,f20.16,4X,a,f20.16)",& 
                 "XREAL(",K,")_2=XIMAG(",K,")+",TIMAG,"=",XIMAG(K)  

         K=K+1   !GO TO THE NEXT NODE
         
!==================================================================
!=====DUAL-NODE SPACING & DUAL-NODE COMPUTATION====================
           IF(I==N2) THEN
           
              K = K + N2
              
              IF (K < S-1) THEN
                  GO TO 140              
            
              ELSE 

                  L = L+1
                  N2 = N2/2
                  NU1 = NU1 -1
                  K = 0
                  GO TO 150
              
              END IF   
                 
           ELSE

             I=I+1

             GO TO 130

           END IF    

    ELSE     
!   ELSE  (L > NU) !WE PROCEED TO UNSCRAMBLE THE FINAL RESULTS
!======UNSCRAMBLE THE COMPUTED RESULTS BY BIT INVERSION==============   
       
       PRINT *, "================================================="       
200     PRINT *, ""
       PRINT *, "K_1=",K
       CALL INTEGER2BINARY(K,NU,I_BINARY)
       
       PRINT *, "I_BINARY=", I_BINARY 
       
       CALL REVERSED_BINARY(I_BINARY,I_BINARY_REVERSED)

       PRINT *, "I_BINARY_REVERSED=",I_BINARY_REVERSED
      
       CALL BINARY2INTEGER (I_BINARY_REVERSED,I_INTEGER)

       PRINT *, "I_INTEGER=",I_INTEGER
       PRINT *, "===================================================="

! DO Y=0, S-1

!    PRINT *, "XREAL(",Y,")=",XREAL(Y)
    !WRITE(10,*) XREAL(t)
!END DO



       
!======================================================================
!THIS STEP IS NECESSARY TO PROHIBIT THE ALTERING OF PREVIOUSLY UNSCRAMBLED NODES.
             
        IF (I_INTEGER < K) THEN
             PRINT *, ""
             PRINT *, "====I_INTEGER < K====="
             PRINT "(a,i4)", "I_INTEGER=",I_INTEGER
             PRINT "(a,i4)", "K=",K
             PRINT *, ""
             PRINT *, "K_2=",K
             GO TO 300
         
        ELSE
            PRINT *, ""
            PRINT *, "======UNSCRABLE========="
            PRINT "(a,i4,a,f20.16)", "XREAL_K(",K,")_1=",XREAL(K)
            PRINT "(a,i4,a,f20.16)", "XIMAG_K(",K,")_1=",XIMAG(K)
            PRINT "(a,i4,a,f20.16)", &
                  "XREAL_I(",I_INTEGER,")_1=",XREAL(I_INTEGER)
            PRINT "(a,i4,a,f20.16)", &
                  "XIMAG_I(",I_INTEGER,")_1=",XIMAG(I_INTEGER)
            
            TREAL = XREAL(K)
            TIMAG = XIMAG(K)

            XREAL(K) = XREAL(I_INTEGER)
            XIMAG(K) = XIMAG(I_INTEGER)
            XREAL(I_INTEGER) = TREAL
            XIMAG(I_INTEGER) = TIMAG

            PRINT "(a,i4,a,f20.16)", "XREAL_K(",K,")_2=",XREAL(K)
            PRINT "(a,i4,a,f20.16)", "XIMAG_K(",K,")_2=",XIMAG(K)
            PRINT "(a,i4,a,f20.16)", &
                  "XREAL_I(",I_INTEGER,")_2=",XREAL(I_INTEGER)
            PRINT "(a,i4,a,f20.16)", &
                  "XIMAG_I(",I_INTEGER,")_2=",XIMAG(I_INTEGER)
 
            GO TO 300
         
            
        END IF

            
300      IF (K==(S-1)) THEN
!             PRINT *, ""
!             PRINT *, ""
             !PRINT *, "XREAL(0)=", XREAL(5)
             PRINT *, "*****YOU COMPLETELY CALCULATED FFT*****"
             !=====OUTPUTDATA=========================================    
             DO Z=0, S-1

               PRINT "(a,i4,a,f20.16,4X,a,i4,a,f20.16)", "XREAL(",Z,")=",&
                     XREAL(Z),"XIMAG(",Z,")=",XIMAG(Z)      
               !PRINT *, "XREAL(",Z,")=",XREAL(Z)
                
!               WRITE(11,*) XREAL(Z),XIMAG(Z)
                WRITE(11,*) XREAL(Z)
             END DO
             
         ELSE

             K =  K+1
             GO TO 200
           
         END IF

    END IF

DEALLOCATE (K_BINARY, K_BINARY_SCALED, K_BINARY_SCALED_REVERSED)
DEALLOCATE (I_BINARY, I_BINARY_REVERSED)


CONTAINS

FUNCTION BINARY_DIGIT(I) 
    
    INTEGER, INTENT (IN) :: I
    INTEGER, DIMENSION (32) :: B
    INTEGER ::  J,K
    INTEGER :: BINARY_DIGIT
    
    J = I
    BINARY_DIGIT = 0
   
    DO K=1,SIZE(B)
       
       IF(MOD(J,2)==0) THEN
          B(K) = 0
       ELSE
          B(K) = 1
       END IF

       J = INT(J / 2)

       BINARY_DIGIT = BINARY_DIGIT + 1
       
       !PRINT *, "***J***"
       !PRINT *, J
       !PRINT *, "***BD***"
       !PRINT *, BINARY_DIGIT

      IF(J == 0 ) THEN 
        RETURN
      END IF       

    END DO 

!   PRINT *, "***CHECK2***"  
END FUNCTION BINARY_DIGIT   

SUBROUTINE INTEGER2BINARY(I,NU,B) 
IMPLICIT NONE
    INTEGER, INTENT(IN) :: I
    INTEGER, INTENT(IN) :: NU
    !INTEGER, DIMENSION (32) :: B
    INTEGER, DIMENSION (:), ALLOCATABLE, INTENT(OUT) :: B
    !INTEGER, DIMENSION (:), ALLOCATABLE, INTENT(OUT) :: B2
    INTEGER :: C,J,L, ALLOCATESTATUS 

    ALLOCATE(B(NU),STAT=ALLOCATESTATUS)
    IF(ALLOCATESTATUS /=0) STOP "***1NOT ENOUGH MEMORY***"
    !ALLOCATE(B2(NU),STAT=ALLOCATESTATUS)
    !IF(ALLOCATESTATUS /=0) STOP "***!NOT ENOUGH MEMORY***"

    !B2=0
    B = 0 
    J = I
    
    !PRINT *, "NU=",NU
    !PRINT *, SIZE(B), SIZE(B2)
    !PRINT *, "***B1***"
    !PRINT *, B

    DO C=NU,1,-1
    
       IF(MOD(J,2)==0) THEN
         B(C) = 0
       
       ELSE
         B(C) = 1
       END IF

       J=J/2 !NOTICE THE USE OF TRUNCATED RESULT

       IF (J==0) THEN
          EXIT
       END IF

    END DO
 
    !PRINT *, "B=",B
    !PRINT *, "B2=",B2 

    !DO L=1,NU
       
    !   B2(NU+1-L)=B(L)
       
    !END DO
   
    !PRINT *, "***B2***"
    !PRINT *, B
  
    !PRINT *, "***B2***"
    !PRINT *, B2

 !   DEALLOCATE(B)

END SUBROUTINE INTEGER2BINARY
    
SUBROUTINE SCALE2RIGHT(K_BINARY,NU1,K_BINARY_SCALED)
    INTEGER, INTENT(IN) :: NU1
    INTEGER, DIMENSION(:) :: K_BINARY
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: K_BINARY_SCALED
    INTEGER :: I,J,K,T,L
   
    !INITIALIZATION
    I=0
    J=1
    T=0
  
    !PRINT *, "NU1=",NU1 
        
    !DO I=1,SIZE(K_BINARY)

    !   PRINT *, "K_BINARY(",I,")=",K_BINARY(I)
    
    !END DO
    
    T=SIZE(K_BINARY)-NU1

    !PRINT *, "T=CT-NU1=",T
    !PRINT *, "J+NU1=",J+NU1
    !PRINT *, "SIZE(K_BINARY)=",SIZE(K_BINARY)
    !PRINT *, "SIZE(K_BINARY_SCALED)",SIZE(K_BINARY_SCALED)
    
    ALLOCATE(K_BINARY_SCALED(SIZE(K_BINARY)),STAT=ALLOCATESTATUS)
    IF(ALLOCATESTATUS /= 0) STOP "***!!NOT ENOUGH MONEY***"

    DO J=1,T
     
       K_BINARY_SCALED(J+NU1)=K_BINARY(J)       
    
    END DO

    DO L=1,NU1
       K_BINARY_SCALED(L)=0
    END DO

    !DO K=1,SIZE(K_BINARY)

    !   PRINT *, "K_BINARY_SCALED(",K,")=",K_BINARY_SCALED(K)
    
    !END DO
    
    !PRINT *, "SIZE(K_BINARY_SCALED)",SIZE(K_BINARY_SCALED)
 
!  DEALLOCATE(K_BINARY_SCALED)

END SUBROUTINE

SUBROUTINE REVERSED_BINARY(K_BINARY_SCALED,K_BINARY_SCALED_REVERSED)
IMPLICIT NONE

INTEGER, DIMENSION(:) :: K_BINARY_SCALED
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: K_BINARY_SCALED_REVERSED
INTEGER :: I,CT,NU

!       PRINT *, "SIZE(K_BINARY_SCALED)=",SIZE(K_BINARY_SCALED)        
!       PRINT *, "K_BINARY_SCALED=",K_BINARY_SCALED
       
       !PRINT *, "SIZE(K_BINARY_SCALED_REVERSED)=",SIZE(K_BINARY_SCALED_REVERSED)
     
    ALLOCATE(K_BINARY_SCALED_REVERSED(SIZE(K_BINARY_SCALED)),STAT=ALLOCATESTATUS)
    IF(ALLOCATESTATUS /= 0) STOP "***!!!NOT ENOUGH MONEY***"

      
        DO I=1,SIZE(K_BINARY_SCALED)
       
          K_BINARY_SCALED_REVERSED(SIZE(K_BINARY_SCALED)+1-I) = K_BINARY_SCALED(I)
       
        END DO
     
        !K_BINARY_SCALED_REVERSED(1) = K_BINARY_SCALED(1)
       

!      PRINT *, "SIZE(K_BINARY_SCALED_REVERSED)=",SIZE(K_BINARY_SCALED_REVERSED)
!      PRINT *, "K_BINARY_SCALED_REVERSED=",K_BINARY_SCALED_REVERSED

          !PRINT *, K_BINARY_SCALED
          !PRINT *, K_BINARY_SCALED_REVERSED

!    DEALLOCATE(K_BINARY_SCALED_REVERSED)
END SUBROUTINE


SUBROUTINE BINARY2INTEGER(K_BINARY_SCALED_REVERSED, P_INTEGER)
IMPLICIT NONE

INTEGER,DIMENSION(:) :: K_BINARY_SCALED_REVERSED
INTEGER, INTENT(OUT) :: P_INTEGER
INTEGER :: I,J
!***INITIALIZATION***
P_INTEGER=0
   
    !DO I=1,SIZE(K_BINARY_SCALED)

    !   PRINT *, "K_BINARY_SCALED2(",I,")=",K_BINARY_SCALED(I) 
     
    !END DO

    DO J=1,SIZE(K_BINARY_SCALED_REVERSED)

       !PRINT *,"K_BINARY_SCALED2(",J,")=",K_BINARY_SCALED(J)
       !PRINT *, "P_INTEGER1=",P_INTEGER
       P_INTEGER=P_INTEGER+K_BINARY_SCALED_REVERSED(J)*2**(SIZE(K_BINARY_SCALED_REVERSED)-J) 
       !PRINT *, "P_INTEGER2=",P_INTEGER
    END DO

    !PRINT *, "P_INTEGER3=",P_INTEGER 
 
END SUBROUTINE

END PROGRAM FFT
