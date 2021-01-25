Program FFT
IMPLICIT NONE

!INPUT DATA & INITIALIZATION
INTEGER,PARAMETER :: S=32  !THE NUMBER OF SAMPLE POINT
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
 

REAL*8 :: ARG
REAL*8, DIMENSION(0:S-1) :: xreal
REAL*8, DIMENSION(0:S-1) :: ximag
REAL*8, DIMENSION(0:S-1) :: magnitude

REAL*8 :: xreal_value
REAL*8 :: t_sampling_interval

REAL*8 :: TREAL_K
REAL*8 :: TIMAG_K
REAL*8 :: TREAL_I
REAL*8 :: TIMAG_I

REAL*8 :: CC
REAL*8 :: SS

REAL*8 :: T1  !TEMPORARY VALUE
REAL*8 :: T3  !TEMPORARY VALUE
INTEGER :: T4 !TEMPORARY VALUE
INTEGER :: t,  Z, zz,zzz

INTEGER, DIMENSION(:), ALLOCATABLE :: K_BINARY
INTEGER, DIMENSION(:), ALLOCATABLE :: K_BINARY_SCALED
INTEGER, DIMENSION(:), ALLOCATABLE :: K_BINARY_SCALED_REVERSED
INTEGER, DIMENSION(:), ALLOCATABLE :: I_BINARY
INTEGER, DIMENSION(:), ALLOCATABLE :: I_BINARY_REVERSED

REAL :: f

OPEN(10, FILE="sine_testFFT.txt", status='replace')
OPEN(11, FILE="output_testFFT.txt",status='replace')

f = 1.0/8.0

XREAL = 0
XIMAG = 0
T1 = 0
T3 = 0
T4 = 0
P = 0
P_INTEGER = 0
I = 0
I_INTEGER = 0
K = 0
N2 = 0
NU = 0
NU1 = 0
CC = 0
SS = 0
ARG = 0
L = 1

!++++++++++++INPUTDATA++++++++++++++++
!Do t=0, S-1
!   XREAL(t)=COS(2*pi*f*t/(S-1)) 
!   PRINT *, XREAL(t),XIMAG(t)  
!   WRITE(10,*) XREAL(t),XIMAG(t)
!END DO 

t_sampling_interval = 0.25

Do t=0,S-1 
 
   IF(t == 0) THEN  
      
      xreal (t) = 1.0/2.0
   
   ELSE 
 
      xreal_value = -t*t_sampling_interval
      xreal (t) = exp(xreal_value) 
   
   END IF 

END DO



!Do t=0,S-1 
!   IF(t<=2047) THEN  
!      XREAL(t) = -1   
!   ELSE    
!      XREAL(t) = 1   
!   END IF
!END DO

DO t=0,S-1
   PRINT *, xreal(t), ximag(t)
   WRITE(10,*) xreal(t), ximag(t)
END DO

DO 
  IF (N==1) EXIT
  N=N/2
  NU=NU+1
END DO

ALLOCATE(K_BINARY(NU))
ALLOCATE(K_BINARY_SCALED(NU))
ALLOCATE(K_BINARY_SCALED_REVERSED(NU))
ALLOCATE(I_BINARY(NU))
ALLOCATE(I_BINARY_REVERSED(NU))

NU1 = NU - 1
N2 = S / 2**(L)

T4 = S-1   

!THE LAST VALUE OF THE K! I NEED THIS VALUE TO GET THE DIGIT OF 
!THE MOST LARGE BINARY VALUE IN THIS CALCUATION.

150 IF(L<=NU) THEN
 
140      I=1
  
130      CALL INTEGER2BINARY(K,NU,K_BINARY)
         CALL SCALE2RIGHT(K_BINARY,NU1,K_BINARY_SCALED)
         CALL REVERSED_BINARY(K_BINARY_SCALED,K_BINARY_SCALED_REVERSED)  
         CALL BINARY2INTEGER(K_BINARY_SCALED_REVERSED,P_INTEGER)

         ARG = 2*pi*P_INTEGER/S
 
         CC = COS(ARG)
         SS = SIN(ARG)
        
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
!        XREAL(K) = DEFINED ABOVE
!        XIMAG(K) = 0
         
!         X(K) = XREAL(K) + j XIMAG(K)
!         X(K+N2) = XREAL(K+N2) + j XIMAG(K+N2)
!         X(K+N2) * W^p
           
!        ** [FOURIER TRANSFORM]
!         =[XREAL(K+N2) + j XIMAG(K+N2) * (COS(ARG) - j SIN(ARG)]
                
!         =[ XREAL(K+N2) * COS(ARG) + XIMAG(K+N2) *  SIN(ARG)]
!          j [ XIMAG(K+N2) * COS(ARG) -  XREAL(K+N 2) * SIN(ARG) ]  
!
!        ** [INVERSE FOURIER TRANSFORM!
!         =[XREAL(K+N2) + j XIMAG(K+N2) * (COS(ARG) + j SIN(ARG)]
         
!         =[ XREAL(K+N2) * COS(ARG) - XIMAG(K+N2) *  SIN(ARG)]
!          j [ XIMAG(K+N2) * COS(ARG) +  XREAL(K+N 2) * SIN(ARG) ]  
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
         TREAL_K = XREAL(K+N2) * CC + XIMAG(K+N2) * SS
         TIMAG_K = XIMAG(K+N2) * CC - XREAL(K+N2) * SS

         XREAL(K+N2) = XREAL(K) - TREAL_K
         XIMAG(K+N2) = XIMAG(K) - TIMAG_K
     
         XREAL(K) = XREAL(K) + TREAL_K
         XIMAG(K) = XIMAG(K) + TIMAG_K
                
         K=K+1   !GO TO THE NEXT NODE
         
          IF(I==N2) THEN
           
              K = K + N2
              
              IF (K < S-1) THEN
                  GO TO 140              
            
              ELSE 

                  L = L+1
                  N2 = N2/2
                  NU1 = NU1 -1
                  K = 0
                  GO TO 150  !GO TO THE NEXT ARRAY
              
              END IF   
                 
          ELSE

             I=I+1

             GO TO 130

           END IF    

    ELSE 
      
200    CALL INTEGER2BINARY(K,NU,I_BINARY)           
       CALL REVERSED_BINARY(I_BINARY,I_BINARY_REVERSED)
       CALL BINARY2INTEGER (I_BINARY_REVERSED,I_INTEGER)
        
        IF (I_INTEGER < K) THEN
            GO TO 300         
        ELSE
           
            TREAL_I = XREAL(K)
            TIMAG_I = XIMAG(K)

            XREAL(K) = XREAL(I_INTEGER)
            XIMAG(K) = XIMAG(I_INTEGER)
            XREAL(I_INTEGER) = TREAL_I
            XIMAG(I_INTEGER) = TIMAG_I

            GO TO 300
                   
        END IF
            
300      IF (K==(S-1)) THEN

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++++++++++++

!            +++++++++++++++++real part and imaginart part++++++++++++++++++
!            DO Z=0, S-1
!
!               PRINT "(a,i4,a,f20.16,4X,a,i4,a,f20.16)", "XREAL(",Z,")=",&
!                     XREAL(Z),"XIMAG(",Z,")=",XIMAG(Z)      
!              WRITE(11,*) XREAL(Z)
!!              WRITE(11,*) XREAL(Z),XIMAG(Z)
!              PRINT *, XREAL(Z), XIMAG(Z)
!              PRINT *, XREAL(Z)
!            END DO
          
!               +++++++++++ 
!           +++++amplitude+++++ 
!               +++++++++++ 
            DO  zz = 0, S-1
                magnitude(zz) = SQRT( xreal(zz)**2 + ximag(zz)**2)              
            END DO 
         
!           ++++++++++++++++++++++++write+++++++++++++++++++++++++++++++++
            DO zzz = 0, S-1
               WRITE(11,*) xreal(zzz), ximag(zzz), magnitude(zzz)
            END DO
             
                
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


   
             STOP

         ELSE

             K =  K+1
             GO TO 200
           
         END IF

    END IF

DEALLOCATE(K_BINARY)
DEALLOCATE(K_BINARY_SCALED)
DEALLOCATE(K_BINARY_SCALED_REVERSED)
DEALLOCATE(I_BINARY)
DEALLOCATE(I_BINARY_REVERSED)


CONTAINS

SUBROUTINE INTEGER2BINARY(I,NU,B) 
IMPLICIT NONE
    INTEGER, INTENT(IN) :: I
    INTEGER, INTENT(IN) :: NU
    INTEGER, DIMENSION (NU), INTENT(OUT) :: B
    INTEGER :: C,J,L

    B = 0 
    J = I
    
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
 
END SUBROUTINE INTEGER2BINARY
    
SUBROUTINE SCALE2RIGHT(K_BINARY,NU1,K_BINARY_SCALED)
    INTEGER, INTENT(IN) :: NU1
    INTEGER, DIMENSION(:) :: K_BINARY
    INTEGER, DIMENSION(:), INTENT(OUT) :: K_BINARY_SCALED
    INTEGER :: I,J,K,T,L
   
    I=0
    J=1
    T=0
     
    T=SIZE(K_BINARY)-NU1
   
    DO J=1,T
     
       K_BINARY_SCALED(J+NU1)=K_BINARY(J)       
    
    END DO

    DO L=1,NU1
       K_BINARY_SCALED(L)=0
    END DO

END SUBROUTINE

SUBROUTINE REVERSED_BINARY(K_BINARY_SCALED,K_BINARY_SCALED_REVERSED)
IMPLICIT NONE

INTEGER, DIMENSION(:) :: K_BINARY_SCALED
INTEGER, DIMENSION(:), INTENT(OUT) :: K_BINARY_SCALED_REVERSED
INTEGER :: I,NU
     
        DO I=1,SIZE(K_BINARY_SCALED)
       
          K_BINARY_SCALED_REVERSED(SIZE(K_BINARY_SCALED)+1-I) = K_BINARY_SCALED(I)
       
        END DO
    
END SUBROUTINE


SUBROUTINE BINARY2INTEGER(K_BINARY_SCALED_REVERSED, P_INTEGER)
IMPLICIT NONE

INTEGER,DIMENSION(:) :: K_BINARY_SCALED_REVERSED
INTEGER, INTENT(OUT) :: P_INTEGER
INTEGER :: I,J

P_INTEGER=0
   
    DO J=1,SIZE(K_BINARY_SCALED_REVERSED)

       P_INTEGER=P_INTEGER+K_BINARY_SCALED_REVERSED(J)*2**(SIZE(K_BINARY_SCALED_REVERSED)-J) 

    END DO

END SUBROUTINE

END PROGRAM FFT


