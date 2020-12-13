Program FFT
Implicit none

!INPUT DATA & INITIALIZATION
INTEGER,PARAMETER :: N=4      !THE NUMBER OF SAMPLE POINTS
INTEGER           :: K        !SAMPLE POINT
REAL, DIMENSION :: XREAL
REAL, DIMENSION :: XIMAG

!COMPLEX,Dimension(N) :: x  !DATA VECTOR. IMAGINARTY PART SHOULD BE SET TO  ZERO.



INTEGER :: N2                 !THE SPACING BETWEEN DUAL NODES

INTEGER :: NU                 !GAMMA 
INTEGER :: L=1                !GAMMA FOR DO LOOP                  

 
INTEGER :: NU1                !THE NU1 IS THE RIGHT SHIFT REQUIRED WHEN DETERMINING THE VALUE OF P.
INTEGER :: I                  !THIS COUNTER MONITORS THE NUMBER OF DUAL
!NODE PAIRS THAT HAVE BEEN CONSIDERED.                
!THE COUNTER I IS THE CONTROL FOR DETERMINING WHEN THE PROGRAM MUST SKIP.
COMPLEX :: W=EXP(-2*PI()/N)
INTEGER :: M 
INTEGER :: T1                 !TEMPORARY VALUE
INTEGER :: T3                 !TEMPORARY VALUE

!RELATIONSHIPS & INITIALIZATION 
K=0

!NU1 = NU-1 

!DEFINE NU (GAMMA)
NU = 0
    
DO WHILE(N==1)

  N=N/2
  NU=NU+1

END DO 


PRINT*, "NU (GAMMA)=",NU

  DO L=1,NU

     N2 = N /2**(L)

      DO I=1,N2
       
        !J=I/(2**(L))            
        NU1=NU-L


!100 IF (L < NU)    !TO SEE IF ALL ARRAYS HAVE BEEN COMPUTED
!110    I=1

120    M=INT(I/(2**NU1)) 

       P=IBR(M)

       T1=(W**p)*x(k+N2)    

       k=k+1

       IF (I /= N2) 
          GO TO 120
       
       ELSE IF 
          k=k+N2
               IF (k < N -1)
                  GO TO 110
               ELSE IF
                  l=l+1
                  N2=N2/2
                  NU1=NU1-1
                  k=0
                  GO TO 100
          
!    ELSE IF (L > NU) !WE PROCEED TO UNSCRAMBLE THE FINAL RESULTS

130     i=IBR(k)
         
         IF (i<k)   

!THIS STEP IS NECESSARY TO PROHIBIT THE ALTERING OF PREVIOUSLY UNSCRAMBLED
!NODES.

140          IF (k=N-1)

                STOP
             ELSE IF 
                k = k + 1
                GO TO 130 
         ELSE IF 
             T3=x(k)
             x(k) = x(i)
             x(i)=T3
             GO TO 140 


             

FUNCTION IBR(M)

INTEGER :: M, NU, J2, I1, IBR

!INITIALIZATION
I1 = 1
IBR = 0


150  IF (I1>NU)
        RETURN IBR
    
     ELSE IF
            J2 = M/2
            IBR = 2*IBR + (M-2*J2)
            M = J2

            I1=I1+1

            GO TO 150


