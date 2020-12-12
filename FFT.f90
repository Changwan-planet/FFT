Program FFT
Implicit none

!INPUT DATA & INITIALIZATION
INTEGER, PARAMETER :: N=1024  !THE NUMBER OF SAMPLE POINTS
COMPLEX,Dimension(N) :: x  !DATA VECTOR. IMAGINARTY PART SHOULD BE SET TO  ZERO.
COMPLEX :: 
INTEGER :: gam
INTEGER :: l=1                !ARRAY NUMBER
INTEGER :: N
INTEGER :: N2                 !THE SPACING BETWEEN DUAL NODES
INTEGER :: NU
INTEGER :: NU1                !THE NU1 IS THE RIGHT SHIFT REQUIRED WHEN DETERMINING THE VALUE OF P.
INTEGER :: I                  !THIS COUNTER MONITORS THE NUMBER OF DUAL
!NODE PAIRS THAT HAVE BEEN CONSIDERED.                
!THE COUNTER I IS THE CONTROL FOR DETERMINING WHEN THE PROGRAM MUST SKIP.
COMPLEX :: W=EXP(-2*PI()/N)
INTEGER :: M 
INTEGER :: T1                 !TEMPORARY VALUE
INTEGER :: T3                 !TEMPORARY VALUE

!RELATIONSHIP
gam=log2(N)
NU=gam
N2=N/(2**(l))            
NU1=gam-l


100 IF (l < gam)    !TO SEE IF ALL ARRAYS HAVE BEEN COMPUTED
110    I=1

120    M=INT(k/(2**NU1)) 

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
          
    ELSE IF (l > gam) !WE PROCEED TO UNSCRAMBLE THE FINAL RESULTS

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

INTEGER :: M
INTEGER :: NU
INTEGER :: J2
INTEGER :: I1
INTEGER :: IBR

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


