!> \file test.f90

PROGRAM test

USE module_signal


IMPLICIT NONE

INTEGER :: aa, bb, cc

INTEGER, PARAMETER :: S =4096

REAL*8, DIMENSION(0:S-1) :: xreal
REAL*8, DIMENSION(0:S-1) :: ximag
REAL*8, DIMENSION(0:S-1) :: fft_real
REAL*8, DIMENSION(0:S-1) :: fft_imag
REAL*8, DIMENSION(0:S-1) :: ifft_real
REAL*8, DIMENSION(0:S-1) :: ifft_imag


INTEGER :: i, ii
OPEN(UNIT=10, FILE = "/home/changwan/GPR/A_SCOPE_GPR.txt", STATUS='OLD', FORM='FORMATTED',ACTION='READ')
OPEN(UNIT=20, FILE = "test_module.txt", STATUS='REPLACE', ACTION='WRITE')

READ(10,*) xreal

aa=1
bb=2

!CALL add(aa,bb,cc)

xreal = xreal - xreal(0)


CALL fft(xreal, ximag, S, fft_real, fft_imag)

!DO i = 0,S-1
!   WRITE(20,*) fft_real(i), fft_imag(i)
!END DO

CALL ifft(fft_real, fft_imag, S, ifft_real, ifft_imag)

DO ii = 0, S-1

   PRINT*,     xreal(ii), ifft_real(ii)
   WRITE(20,*) xreal(ii), ifft_real(ii)
!   WRITE(20,*) ifft_real(ii), ifft_imag(ii)

END DO

END PROGRAM
