import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from scipy.fft import fft, ifft


input_path="/home/Changwan/Fortran/FFT/sine_testFFT.txt"
output_path="/home/Changwan/Fortran/FFT/output_testFFT.txt"

Source=np.loadtxt(input_path)
Output=np.loadtxt(output_path)


plt.subplot(3,1,1)
plt.plot(Source)
plt.title("SOURCE")
plt.grid()
plt.minorticks_on()

plt.subplot(3,1,2)
plt.plot(Output)
plt.title("FFT")
plt.grid()
plt.minorticks_on()

#print (Source)
FFT_scipy = fft(Source)

plt.subplot(3,1,3)
plt.plot(FFT_scipy)	
plt.title("FFT FROM SCIPY")
plt.grid()
plt.minorticks_on()

plt.show()
