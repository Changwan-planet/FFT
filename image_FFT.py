import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from scipy.fftpack import fft,fftfreq, ifft
import pandas as pd

#real=np.zeros(4096)
#imag=np.zeros(4096)

input_path="/home/changwan/GPR/A_SCOPE_GPR.txt"

#input_path="/home/changwan/FFT/sine_testFFT.txt"
#input_path2="/home/changwan/FFT/IFFT_output.txt"



#output_path="/home/changwan/FFT/output_testFFT.txt"
output_path="/home/changwan/FFT/FFT_GPR_output.txt"


Input = np.loadtxt(input_path)
#Input2=np.loadtxt(input_path2)	
Output = np.loadtxt(output_path)

Input_2 = pd.DataFrame(Input)	
#Input2_2 = pd.DataFrame(Input2)
Output_2  =pd.DataFrame(Output)

S = 4096


#++++FFT_GRAPH+++++++++++++++

plt.subplot(3,2,1)
plt.plot(Input_2.loc[:,0])
plt.title("INPUT_REAL")
plt.grid()
plt.minorticks_on()

Input_2.loc[:,1]=0

plt.subplot(3,2,2)
plt.plot(Input_2.loc[:,1])
plt.title("INPUT_IMAG")
plt.grid()
plt.minorticks_on()


plt.subplot(3,2,3)
plt.plot(Output_2.loc[:,0])
plt.title("FFT_REAL FROM SUN")
plt.grid()
plt.minorticks_on()


plt.subplot(3,2,4)
plt.plot(Output_2.loc[:,1])
plt.title("FFT_IMAG FROM SUN")
plt.grid()
plt.minorticks_on()


t = np.arange(S)
sp = fft(Input_2.loc[:,0].values)
freq = fftfreq(t.shape[-1])
plt.subplot(3,2,5)
plt.plot(sp.real)
plt.title("FFT_real_FROM SCIPY")
plt.grid()
plt.minorticks_on()


plt.subplot(3,2,6)	
plt.plot(sp.imag)
plt.title("FFT_imag_FROM SCIPY")
plt.grid()
plt.minorticks_on()

#+++++++++++++++++++++++++++++++++++
"""
#++++IFFT_GRAPTH+++++++++++++
plt.subplot(2,2,1)
plt.plot(Input2_2.loc[:,0])
plt.title("IFFT_OUTPUT_REAL")
plt.grid()
plt.minorticks_on()


plt.subplot(2,2,2)
plt.plot(Input2_2.loc[:,1])
plt.title("IFFT_OUTPUT_IMAG")
plt.grid()
plt.minorticks_on()

plt.subplot(2,2,3)
plt.plot(Input2_2.loc[:,0])
plt.title("IFFT_OUTPUT_REAL_PYTHON")
plt.grid()
plt.minorticks_on()


plt.subplot(2,2,4)
plt.plot(Input2_2.loc[:,1])
plt.title("IFFT_OUTPUT_IMAG_PYTHON")
plt.grid()
plt.minorticks_on()
#+++++++++++++++++++++++++++++
"""

plt.tight_layout()
plt.show()


