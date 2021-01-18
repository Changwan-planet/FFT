import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
from scipy.fftpack import fft,fftfreq, ifft
import pandas as pd
import math

#GPR_SIGNAL
input_path="/home/changwan/GPR/A_SCOPE_GPR2.txt"
output_path="/home/changwan/FFT/FFT_GPR_output.txt"

Input = np.loadtxt(input_path)
#Input2=np.loadtxt(input_path2)	

Input_2 = pd.DataFrame(Input)	
#Input2_2 = pd.DataFrame(Input2)

Output = np.loadtxt(output_path)
index = np.linspace(0, (Output.shape[0]-1), Output.shape[0], dtype = 'int64')

#print(Output.shape[0])
#print(Output.shape[1])

columns = ['fft_real_from_sun','fft_imag_from_sun','fft_magnitude_from_sun']
Output_2  = pd.DataFrame(Output, index, columns)


S = 4096                              #Sampling points
Input_2.loc[:,1]=0                    #zero initialization of the imaginary input
py_mag=np.zeros(S)
sp = fft(Input_2.loc[:,0].values)     #fft result from scipy fft
py_mag =  np.abs(sp)                  #calcaulating amplitude from scipy

#    +++++++++++ 
#+++++FFT_GRAPH+++++
#    +++++++++++


ti = S/4                                #tick interval                               
t_sam_in=0.25                           #time_sampling_interval
f_sam_in=1.0/(t_sam_in*S)               #frequency_sampling_interval
xt=np.arange(0,S-1, ti)
xt2=np.arange(0,(S/2)-1,ti/2)

Output_2["fft_real_from_scipy"] = sp.real
Output_2["fft_imag_from_scipy"] = sp.imag
Output_2["fft_magnitude_from_scipy"] = py_mag

#Output_2.info()
print(Output_2.head())


plt.subplot(3,3,1)
plt.plot(Input_2.loc[:,0])
plt.title("input_real")
plt.xticks(xt,(i*t_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("TIME")
plt.ylabel("AMPLITUDE")


plt.subplot(3,3,2)
plt.plot(Input_2.loc[:,1])
plt.title("input_imag")
plt.xticks(xt,(i*t_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("TIME")
plt.ylabel("AMPLITUDE")


plt.subplot(3,3,4)
plt.plot(Output_2["fft_real_from_sun"])
plt.title("fft_real_from_sun")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")


plt.subplot(3,3,5)
plt.plot(Output_2["fft_imag_from_sun"])
plt.title("fft_imag_from_sun")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")


plt.subplot(3,3,6)
plt.plot(Output_2["fft_magnitude_from_sun"].loc[0:S/2])
plt.title("fft_magnitude_from_sun")
plt.xticks(xt2,(i*f_sam_in for i in xt2))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("MAGNITUDE")


plt.subplot(3,3,7)
plt.plot(sp.real)
plt.title("fft_real_from_scipy")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")


plt.subplot(3,3,8)
plt.plot(sp.imag)
plt.title("fft_imag_from_scipy")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")


plt.subplot(3,3,9)
plt.plot(Output_2["fft_magnitude_from_scipy"].loc[0:S/2])
plt.title("fft_magnitude from scipy")
plt.xticks(xt2,(i*f_sam_in for i in xt2))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("MAGNITUDE")


plt.tight_layout()

#display image
plt.show()

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


