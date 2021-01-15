import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
from scipy.fftpack import fft,fftfreq, ifft
import pandas as pd
import math


input_path="/home/changwan/GPR/A_SCOPE_GPR2.txt"

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
Input_2.loc[:,1]=0                    #zero initialization of the imaginary input
py_mag=np.zeros(S)
sp = fft(Input_2.loc[:,0].values)     #fft result from scipy fft
py_mag =  np.abs(sp)                  #calcaulating amplitude from scipy

#    +++++++++++ 
#+++++FFT_GRAPH+++++
#    +++++++++++

fig,graph = plt.subplots(3,3)


graph[0,0].plot(Input_2.loc[:,0])
graph[0,1].plot(Input_2.loc[:,1])
graph[1,0].plot(Output_2.loc[:,0])
graph[1,1].plot(Output_2.loc[:,1])
graph[1,2].plot(Output_2.loc[:,2])
graph[2,0].plot(sp.real)
graph[2,1].plot(sp.imag)
graph[2,2].plot(py_mag)


#    +++++++++++++++++++++++ 
#+++++decorating the graphs+++++
#    +++++++++++++++++++++++
#title
graph[0,0].set_title("input_real")
graph[0,1].set_title("input_imag")
graph[1,0].set_title("fft_real from sun")
graph[1,1].set_title("fft_imag from sun")
graph[1,2].set_title("fft_magnitude from sun")
graph[2,0].set_title("fft_real_from_scipy")
graph[2,1].set_title("fft_imag_from_scipy")
graph[2,2].set_title("fft_magnitude_from_scipy")


#limit
#t_sam_in=0.25                           #time_sampling_interval
#f_sam_in=1.0/(t_sam_in*S)               #frequency_sampling_interval
#minus_f_sam_in=-f_sam_in                  #minus_frequency_sampling_interval_

#graph[0,0].set_xlim([0,(S-1)*t_sam_in])
#graph[0,1].set_xlim([0,(S-1)*t_sam_in])
#graph[1,2].set_xlim([0,(S-1)*f_sam_in])
#graph[2,2].set_xlim([0,(S-1)*f_sam_in])


#tick
#x_f_domain=np.arange(0,S,1) 


#x_f1_domain=np.arange(0,S/2,S/4) * f_sam_in
#x_f2_domain=np.arange(S/2,1,-S/4) * minus_f_sam_in
#x_f2_domain=np.arange(S/2,1,S/4)


#print(x_f_domain)
#print(x_f2_domain)

#print(x_f1_domain.shape)
#print(x_f2_domain.shape)

#x_f_domain=np.concatenate((x_f1_domain,x_f2_domain))

#print(x_f_domain)

#graph[1,0].set_xticks([i*f_sam_in for i in x_f_domain])
#graph[1,1].set_xticks(x_f_domain)
#graph[1,2].set_xticks(x_f_domain)
#graph[2,0].set_xticks(x_f_domain)
#graph[2,1].set_xticks(x_f_domain)
#graph[2,2].set_xticks(x_f_domain)


#minortick
graph[0,0].minorticks_on()
graph[0,1].minorticks_on()
graph[1,0].minorticks_on()
graph[1,1].minorticks_on()
graph[1,2].minorticks_on()
graph[2,0].minorticks_on()
graph[2,1].minorticks_on()
graph[2,2].minorticks_on()

#grid
graph[0,0].grid()
graph[0,1].grid()
graph[1,0].grid()
graph[1,1].grid()
graph[1,2].grid()
graph[2,0].grid()
graph[2,1].grid()
graph[2,2].grid()



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


