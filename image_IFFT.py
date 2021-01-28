import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import scipy
from scipy.fftpack import fft,fftfreq, ifft
import pandas as pd
import math

#GPR_SIGNAL
#input_path="/home/changwan/GPR/A_SCOPE_GPR2.txt"
#output_path="/home/changwan/FFT/FFT_GPR_output.txt"


#source-Input_2
input_path="/home/changwan/FFT/sine_testFFT.txt"          #source
Input = np.loadtxt(input_path)
Input_2 = pd.DataFrame(Input)	
#######


#ifft data-Input2_2
input_path2="/home/changwan/FFT/IFFT_output.txt"          #ifft result from Fortra90 code
Input2=np.loadtxt(input_path2)	
index_1 = np.linspace(0, (Input2.shape[0]-1), Input2.shape[0], dtype = 'int64')
#columns_1 = ['ifft_real_from_sun','ifft_imag_from_sun','ifft_magnitude_from_sun']
columns_1 = ['ifft_real_from_sun','ifft_imag_from_sun']
Input2_2  = pd.DataFrame(Input2, index_1, columns_1)
#######


#fft data-Otput_2
output_path="/home/changwan/FFT/output_testFFT.txt"       #fft result from the Foran90 code
Output = np.loadtxt(output_path)
index_2 = np.linspace(0, (Output.shape[0]-1), Output.shape[0], dtype = 'int64')
#columns_2 = ['fft_real_from_sun','fft_imag_from_sun','fft_magnitude_from_sun']
columns_2 = ['fft_real_from_sun','fft_imag_from_sun']
Output_2  = pd.DataFrame(Output, index_2, columns_2)
########



S = 32                                  #Sampling points

#ifft_sci=scipy.ifft(Output_2["fft_magnitude_from_sun"].values)
ifft_sci=scipy.ifft(scipy.fft(Input_2.loc[:,0].values))



print(ifft_sci)
#print(ifft_sci.real)
#print(ifft_sci.imag)



#py_mag=np.zeros(S)
#sp = fft(Input_2.loc[:,0].values)       #fft result from scipy fft
#py_mag =  np.abs(sp)                    #calcaulating amplitude from scipy


ti = 10                                 #tick interval                               
t_sam_in=0.25                           #time_sampling_interval
f_sam_in=1.0/(t_sam_in*S)               #frequency_sampling_interval
xt=np.arange(0,S-1, ti)
xt2=np.arange(0,(S/2)-1,ti/2)


#Output_2["fft_real_from_scipy"] = sp.real
#Output_2["fft_imag_from_scipy"] = sp.imag
#Output_2["fft_magnitude_from_scipy"] = py_mag

#Output_2.info()
#print(Output_2.head())


#++++IFFT_GRAPTH+++++++++++++
#source
plt.subplot(4,3,1)
plt.plot(Input_2.loc[:,0])
plt.title("input_real")
plt.xticks(xt,(i*t_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("TIME")
plt.ylabel("AMPLITUDE")


plt.subplot(4,3,2)
plt.plot(Input_2.loc[:,1])
plt.title("input_imag")
plt.xticks(xt,(i*t_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("TIME")
plt.ylabel("AMPLITUDE")


#FFT result from sun
plt.subplot(4,3,4)
plt.plot(Output_2["fft_real_from_sun"])
plt.title("fft_real_from_sun")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")


plt.subplot(4,3,5)
plt.plot(Output_2["fft_imag_from_sun"])
plt.title("fft_imag_from_sun")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")


#plt.subplot(4,3,6)
#plt.plot(Output_2["fft_magnitude_from_sun"].loc[0:S/2])
#plt.title("fft_magnitude_from_sun")
#plt.xticks(xt2,(i*f_sam_in for i in xt2))
#plt.grid()
#plt.minorticks_on()
#plt.xlabel("FREQUENCY")
#plt.ylabel("MAGNITUDE")




a = round(Input2_2["ifft_real_from_sun"].min())
b = round(Input2_2["ifft_real_from_sun"].max())


ylim_min = -1+a
ylim_max = b




#ifft from sun and sci_py
plt.subplot(4,3,7)
plt.plot(Input2_2["ifft_real_from_sun"], 'or',label='from Sun')
plt.plot(ifft_sci.real, '*b',label='from sci_py')
plt.title("ifft_real")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.ylim(ylim_min , ylim_max)
plt.legend()
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")



plt.subplot(4,3,8)
plt.plot(Input2_2["ifft_imag_from_sun"],'or', label='from Sun')
plt.plot(ifft_sci.imag,'*b',label='from sci_py')
plt.title("ifft_imag")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.ylim(ylim_min , ylim_max)
plt.legend()
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")

#comparison between sun and original data
plt.subplot(4,3,10)
plt.plot(Input2_2["ifft_real_from_sun"], 'or',label='from Sun')
plt.plot(Input_2.loc[:,0],'*b',label='original data')
plt.title("ifft_real")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.ylim(ylim_min , ylim_max)
plt.legend()
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")


print("")
print('original_real_min=',Input_2.loc[:,0].min())
print('ifft_real_from_sun_min=',Input2_2["ifft_real_from_sun"].min())
print('ifft_real_from_sci_py_min=',ifft_sci.real.min())

print("")
print('original_real_max==',Input_2.loc[:,0].max())
print('ifft_real_from_sun_max==',Input2_2["ifft_real_from_sun"].max())
print('ifft_real_from_sci_py_max==',ifft_sci.real.max())


"""
plt.subplot(4,3,11)
plt.plot(Input2_2["ifft_imag_from_sun"],'or', label='from Sun')
plt.plot(ifft_sci.imag,'*b',label='from sci_py')
plt.title("ifft_imag")
plt.xticks(xt,(i*f_sam_in for i in xt))
plt.ylim(ylim_min , ylim_max)
plt.legend()
plt.grid()
plt.minorticks_on()
plt.xlabel("FREQUENCY")
plt.ylabel("AMPLITUDE")



#plt.subplot(4,3,9)
#plt.plot(Input2_2["ifft_magnitude_from_sun"].loc[0:S/2])
#plt.title("ifft_magnitude_from_sun")
#plt.xticks(xt2,(i*f_sam_in for i in xt2))
#plt.grid()
#plt.minorticks_on()
#plt.xlabel("FREQUENCY")
#plt.ylabel("MAGNITUDE")

#plt.subplot(4,3,12)
#plt.plot(ifft_sci)
#plt.plot(ifft_sci.loc[0:S/2])
#plt.title("ifft_magnitude_from_scipy")
#plt.xticks(xt2,(i*f_sam_in for i in xt2))
#plt.grid()
#plt.minorticks_on()
#plt.xlabel("FREQUENCY")
#plt.ylabel("MAGNITUDE")
"""
#+++++++++++++++++++++++++++++



plt.tight_layout()

#display image
plt.show()

#+++++++++++++++++++++++++++++++++++



