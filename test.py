import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import math
from scipy.fftpack import 


input_path="/home/changwan/GPR/A_SCOPE_GPR2.txt"
input_path2="/home/changwan/FFT/IFFT_output.txt"
input_paht3="/home/changwan/FFT/FFT_GPR_output.txt"


path="IFFT_output.txt"

data = np.loadtxt(input_path)
data_2 = pd.DataFrame(data)

data2 = np.loadtxt(input_path2)
data2_2 = pd.DataFrame(data2)

data3 = np.loadtxt(input_path3)
data3_2 = pd.DataFrame(data3)

plt.subplot(2,3,1)
plt.plot(data_2.loc[:,0])
plt.title("input_real")

plt.subplot(2,3,2)
plt.plot(data2.loc[:,0])
plt.title("ifft_real from sun")


plt.subplot(2,3,3)
plt.plot(data2.loc[:,0])
plt.title("ifft_real from sun")

plt.subplot(2,3,4)
plt.plot(data2.loc[:,1])
plt.title("ifft_imag from sun")

plt.subplot(2,3,5)
plt.plot(data2.loc[:,0])
plt.title("ifft_real from sun")

plt.subplot(2,3,6)
plt.plot(data2.loc[:,1])
plt.title("ifft_imag from sun")


plt.tight_layout()
plt.show()
