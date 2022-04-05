import matplotlib.pyplot as plt
import numpy as np

data = np.genfromtxt('expt2_sub_x_classifed_x_ROI_x_Task.csv', delimiter=',')
# centre on chance level
data = data - 0.5

# data = data.reshape((10, 2, 3, 3))
data = data.reshape((18, 4, 4, 3))

barWidth = 0.25
r1 = np.arange(len(bars1))
r2 = [x + barWidth for x in r1]

fig, ax_array = plt.subplots(1,2)
ax1 = ax_array[0]
ax2 = ax_array[1]

bars1 = np.median(data[:,:,:,:], axis=0)
bars1.shape
for i in range(8):
    ax.bar(r1, bars1, color='#7f6d5f', width=barWidth, edgecolor='white', label='var1')

plt.show()
