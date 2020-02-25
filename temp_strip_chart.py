import numpy as np
import cv2
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math
import serial
import serial.tools.list_ports
import plotly.graph_objects as go
import time

try:
    ser.close() # try to close the last opened port
except:
    print('')

portlist=list(serial.tools.list_ports.comports())
print ('Available serial ports (will try to open the last one):')
for item in portlist:
    print (item[0])

fahrenheit=["Part 1. It was a pleasure to burn.",
            "It was a special pleasure to see things eaten",
            "to see things blackened and changed.",
            "With the brass nozzles in his fists,",
            "with this great python spitting it's venomous kerosene upon the world,",
            "the blood pounded in his head,",
            "and his hands were the hands of some amazing conductor",
            " playing all the symphonies of blazing and burning",
            " to bring down the tatters and charcoal ruins of history.",
            "With his symbolic helmet",
            "numbered 451 on his stolid head",
            "and his eyes all orange flame",
            "with the thought of what came next",
            "he flicked the igniter",
            "and the house jumped up",
            "in a gorging fire that burned the evening sky",
            "red and black.",
            "He strode in a swarm of fireflies.",
            "He wanted above all,",
            "like the old joke,",
            "to shove a marshmallow on a stick in the furance",
            "while the flapping pigeon-winged books died on the porch",
            "and lawn of the house."]

# configure the serial port
ser = serial.Serial(
    port=item[0],
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_TWO,
    bytesize=serial.EIGHTBITS
)
ser.isOpen()

xsize=60
cap = cv2.VideoCapture(0)
fourcc = cv2.VideoWriter_fourcc(*'XVID')
out = cv2.VideoWriter('output.avi',fourcc, 20.0, (640,480))

def data_gen():
    line = 0
    t = data_gen.t
    while True:
        t+=1
        strin = ser.readline()
        temp=strin.decode('ASCII')
        val=re.finall('\d+\.\d+", temp)
        val1=float(strin)
        val2=(9.0/5.0*float(strin)+32.0)
        val3=273.0+float(strin)
        if val>150:
                      if line<23:
                          print(fahrenheit[line])
                          line+=1
        yield t, val, val1, val2, val3

def run(data):
    # update the data
    t,y,y1,y2,y3 = data
    if t>-1 or t:
        xdata.append(t)
        ydata.append(y)
        ydata1.append(y1)
        ydata2.append(y2)
        ydata3.append(y3)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
        line.set_data(xdata, ydata)
        line1.set_data(xdata, ydata1)
        line2.set_data(xdata, ydata2)
        line3.set_data(xdata, ydata3)

        ret, frame = cap.read()
        frame = cv2.flip(frame,1)
        out.write(frame)
        cv2.imshow('frame',frame)
    return line, line1, line2, line3

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, =ax.plot([], [], lw=2)
line1, = ax.plot([], [], lw=2, label='Celcius')
line2, = ax.plot([], [], lw=2, label='Farenheit')
line3, = ax.plot([], [], lw=2, label='Kelvin')
ax.set_ylim(10, 500)
ax.set_xlim(0, xsize)
ax.grid()
ax.legend()
xdata, ydata, ydata1, ydata2, ydata3 = [], [], [], [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, 
repeat=False)
plt.show()

# Release everything if job is finished
cap.release()
out.release()
cv2.destroyAllWindows()
