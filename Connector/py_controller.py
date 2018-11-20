import serial
import threading
import time
import sys
from multiprocessing.pool import ThreadPool
import psutil
import RPi.GPIO as GPIO

_PROCESS=True #if True then multiprocessing run
_LISTENING = True #Listening serial port
_TEMP=True #Get update cpu temperature

def readArdu():
    global _LISTENING
    global _PROCESS
    print(">>Start Listening.")
    info = [0,0]
    data = ''
    while _PROCESS:
        #  on rpi
        if ser.inWaiting()>0:
            tmp = str(ser.read())
            if tmp[2]=='\\':
                if data:
                    mode = data[0]+data[1]
                    if(mode=='s-'):
                        print('>>Light: {}'.format(data[2]))
                    elif(mode=='b-'):
                        print('>>Button Down: {}'.format(data[2]))
                data = ''
            else:
                data = data + tmp[2]
            
        
    _LISTENING = False
    #print('Stop Listening')
    ser.close()
    
    
def readTemp():
    global _TEMP
    global _PROCESS
    fan = False
    maxTemp = 50
        
    print(">>Start Temp Reading.")
    while _PROCESS:
        time.sleep(3)
        temp = psutil.sensors_temperatures()
        temp = temp['cpu-thermal']
        temp = temp[0]
        temp = temp[1]
        if temp>=maxTemp and fan==False:
            GPIO.output(21, GPIO.HIGH)
            fan = True
        elif temp<maxTemp and fan==True:
            GPIO.output(21, GPIO.LOW)
            fan = False
        print('CPU Temp: {}'.format(temp))
        sendMsg('p-','')
    _TEMP = False    
    fan = False
    #print('Stop Temp Loop')
    

def sendMsg(prefix='s-', mess='', suffix=''):
    mess = str(prefix) + str(mess) + str(suffix)
    ser.write(mess.encode("UTF-8")) # on rpi

def main():
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(21, GPIO.OUT)
    GPIO.output(21, GPIO.LOW)
    global _PROCESS
    global _LISTENING
    global _TEMP
    pool1 = ThreadPool(processes=1)
    pool2 = ThreadPool(processes=2)
    pool1.apply_async(readArdu)
    pool2.apply_async(readTemp)
    menuInput = ''
    while menuInput!='exit':
        print()
        print("1. Send test message.\n0. Exit")
        try:
            menuInput = int(input())
        except:
            print('-')
        # Close app
        if menuInput == 0:
            sendMsg('','@','')
            _PROCESS = False
            pool1.terminate()
            pool1.join()
            pool2.terminate()
            pool2.join()
            print('Closing app')
            menuInput = 'exit'

        # Pin test
        if menuInput == 1:   
            sendMsg('t-')

    GPIO.output(21, GPIO.LOW)
    print('App closed.')

isConnection = False

try:
    ser = serial.Serial("/dev/ttyACM0",9600)
    isConnection = True
    print("Connected to port ttyACM0")
except:
    try:
        ser = serial.Serial("/dev/ttyACM1",9600)
        isConnection = True
        print("Connected to port ttyACM1")
    except:
        try:
            ser = serial.Serial("COM3",9600)
            isConnection = True
            print("Connected to port COM3")
        except:
            print("Brak polaczenia z portem.")

if isConnection:
    main()
