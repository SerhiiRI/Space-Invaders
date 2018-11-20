import serial
import threading
import time
import sys
from multiprocessing.pool import ThreadPool

_PROCESS=True #if True then multiprocessing run
_LISTENING = True #Listening serial port
_TEMP=True #Get update cpu temperature

def readArdu():
    global _LISTENING
    print(">>Start Listening.")
    info = [0,0]
    data = ''
    while _PROCESS:

        #  on rpi
        if ser.inWaiting()>0:
            data = data + ser.read()
            info[0]=1
        else:
            if info[0]==1:
                info[1]=1
                print('>>{}'.format(data))
                info[0]=0
                info[1]=0
    _LISTENING = False
    #print('Stop Listening')
    ser.close()
    
    
def readTemp():
    global _TEMP
    print(">>Start Temp Reading.")
    while _PROCESS:
        time.sleep(5)
        print('CPU Temp: 25')
    _TEMP = False    
    #print('Stop Temp Loop')
    

def sendMsg(prefix='s-', mess='', suffix=''):
    mess = str(prefix) + str(mess) + str(suffix)
    ser.write(mess.encode("UTF-8")) # on rpi

def main():
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
        menuInput = int(input())
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
