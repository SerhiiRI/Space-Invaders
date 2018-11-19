import serial
import threading
import time
import sys
from multiprocessing.pool import ThreadPool

_LISTENING = True

def readArdu():
    print("Start Listening.")
    info = [0,0]
    data = ''
    while _LISTENING:

        #  on rpi
        if ser.inWaiting()>0:
            data = data + ser.read()
            info[0]=1
        else:
            if info[0]==1:
                info[1]=1
                print(data)
                info[0]=0
                info[1]=0

        # on windows and linux
        # m = ser.readline()
        # m = str(m)
        # if m[2]=='i':
        #     print("\nRead message: {}".format(m))

        # if m[2]=='b':
        #     if(m[4]==' '):
        #         print('Space')
        #     else:
        #         print(m[4])
    ser.close()

def sendMsg(prefix='s-', mess='', suffix=''):
    '''
    mode space message
    s like send
    s-message
    '''
    mess = str(prefix) + str(mess) + str(suffix)
    #ser.write(bytes(mess, "UTF-8")) # on windows and linux
    ser.write(mess.encode("UTF-8")) # on rpi

def main():
    global _LISTENING
    pool = ThreadPool(processes=1)
    pool.apply_async(readArdu)
    menuInput = ''
    while menuInput!='exit':
        print()
        print("1. Send test message.\n2. Send your own led change.\n3. Add yours keys.\n0. Exit")
        menuInput = int(input())
        # Close app
        if menuInput == 0:
            sendMsg('','@','')
            _LISTENING = False
            print('Finish listening.')
            pool.terminate()
            print('Pool terminate.')
            pool.join()
            print('Pool join.')
            menuInput = 'exit'

        # Pin test
        if menuInput == 1:   
            sendMsg('t-')

        # Pin mode
        if menuInput == 2:
            print("Choose pin: ")
            mess = str(input())
            sendMsg('l-',mess)
        
        # Change input buttons
        if menuInput == 3:
            movement = []
            print('Move left: ')
            inp = input()
            movement.append(inp[0])
            repeat = True
            while repeat:
                print('Move Right: ')
                inp = input()
                repeat = False
                if inp in movement:
                    repeat = True
            movement.append(inp[0])
            repeat = True
            while repeat:
                print('Shoot: ')
                inp = input()
                repeat = False
                if inp in movement:
                    repeat = True
            movement.append(inp[0])
            print('HotKey: {} {} {}'.format(movement[0], movement[1], movement[2]))
            sendMsg('b-', movement[0] + movement[1] + movement[2])
    print('Close app.')

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
