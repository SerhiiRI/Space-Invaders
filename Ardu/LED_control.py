import serial
import threading
from pynput.keyboard import Key, Controller

def readArdu():
    read = True
    print("Start Listening.")
    while read:
        m = ser.readline()
        m = str(m)
        if m[2]=='i':
            print("\nRead message: {}".format(m))

        if m[2]=='x':
            ser.write(b'a')
            read = False

        if m[2]=='b':
            if(m[4]==' '):
                print('Space')
            else:
                print(m[4])

    print("Stop loop.")


def main():
    AL = threading.Thread(target=readArdu)
    AL.start()
    menuInput = ''
    while menuInput!='exit':
        print()
        print("1. Send test message.\n2. Send your own message.\n3. Add yours keys.\n0. Exit")
        menuInput = int(input())
        if menuInput == 0:
            ser.write(b'x')
            menuInput = 'exit'
            print('Close app')

        if menuInput == 1:   
            ser.write(b'567')

        if menuInput == 2:
            print("Choose pin: ")
            mess = str(input())
            print(bytes(mess,"UTF-8"))
            ser.write(bytes(mess, "UTF-8"))
        
        if menuInput == 3:
            movement = []
            print('Move left: ')
            inp = input()
            movement.append(inp[0])
            print('Move Right: ')
            inp = input()
            #if inp in movement # todo
            movement.append(inp[0])
            print('Shoot: ')
            inp = input()
            movement.append(inp[0])


'''
ser = serial.Serial("/dev/ttyACM0",9600)
main()
'''

try:
    ser = serial.Serial("/dev/ttyACM0",9600)
    print("Connected to port ttyACM0")
    main()
except:
    try:
        ser = serial.Serial("/dev/ttyACM1",9600)
        print("Connected to port ttyACM1")
        main()
    except:
        try:
            ser = serial.Serial("COM3",9600)
            print("Connected to port COM3")
            main()
        except:
            print("Brak polaczenia z portem.")

