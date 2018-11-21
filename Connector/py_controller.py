import serial
import threading
import time
import sys
from multiprocessing.pool import ThreadPool
import psutil
import RPi.GPIO as GPIO
import Adafruit_CharLCD as LCD

_PROCESS=True #if True then multiprocessing run
_LISTENING = True #Listening serial port
_TEMP=True #Get update cpu temperature
# Raspberry Pi pin configuration:
lcd_rs        = 25#= 22  # Note this might need to be changed to 21 for older revision Pi's.
lcd_en        = 24#= 18
lcd_d4        = 23#= 16
lcd_d5        = 17#= 12
lcd_d6        = 18#= 13
lcd_d7        = 22#= 15
lcd_backlight = 2

# Define LCD column and row size for 16x2 LCD.
lcd_columns = 16
lcd_rows    = 2

# Initialize the LCD using the pins above.
_lcd = LCD.Adafruit_CharLCD(lcd_rs, lcd_en, lcd_d4, lcd_d5, lcd_d6, lcd_d7,
                               lcd_columns, lcd_rows, lcd_backlight)
    


def readArdu():
    global _LISTENING
    global _PROCESS
    print(">>Start Listening.")
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
                        open('../bright','w').write(data[2])
                    elif(mode=='b-'):
                        print('>>Button Down: {}'.format(data[2]))
                        open('../key','w').write(data[2])
                        #print('>>Button Down: {}'.format(open('btn.txt').read()))
                data = ''
            else:
                data = data + tmp[2]
        
    _LISTENING = False
    #print('Stop Listening')
    ser.close()
    
    
def readTemp():
    global _lcd
    global _TEMP
    global _PROCESS
    global _lcd
    fan = False
    maxTemp = 50
    info = [0,0,0]
    first = True
    
    print(">>Start Temp Reading.")
    while _PROCESS:
        
        try:
            filedata = open('../DATA').read()
        except:
            filedata = '3;0;33'
        
        filedata = filedata.split(';')
        
        if first:
            info[0]=filedata[0]
            info[1]=filedata[1]
            info[2]=filedata[2]
            
        if info[0] != filedata[0] or first:
            info[0] = filedata[0] #life
            if info[0] == 3:
                sendMsg('l-','123')
            if info[0] == 2:
                sendMsg('l-','1')
            if info[0] == 1:
                sendMsg('l-','2')
            if info[0] == 0:
                sendMsg('l-','3')
                sendMsg('t-')
                
        if info[1] != filedata[1] or first:
            info[1] = filedata[1] #score
            _lcd.clear()
            _lcd.message('Score: {}\nEnemies: {}'.format(info[1],info[2]))
            
        if info[2] != filedata[2] or first:
            info[2] = filedata[2] #enemies
            _lcd.clear()
            _lcd.message('Score: {}\nEnemies: {}'.format(info[1],info[2]))
        
        first = False
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
    
    _lcd.clear()
    _lcd.message('Run the game!')
    
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
    _lcd.clear()
    _lcd.message('App closed.')
    time.sleep(2.0)
    _lcd.clear()

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
