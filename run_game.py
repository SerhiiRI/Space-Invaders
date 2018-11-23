import serial
import threading
import time
import sys
from multiprocessing.pool import ThreadPool
import psutil
import RPi.GPIO as GPIO
import Adafruit_CharLCD as LCD
#from pynput.keyboard import Key, Controller
import os
import subprocess

#_keyboard = Controller()
_PROCESS=True #if True then multiprocessing run
_LISTENING = True #Listening serial port
_TEMP=True #Get update cpu temperature
_pool1 = ThreadPool(processes=1)
_pool2 = ThreadPool(processes=2)
_menuInput = ''
_WINID = ''

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
    

def sendMsg(prefix='s-', mess='', suffix=''):
    mess = str(prefix) + str(mess) + str(suffix)
    ser.write(mess.encode("UTF-8")) # on rpi
    #print('>{}'.format(mess))
    

def quitapp():
    global _PROCESS
    global _LISTENING
    global _TEMP
    global _pool1
    global _pool2
    global _menuInput    
    sendMsg('','@','')
    _PROCESS = False
    while _LISTENING and _TEMP:
        print('>>Wait for close pool.')
        time.sleep(1)
    _pool1.terminate()
    _pool1.join()
    _pool2.terminate()
    _pool2.join()
    ser.close()
    print('Closing app')
    _menuInput = 'exit'


def readArdu():
    global _PROCESS
    global _LISTENING
    global _TEMP
    global _pool1
    global _pool2
    global _menuInput  
    #global _keyboard
    tmpKey =''
    tmpBright = ''
    data = ''
    print(">>Start Listening.")
    while _PROCESS:
        #  on rpi
        time.sleep(0.003)
        if ser.inWaiting()>0:
            tmp = str(ser.read())
            if tmp[2]=='\\':
                if data:
                    mode = data[0]+data[1]
                    if(mode=='s-'):
                        #print('>>Bright: {}'.format(data[2]))
                        try:
                            if tmpBright != data[2]:
                                tmpBright = data[2]
                                open('bright','w').write(data[2])
                        except:
                            print('Open faild')
                    elif(mode=='b-'):
                        #print('>>Key: {}'.format(data[2]))
                        try:
                            os.system('xdotool key {}'.format(data[2]))
                        except:
                            print('Press faild')
                data = ''
            else:
                data = data + tmp[2]
        
    _LISTENING = False
    print('Stop Listening')
    
    
def readTemp():
    global _PROCESS
    global _LISTENING
    global _TEMP
    global _pool1
    global _pool2
    global _menuInput  
    global _lcd
    global _WINID
    fan = False
    life = 0
    maxTemp = 50
    info = ['0','0','0']
    sendMsg('t-')
    time.sleep(0.7)
    
    print(">>Start Temp Reading.")
    while _PROCESS:
        try:
            filedata = open('DATA').read()
            quitme = open('gamestatus').read()
            if quitme[0] == '0':
                print(">>Game quit.")
                os.system('xdotool key ctrl+c')
                os.system('xdotool type \'exit\'')
                os.system('xdotool key Return')
                os.system('xdotool windowactivate {}'.format(_WINID))
                os.system('xdotool key 0')
                os.system('xdotool key Return')
        except:
            filedata = '0;0;0'
                    
        filedata = filedata.split(';')
        if len(filedata)==3:
            #print('Info 0: {}'.format(filedata[2]))
            #if info[0] != filedata[0]:
            info[0] = filedata[0] #life
            waitme = 0.1
            if info[0] == '3':
                life = 3
                time.sleep(waitme)
                sendMsg('c-')
                time.sleep(waitme)
                sendMsg('l-','123')
                time.sleep(waitme)
            elif info[0] == '2':
                life = 2
                time.sleep(waitme)
                sendMsg('c-')
                time.sleep(waitme)
                sendMsg('l-','23')
                time.sleep(waitme)
            elif info[0] == '1':
                life = 1
                time.sleep(waitme)
                sendMsg('c-')
                time.sleep(waitme)
                sendMsg('l-','3')
                time.sleep(waitme)
            elif info[0] == '0':
                life = 0
                time.sleep(waitme)
                sendMsg('c-')
                time.sleep(waitme)
                sendMsg('t-')
                time.sleep(0.7)                
            if info[1] != filedata[1]:
                info[1] = filedata[1] #score
            if info[2] != filedata[2]:
                info[2] = filedata[2] #enemies
            #print('Score: {}\nEnemies: {}'.format(info[1],info[2]))
    
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
        #print('CPU Temp: {}'.format(temp))
        _lcd.clear()
        if life:
            _lcd.message('Life: {}  T:{}\nEnemies: {}'.format(life,temp,info[2]))
        else:
            life = -1
            _lcd.message('Game Over\nYour Score: {}'.format(info[1]))
            time.sleep(5)
        sendMsg('p-','')
        time.sleep(0.5)
        
    _TEMP = False    
    fan = False
    print('Stop Temp Loop')

def main():
    open('gamestatus','w').write('1')
    id_cmd='xdotool getactivewindow'
    id_cmd = subprocess.run(id_cmd.split(' '), stdout=subprocess.PIPE)
    id_cmd = str(id_cmd.stdout)
    global _WINID
    _WINID = id_cmd[2:-3] #Window id
    
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(21, GPIO.OUT)
    GPIO.output(21, GPIO.LOW)
    
    global _PROCESS
    global _LISTENING
    global _TEMP
    global _pool1
    global _pool2
    global _menuInput
    
    _pool1.apply_async(readArdu)
    _pool2.apply_async(readTemp)
    
    os.system('xdotool key ctrl+alt+t')
    time.sleep(1)
    path = os.getcwd()
    os.system('xdotool type \'cd {}\''.format(path))
    os.system('xdotool key Return')
    os.system('xdotool type \'python3 si_run.py\'')
    os.system('xdotool key Return')
    
    _lcd.clear()
    _lcd.message('Run the game!')
    
    while _menuInput!='exit':
        print()
        #print("1. Send test message.\n0. Exit")
        print("0. Exit")
        try:
            _menuInput = int(input())
        except:
            print('-')
        # Close app
        if _menuInput == 0:
            quitapp()

        # Pin test
        '''
         _menuInput == 1:   
            sendMsg('t-')
        '''
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
