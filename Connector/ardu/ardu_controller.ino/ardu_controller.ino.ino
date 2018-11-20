int btn[3] = {2,3,4}; // pin for buttons
int led[3] = {5,6,7}; // pin of led
String key[] = {"a", "d", " "}; //ASCII symbol for buttons
String serialRead="";
bool info[] = {false, false};
bool quit = false;
int cor = 0;
int sensor[] = {0, 0};

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  for(int i = 0; i<3; i++){
      pinMode(btn[i], INPUT_PULLUP);
      pinMode(led[i], OUTPUT);
    }
  Serial.println("i-Start program."); // i- send info
  for(int i = led[0]; i<=led[2]; i++){
    digitalWrite(i, HIGH);
    delay(50);
  }
  delay(100);
  for(int i = led[0]; i<=led[2]; i++){
    digitalWrite(i, LOW);
  }
}

void loop() {
  // put your main code here, to run repeatedly:
  GetSerialData();
  if(serialRead == "@" && !quit){
      quit = true;
      OutOfScript();
      reset();
    }else{
      quit = false;
      ReadMessage();
      ReadKeyboard();
    }
}

// React for close py script
void OutOfScript(){
    Serial.println("i-: Py Script has already been closed.");
    digitalWrite(led[0], HIGH);
    digitalWrite(led[2], HIGH);
    delay(1000);
    digitalWrite(led[0], LOW);
    digitalWrite(led[2], LOW);
}

//Waiting for data from Serial
void GetSerialData(){
  if(Serial.available()>0){
    char serialData = Serial.read();
    serialRead += serialData;
    info[0]=true;
    delay(10);
  }else{
    if(info[0]){
      info[1]=true;
    }
  }
}

//Read message from serial
void ReadMessage(){
  if(info[0] && info[1]){
    Serial.println("i-: " + serialRead);
    String tmp = ":(";
    int caseme = 0;
    tmp = String(serialRead[0]) + String(serialRead[1]);
    Serial.println("Type: " + tmp);
    if(tmp == "l-"){caseme = 1;}else
    if(tmp == "b-"){caseme = 2;}else
    if(tmp == "t-"){caseme = 3;}else
    if(tmp == "p-"){caseme = 4;}
    switch (caseme) {
      case 1:
          Serial.println("Change led stat.");
          for(int i = 2; i<=4; i++){
            for(int j = 1; j<=3; j++){
              if(String(serialRead[i]).toInt()==j || String(serialRead[i]).toInt()==j+4)LedSwitch(j-1);  
            }
          }
        break;
      case 2:
          Serial.println("Change button hotkey.");
          for(int j = 0; j<=2; j++){
            key[j] = String(serialRead[j+2]);
          }
            Serial.println("-- New HotKey --");
            Serial.println("Left: " + key[0]);
            Serial.println("Rigth: " + key[1]);
            Serial.println("Shoot: " + key[2]);
            Serial.println("----------------");
        break;
      case 3:
          reset();
          cor = 0;
          for(int i = 0; i<=4; i++){
            delay(100);
            LedSwitch(i-cor);
            if(i==2){LedSwitch(0); cor = 2;}
          }
        break;
      case 4:
          sensor[1] = analogRead(sensor[0]);//Value of sensor get read from sensor pin 0
          Serial.println(sensor[1]);
        break;
      default:
          Serial.println("WTF is this? I don't get it.");
        break;
     }
    
    serialRead = "";
    info[0]=false;
    info[1]=false;
  }
}

void LedSwitch(int x){
    if(digitalRead(led[x])==0){digitalWrite(led[x], HIGH);}else{digitalWrite(led[x], LOW);}
}

// click the button and LED up
void BtnPressTest(int x){
    digitalWrite(x, HIGH);
    delay(3);
    digitalWrite(x, LOW);
  }

// for ReadKeyboard()
void pressBtn(int x){
    BtnPressTest(led[x]);
    Serial.println("b-" + String(key[x])); // b- send button syntax
    delay(50);
}

// Used to read button and send emulated keboard key.
void ReadKeyboard(){
  if(digitalRead(btn[0])==0)pressBtn(0);
  if(digitalRead(btn[1])==0)pressBtn(1);
  if(digitalRead(btn[2])==0)pressBtn(2);
}

//Reset to default set
void reset(){
    for(int i=led[0];i<=led[2];i++)digitalWrite(i,LOW);
  }
