int btn[3] = {2,3,4}; // pin for buttons
int led[3] = {5,6,7}; // pin of led
boolean info[] = {0,0,0}; // newData, message1, message2
String key[] = {"A", "D", " "}; //ASCII symbol for buttons
char serialData; //Data from ruspberry
String serialRead;

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
    delay(200);
  }
  delay(500);
  for(int i = led[0]; i<=led[2]; i++){
    digitalWrite(i, LOW);
  }
}

void loop() {
  // put your main code here, to run repeatedly:
  GetSerialData();
  ChangeLED();
  ReadKeyboard();
}

//Waiting for data from ruspberry
void GetSerialData(){
  if (Serial.available()>0){
    info[0] = true;
    serialData = Serial.read();
  }
}

//Check how many player have lives.
void ChangeLED(){
//  delay(500);
  serialRead = String(serialData);
  if(serialRead == "x")Serial.println("x"); //x close app loop
  int ledNum = int((serialData - '0'));
  boolean ledStat;
  if(info[0] == true){
    Serial.println(String(ledNum));
    if(digitalRead(ledNum)==LOW){
      digitalWrite(ledNum, HIGH);
      Serial.println("Pin " + String(ledNum) + " is HIGH");
    }else{  
      digitalWrite(ledNum, LOW);
      Serial.println("Pin " + String(ledNum) + " is LOW");
    }
    info[0] = false;
  }
}

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
