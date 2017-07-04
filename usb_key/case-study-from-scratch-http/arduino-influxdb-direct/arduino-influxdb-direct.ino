
#include <Ethernet.h>
#include <Timer.h>

EthernetClient client;
Timer timer;

const int bufferSize = 2048;
char buf[bufferSize] = {'\0'};


IPAddress influxdb_ip(160,98,61,150);
const int influxdb_port = 8086;
const char* influxdb_database_name = "example";
const char* influxdb_database_name_eth = "example,";
         
IPAddress arduino1_ip(172,16,0,100);
byte arduino1_mac[] {0x98,0x4F,0xEE,0x00,0x81,0x54};
         int temp_pin = 1;
int lum_pin = 0;

float tf (int x)
  {
  const int B = 3975;
const float resistance = (((float)(1023 - x) * 1000) / x);
const float temperature = ((1 / ((log((resistance / 1000)) / B) + (1 / 298.15))) - 273.15);
return temperature;
}
       
void send_temp() {
  int rawData = analogRead(temp_pin);
  float data = tf(rawData);
  int numChars = 0;
  numChars = sprintf(buf,influxdb_database_name_eth);
  numChars += sprintf(&buf[numChars],"SOURCE=arduino1 ");
  numChars += sprintf(&buf[numChars],"temp=%d,");
  sendData(buf,numChars);
  memset(buf,'\0',bufferSize);
  // delay(1000); // some small delay
}
             
void send_lum() {
  int data = analogRead(lum_pin);
  int numChars = 0;
  numChars = sprintf(buf,influxdb_database_name_eth);
  numChars += sprintf(&buf[numChars],"SOURCE=arduino1 ");
  numChars += sprintf(&buf[numChars],"lum=%d,");
  sendData(buf,numChars);
  memset(buf,'\0',bufferSize);
  // delay(1000); // some small delay
}
             
void sendData(char* data, int dataSize) {
  //first we need to connect to InfluxDB server
  int conState = client.connect(influxdb_ip, influxdb_port);

  if (conState <= 0) { //check if connection to server is stablished
    Serial.print("Could not connect to InfluxDB Server, Error #");
    Serial.println(conState);
    return;
  }

  //Send HTTP header and buffer
  client.println("POST /write?db=arduino HTTP/1.1");
  client.println("Host: www.embedonix.com");
  client.println("User-Agent: Arduino/1.0");
  client.println("Connection: close");
  client.println("Content-Type: application/x-www-form-urlencoded");
  client.print("Content-Length: ");
  client.println(dataSize);
  client.println();
  client.println(data);

  delay(50); //wait for server to process data

  //Now we read what server has replied and then we close the connection
  Serial.println("Reply from InfluxDB");
  while (client.available()) { //receive char
    Serial.print((char)client.read());
  }
  Serial.println(); //empty line

  client.stop();
}

void connectToInflux() {
  if (Ethernet.begin(arduino1_mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
    // no point in carrying on, so do nothing forevermore:
    // try to congifure using IP address instead of DHCP:
    Ethernet.begin(arduino1_mac, arduino1_ip);
  }
  delay(2000); // give time to allow connection

  //do a fast test if we can connect to server
  int conState = client.connect(influxdb_ip, influxdb_port);

  if (conState > 0) {
    Serial.println("Connected to InfluxDB server");
    client.stop();
  }

  //print the error number and return false
  Serial.print("Could not connect to InfluxDB Server, Error #");
  Serial.println(conState);
}
     

void setup() {
  timer.every(1000,send_temp);
timer.every(1000,send_lum);

}

void loop() {
  timer.update();

}
     
