#include <Ethernet.h>
#include <PubSubClient.h>
#include <Timer.h>
#include <Event.h>
#include <SPI.h>

int temperature_sensor = 1;
int luminosity_sensor = 0;

int timerAction;

int state = 1;

Timer timer;

// MQTT Communication
byte mac[] = {0x98, 0x4F, 0xEE, 0x00, 0x81, 0x54};
IPAddress ip(172, 16, 0, 100);
IPAddress server(160, 98, 61, 150);

// MQTT Topic to publish
char* topic_temp = "temp";
char* topic_light = "light";

// Temperature
int a;
float temperature;
int B = 3975;
float resistance;
String temp_str;
char temp[50];

// Luminosity
String light_str;
char light[50];

// Callback function header
void callback(char* topic, byte* payload, unsigned int length);

EthernetClient ethClient;
PubSubClient client(ethClient);

// Callback function
void callback(char* topic, byte* payload, unsigned int length) {
  Serial.println("Callback");
}

void setup() {
  Serial.begin(57600);
  Serial.print("Setup\n");

  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
    // no point in carrying on, so do nothing forevermore:
    // try to congifure using IP address instead of DHCP:
    Ethernet.begin(mac, ip);
  }

  // give the Ethernet shield a second to initialize:
  delay(1000);
  Serial.print("Connecting...");
  Serial.print("IP Adresse is : ");
  Serial.println(Ethernet.localIP());

  Serial.print("Set server\n");
  client.setServer(server, 1883);
  Serial.print("Set callback\n");
  client.setCallback(callback);

  Serial.print("Connect\n");
  if (client.connect("arduinoClient")) {
    Serial.print("Publish\n");
  } else {
    Serial.println(client.state());
  }
  Serial.println("End setup");
  pinMode(temperature_sensor, INPUT);
  pinMode(luminosity_sensor, INPUT);
  timerAction = timer.every(1000, updateData); // each second call
}

void loop() {
  /*
    Serial.print("Reconnect\n");
    if (!client.connected()) {
    reconnect();
    }
  */
  Serial.println("Loop");
  timer.update();
  Serial.println("Loop 1");
  client.loop();
  Serial.println("Loop 2");
}

void updateData() {
  // get temperature
  a = analogRead(temperature_sensor);
  resistance = (float)(1023 - a) * 10000 / a;
  temperature = 1 / (log(resistance / 10000) / B + 1 / 298.15) - 273.15;
  // publish temp
  sprintf(temp, "%f", temperature);
  // get light
  int luminosity = analogRead(luminosity_sensor);
  // publish light;
  sprintf(light, "%d", luminosity);
  // transmit float and int
  client.publish(topic_temp, temp);
  client.publish(topic_light, light);
}

void reconnect() {
  // Loop until we're reconnected
  while (!client.connected()) {
    Serial.print("Attempting MQTT connection...");
    // Attempt to connect
    if (client.connect("arduinoClient")) {
      Serial.println("connected");
    } else {
      Serial.print("failed, rc=");
      Serial.print(client.state());
      Serial.println(" try again in 5 seconds");
      // Wait 5 seconds before retrying
      delay(5000);
    }
  }
}
