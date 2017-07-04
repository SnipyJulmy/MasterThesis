package apdl.visualisation

import java.util.concurrent.TimeUnit

import apdl.Utils._
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import org.eclipse.paho.client.mqttv3.{IMqttDeliveryToken, MqttCallback, MqttClient, MqttMessage}
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scodec.codecs._

object Main extends App {
  // Global information
  val name = "apdl-case-study-from-scratch"

  // Codec information
  val codec = int32 ~ int32

  // MQTT Subscribe
  val brokerUrl = "tcp://mosquitto-mqtt:1883"
  val topic_temp = s"temp"
  val topic_light = s"light"
  val persistence = new MemoryPersistence
  val client = new MqttClient(brokerUrl, MqttClient.generateClientId(), persistence)

  // InfluxDB Connection
  val serverAddress = "http://influxdb:8086"
  val dbName = s"$name-influxdb"

  val influxDB: InfluxDB = Option(InfluxDBFactory.connect(serverAddress)) match {
    case Some(value) =>
      println(s"connection successful".blue)
      value
    case None =>
      println(s"can't connect to $serverAddress".red)
      null
  }

  println(s"create database $dbName".blue)
  influxDB.createDatabase(dbName)

  // enable batch, flush 2000 point every 100 Millisecond
  println(s"enable batch".blue)
  influxDB.enableBatch(2000, 100, TimeUnit.MILLISECONDS)

  println(s"Connect to MQTT server".green)
  client.connect()
  println(s"subscribe to $topic_light and $topic_temp".green)
  client.subscribe(Array(topic_light, topic_temp))

  println(s"Start listening on $brokerUrl".green)

  client.setCallback(new MqttCallback {
    override def deliveryComplete(token: IMqttDeliveryToken): Unit = {
      // TODO
    }

    override def messageArrived(topic: String, message: MqttMessage): Unit = {
      val data = message.getPayload
      println(s"Receive msg $data from $message !!!".yellow)

      if (topic == topic_temp) {
        influxDB.write(dbName, "autogen", PointBuilder(
          measurement = "arduino-sensor",
          time = System.currentTimeMillis(),
          timeUnit = TimeUnit.MILLISECONDS,
          InfluxDBField("temp", message.toString.toDouble)
        ))
      } else if (topic == topic_light) {
        influxDB.write(dbName, "autogen", PointBuilder(
          measurement = "arduino-sensor",
          time = System.currentTimeMillis(),
          timeUnit = TimeUnit.MILLISECONDS,
          InfluxDBField("light", message.toString.toInt)
        ))
      } else {
        println("Unknow topic".red)
      }
    }

    override def connectionLost(cause: Throwable): Unit = {
      println(s"$cause".red)
    }
  })

  // Close database on exit
  Runtime.getRuntime.addShutdownHook(new Thread(() => {
    println("On exit".yellow)
    println("Close database".yellow)
    influxDB.close()
  }))
}

