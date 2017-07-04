import org.eclipse.paho.client.mqttv3.persist.MqttDefaultFilePersistence
import org.eclipse.paho.client.mqttv3.{MqttClient, MqttMessage}

import scala.util.{Failure, Random, Success, Try}

/**
  * Created by snipy
  * Project simple-publisher
  */
object Publisher extends App {
  // Global information
  val name = "apdl-case-study-from-scratch"

  // MQTT Server info to publish
  val brokerURL = "tcp://mosquitto-mqtt:1883"
  val topicTemperature = s"temp"
  val topicLuminosity = s"light"
  val persistence = new MqttDefaultFilePersistence("tmp")

  Try {
    val client = new MqttClient(brokerURL, MqttClient.generateClientId(), persistence)
    client.connect()

    val topicTemp = client.getTopic(topicTemperature)
    val topicLight = client.getTopic(topicLuminosity)

    while (true) {

      val temp = Random.nextInt(10) + 20
      val light = Random.nextInt(40) + 640
      val msgTemp = new MqttMessage(s"$temp".getBytes)
      val msgLight = new MqttMessage(s"$light".getBytes)

      println(s"Publish $temp to $topicTemperature")
      println(s"Publish $light to $topicLight")

      topicTemp.publish(msgTemp)
      topicLight.publish(msgLight)
      Thread.sleep(1000)
    }
    while(true) {
      println("OK !")
      Thread.sleep(1000)
    }

  } match {
    case Failure(exception) => println(s"ERROR : $exception + ${exception.getCause}")
    case Success(_) => println(s"OK !")
  }
}
