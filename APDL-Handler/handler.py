import serial

from datetime import datetime
from influxdb import InfluxDBClient


def main():
    arduino = serial.Serial('/dev/ttyACM0', 9600, bytesize=8, timeout=1)

    user = 'root'
    password = 'root'
    dbname = 'apdl-default'

    client = InfluxDBClient('localhost', 8086, user, password, dbname)
    client.drop_database(dbname)
    print("Create database")
    client.create_database(dbname)

    # The format is
    # topic : value

    while True:
        data = arduino.readline()
        asciiData: str = data.decode('ascii')
        try:
            array = asciiData.split(":")
            topic: str = array[0]
            value: str = array[1]
            value = value.replace("\n", "", 1)
            value = value.replace("\r", "", 1)
            value = value.replace(" ", "")
            topic = topic.replace(" ", "")
            if "temp" not in topic:
                continue
            print("Send to influxdb == " + topic + " : " + value)
            json = [
                {
                    "measurement": topic,
                    "fields": {
                        "value": int(value)
                    }
                }
            ]
            client.write_points(json)
        except IndexError:
            continue
        except serial.serialutil.SerialException:
            continue

if __name__ == "__main__":
    main()
