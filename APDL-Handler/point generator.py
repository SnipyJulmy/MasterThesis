from influxdb import InfluxDBClient
import random
import time

client = InfluxDBClient('localhost', 8086, 'root', 'root', 'apdl-point-test')

client.drop_database('apdl-point-test')
client.create_database('apdl-point-test')
client.switch_database('apdl-point-test')

for i in range(0, 100):
    json = [
        {
            "measurement": "temp",
            "fields": {
                "value": 29 + (random.randint(0,1))
            }
        }
    ]
    client.write_points(json)
    time.sleep(1)
