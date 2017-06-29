import serial

from datetime import datetime
from influxdb import InfluxDBClient


client = InfluxDBClient('localhost', 8086, 'root', 'root', 'apdl-default')
result = client.query('SELECT value FROM "temp"') # WHERE time > \'2017-06-25\' AND time  < \'2017-06-30\'')

print(result)
