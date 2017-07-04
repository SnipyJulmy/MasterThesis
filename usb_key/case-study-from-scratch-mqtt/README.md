# APDL-from-scratch-mqtt

A small pipeline from an arduino sensor to a grafana visualization

## Getting startred
* pull this repo
* build the docker file and the app for the visualization server
```
cd visualization-server
sbt
> clean
> reload
> update
> package
> docker
> exit
```
* launch the ecosystem
  `docker-compose -d --build`
* upload the sketched to an Arduino (only tested with the Galileo Gen 1)
