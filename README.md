# Welthungerhilfe-Index Visualization Project 

This repository contains `R` code to:

 1. Parse data from the [Welthunger-Index](https://www.welthungerhilfe.de/hunger/welthunger-index/) to create world map visualizations. 
 
 2. Create Shiny App to visualize the index and related variables. 
 
## Docker 

To build the Docker image run
```
docker build -t viz-image .
```

To create a container run
```
docker run --rm -p 3838:3838 viz-image
```
