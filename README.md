# BigDataInnovationTeam: Incidents
*Analyzing impacts of traffic incidents on road congestion*

## Goal
Processing useful information from the City's incident logs to quantify traffic delays from incidents

### 1. Transforming incident data
Ultimately we want the following information:  
 - [x] start time
 - [x] end time
 - [x] geographic location
 - [x] type of incident
 - [x] lane closures
 - [ ] updates for finer-grained analyses of impacts
 
### 2. Comparing with Traffic Speeds
By matching incident location and time to traffic data from 3rd party vendors and bluetooth we want to estimate 
the delay from loss of capacity due to an incident. This requires merging the incident data with traffic speed 
data as well as volume data to understand the aggregate vehicle-delay. 

## Contents 

### Python
- CSV Wrangling: Extracts infromation from EventDescription column and creates new columns for this information
- Plotting: Initial visualization project for Jesse's presentations

### SQL
- Tags incidents to closest TMC so INRIX data can be used

### R
- Previous student's code

## Current workflow
1. Import CSV to python as pandas dataframe and run clean_csv.py
2. Remove any unnecessary columns (ex: individual date and time columns bc there is a single datetime column)
3. Convert lat/long to postGIS point and tag incident to closest TMC
