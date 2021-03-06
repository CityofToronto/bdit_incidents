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
 - [x] vehicle-hours of delay due to incidents
 - [ ] updates for finer-grained analyses of impacts
 
### 2. Comparing with Traffic Speeds
By matching incident location and time to traffic data from 3rd party vendors and bluetooth we want to estimate 
the delay from loss of capacity due to an incident. This requires merging the incident data with traffic speed 
data as well as volume data to understand the aggregate vehicle-delay. Ideally we will be able to isolate the delay due to incidents alone. 

## Contents 

### Python
- CSV Wrangling: Extracts infromation from EventDescription column and creates new columns for this information
- Plotting: Initial visualization project for Jesse's presentations
- Incident Delay: Isolating delay due to incidents

### SQL
- Tags incidents to closest TMC so INRIX data can be used

### R
- Previous student's code

## Tables in `incidents` schema
### bt
- Associates TMCs with bluetooth segments
### mvp_2016
- Incidents table from RESCU used in `quantify_delay_2016.py`
### volumes
 - Volume data for each bluetooth segment created using Sunny's programs

## Current workflow
1. Import CSV to python as pandas dataframe and run `clean_csv.py`
2. Remove any unnecessary columns (ex: individual date and time columns bc there is a single datetime column)
3. Convert lat/long to postGIS point and tag incident to closest TMC and corresponding bluetooth segment
4. Establish volume profiles for each bluetooth segment (with Sunny's help)
5. Run `quantify_delay_2016.py`

## Key Innovations
- `df['column'] = df.apply(lambda x: function(x), axis=1)`: by far the fastest way to apply functions to whole dataframes 
- Using `ST_DISTANCE()` combined with `DISTINCT ON` and `ORDER BY` to find the closest segment to each incident

## Next Steps
- Decide on how to best represent this data
