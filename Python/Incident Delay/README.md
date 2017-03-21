# Incident Delay

We want to be able to attribute spikes in travel time to traffic incidents. As a first cut, it may be best to create some sort of basic incident delay determining heuristic to attribute delay due to incidents for any year. 

## `quantify_delay.py` 
This program reads the incident table from the data base and for each major incident:
- Pulls bluetooth travel time on the segment the incident is on and the segment directly upstream (if it exists)
- Establishes a baseline for these two segments based on a year of data
- Calculates incident delay based on:
  - Start and end time of incident
  - Assume 1800 veh/hr/ln
- Plot the travel times on the two segements with vertical lines corresponding to the start and end time of each incident, see example shown below
