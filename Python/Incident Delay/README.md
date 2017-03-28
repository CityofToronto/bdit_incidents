# Incident Delay

We want to be able to attribute spikes in travel time to traffic incidents. As a first cut, it may be best to create some sort of basic incident delay determining heuristic to attribute delay due to incidents for any year. 

## `quantify_delay.py` 
This program calculates incident delay due to major incidents by performing the following operations in the `main()` function:
1. Read incidents from the database
  - Take only incidents where `MIR` is `Yes` and are on the DVP or FGG
  - Create a column corresponding to what bluetooth segment the incident is in and the segment directly upstream
2. Pull bluetooth travel time data from the database as a 2D list of `pandas.DataFrame` using the newly created column
  - First nested list is travel time on the specific date
  - Second nested list is a baseline travel time defined as the median from the past year
3. Pull volume data data for the hour the incident is occuring on
4. Calculate the area between the current day and the baseline for the time the incident is occuring, multiply this by the volume during this time as determined in the previous step
5. Sum the delay caluculated for each incident to determine the total delay

