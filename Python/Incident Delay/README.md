# Incident Delay

We want to be able to attribute spikes in travel time to traffic incidents. As a first cut, it may be best to create some sort of basic incident delay determining heuristic to attribute delay due to incidents for any year. 

## Pseudo code 
```
for day in year:
  - establish baseline travel time for whatever the day of the week is based on past year of bt data
  for indicent in day:
     - if travel time on segments upstream of the incident is greater than some percentage of the baseline then consider that incident delay, multiply by volume, and add to delay variable
    
```
