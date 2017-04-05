# Incident Delay

Professional photoshop to start: http://imgur.com/a/KBzMx

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

## 2016 Results

### Major Incidents
| Month | No. of MIR| Veh-hours of delay  | Veh-hours/MIR |
|---|---:|---:| ---:|
| Jan |23|12797|556|
| Feb |26|6216|239|
| Mar |20|3286|164|
| Apr |27|16043|594|
| May |18|25643|1424|
| Jun |22|13059|593|
| Jul |18|10986|610|
| Aug |25|14044|561|
| Sep |31|9670|311|
| Oct |40|21665|541|
| Nov |25|8530|337|
| Dec |20|7512|375|
|**Total**|**295**|**149351**|**506**|

### Collisions
| Month | No. of Collisions| Veh-hours of delay | Veh-hours/collision |
|---|---:|---:| ---:|
| Jan |98|21981|224|
| Feb |93|13972|150|
| Mar |117|12602|107|
| Apr |95|22869|240|
| May |121|29692|245|
| Jun |109|28327|259|
| Jul |132|25168|190|
| Aug |154|29357|202|
| Sep |157|22054|140|
| Oct |160|39217|245|
| Nov |121|23291|192|
| Dec |110|23521|213|
|**Total**|**1459**|**292057**|**200**|

### All Incidents
| Month | No. of Incidents| Veh-hours of delay | Veh-hours/Inc |
|---|---:|---:| ---:|
| Jan |486|29148|60|
| Feb |462|27855|60|
| Mar |515|27250|52|
| Apr |435|37477|86|
| May |532|75317|141|
| Jun |518|46936|90|
| Jul |544|37359|69|
| Aug |574|39386|69|
| Sep |568|50949|90|
| Oct |516|56236|109|
| Nov |431|44977|104|
| Dec |334|39156|117|
|**Total**|**5915**|**511975**|**86**|

