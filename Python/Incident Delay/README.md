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
| Jan |23|14633|636|
| Feb |26|6242|240|
| Mar |20|3315|165|
| Apr |27|16044|594|
| May |18|26006|1444|
| Jun |22|13059|593|
| Jul |18|11066|614|
| Aug |25|14044|561|
| Sep |31|10088|325|
| Oct |40|21793|544|
| Nov |25|8869|354|
| Dec |20|7538|376|
|**Total**|**295**|**152701**|**376**|

### Collisions
| Month | No. of Collisions| Veh-hours of delay | Veh-hours/collision |
|---|---:|---:| ---:|
| Jan |98|24759|252|
| Feb |93|14329|154|
| Mar |117|12888|110|
| Apr |95|22932|241|
| May |121|30782|254|
| Jun |109|29100|266|
| Jul |132|25906|196|
| Aug |154|30348|209|
| Sep |157|23398|149|
| Oct |160|40008|250|
| Nov |121|23913|197|
| Dec |110|25040|227|
|**Total**|**1459**|**303409**|**208**|

### All Incidents
| Month | No. of Incidents| Veh-hours of delay | Veh-hours/Inc |
|---|---:|---:| ---:|
| Jan |486|41682|85|
| Feb |462|31925|69|
| Mar |515|31359|60|
| Apr |435|38929|89|
| May |532|77838|146|
| Jun |518|52706|101|
| Jul |544|42208|77|
| Aug |574|49959|87|
| Sep |568|54185|95|
| Oct |516|62752|121|
| Nov |431|47777|110|
| Dec |334|44241|132|
|**Total**|**5915**|**575565**|**97**|

