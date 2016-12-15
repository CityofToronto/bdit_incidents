# Python

Folders:
- CSV Wrangling  
  - Functions that extract important information from the EventDescription field for each incident and create new columns to store this information
- Plotting
  - Creates travel time plots for each bluetooth segement along with vertical lines indicating which segment incidents are occuring on
  - Currently uses Rida's tagged incident files, future implementation will take advantage of incidents that are tagged to tmcs and the tmc to bt lookup table  
- Incident Delay
  - Will (in theory) be able to determine if an increase in travel time is due to a traffic incident or some other factor 
