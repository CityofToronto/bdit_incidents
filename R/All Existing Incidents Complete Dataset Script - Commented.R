library("dplyr")

### LIST OF FUNCTIONS ###

## BEGINNING OF 'EXTRACT_DATETIMESTAMP' FUNCTION ##
#Create 'extract_datetimestamp' function, to extract the recorded times for each incident
#This function could be used for all rows, regardless of format of recorded time

extract_datetimestamp <- function(df, field_name, field_name2) {
field_name <- deparse(substitute(field_name))
field_name2 <- deparse(substitute(field_name2))
    
extract_datetimestamp_out <- df %>%
      mutate(
        
        #Group temporary variables for extracting date/time stamps
        datetime_stamp_format <- sub(".*\\((.*)\\).*", "\\1", df[,field_name]),              											                                                     #extracts date/time from format 1 "%m/%d/%Y %I:%M %p"
        time_only_format <- sub ("((?:(?:[0-1][0-9])|(?:[2][0-3])|(?:[0-9])):(?:[0-5][0-9])(?::[0-5][0-9])?(?:\\s?(?:am|AM|pm|PM))?).*", "\\1", df[,field_name]),			#extracts time only for 2nd format 
        
        #identify ifelse statement to accomodate the extraction of different datetime stamps formats
        
        date_time_test <- ifelse(datetime_stamp_format == df[,field_name], NA, datetime_stamp_format),              #checks whether extracted 'date/timestamp' matches 'df[,field_name]' column  
        time_test <- ifelse(time_only_format == df[,field_name], NA, time_only_format),	                           #checks whether extracted 'time_only' matches 'df[,field_name]' column
        time_ampm_test <- ifelse(substrRight(time_test, 2) == "am" | substrRight(time_test, 2) == "pm", 1, 0),
        
        
        #for rows where time has am/pm displayed
        time_only_ampm01 <- format(as.POSIXct(strptime(time_test, "%I:%M %p")), "%H:%M"),
        time_only_ampm02 <- paste(df[,field_name2], time_only_ampm01),
        
        
        #for rows where time does not display am/pm
        time_only01 <- format(as.POSIXct(strptime(time_test, "%H:%M")), "%H:%M"),
        time_only02 <- paste(df[,field_name2], time_only01),
        
        
        #Extract times into temporary columns and convert to "%m/%d/%Y %I:%M %p" format
        datetime_stamp = as.POSIXct(strptime(date_time_test, "%m/%d/%Y %I:%M %p"), tz = "EST5EDT", format = "%m/%d/%Y %H:%M"),    					      #reformat 'datetime_stamp' into "%m/%d/%Y %H:%M" with EST/EDT time zone
        time_only_ampm = as.POSIXct(strptime(time_only_ampm02, "%m/%d/%Y %H:%M"), tz = "EST5EDT", format = "%m/%d/%Y %H:%M"),  									#reformat 'time_only_ampm' into %m/%d/%Y %H:%M with EST/EDT time zone
        time_only = as.POSIXct(strptime(time_only02, "%m/%d/%Y %H:%M"), tz = "EST5EDT", format = "%m/%d/%Y %H:%M"),												#reformat 'time_only' into %m/%d/%Y %H:%M with EST/EDT time zone
        
        
        date_and_time = ifelse(!is.na(datetime_stamp),datetime_stamp,
                               ifelse(!is.na(time_only_ampm),time_only_ampm, 
                                      ifelse(!is.na(time_only),time_only, NA)))
      )
    class(extract_datetimestamp_out$date_and_time) <-  c("POSIXct", "POSIXt")
    
    return(extract_datetimestamp_out)
  }
## END OF 'EXTRACT_DATETIMESTAMP' FUNCTION ##

## BEGINNING OF 'SUBSTRRIGHT' FUNCTION ##
#This function is used to analyse whether am/pm is present for the extraction of times as seperate columns
substrRight <- function(x, n)
{
  substr(x, nchar(x)-n+1, nchar(x))
}
## END OF 'SUBSTRRIGHT' FUNCTION 

## BEGINNING OF 'EXTRACT_INFO' FUNCTION ##
#Creating a function that creates new columns on the data frame
#This base function applies to recorded incidents that do not have updates and original records

extract_info_original <- function(df, field_name) {
  field_name <- deparse(substitute(field_name))
  
df_original <- df %>% 
    mutate( 
      nlanes = 
        ifelse(grepl(pattern = "intersection", df[,field_name]), 99.0,
        ifelse(grepl(pattern = "ramp partially", df[,field_name]), 0.25,
        ifelse(grepl(pattern = "ramp", df[,field_name]), 0.5,
        ifelse(grepl(pattern = "centre lane partially", df[,field_name]), 0.5,
        ifelse(grepl(pattern = "lane partially ", df[,field_name]), 0.5,
        ifelse(grepl(pattern = "bull nose", df[,field_name]), 0.25,
        ifelse(grepl(pattern = "hov lane", df[,field_name]), 0.75,
        ifelse(grepl(pattern = "4 lanes", df[,field_name]), 4,
        ifelse(grepl(pattern = "3 left lanes", df[,field_name]), 3,
        ifelse(grepl(pattern = "3 right lanes", df[,field_name]),3,
        ifelse(grepl(pattern = "3 lanes", df[,field_name]),3,
        ifelse(grepl(pattern = "2 left lanes", df[,field_name]), 2,
        ifelse(grepl(pattern = "2 right lanes", df[,field_name]), 2,
        ifelse(grepl(pattern = "2 lanes", df[,field_name]),2,
        ifelse(grepl(pattern = "centre lane", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "right shoulder", df[,field_name]),0.1,
        ifelse(grepl(pattern = "right lane", df[,field_name]),1.0,
        ifelse(grepl(pattern = "left shoulder", df[,field_name]), 0.1,
        ifelse(grepl(pattern = "left lane", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "left turn lane", df[,field_name]), 1.25,
        ifelse(grepl(pattern = "right turn lane", df[,field_name]), 1.25,
        ifelse(grepl(pattern = "curb lane partially", df[,field_name]), 0.75,
        ifelse(grepl(pattern = "left curb lane", df[,field_name]), 1.5,
        ifelse(grepl(pattern = "right curb lane", df[,field_name]), 1.5,
        ifelse(grepl(pattern = "southbound curb lane", df[,field_name]), 1.5,
        ifelse(grepl(pattern = "northbound curb lane", df[,field_name]), 1.5,
        ifelse(grepl(pattern = "eastbound curb lane", df[,field_name]), 1.5,
        ifelse(grepl(pattern = "curb lane", df[,field_name]), 1.5,
        ifelse(grepl(pattern = "northbound and southbound lanes", df[,field_name]), 99,
        ifelse(grepl(pattern = "all northbound lanes", df[,field_name]), 99,
        ifelse(grepl(pattern = "all southbound lanes", df[,field_name]), 99,
        ifelse(grepl(pattern = "eastbound and westbound lanes", df[,field_name]),99,
        ifelse(grepl(pattern = "all eastbound lanes", df[,field_name]), 99, 
        ifelse(grepl(pattern = "all westbound lanes", df[,field_name]), 99,
        ifelse(grepl(pattern = "northbound lane", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "southbound lane", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "eastbound lane", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "westbound lane", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "shoulder", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "exit", df[,field_name]), 1.1,
        ifelse(grepl(pattern = "gate", df[,field_name]), 1.1,
        ifelse(grepl(pattern = "lane", df[,field_name]), 1.0,
        ifelse(grepl(pattern = "road", df[,field_name]), 99.0,
        ifelse(grepl(pattern = "all", df[,field_name]), 99.0,
        ifelse(grepl(pattern = "partially blocked", df[,field_name]), 0.5,
        ifelse(grepl(pattern = "blocked", df[,field_name]), 1.0,
        NA)))))))))))))))))))))))))))))))))))))))))))))),
      
      lanes_desc = 
          ifelse(grepl(pattern = "intersection", df[,field_name]), "Intersection Blocked",
          ifelse(grepl(pattern = "ramp partially ", df[,field_name]), "Ramp-Partially",
          ifelse(grepl(pattern = "ramp", df[,field_name]), "Ramp",
          ifelse(grepl(pattern = "centre lane partially ", df[,field_name]), "Centre Lane-Partially",
          ifelse(grepl(pattern = "lane partially", df[,field_name]), "One Lane-Partially",
          ifelse(grepl(pattern = "bull nose", df[,field_name]), "Bull Nose", 
          ifelse(grepl(pattern = "hov lane", df[,field_name]), "HOV lane",
          ifelse(grepl(pattern = "4 lanes", df[,field_name]), "4 Lanes",
          ifelse(grepl(pattern = "3 left lanes", df[,field_name]), "Left Lanes",
          ifelse(grepl(pattern = "3 right lanes", df[,field_name]), "Right Lanes",
          ifelse(grepl(pattern = "3 lanes", df[,field_name]), "Traffic Lanes",
          ifelse(grepl(pattern = "2 left lanes", df[,field_name]), "Left Lanes", 
          ifelse(grepl(pattern = "2 right lanes", df[,field_name]), "Right Lanes",
          ifelse(grepl(pattern = "2 lanes", df[,field_name]), "Traffic Lanes",
          ifelse(grepl(pattern = "centre lane", df[,field_name]), "Centre Lane",
          ifelse(grepl(pattern = "right shoulder", df[,field_name]),"Right Shoulder",
          ifelse(grepl(pattern = "right lane", df[,field_name]), "Right Lane",
          ifelse(grepl(pattern = "left shoulder", df[,field_name]), "Left Shoulder",
          ifelse(grepl(pattern = "left lane", df[,field_name]), "Left Lane",
          ifelse(grepl(pattern = "left turn lane", df[,field_name]), "Left Turn Lane",
          ifelse(grepl(pattern = "right turn lane", df[,field_name]), "Right Turn Lane",
          ifelse(grepl(pattern = "curb lane partially", df[,field_name]), "Curb Lane-Partially",
          ifelse(grepl(pattern = "left curb lane", df[,field_name]),"Left Curb Lane",
          ifelse(grepl(pattern = "right curb lane ", df[,field_name]), "Right Curb Lane",
          ifelse(grepl(pattern = "southbound curb lane", df[,field_name]), "Southbound Curb Lane",
          ifelse(grepl(pattern = "northbound curb lane ", df[,field_name]), "Northbound Curb Lane",
          ifelse(grepl(pattern = "eastbound curb lane", df[,field_name]), "Eastbound Curb Lane",
          ifelse(grepl(pattern = "curb lane", df[,field_name]), "Curb Lane(s)",
          ifelse(grepl(pattern = "northbound and southbound lanes", df[,field_name]), "All Lanes-Both Directions",
          ifelse(grepl(pattern = "all northbound lanes", df[,field_name]), "All Lanes-Northbound",
          ifelse(grepl(pattern = "all southbound lanes", df[,field_name]), "All Lanes-Southbound",
          ifelse(grepl(pattern = "eastbound and westbound lanes", df[,field_name]),"All Lanes-Both Directions",
          ifelse(grepl(pattern = "all eastbound lanes", df[,field_name]), "All Lanes-Eastbound", 
          ifelse(grepl(pattern = "all westbound lanes", df[,field_name]), "All Lanes-Westbound",
          ifelse(grepl(pattern = "northbound lane", df[,field_name]), "Northbound Lane",
          ifelse(grepl(pattern = "southbound lane", df[,field_name]), "Southbound Lane",
          ifelse(grepl(pattern = "eastbound lane", df[,field_name]), "Eastbound Lane",
          ifelse(grepl(pattern = "westbound lane", df[,field_name]), "Westbound Lane",
          ifelse(grepl(pattern = "exit", df[,field_name]), "Exit",
          ifelse(grepl(pattern = "gate", df[,field_name]), "Gate",
          ifelse(grepl(pattern = "lane", df[,field_name]), "Lane",
          ifelse(grepl(pattern = "road", df[,field_name]), "Road Blocked",
          ifelse(grepl(pattern = "shoulder", df[,field_name]), "Shoulder",
          ifelse(grepl(pattern = "all", df[,field_name]), "All Lanes-Both Directions",
          ifelse(grepl(pattern = "partially blocked", df[,field_name]), "Partially Blocked",
          ifelse(grepl(pattern = "blocked", df[,field_name]), "Blocked",
          NA)))))))))))))))))))))))))))))))))))))))))))))),
    
    incident_reason =
        ifelse(grepl(pattern = "disabled vehicle", df[,field_name]), "Disabled Vehicle",
         ifelse(grepl(pattern = "disabled truck", df[,field_name]), "Disabled Truck",
         ifelse(grepl(pattern = "disabled bus", df[,field_name]), "Disabled Bus",
         ifelse(grepl(pattern = "disabled TTC streetcar", df[,field_name]), "Disabled TTC Streetcar",
         ifelse(grepl(pattern = "disabled transport trucks", df[,field_name]), "Disabled Transport Trucks",
         ifelse(grepl(pattern = "disabled transport truck", df[,field_name]), "Disabled Transport Truck",
         ifelse(grepl(pattern = "disabled motorcycle(s)", df[,field_name]), "Disabled Motorcycle(s)", 
         ifelse(grepl(pattern = "stopped motorcycle(s)", df[,field_name]), "Stopped Motorcycle(s)",
         ifelse(grepl(pattern = "stopped transport truck", df[,field_name]), "Stopped Transport Truck", 
         ifelse(grepl(pattern = "stopped car", df[,field_name]), "Stopped Vehicle",
         ifelse(grepl(pattern = "stopped vehicles", df[,field_name]), "Stopped Vehicles",
         ifelse(grepl(pattern = "stopped vehicle", df[,field_name]), "Stopped Vehicle",
         ifelse(grepl(pattern = "stopped truck", df[,field_name]), "Stopped Truck", 
         ifelse(grepl(pattern = "stopped bus", df[,field_name]), "Stopped Bus",
         ifelse(grepl(pattern = "single vehicle", df[,field_name]), "Single Vehicle Collision",
         ifelse(grepl(pattern = "multi vehicle", df[,field_name]), "Multi Vehicle Collision",
         ifelse(grepl(pattern = "collision involving a motorcycle", df[,field_name]), "Collision Involving a Motorcycle", 
         ifelse(grepl(pattern = "collision", df[,field_name]), "Collision",
         ifelse(grepl(pattern = "police investigation", df[,field_name]), "Police Investigation",
         ifelse(grepl(pattern = "police activity", df[,field_name]), "Police Investigation",
         ifelse(grepl(pattern = "scheduled maintenance", df[,field_name]), "Scheduled Maintenance",
         ifelse(grepl(pattern = "tow vehicle", df[,field_name]),  "Tow Truck",
        ifelse(grepl(pattern = "slow moving maintenance vehicles", df[,field_name]), "Slow Moving Maintenance Vehicles",  
        ifelse(grepl(pattern = "slow moving maintenance vehicle", df[,field_name]), "Slow Moving Maintenance Vehicle",
        ifelse(grepl(pattern = "slow moving maintenance crews", df[,field_name]), "Slow Moving Maintenance Crews",
         ifelse(grepl(pattern = "slow moving", df[,field_name]), "Slow Moving Maintenance Crew", 
        ifelse(grepl(pattern = "maintenance vehicles", df[,field_name]), "Maintenance Vehicles",
        ifelse(grepl(pattern = "maintenance vehicle", df[,field_name]), "Maintenance Vehicle", 
        ifelse(grepl(pattern = "maintenance crews", df[,field_name]), "Maintenance Crews",
        ifelse(grepl(pattern = "maintenance crew", df[,field_name]), "Maintenance Crew", 
        ifelse(grepl(pattern = "road maintenance", df[,field_name]), "Road Maintenance",
        ifelse(grepl(pattern = "road work", df[,field_name]), "Road maintenance",
        ifelse(grepl(pattern = "roadops", df[,field_name]),  "Road maintenance", 
        ifelse(grepl(pattern = "rd ops", df[,field_name]), "Road maintenance", 
        ifelse(grepl(pattern = "fallen electrical wires", df[,field_name]), "Fallen Electrical Wires",
        ifelse(grepl(pattern = "emergency vehicle(s)", df[,field_name]), "Emergency Vehicle(s)",
        ifelse(grepl(pattern = "parked", df[,field_name]), "Parked", 
        ifelse(grepl(pattern = "walking", df[,field_name]), "Illegal Walking",
        ifelse(grepl(pattern = "guard rail repairs", df[,field_name]), "Guard Rail Repairs",    
        ifelse(grepl(pattern = "water main repair", df[,field_name]), "Water Main Repair",
        ifelse(grepl(pattern = "waterfront marathon", df[,field_name]), "Waterfront Marathon",
        ifelse(grepl(pattern = "wheel trans", df[,field_name]), "Disabled Wheel Trans",
        ifelse(grepl(pattern = "pedestrian", df[,field_name]), "Pedestrian Incident",
        ifelse(grepl(pattern = "special event", df[,field_name]), "Special Event",
        NA)))))))))))))))))))))))))))))))))))))))))))),
      
      
      respondents =
        ifelse(grepl(pattern = "police/fire on scene", df[,field_name]),  "Police/ Fire Services",
        ifelse(grepl(pattern = "police/fire/ems on scene", df[,field_name]), "police/fire/ems",
        ifelse(grepl(pattern = "toronto paramedic service vehicle", df[,field_name]), "Paramedics",
        ifelse(grepl(pattern = "toronto paramedics services", df[,field_name]), "Paramedics",
        ifelse(grepl(pattern = "ems", df[,field_name]), "Paramedics",
        ifelse(grepl(pattern = "toronto police", df[,field_name]), "Police",
        ifelse(grepl(pattern = "police", df[,field_name]), "Police",
        ifelse(grepl(pattern = "police cruiser",df[,field_name]), "Police",
        ifelse(grepl(pattern = "police cruisers", df[,field_name]), "Police",
        ifelse(grepl(pattern = "toronto fire", df[,field_name]), "Fire Services",
        ifelse(grepl(pattern = "to fire", df[,field_name]), "Fire Services",
        ifelse(grepl(pattern = "emergency vehicles ", df[,field_name]), "Emergency Vehicles",
        ifelse(grepl(pattern = "emergency vehicle", df[,field_name]), "Emergency Vehicle",
        ifelse(grepl(pattern = "toronto fire and police", df[,field_name]), "Fire and Police Services",
        ifelse(grepl(pattern = "road operations", df[,field_name]), "Road Operations", 
        ifelse(grepl(pattern = "emergency crews", df[,field_name]), "Emergency Services",
        ifelse(grepl(pattern = "fire truck", df[,field_name]), "Fire Services",
        ifelse(grepl(pattern = "tow truck", df[,field_name]), "Tow Truck",
        ifelse(grepl(pattern = "crash truck", df[,field_name]), "Crash Truck",
        ifelse(grepl(pattern = "city vehicles", df[,field_name]), "City Vehicles",  
        ifelse(grepl(pattern = "city vehicle", df[,field_name]), "City Vehicle",
        ifelse(grepl(pattern = "toronto water", df[,field_name]), "Water Services", 
        ifelse(grepl(pattern = "fire", df[,field_name]), "Fire Services",
        NA)))))))))))))))))))))))    ) 
  
  return(df_original)
}
##END OF 'EXTRACT_INFO_ORIGINAL' FUNCTION

#This function would eventually be reused for all recorded incidents, even those with at least one update(s) or without one
#Includes temporary variables for 'nlanes' and 'lanes_desc'

extract_info_updates <- function(df, field_name) {
field_name <- deparse(substitute(field_name))
  
df_updates <- df %>% 
    mutate( 
      
  lanes_desc_blocking01 = sub(".*?blocking\\s(.*?)", "\\1", df[,field_name]),
  lanes_desc_blocking02 = sub(".*?the\\s(.*?)", "\\1", lanes_desc_blocking01), 
  
  lanes_desc_blocking03 = 
      ifelse(grepl(pattern = "intersection", lanes_desc_blocking02), "Intersection Blocked",
      ifelse(grepl(pattern = "ramp partially", lanes_desc_blocking02), "Ramp-Partially",
      ifelse(grepl(pattern = "ramp", lanes_desc_blocking02), "Ramp",
      ifelse(grepl(pattern = "centre lane partially", lanes_desc_blocking02), "Centre Lane-Partially",
      ifelse(grepl(pattern = "lane partially", lanes_desc_blocking02), "One Lane-Partially",
      ifelse(grepl(pattern = "road", lanes_desc_blocking02), "Road Blocked",
      ifelse(grepl(pattern = "bull nose",lanes_desc_blocking02), "Bull Nose", 
      ifelse(grepl(pattern = "hov lane",lanes_desc_blocking02), "HOV lane",
      ifelse(grepl(pattern = "3 left lanes", lanes_desc_blocking02), "Left Lanes",
      ifelse(grepl(pattern = "3 right lanes",lanes_desc_blocking02), "Right Lanes",
      ifelse(grepl(pattern = "3 lanes", lanes_desc_blocking02), "Traffic Lanes",
      ifelse(grepl(pattern = "2 left lanes", lanes_desc_blocking02), "Left Lanes", 
      ifelse(grepl(pattern = "2 right lanes", lanes_desc_blocking02), "Right Lanes",
      ifelse(grepl(pattern = "2 lanes", lanes_desc_blocking02), "Traffic Lanes",
      ifelse(grepl(pattern = "centre lane", lanes_desc_blocking02), "Centre Lane",
      ifelse(grepl(pattern = "right shoulder", lanes_desc_blocking02),"Right Shoulder",
      ifelse(grepl(pattern = "right lane", lanes_desc_blocking02),"Right Lane",
      ifelse(grepl(pattern = "left shoulder", lanes_desc_blocking02), "Left Shoulder",
      ifelse(grepl(pattern = "left lane", lanes_desc_blocking02), "Left Lane",
      ifelse(grepl(pattern = "left turn lane", lanes_desc_blocking02), "Left Turn Lane",
      ifelse(grepl(pattern = "right turn lane", lanes_desc_blocking02), "Right Turn Lane",
      ifelse(grepl(pattern = "curb lane partially", lanes_desc_blocking02), "Curb Lane-Partially",
      ifelse(grepl(pattern = "left curb lane", lanes_desc_blocking02),"Left Curb Lane",
      ifelse(grepl(pattern = "right curb lane", lanes_desc_blocking02), "Right Curb Lane",
      ifelse(grepl(pattern = "southbound curb lane", lanes_desc_blocking02), "Southbound Curb Lane",
      ifelse(grepl(pattern = "northbound curb lane", lanes_desc_blocking02), "Northbound Curb Lane",
      ifelse(grepl(pattern = "eastbound curb lane", lanes_desc_blocking02), "Eastbound Curb Lane",
      ifelse(grepl(pattern = "left and right curb lanes", lanes_desc_blocking02), "Left and Right Curb Lanes",
      ifelse(grepl(pattern = "curb lane", lanes_desc_blocking02), "Curb Lane",
      ifelse(grepl(pattern = "all traffic", lanes_desc_blocking02), "All Traffic Must Exit",
      ifelse(grepl(pattern = "road", lanes_desc_blocking02), "Road Blocked",
      ifelse(grepl(pattern = "all lanes", lanes_desc_blocking02), "All Lanes",
      ifelse(grepl(pattern = "northbound and southbound", lanes_desc_blocking02), "All Lanes-Both Directions",
      ifelse(grepl(pattern = "northbound lanes", lanes_desc_blocking02), "All Lanes-Northbound",
      ifelse(grepl(pattern = "southbound lanes", lanes_desc_blocking02), "All Lanes-Southbound",
      ifelse(grepl(pattern = "eastbound lanes", lanes_desc_blocking02), "All Lanes-Eastbound", 
      ifelse(grepl(pattern = "westbound lanes", lanes_desc_blocking02), "All Lanes-Westbound",
      ifelse(grepl(pattern = "exit", lanes_desc_blocking02), "Exit",
      ifelse(grepl(pattern = "gate", lanes_desc_blocking02), "Gate",
      ifelse(grepl(pattern = "lane", lanes_desc_blocking02), "Lane",
      ifelse(grepl(pattern = "shoulder", lanes_desc_blocking02), "Shoulder",
      ifelse(grepl(pattern = "all", lanes_desc_blocking02), "All Lanes-Both Directions",
      ifelse(grepl(pattern = "collector lane",lanes_desc_blocking02), "Collector Lanes", 
      ifelse(grepl(pattern = "express lane", lanes_desc_blocking02), "Express Lanes",
      ifelse(grepl(pattern = "partially blocked", lanes_desc_blocking02), "Partially Blocked",
      ifelse(grepl(pattern = "blocked", lanes_desc_blocking02), "Blocked",
      NA)))))))))))))))))))))))))))))))))))))))))))))),


  lanes_desc_blocking04 = 
        ifelse(grepl(pattern = "intersection blocked", df[,field_name]), "Intersection Blocked",
        ifelse(grepl(pattern = "ramp partially blocked", df[,field_name]), "Ramp-Partially",
        ifelse(grepl(pattern = "ramp blocked", df[,field_name]), "Ramp",
        ifelse(grepl(pattern = "centre lane partially blocked", df[,field_name]), "Centre Lane-Partially",
        ifelse(grepl(pattern = "lane partially blocked", df[,field_name]), "One Lane-Partially",
        ifelse(grepl(pattern = "road blocked", df[,field_name]), "Road Blocked",
        ifelse(grepl(pattern = "bull nose", df[,field_name]), "Bull Nose", 
        ifelse(grepl(pattern = "hov lane blocked", df[,field_name]), "HOV lane",
        ifelse(grepl(pattern = "4 lanes blocked", df[,field_name]), "4 Lanes",
        ifelse(grepl(pattern = "3 left lanes", df[,field_name]), "Left Lanes",
        ifelse(grepl(pattern = "3 right lanes blocked", df[,field_name]), "Right Lanes",
        ifelse(grepl(pattern = "3 lanes", df[,field_name]), "Traffic Lanes",
        ifelse(grepl(pattern = "2 left lanes blocked", df[,field_name]), "Left Lanes", 
        ifelse(grepl(pattern = "2 right lanes blocked", df[,field_name]), "Right Lanes",
        ifelse(grepl(pattern = "2 lanes", df[,field_name]), "Traffic Lanes",
        ifelse(grepl(pattern = "centre lane blocked", df[,field_name]), "Centre Lane",
        ifelse(grepl(pattern = "right shoulder blocked", df[,field_name]),"Right Shoulder",
        ifelse(grepl(pattern = "right lane blocked", df[,field_name]),"Right Lane",
        ifelse(grepl(pattern = "left shoulder blocked", df[,field_name]), "Left Shoulder",
        ifelse(grepl(pattern = "left lane blocked", df[,field_name]), "Left Lane",
        ifelse(grepl(pattern = "left turn lane", df[,field_name]), "Left Turn Lane",
        ifelse(grepl(pattern = "right turn lane", df[,field_name]), "Right Turn Lane",
        ifelse(grepl(pattern = "curb lane partially", df[,field_name]), "Curb Lane-Partially",
        ifelse(grepl(pattern = "left curb lane blocked", df[,field_name]),"Left Curb Lane",
        ifelse(grepl(pattern = "right curb lane blocked", df[,field_name]), "Right Curb Lane",
        ifelse(grepl(pattern = "southbound curb lane blocked", df[,field_name]), "Southbound Curb Lane",
        ifelse(grepl(pattern = "northbound curb lane blocked", df[,field_name]), "Northbound Curb Lane",
        ifelse(grepl(pattern = "eastbound curb lane blocked", df[,field_name]), "Eastbound Curb Lane",
        ifelse(grepl(pattern = "shoulder/lane", df[,field_name]), "Shoulder/Lane",
        ifelse(grepl(pattern = "left and right curb lanes blocked", df[,field_name]), "Left and Right Curb Lanes",
        ifelse(grepl(pattern = "curb lane blocked", df[,field_name]), "Curb Lane",
        ifelse(grepl(pattern = "all traffic", df[,field_name]), "All Traffic Must Exit",
        ifelse(grepl(pattern = "road closed", df[,field_name]), "Road Closed",
        ifelse(grepl(pattern = "all lanes blocked", df[,field_name]), "All Lanes",
        ifelse(grepl(pattern = "northbound and southbound", df[,field_name]), "All Lanes-Both Directions",
        ifelse(grepl(pattern = "northbound lanes", df[,field_name]), "All Lanes-Northbound",
        ifelse(grepl(pattern = "southbound lanes", df[,field_name]), "All Lanes-Southbound",
        ifelse(grepl(pattern = "eastbound and westbound", df[,field_name]),"All Lanes-Both Directions",
        ifelse(grepl(pattern = "eastbound lanes blocked", df[,field_name]), "All Lanes-Eastbound", 
        ifelse(grepl(pattern = "westbound lanes blocked", df[,field_name]), "All Lanes-Westbound",
        ifelse(grepl(pattern = "exit", df[,field_name]), "Exit",
        ifelse(grepl(pattern = "gate", df[,field_name]), "Gate",
        ifelse(grepl(pattern = "all", df[,field_name]), "All Lanes-Both Directions",
        ifelse(grepl(pattern = "partially blocked", df[,field_name]), "Partially Blocked",
        ifelse(grepl(pattern = "blocked", df[,field_name]), "Blocked",
        NA))))))))))))))))))))))))))))))))))))))))))))),

    nlanes01 =
        ifelse(grepl(pattern = "intersection", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "ramp partially", lanes_desc_blocking01), 0.25,
        ifelse(grepl(pattern = "ramp", lanes_desc_blocking01), 0.5,
        ifelse(grepl(pattern = "centre lane partially", lanes_desc_blocking01), 0.5,
        ifelse(grepl(pattern = "lane partially", lanes_desc_blocking01), 0.5,
        ifelse(grepl(pattern = "bull nose", lanes_desc_blocking01), 0.25,
        ifelse(grepl(pattern = "hov lane", lanes_desc_blocking01), 0.75,
        ifelse(grepl(pattern = "3 left lanes", lanes_desc_blocking01), 3.0,
        ifelse(grepl(pattern = "3 right lanes", lanes_desc_blocking01),3.0,
        ifelse(grepl(pattern = "3 lanes", lanes_desc_blocking01),3.0,
        ifelse(grepl(pattern = "2 left lanes", lanes_desc_blocking01), 2.0,
        ifelse(grepl(pattern = "2 right lanes", lanes_desc_blocking01), 2.0,
        ifelse(grepl(pattern = "2 lanes", lanes_desc_blocking01),2.0,
        ifelse(grepl(pattern = "centre lane", lanes_desc_blocking01), 1.0,
        ifelse(grepl(pattern = "right shoulder", lanes_desc_blocking01),0.1,
        ifelse(grepl(pattern = "right lane", lanes_desc_blocking01),1.0,
        ifelse(grepl(pattern = "left shoulder", lanes_desc_blocking01), 0.1,
        ifelse(grepl(pattern = "left lane", lanes_desc_blocking01), 1.0,
        ifelse(grepl(pattern = "left turn lane", lanes_desc_blocking01), 1.25,
        ifelse(grepl(pattern = "right turn lane", lanes_desc_blocking01), 1.25,
        ifelse(grepl(pattern = "curb lane partially", lanes_desc_blocking01), 0.75,
        ifelse(grepl(pattern = "left curb lane", lanes_desc_blocking01), 1.5,
        ifelse(grepl(pattern = "right curb lane", lanes_desc_blocking01), 1.5,
        ifelse(grepl(pattern = "southbound curb lane", lanes_desc_blocking01), 1.5,
        ifelse(grepl(pattern = "northbound curb lane", lanes_desc_blocking01), 1.5,
        ifelse(grepl(pattern = "eastbound curb lane", lanes_desc_blocking01), 1.5,
        ifelse(grepl(pattern = "shoulder/lane", lanes_desc_blocking01), 1.5,
        ifelse(grepl(pattern = "left and right curb lanes", lanes_desc_blocking01), 1.75,                                                                                                                                                                                                        ifelse(grepl(pattern = "curb lane blocked", lanes_desc_blocking01), 1.5,
        ifelse(grepl(pattern = "all traffic", lanes_desc_blocking01), 50.0,
        ifelse(grepl(pattern = "road", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "all lanes blocked", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "northbound and southbound", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "northbound lanes", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "southbound lanes", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "eastbound and westbound", lanes_desc_blocking01),99.0,
        ifelse(grepl(pattern = "eastbound lanes", lanes_desc_blocking01), 99.0, 
        ifelse(grepl(pattern = "westbound lanes", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "shoulder", lanes_desc_blocking01), 1.0,
        ifelse(grepl(pattern = "exit", lanes_desc_blocking01), 1.1,
        ifelse(grepl(pattern = "gate", lanes_desc_blocking01), 1.1,
        ifelse(grepl(pattern = "lane", lanes_desc_blocking01), 1.0,
        ifelse(grepl(pattern = "all", lanes_desc_blocking01), 99.0,
        ifelse(grepl(pattern = "partially blocked", lanes_desc_blocking01), 0.5,
        ifelse(grepl(pattern = "blocked", lanes_desc_blocking01), 1.0,
        NA))))))))))))))))))))))))))))))))))))))))))))),
    
  nlanes02 = 
      ifelse(grepl(pattern = "intersection blocked", df[,field_name]), 99.0,
      ifelse(grepl(pattern = "ramp partially blocked", df[,field_name]), 0.25,
      ifelse(grepl(pattern = "ramp blocked", df[,field_name]), 0.5,
      ifelse(grepl(pattern = "centre lane partially blocked", df[,field_name]), 0.5,
      ifelse(grepl(pattern = "lane partially blocked", df[,field_name]), 0.5,
      ifelse(grepl(pattern = "bull nose", df[,field_name]), 0.25,
      ifelse(grepl(pattern = "hov lane blocked", df[,field_name]), 0.75,
      ifelse(grepl(pattern = "4 lanes blocked", df[,field_name]), 4.0,
      ifelse(grepl(pattern = "3 left lanes", df[,field_name]), 3.0,
      ifelse(grepl(pattern = "3 right lanes blocked", df[,field_name]),3.0,
      ifelse(grepl(pattern = "3 lanes", df[,field_name]),3.0,
      ifelse(grepl(pattern = "2 left lanes blocked", df[,field_name]), 2.0,
      ifelse(grepl(pattern = "2 right lanes blocked", df[,field_name]), 2.0,
      ifelse(grepl(pattern = "2 lanes", df[,field_name]),2.0,
      ifelse(grepl(pattern = "centre lane blocked", df[,field_name]), 1.0,
      ifelse(grepl(pattern = "right shoulder blocked", df[,field_name]),0.1,
      ifelse(grepl(pattern = "right lane blocked", df[,field_name]),1.0,
      ifelse(grepl(pattern = "left shoulder blocked", df[,field_name]), 0.1,
      ifelse(grepl(pattern = "left lane blocked", df[,field_name]), 1.0,
      ifelse(grepl(pattern = "left turn lane", df[,field_name]), 1.25,
      ifelse(grepl(pattern = "right turn lane", df[,field_name]), 1.25,
      ifelse(grepl(pattern = "curb lane partially", df[,field_name]), 0.75,
      ifelse(grepl(pattern = "left curb lane blocked", df[,field_name]), 1.5,
      ifelse(grepl(pattern = "right curb lane blocked", df[,field_name]), 1.5,
      ifelse(grepl(pattern = "southbound curb lane blocked", df[,field_name]), 1.5,
      ifelse(grepl(pattern = "northbound curb lane blocked", df[,field_name]), 1.5,
      ifelse(grepl(pattern = "eastbound curb lane blocked", df[,field_name]), 1.5,
      ifelse(grepl(pattern = "shoulder/lane", df[,field_name]), 1.5,
      ifelse(grepl(pattern = "left and right curb lanes blocked", df[,field_name]), 1.75,
      ifelse(grepl(pattern = "curb lane blocked", df[,field_name]), 1.5,
      ifelse(grepl(pattern = "all traffic", df[,field_name]), 50,
      ifelse(grepl(pattern = "road closed", df[,field_name]), 99,
      ifelse(grepl(pattern = "road blocked", df[,field_name]), 99.0,
      ifelse(grepl(pattern = "all lanes blocked", df[,field_name]), 99,
      ifelse(grepl(pattern = "northbound and southbound", df[,field_name]), 99.0,
      ifelse(grepl(pattern = "northbound lanes", df[,field_name]), 99.0,
      ifelse(grepl(pattern = "southbound lanes", df[,field_name]), 99.0,
      ifelse(grepl(pattern = "eastbound and westbound", df[,field_name]),99.0,
      ifelse(grepl(pattern = "eastbound lanes blocked", df[,field_name]), 99.0, 
      ifelse(grepl(pattern = "westbound lanes blocked", df[,field_name]), 99.0,
      ifelse(grepl(pattern = "exit", df[,field_name]), 1.1,
      ifelse(grepl(pattern = "gate", df[,field_name]), 1.1,
      ifelse(grepl(pattern = "all", df[,field_name]), 99.0,
      ifelse(grepl(pattern = "partially blocked", df[,field_name]), 0.5,
      ifelse(grepl(pattern = "blocked", df[,field_name]), 1.0,
      NA))))))))))))))))))))))))))))))))))))))))))))),

    nlanes = ifelse(!is.na(nlanes02), nlanes02,
             ifelse(!is.na(nlanes01), nlanes01,
              NA)),
  
    lanes_desc = ifelse(!is.na(lanes_desc_blocking04), lanes_desc_blocking04, 
                      ifelse(!is.na(lanes_desc_blocking03),lanes_desc_blocking03, NA)),
  
    incident_reason =
        ifelse(grepl(pattern = "disabled vehicle", df[,field_name]), "Disabled Vehicle",
        ifelse(grepl(pattern = "disabled truck", df[,field_name]), "Disabled Truck",
        ifelse(grepl(pattern = "disabled bus", df[,field_name]), "Disabled Bus",
        ifelse(grepl(pattern = "disabled TTC streetcar", df[,field_name]), "Disabled TTC Streetcar",
        ifelse(grepl(pattern = "disabled transport trucks", df[,field_name]), "Disabled Transport Trucks",
        ifelse(grepl(pattern = "disabled transport truck", df[,field_name]), "Disabled Transport Truck",
        ifelse(grepl(pattern = "disabled motorcyles", df[,field_name]), "Disabled Motorcycles",
        ifelse(grepl(pattern = "disabled motorcycle", df[,field_name]), "Disabled Motorcycle", 
        ifelse(grepl(pattern = "stopped motorcycles", df[,field_name]), "Stopped Motorcycles",
        ifelse(grepl(pattern = "stopped motorcycle", df[,field_name]), "Stopped Motorcycle",
        ifelse(grepl(pattern = "stopped transport truck", df[,field_name]), "Stopped Transport Truck", 
        ifelse(grepl(pattern = "stopped car", df[,field_name]), "Stopped Vehicle",
        ifelse(grepl(pattern = "stopped vehicles", df[,field_name]), "Stopped Vehicles",
        ifelse(grepl(pattern = "stopped vehicle", df[,field_name]), "Stopped Vehicle",
        ifelse(grepl(pattern = "stopped truck", df[,field_name]), "Stopped Truck", 
        ifelse(grepl(pattern = "stopped bus", df[,field_name]), "Stopped Bus",
        ifelse(grepl(pattern = "single vehicle", df[,field_name]), "Single Vehicle Collision",
        ifelse(grepl(pattern = "multi vehicle", df[,field_name]), "Multi Vehicle Collision",
        ifelse(grepl(pattern = "collision involving a motorcycle", df[,field_name]), "Collision Involving a Motorcycle", 
        ifelse(grepl(pattern = "collision", df[,field_name]), "Collision",
        ifelse(grepl(pattern = "police investigation", df[,field_name]), "Police Investigation",
        ifelse(grepl(pattern = "police activity", df[,field_name]), "Police Investigation",
        ifelse(grepl(pattern = "scheduled maintenance", df[,field_name]), "Scheduled Maintenance",
        ifelse(grepl(pattern = "tow vehicle", df[,field_name]),  "Tow Truck",
        ifelse(grepl(pattern = "slow moving maintenance vehicles", df[,field_name]), "Slow Moving Maintenance Vehicles",  
        ifelse(grepl(pattern = "slow moving maintenance vehicle", df[,field_name]), "Slow Moving Maintenance Vehicle",
        ifelse(grepl(pattern = "slow moving maintenance crews", df[,field_name]), "Slow Moving Maintenance Crews",
        ifelse(grepl(pattern = "slow moving", df[,field_name]), "Slow Moving Maintenance Crew", 
        ifelse(grepl(pattern = "maintenance vehicles", df[,field_name]), "Maintenance Vehicles",
        ifelse(grepl(pattern = "maintenance vehicle", df[,field_name]), "Maintenance Vehicle", 
        ifelse(grepl(pattern = "maintenance crews", df[,field_name]), "Maintenance Crews",
        ifelse(grepl(pattern = "maintenance crew", df[,field_name]), "Maintenance Crew", 
        ifelse(grepl(pattern = "road maintenance", df[,field_name]), "Road Maintenance",
        ifelse(grepl(pattern = "road work", df[,field_name]), "road maintenance",
        ifelse(grepl(pattern = "roadops", df[,field_name]),  "road maintenance", 
        ifelse(grepl(pattern = "rd ops", df[,field_name]), "road maintenance", 
        ifelse(grepl(pattern = "fallen electrical wires", df[,field_name]), "Fallen Electrical Wires",
        ifelse(grepl(pattern = "emergency vehicle", df[,field_name]), "Emergency Vehicle(s)",
        ifelse(grepl(pattern = "parked", df[,field_name]), "Parked", 
        ifelse(grepl(pattern = "guard rail repairs", df[,field_name]), "Guard Rail Repairs",
        ifelse(grepl(pattern = "guard rail", df[,field_name]), "vehicle collision damaging guard rail",
        ifelse(grepl(pattern = "walking", df[,field_name]), "Illegal Walking Incident",
        ifelse(grepl(pattern = "water main repair", df[,field_name]), "Water Main Repair",
        ifelse(grepl(pattern = "wheel trans", df[,field_name]), "Disabled Wheel Trans",
        ifelse(grepl(pattern = "pedestrian", df[,field_name]), "Pedestrian Incident",
        ifelse(grepl(pattern = "special event", df[,field_name]), "Special Event",
        NA)))))))))))))))))))))))))))))))))))))))))))))),
      
      
      respondents =
        ifelse(grepl(pattern = "police/fire on scene", df[,field_name]),  "Police/ Fire Services",
        ifelse(grepl(pattern = "police/fire/ems on scene", df[,field_name]), "police/fire/ems",
        ifelse(grepl(pattern = "toronto paramedic service vehicle", df[,field_name]), "Paramedics",
        ifelse(grepl(pattern = "toronto paramedics services", df[,field_name]), "Paramedics",
        ifelse(grepl(pattern = "ems", df[,field_name]), "Paramedics",
        ifelse(grepl(pattern = "toronto police", df[,field_name]), "Police",
        ifelse(grepl(pattern = "police", df[,field_name]), "Police",
        ifelse(grepl(pattern = "police cruiser",df[,field_name]), "Police",
        ifelse(grepl(pattern = "police cruisers", df[,field_name]), "Police",
        ifelse(grepl(pattern = "toronto fire", df[,field_name]), "Fire Services",
        ifelse(grepl(pattern = "to fire", df[,field_name]), "Fire Services",
        ifelse(grepl(pattern = "emergency vehicles ", df[,field_name]), "Emergency Vehicles",
        ifelse(grepl(pattern = "emergency vehicle ", df[,field_name]), "Emergency Vehicle",
        ifelse(grepl(pattern = "toronto fire and police", df[,field_name]), "Fire and Police Services",
        ifelse(grepl(pattern = "road operations", df[,field_name]), "Road Operations", 
        ifelse(grepl(pattern = "emergency crews", df[,field_name]), "Emergency Services",
        ifelse(grepl(pattern = "fire truck", df[,field_name]), "Fire Services",
        ifelse(grepl(pattern = "tow truck", df[,field_name]), "Tow Truck",
        ifelse(grepl(pattern = "crash truck", df[,field_name]), "Crash Truck",
        ifelse(grepl(pattern = "city vehicles", df[,field_name]), "City Vehicles",  
        ifelse(grepl(pattern = "city vehicle", df[,field_name]), "City Vehicle",
        ifelse(grepl(pattern = "toronto water", df[,field_name]), "Water Services",
        ifelse(grepl(pattern = "fire", df[,field_name]), "Fire Services",
        ifelse(grepl(pattern = "guild", df[,field_name]), "Guild",
        NA)))))))))))))))))))))))) )
  
  return(df_updates)
}
## END OF 'extract_info_updates' FUNCTION ##
###END OF LIST OF FUNCTIONS ###




















##BEGINNING OF CODE##



### DAN STEP 1 ###



#set up working directory and import "All Existing Incident" table as 'complete.dataset'
getwd()
setwd("Collision and Incidents Data Task(1)")
complete.dataset <- read.csv ("04 04 2016 All Existing Incidents_v1.0.csv", header = TRUE, sep = ",")

#check data type for EventDescription-since it should be initially variable type : 'factor'
#transform EventDescrition column on existing complete.dataset, from type 'factor' to 'character'
#confirm data type after transformation-should be now reassigned as 'character'
class(complete.dataset$EventDescription)
COMPLETE.DATASET <- transform(complete.dataset, EventDescription = as.character(EventDescription))
class(COMPLETE.DATASET$EventDescription)

#Delete any rows on EventDescription containing "Delete By: "
#These rows are either duplicates or reclassified as non-incident
Complete_Dataset_draft <- filter(COMPLETE.DATASET, !grepl("Deleted By:", EventDescription))
Complete_Dataset_v01 <- filter(Complete_Dataset_draft, !grepl("################", EventDescription))
Complete_Dataset_v02 <- filter(Complete_Dataset_v01, !grepl("input error", EventDescription))

Complete_Dataset_v03 <- filter(Complete_Dataset_v02, !grepl("fweffefff", EventDescription))
Complete_Dataset_v04 <- filter(Complete_Dataset_v03, !grepl("sdfsdfsdfsdf", EventDescription))
Complete_Dataset_v05 <- filter(Complete_Dataset_v04, !grepl("blank incident.", EventDescription))
Complete_Dataset_0001 <- filter(Complete_Dataset_v05, !grepl("sdfsdfsdfsdfsdf", EventDescription))



#Convert the text from entire column into lowercase letters
Complete_Dataset_0001$EventDescription <- tolower(Complete_Dataset_0001$EventDescription)

##Re-edit certain details of incident for consistency and fixing grammar errors
#Edit traffic incident extent details

Complete_Dataset_0001$EventDescription <- gsub("now", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("one", "1", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("two", "2", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("three", "3", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("four", "4", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("remains ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("remainds ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("still ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("fully ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("bull-nose ", "bull nose", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("bullnose ", "bull nose", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("re-opend", "reopened", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("bolcked", "blocked", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("bloacked", "blocked", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("no access to", "all lanes closed", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("completely ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("additionally ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("occupied", "blocked", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("also ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("temporarily", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("remained ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("s.kingsway", "s kingsway", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub(" w. ", " w ", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("turning", "turn", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("will be ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("will ", "", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("are ","", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("closed", "blocked", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("should", "shoulder", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("h.o.v", "hov", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("shoulderer", "shoulder", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("shouder", "shoulder", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("right side", "right lane", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("left side", "left lane", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("temporary blockage", "blocked", Complete_Dataset_0001$EventDescription)

#Edit traffic incident reason details

Complete_Dataset_0001$EventDescription <- gsub("vehile", "vehicle", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("moved to", "blocking", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("water main break", "water main repair", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("watermain break","water main repair",Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("pedestrian on bicycle", "pedestrian", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("indy", "special event", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("waterfront marathon", "special event", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("ride for heart", "special event", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("2014 scotiabank caribbean carnival", "special event", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("tree branch removal", "road maintenance", Complete_Dataset_0001$EventDescription)


#Edit type of responder to traffic incident

Complete_Dataset_0001$EventDescription <- gsub("polcie", "police", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("policy", "police", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("personnel", "services", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("stopped road maintenance", "road maintenance", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- sub("a stopped maintenance", "stopped maintenance", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("tuck", "truck", Complete_Dataset_0001$EventDescription)

#Remove double spaces in column and '-'
Complete_Dataset_0001$EventDescription <- gsub("  ", " ", Complete_Dataset_0001$EventDescription)
Complete_Dataset_0001$EventDescription <- gsub("-", " ", Complete_Dataset_0001$EventDescription)
##-End of EventDescription Column' ##



### DAN STEP 2 ###



#Create 'new_df' dataframe, to split the 'EventDescription' column into multiple new columns for all rows of the existing data frame#

new_df <- Complete_Dataset_0001 %>%
mutate(
    # Rows with [...](mm/dd/yyyy hh:mm)
    #temporary rows
    OrigEvent = sub(".*original:\\s(.*)", "\\1", EventDescription),
    Up1temp = sub(".*update:\\s(.*)original:.*", "\\1", EventDescription),
    Up2temp = sub(".*update:*\\s(.*)update:.*original:.*", "\\1", EventDescription),
    Up3temp = sub(".*update:\\s(.*)update:.*update:.*original:.*", "\\1", EventDescription),
    Up4temp = sub(".*update:\\s(.*)update:.*update:.*update:.*original:.*", "\\1", EventDescription),
    Up5temp = sub(".*update:\\s(.*)update:.*update:.*update:.*update:.*original:.*", "\\1", EventDescription),
    
    # filter out any rows that contain '[...] (mm/dd/yyyy hh:mm)' to avoid duplication
    # if tempX does not equal to EventDescription, replace with 'NA'
    # else output current row by outputing 'temp', 'temp2' and so on
    temp_remove_orig = sub(".*original:\\s(.*)", "\\1", EventDescription), 
    tempcol_checkorig = ifelse(temp_remove_orig != EventDescription, "", temp_remove_orig), 
    temp_remove_up1 = sub(".*update:\\s(.*)original:.*", "\\1", EventDescription),
    tempcol_up1 = ifelse(temp_remove_up1 != EventDescription, "", temp_remove_up1),
    temp_remove_up2 = sub(".*update:*\\s(.*)update:.*original:.*", "\\1", EventDescription),
    tempcol_up2 = ifelse(temp_remove_up2 != EventDescription, "", temp_remove_up2),
    temp_remove_up3 = sub(".*update:\\s(.*)update:.*update:.*original:.*", "\\1", EventDescription),
    tempcol_up3 = ifelse(temp_remove_up3 != EventDescription, "", temp_remove_up3),
    temp_remove_up4 = sub(".*update:\\s(.*)update:.*update:.*update:.*original:.*", "\\1", EventDescription),
    tempcol_up4 = ifelse(temp_remove_up4 != EventDescription, "", temp_remove_up4),
    temp_remove_up5 = sub(".*update:\\s(.*)update:.*update:.*update:.*update:.*original:.*", "\\1", EventDescription),
    tempcol_up5 = ifelse(temp_remove_up5 != EventDescription, "", temp_remove_up5),
    
    #Group all temporary variables together
      
    temp_col_orig = sub("(.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
        
    #Group first column (descending order)
    temp_col_51 = sub(".*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_41 = sub(".*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_31 = sub(".*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_21 = sub(".*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_11 = sub(".*?(\\d{1,2}:.*$?)", "\\1", tempcol_up1),
    
    #Group second column temporary variables (descending order)       
    temp_col_52 = sub(".*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_42 = sub(".*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_32 = sub(".*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_22 = sub(".*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1),
    
    #Group third column temporary variables (descending order)
    temp_col_53 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_43 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_33 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1),        
    
    #Group fourth column temporary variables (descending order)   
    temp_col_54 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_44 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1),
    
    
    #Group fifth column temporary variables (descending order) 
    temp_col_115 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_105 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_95 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_85 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_75 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_65 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_55 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1),
    
    #Group sixth column temporary variables (descending order)
    temp_col_116 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_106 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_96 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_86 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_76 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_66 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1),
    
    #Group seventh column temporary variables
    temp_col_117 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_107 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_97 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_87 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_77 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1),
    
    #Group eigth column temporary variables
    temp_col_118 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_108 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_98 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_88 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1), 
      
    #Group ninth column temporary variables
    temp_col_119 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_109 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1), 
    temp_col_99 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1), 
    
    #Group tenth column temporary variables
    temp_col_1110 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)\\d{1,2}:.*", "\\1", tempcol_up1),
    temp_col_1010 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*?)", "\\1", tempcol_up1),
    
    #Group 11th column temporary variables
    temp_col_1111 = sub(".*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?\\d{1,2}:.*?(\\d{1,2}:.*)", "\\1", tempcol_up1),
    
    # Location description-supplementary
    temp_col_orig2 = sub(".*?;(.*?)", "\\1", temp_col_orig),
    temp_col_location_detail = sub("^(.*?);.*", "\\1", tempcol_up1),
    
       
    #Create Columns
    #First update column
    Up1EventDescription = ifelse(Up1temp != EventDescription, Up1temp,
                          ifelse(temp_col_51 != tempcol_up1, temp_col_51, 
                          ifelse(temp_col_41 != tempcol_up1, temp_col_41, 
                          ifelse(temp_col_31 != tempcol_up1, temp_col_31, 
                          ifelse(temp_col_21 != tempcol_up1, temp_col_21, 
                          ifelse(temp_col_11 != tempcol_up1, temp_col_11, NA)))))),
   
    OrigOnly = ifelse(is.na(Up1EventDescription), EventDescription, NA),
       
    #Original description column
    OrigEventDescription = ifelse(temp_col_orig2 != tempcol_up1, temp_col_orig2,
                          ifelse(temp_col_orig != tempcol_up1, temp_col_orig, 
                          ifelse(OrigEvent != EventDescription, OrigEvent,
                          ifelse(OrigOnly == EventDescription, OrigOnly, NA)))),
    
    #second update column
    Up2EventDescription = ifelse(Up2temp != EventDescription, Up2temp,
                          ifelse(temp_col_52 != tempcol_up1, temp_col_52,
                          ifelse(temp_col_42 != tempcol_up1, temp_col_42,
                          ifelse(temp_col_32 != tempcol_up1, temp_col_32,        
                          ifelse(temp_col_22 != tempcol_up1, temp_col_22, NA))))),
    
    
    
    #third update column
    Up3EventDescription = ifelse(Up3temp != EventDescription, Up3temp,
                          ifelse(temp_col_53 != tempcol_up1, temp_col_53,                
                          ifelse(temp_col_43 != tempcol_up1, temp_col_43, 
                          ifelse(temp_col_33 != tempcol_up1, temp_col_33, NA)))),
    
    
    
    #fourth update column
    Up4EventDescription = ifelse(Up4temp != EventDescription, Up4temp,
            ifelse(temp_col_54 != tempcol_up1, temp_col_54, 
                   ifelse(temp_col_44 != tempcol_up1, temp_col_44, NA))),
    
    
    #fifth update column
    Up5EventDescription = ifelse(Up5temp != EventDescription, Up5temp,
                          ifelse(temp_col_115 != tempcol_up1, temp_col_115,
                          ifelse(temp_col_105 != tempcol_up1, temp_col_105,
                          ifelse(temp_col_95 != tempcol_up1, temp_col_95,
                          ifelse(temp_col_85 != tempcol_up1, temp_col_85,
                          ifelse(temp_col_75 != tempcol_up1, temp_col_75,
                          ifelse(temp_col_65 != tempcol_up1, temp_col_65, 
                          ifelse(temp_col_55 != tempcol_up1, temp_col_55, NA)))))))),
    
    #
    Up6EventDescription = ifelse(temp_col_116 != tempcol_up1, temp_col_116,
                          ifelse(temp_col_106 != tempcol_up1, temp_col_106,
                          ifelse(temp_col_96 != tempcol_up1, temp_col_96,
                          ifelse(temp_col_86 != tempcol_up1, temp_col_86,
                          ifelse(temp_col_76 != tempcol_up1, temp_col_76,
                          ifelse(temp_col_66 != tempcol_up1, temp_col_66, NA)))))),
    
    #
    Up7EventDescription = ifelse(temp_col_117 != tempcol_up1, temp_col_117,
                          ifelse(temp_col_107 != tempcol_up1, temp_col_107,
                          ifelse(temp_col_97 != tempcol_up1, temp_col_97,
                          ifelse(temp_col_87 != tempcol_up1, temp_col_87, 
                          ifelse(temp_col_77 != tempcol_up1, temp_col_77, NA))))),
    
  #
  Up8EventDescription = ifelse(temp_col_118 != tempcol_up1, temp_col_118,
                        ifelse(temp_col_108 != tempcol_up1, temp_col_108,
                        ifelse(temp_col_98 != tempcol_up1, temp_col_98,
                        ifelse(temp_col_88 != tempcol_up1, temp_col_88,
                        NA)))),
  
  Up9EventDescription = ifelse(temp_col_119 != tempcol_up1, temp_col_119,
                        ifelse(temp_col_109 != tempcol_up1, temp_col_109,
                        ifelse(temp_col_99 != tempcol_up1, temp_col_99,
                        NA))),
  
  Up10EventDescription = ifelse(temp_col_1110 != tempcol_up1, temp_col_1110,
                        ifelse(temp_col_1010 != tempcol_up1, temp_col_1010, NA)), 

  Up11EventDescription = ifelse(temp_col_1111 != tempcol_up1, temp_col_1111, NA),
  
  Location_Detail = ifelse(temp_col_location_detail != EventDescription, temp_col_location_detail, 
                      ifelse(!is.na(temp_col_location_detail), NA, NA)),

  #temp variables to verify formats of 'EventDescription' column
  
  check_format02 = sub(".*\\((.*)\\).*", "\\1", OrigEventDescription),
  check_format03 = sub("((?:(?:[0-1][0-9])|(?:[2][0-3])|(?:[0-9])):(?:[0-5][0-9])(?::[0-5][0-9])?(?:\\s?(?:am|AM|pm|PM))?).*", "\\1", Up1EventDescription),
  check_format04 = sub("original:.*?\\d{1,2}:.*?(.*).*", "\\1", EventDescription),
  check_format05 = sub(".*?;.*?((?:(?:[0-1][0-9])|(?:[2][0-3])|(?:[0-9])):(?:[0-5][0-9])(?::[0-5][0-9])?(?:\\s?(?:am|AM|pm|PM))?).*", "\\1", EventDescription),
  check_format06 = sub(".*?;(.*?)", "\\1", EventDescription),
  format_event_desc =               
                      ifelse(OrigEventDescription == EventDescription, "1.0",
                      ifelse(check_format02 != OrigEventDescription, "2.0",
                      ifelse(check_format05 != EventDescription, "5.0",
                      ifelse(check_format06 != EventDescription, "6.0",
                      ifelse(check_format03 != Up1EventDescription, "3.0",
                      ifelse(check_format04 != OrigEventDescription, "7.0",
                                                                        
                       NA))))))
  
  
  )%>%
  
select(EventDescription, StartDate, EndDate, OrigEventDescription, Up1EventDescription, 
      Up2EventDescription, Up3EventDescription, Up4EventDescription,Up5EventDescription, Up6EventDescription, 
       Up7EventDescription, Up8EventDescription, Up9EventDescription, Up10EventDescription, 
       Up11EventDescription, Location_Detail, check_format02, check_format03, check_format04,check_format05,check_format06, format_event_desc)



### DAN STEP 3 ###



#Apply the function 'extract_datetimestamp' to extract recorded time for original incident and each update
#Creates a new data frame

test_orig_datetime <- extract_datetimestamp(new_df, OrigEventDescription, StartDate)
test_up1_datetime  <- extract_datetimestamp(new_df, Up1EventDescription, EndDate)
test_up2_datetime  <- extract_datetimestamp(new_df, Up2EventDescription, EndDate)
test_up3_datetime  <- extract_datetimestamp(new_df, Up3EventDescription, EndDate)
test_up4_datetime  <- extract_datetimestamp(new_df, Up4EventDescription, EndDate)
test_up5_datetime  <- extract_datetimestamp(new_df, Up5EventDescription, EndDate)
test_up6_datetime  <- extract_datetimestamp(new_df, Up6EventDescription, EndDate)
test_up7_datetime  <- extract_datetimestamp(new_df, Up7EventDescription, EndDate)
test_up8_datetime <- extract_datetimestamp(new_df, Up8EventDescription, EndDate)
test_up9_datetime <- extract_datetimestamp(new_df, Up9EventDescription, EndDate)
test_up10_datetime <- extract_datetimestamp(new_df, Up10EventDescription, EndDate)
test_up11_datetime <- extract_datetimestamp(new_df, Up11EventDescription, EndDate)

#Extract only the column with recorded time for each database, saved as a new data frame
orig_datetime <- test_orig_datetime[35]
update1_datetime <- test_up1_datetime[35]
update2_datetime <- test_up2_datetime[35]
update3_datetime <- test_up3_datetime[35]
update4_datetime <- test_up4_datetime[35]
update5_datetime <- test_up5_datetime[35]
update6_datetime <- test_up6_datetime[35]
update7_datetime <- test_up7_datetime[35]
update8_datetime <- test_up8_datetime[35]
update9_datetime <- test_up9_datetime[35]
update10_datetime <- test_up10_datetime[35]
update11_datetime <- test_up11_datetime[35]


#Rename the new column header, according to either original incident or update number
colnames(orig_datetime)[colnames(orig_datetime) == "date_and_time"] <- "original_date_and_time"
colnames(update1_datetime)[colnames(update1_datetime) == "date_and_time"] <- "update1_date_and_time"
colnames(update2_datetime)[colnames(update2_datetime) == "date_and_time"] <- "update2_date_and_time"
colnames(update3_datetime)[colnames(update3_datetime) == "date_and_time"] <- "update3_date_and_time"
colnames(update4_datetime)[colnames(update4_datetime) == "date_and_time"] <- "update4_date_and_time"
colnames(update5_datetime)[colnames(update5_datetime) == "date_and_time"] <- "update5_date_and_time"
colnames(update6_datetime)[colnames(update6_datetime) == "date_and_time"] <- "update6_date_and_time"
colnames(update7_datetime)[colnames(update7_datetime) == "date_and_time"] <- "update7_date_and_time"
colnames(update8_datetime)[colnames(update8_datetime) == "date_and_time"] <- "update8_date_and_time"
colnames(update9_datetime)[colnames(update9_datetime) == "date_and_time"] <- "update9_date_and_time"
colnames(update10_datetime)[colnames(update10_datetime) == "date_and_time"] <- "update10_date_and_time"
colnames(update11_datetime)[colnames(update11_datetime) == "date_and_time"] <- "update11_date_and_time"



### DAN STEP 4 ###



#Calling 'extract_info' function to split columns below to create new columns for number and type traffic lanes, reason and respondents
#Ensure removal of double spaces in column for correct data extraction
test_df2 <- extract_info_original(new_df, OrigEventDescription)
test_df3 <- extract_info_updates(new_df, Up1EventDescription)
test_df4 <- extract_info_updates(new_df, Up2EventDescription)
test_df5 <- extract_info_updates(new_df, Up3EventDescription)
test_df6 <- extract_info_updates(new_df, Up4EventDescription)
test_df7 <- extract_info_updates(new_df, Up5EventDescription) #up5
test_df8 <- extract_info_updates(new_df, Up6EventDescription)
test_df9 <- extract_info_updates(new_df, Up7EventDescription)
test_df10 <- extract_info_updates(new_df, Up8EventDescription)
test_df11 <- extract_info_updates(new_df, Up9EventDescription)
test_df12 <- extract_info_updates(new_df, Up10EventDescription)
test_df13 <- extract_info_updates(new_df, Up11EventDescription) #up11



#Save new data frames for original incident
#extract the new columns from 'test data frames' below, according to original and updated information
#extract dateandtime/time stamp

Original_Incidents <- test_df2[23:26]
Update1_Incidents <- test_df3[29:32] 
Update2_Incidents <- test_df4[29:32]
Update3_Incidents <- test_df5[29:32]
Update4_Incidents <- test_df6[29:32]
Update5_Incidents <- test_df7[29:32]
Update6_Incidents <- test_df8[29:32]
Update7_Incidents <- test_df9[29:32]
Update8_Incidents <- test_df10[29:32]
Update9_Incidents <- test_df11[29:32]
Update10_Incidents <- test_df12[29:32]
Update11_Incidents <- test_df13[29:32]
location_detail <- test_df2[16]
format_event_desc <- test_df2[22]


#Rename the new columns created on the new data frames above
#Columns below will retain same data, only changes the name of each column header

colnames(Original_Incidents)[colnames(Original_Incidents)=="nlanes"] <- "original_nlanes"
colnames(Original_Incidents)[colnames(Original_Incidents) == "lanes_desc"] <- "original_lanes_desc"
colnames(Original_Incidents)[colnames(Original_Incidents)== "incident_reason"] <- "original_incident_reason"
colnames(Original_Incidents)[colnames(Original_Incidents)== "respondents"]<- "original_respondents"

colnames(Update1_Incidents)[colnames(Update1_Incidents)== "nlanes"] <- "Update1_nlanes"
colnames(Update1_Incidents)[colnames(Update1_Incidents)== "lanes_desc"] <- "Update1_lanes_desc"
colnames(Update1_Incidents)[colnames(Update1_Incidents)== "incident_reason"] <- "Update1_incident_reason"
colnames(Update1_Incidents)[colnames(Update1_Incidents)== "respondents"] <- "Update1_respondents"

colnames(Update2_Incidents)[colnames(Update2_Incidents)== "nlanes"] <- "Update2_nlanes"
colnames(Update2_Incidents)[colnames(Update2_Incidents)== "lanes_desc"] <- "Update2_lanes_desc"
colnames(Update2_Incidents)[colnames(Update2_Incidents)== "incident_reason"] <- "Update2_incident_reason"
colnames(Update2_Incidents)[colnames(Update2_Incidents)== "respondents"] <- "Update2_respondents"

colnames(Update3_Incidents)[colnames(Update3_Incidents)== "nlanes"] <- "Update3_nlanes"
colnames(Update3_Incidents)[colnames(Update3_Incidents)== "lanes_desc"] <- "Update3_lanes_desc"
colnames(Update3_Incidents)[colnames(Update3_Incidents)== "incident_reason"]<- "Update3_incident_reason"
colnames(Update3_Incidents)[colnames(Update3_Incidents)== "respondents"] <- "Update3_respondents"

colnames(Update4_Incidents)[colnames(Update4_Incidents)== "nlanes"]<- "Update4_nlanes"
colnames(Update4_Incidents)[colnames(Update4_Incidents)== "lanes_desc"] <- "Update4_lanes_desc"
colnames(Update4_Incidents)[colnames(Update4_Incidents)== "incident_reason"]<- "Update4_incident_reason"
colnames(Update4_Incidents)[colnames(Update4_Incidents)== "respondents"]<- "Update4_respondents"

colnames(Update5_Incidents)[colnames(Update5_Incidents)== "nlanes"] <- "Update5_nlanes"
colnames(Update5_Incidents)[colnames(Update5_Incidents)== "lanes_desc"]<- "Update5_lanes_desc"
colnames(Update5_Incidents)[colnames(Update5_Incidents)== "incident_reason"] <- "Update5_incident_reason"
colnames(Update5_Incidents)[colnames(Update5_Incidents)== "respondents"] <- "Update5_respondents"

colnames(Update6_Incidents)[colnames(Update6_Incidents)== "nlanes"] <- "Update6_nlanes"
colnames(Update6_Incidents)[colnames(Update6_Incidents)== "lanes_desc"]<- "Update6_lanes_desc"
colnames(Update6_Incidents)[colnames(Update6_Incidents)== "incident_reason"] <- "Update6_incident_reason"
colnames(Update6_Incidents)[colnames(Update6_Incidents)== "respondents"] <- "Update6_respondents"

colnames(Update7_Incidents)[colnames(Update7_Incidents)== "nlanes"] <- "Update7_nlanes"
colnames(Update7_Incidents)[colnames(Update7_Incidents)== "lanes_desc"]<- "Update7_lanes_desc"
colnames(Update7_Incidents)[colnames(Update7_Incidents)== "incident_reason"] <- "Update7_incident_reason"
colnames(Update7_Incidents)[colnames(Update7_Incidents)== "respondents"] <- "Update7_respondents"

colnames(Update8_Incidents)[colnames(Update8_Incidents)== "nlanes"] <- "Update8_nlanes"
colnames(Update8_Incidents)[colnames(Update8_Incidents)== "lanes_desc"]<- "Update8_lanes_desc"
colnames(Update8_Incidents)[colnames(Update8_Incidents)== "incident_reason"] <- "Update8_incident_reason"
colnames(Update8_Incidents)[colnames(Update8_Incidents)== "respondents"] <- "Update8_respondents"

colnames(Update9_Incidents)[colnames(Update9_Incidents)== "nlanes"] <- "Update9_nlanes"
colnames(Update9_Incidents)[colnames(Update9_Incidents)== "lanes_desc"]<- "Update9_lanes_desc"
colnames(Update9_Incidents)[colnames(Update9_Incidents)== "incident_reason"] <- "Update9_incident_reason"
colnames(Update9_Incidents)[colnames(Update9_Incidents)== "respondents"] <- "Update9_respondents"

colnames(Update10_Incidents)[colnames(Update10_Incidents)== "nlanes"] <- "Update10_nlanes"
colnames(Update10_Incidents)[colnames(Update10_Incidents)== "lanes_desc"]<- "Update10_lanes_desc"
colnames(Update10_Incidents)[colnames(Update10_Incidents)== "incident_reason"] <- "Update10_incident_reason"
colnames(Update10_Incidents)[colnames(Update10_Incidents)== "respondents"] <- "Update10_respondents"

colnames(Update11_Incidents)[colnames(Update11_Incidents)== "nlanes"] <- "Update11_nlanes"
colnames(Update11_Incidents)[colnames(Update11_Incidents)== "lanes_desc"]<- "Update11_lanes_desc"
colnames(Update11_Incidents)[colnames(Update11_Incidents)== "incident_reason"] <- "Update11_incident_reason"
colnames(Update11_Incidents)[colnames(Update11_Incidents)== "respondents"] <- "Update11_respondents"




#merge all new columns from multiple new data frames into a single dataframe
Complete_Dataset_0002 <-  bind_cols(format_event_desc, orig_datetime, Original_Incidents, update1_datetime, Update1_Incidents, update2_datetime, Update2_Incidents, update3_datetime, Update3_Incidents, update4_datetime, Update4_Incidents, update5_datetime, Update5_Incidents, 
                                    update6_datetime, Update6_Incidents, update7_datetime, Update7_Incidents, update8_datetime, Update8_Incidents, update9_datetime, Update9_Incidents,
                                    update10_datetime, Update10_Incidents, update11_datetime, Update11_Incidents, location_detail)

Complete_Dataset_Final_Copy <- bind_cols(Complete_Dataset_0001,Complete_Dataset_0002)

#Export final data frame as a 'csv' spreadsheet file
write.csv(Complete_Dataset_Final_Copy, file = "2016-04-04 All Existing Incidents Dataset_v1.1.csv")


#Change the following columns into lower case letter only
Complete_Dataset_Final_Copy$MainStreet <- tolower(Complete_Dataset_Final_Copy$MainStreet)
Complete_Dataset_Final_Copy$AtStreet <- tolower(Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$FromStreet <- tolower(Complete_Dataset_Final_Copy$FromStreet)
Complete_Dataset_Final_Copy$ToStreet <- tolower(Complete_Dataset_Final_Copy$ToStreet)

##Edit other columns ##
Complete_Dataset_Final_Copy$MainStreet <- gsub("kennedy ave", "kennedy rd", Complete_Dataset_Final_Copy$MainStreet)
Complete_Dataset_Final_Copy$MainStreet <- gsub("york ave", "york st", Complete_Dataset_Final_Copy$MainStreet)
Complete_Dataset_Final_Copy$MainStreet <- gsub("black creek blvd", "black creek dr", Complete_Dataset_Final_Copy$MainStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("allen x s lawrence w ramp", "allen rd", Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("beechwood ave", "beechwood dr", Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("british columbia dr", "british columbia rd", Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("british colombia dr", "british columbia rd", Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("browns lane", "browns line", Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("church ave", "church st", Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("yonge blvd", "yonge st", Complete_Dataset_Final_Copy$AtStreet)
Complete_Dataset_Final_Copy$AtStreet <- gsub("york ave", "york st", Complete_Dataset_Final_Copy$AtStreet)


#write.csv(Complete_Dataset_Final_Copy, file = "2016-04-04 All Existing Incidents Dataset_v1.2.csv")



####################################################################
#   Clean any fields containing street names                       #
#   Edit and rename street names under 'Location_Detail column'    #
####################################################################



#Clear direction, ramp, grammar 

Complete_Dataset_Final_Copy$Location_Detail <- gsub("norht of", "north of", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("eastt of", "east of", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("wes of", "west of", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("sotuh", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("&", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub(", ", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("before", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("going", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("the", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("just", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("split", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("end of", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("near", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("above", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("right", "", Complete_Dataset_Final_Copy$Location_Detail)

#clean street names with incorrect spelling#
Complete_Dataset_Final_Copy$Location_Detail <- gsub("on 427 ", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("on 401 ", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("on lawrence", "lawrence", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("allen express", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("allen n", "allen", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("and  on gerrard", "gerrard", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bhurst", "bathurst", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("brookerslane", "brookers", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bayvieave", "bayview", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bayviebloor", "bayview/bloor", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bayvieto", "bayview to", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bayvief ramp", "bayview", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bayvie", "bayview", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bay viebloor", "bayview/bloor", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bayvieww", "bayview", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bayview f", "bayview", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bay view", "bayview", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("b.c. dr", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("b.c. drive", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("b.c dr ", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bc drive", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bc dr ", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("b.c", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("b.c.", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bc", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("british columbia drive", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("british columbia dr", "british columbia rd", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("blood viaduct", "bloor viaduct", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("blooviaduct", "bloor viaduct", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bood viaduct", "bloor viaduct", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bloor viadcut", "bloor viaduct", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("browns line", "brown's line", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bull noseto", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("bull nose", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("carlaavenue", "carla ave", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("carlato", "carla ave to", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("dmills", "don mills", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("donmills", "don mills", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("don mills f", "don mills", Complete_Dataset_Final_Copy$Location_Detail)

Complete_Dataset_Final_Copy$Location_Detail <- gsub("don mills rd. n.", "don mills", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("don mills rd. s.", "don mills", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("don road way", "don roadway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("droadway", "don roadway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("duffeirn", "dufferin st", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("dvalley", "don valley parkway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("dvp", "don valley parkway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("east if", "east of", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("east hwy 427", "hwy 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("east liberty street", "liberty street", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("east jameson", "jameson", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("eastt of", "east of", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("eat", "east", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("e  dufferin", "dufferin", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("e of", "east of", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("eixt", "exit", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("eglintave", "eglinton ave", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("eglington", "eglinton ave", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("end", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("fgg", "gardiner", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("f.g. gardiner", "gardiner", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("f.g gardiner", "gardiner", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("f. g. gardiner", "gardiner", Complete_Dataset_Final_Copy$Location_Detail)

Complete_Dataset_Final_Copy$Location_Detail <- gsub("intersection", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("highway427", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("hy 427", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("hyw 427", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("hwy427", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("on 427", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w  427", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w 427", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("highway 427 s", "highway 427", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("lakeshore", "lake shore", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("lake show", "lake shore", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("laurence", "lawrence", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("lawwrence", "lawrence", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("islingtonon", "islington ", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("islingtto", "islington to", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("islington f", "islington ", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("islinton", "islington", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("islingon", "islington", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("don road way", "don roadway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("jameson ge", "jameson", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("ls/jameson", "jameson", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("ls colborne lodge", "colborne lodge dr", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("newfounland", "newfoundland", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("hwy 401 f", "highway 401", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("milwood", "millwood", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub(" parside", "parkside", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("parklawn", "park lawn", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("parlawn", "park lawn", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("parlawn", "park lawn", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("s. kingsway", "south kingsway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("s kingsway", "south kingsway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("southkingsway", "south kingsway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("sout  lawrence", "lawrence", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("spidina", "spadina", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("sparidge", "spanbridge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("span bridge", "spanbridge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("spandbridge", "spanbridge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("transfer  to w", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("tp", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("windernere", "windermere", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w don roadway", "don roadway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w eglinton", "eglinton", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w lawrence", "lawrence", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w lake shore", "lake shore", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w gardiner", "gardiner", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("w york mills", "york mills", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("wynford   don valley parkway", "don valley parkway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("wynford  don valley parkway", "don valley parkway", Complete_Dataset_Final_Copy$Location_Detail) 
Complete_Dataset_Final_Copy$Location_Detail <- gsub("wynford lawrence", "lawrence", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("wyndord", "wynford", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("yb/yonge", "york/bay/yonge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("y/b/y", "york/bay/yonge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("yby", "york/bay/yonge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("yonge/bay/york   grand", "grand", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("york/bay yonge", "york/bay/yonge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("yorkbayyonge", "york/bay/yonge", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("young", "yonge st", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("yorkmills", "york mills", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("york mils", "york mills", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("geway", "gateway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("/ wynford", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("/ kipling", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("/ jarvis w", "", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("/ jarvis lake shore", "lake shore", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub(":    islington", "islington", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("401 e onto", "allen", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("401 e", "401", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("/ don valley parkway", "don valley parkway", Complete_Dataset_Final_Copy$Location_Detail)
Complete_Dataset_Final_Copy$Location_Detail <- gsub("'s", "", Complete_Dataset_Final_Copy$Location_Detail)

#Create a new column to identify if incident located on: on-ramp, off-ramp and other relevant details
Complete_Dataset_Final_Copy_v2.0 <- Complete_Dataset_Final_Copy %>%
  mutate(
    highway_ramp =
      ifelse(grepl(pattern = "off ramp", EventDescription), "Off-Ramp",
             ifelse(grepl(pattern = "of ramp", EventDescription), "Off-Ramp",
                    ifelse(grepl(pattern = "offramp", EventDescription), "Off-Ramp",
                           ifelse(grepl(pattern = "exit ramp", EventDescription), "Off-Ramp",       
                                  ifelse(grepl(pattern = "exit", EventDescription), "Off-Ramp",
                                         ifelse(grepl(pattern = "on ramp", EventDescription), "On-Ramp",
                                                ifelse(grepl(pattern = "ramp", EventDescription), "Ramp (n.o.s)",
                                                       NA))))))),
    
    direction_incident = 
      ifelse(grepl(pattern = "north of", EventDescription), "North of ",
             ifelse(grepl(pattern = "south of", EventDescription), "South of ",
                    ifelse(grepl(pattern = "eastt of", EventDescription), "East of",
                           ifelse(grepl(pattern = "east of", EventDescription), "East of ",
                                  ifelse(grepl(pattern = "west of", EventDescription), "West of ",
                                         ifelse(grepl(pattern = "wes of", EventDescription), "West of", 
                                                NA)))))),
    
    collectors_or_express =
      ifelse(grepl(pattern = "in collectors", EventDescription), "Collectors",
             ifelse(grepl(pattern = "(collectors)", EventDescription), "Collectors",
                    ifelse(grepl(pattern = "(collector)", EventDescription), "Collectors",
                           ifelse(grepl(pattern = "collector", EventDescription), "Collectors",
                                  
                                  ifelse(grepl(pattern = "in express", EventDescription), "Express",
                                         ifelse(grepl(pattern = "(express)", EventDescription), "Express",
                                                ifelse(grepl(pattern = "express;", EventDescription), "Express",
                                                       ifelse(grepl(pattern = "at the express", EventDescription), "Express",
                                                              ifelse(grepl(pattern = "on the express", EventDescription), "Express",
                                                                     NA)))))))))
    
    
    
  )

#write.csv(Complete_Dataset_Final_Copy_v2.0, file = "2016-04-04 All Existing Incidents Dataset_v2.0.csv")

#Extract street name for AtStreet

Complete_Dataset_Final_Copy_v3.0 <- Complete_Dataset_Final_Copy_v2.0 %>%
  mutate(
    col_temp_from_street = sub(".*from\\s(.*?)to.*", "\\1", Location_Detail),
    col_temp_to_street = sub(".*from.*to\\s(.*?)", "\\1", Location_Detail),
    
    
        
    #For MainStreet field temp variables
    col_a = sub(".*bound\\s(.*)", "\\1", Location_Detail),    
    col_b = sub("(.*?)at.*", "\\1", Location_Detail),
    
    #For AtStreet field temp variables
    col_1 = sub(".*at\\s(.*?)","\\1", Location_Detail),
    col_2 = sub(".*of\\s(.*?)", "\\1", Location_Detail),
    col_3 = sub(".*ramp\\s(.*?)", "\\1", Location_Detail),
    col_4 = sub("(.*\\s)ramp.*", "\\1", Location_Detail),

   
    
    #Save  temp MainStreet field
    col_main_a = ifelse(col_a != Location_Detail, col_a, NA),
    col_main_b = ifelse(col_b != Location_Detail, col_b, NA),
    
    #Save temp AtStreet
    col_at_1 = ifelse(col_1 != Location_Detail, col_1, NA),
    col_at_2 = ifelse(col_2 != Location_Detail, col_2, NA),
    col_at_3 = ifelse(col_3 != Location_Detail, col_3, NA),
    col_at_4 = ifelse(col_4 != Location_Detail, col_4, NA),
    #col_at_5 = ifelse(col_5 != Location_Detail, col_5, NA),
    
    #Save FromStreet column
    col_from_street = ifelse(col_temp_from_street != Location_Detail, col_temp_from_street, NA),
    #Save ToStreet column
    col_to_street = ifelse(col_temp_to_street != Location_Detail, col_temp_to_street, NA),
    
    #save columns
    #MainStreet
    col_main_street = ifelse(!is.na(col_main_a), col_main_a,
                      ifelse(!is.na(col_main_b), col_main_b, NA)),
    #AtStreet
    col_at_street = ifelse(!is.na(col_at_1), col_at_1,
                    ifelse(!is.na(col_at_2), col_at_2,
                    ifelse(!is.na(col_at_3), col_at_3,
                    ifelse(!is.na(col_at_4), col_at_4,
                    NA))))
   
    
    )%>%

select(EventDescription, Location_Detail, col_temp_from_street, col_temp_to_street, col_1, col_2, col_3, col_4, col_a, col_b,
       col_at_1, col_at_2, col_at_3, col_at_4, col_main_a, col_main_b, col_main_street, col_at_street, col_from_street, col_to_street)

#write.csv(Complete_Dataset_Final_Copy_v3.0, "Complete_Dataset_Final_Copy_v3.0.csv")

##Clean by removing certain details: ramp for MainStreet, AtStreet, FromStreet, ToStreet columns
#MainStreet


Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("and southbound", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("and westbound", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("(.*\\s)at", "\\1", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("allen", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at don valley parkway", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at ellesmere", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at gardiner expressway", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at gardiner", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at lake shore blvd", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at lake shore", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at lawrence", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at park lawn", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at queen st e bridge", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("at spadina", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("express and collectors", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("bremner", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("collectors", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("collector", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("overlea blvd", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("don mills  don valley parkway", "don mills", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("don millsorth", "don mills", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("don millsrth of", "don mills", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("don mills  to", "don mills", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("don valley parkway", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("don valley from", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("don mills o'connor dr", "don mills", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("finch", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("from lake shore blvd e", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("from lake shore", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("dundas   don valley parkway", "dundas st", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("east of university", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("eglinton  don mills", "eglinton", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("eglinton  to", "eglinton", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("exit", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("from", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("garfiner", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("expressway", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("expy", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("jameson gate", "jameson", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("jameson g", "jameson", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("kipling   queensway", "kipling", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lanes", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lane", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("of overlea", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on lake shore blvd w", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on lake shoreast", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on lake shore", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on off  ramp", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on  to lake shore", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on bay", "bay", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on yonge", "yonge", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on spadina", "spadina", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on jarvis", "jarvis", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on gerrard", "gerrard", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on  queensway", "queensway", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to  b", "to b", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to b", "to bathurst st", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to don valley parkway", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to gardiner", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to rees", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore blvd west", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore blvd w", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore w", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore blvd east", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore blvd e", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore blvd", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shoreast", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore west", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to lake shore", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to jarvis", "jarvis", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to kipling", "kipling", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("to wynford", "wynford", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore blvd west", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)

Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore blvd east", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore blvd w", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore blvd e", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore blvd", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shoreast", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore west", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore w", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lake shore", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("lawrence  black creek", "lawrence", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("eastern at don valley parkway", "eastern ave", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("eastern ave  don valley parkway", "eastern ave", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("eastern  don valley parkway", "eastern ave", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("gardiner", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("pottery  bayview", "pottery", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("rom", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("warden  ellesmere", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("york est", "york", Complete_Dataset_Final_Copy_v3.0$col_main_street) 
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("york mills to", "york mills", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("xpress", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("ast jarvis", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)

Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("off  ramp", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("off ramp", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("on ramp", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("north of", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("south of", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("east of", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("west of", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("north", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("south", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("west", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("eb/wb", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("wb", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("eb", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("nb", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("sb", "", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("rd drive", "rd", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("mills.", "mills", Complete_Dataset_Final_Copy_v3.0$col_main_street)
Complete_Dataset_Final_Copy_v3.0$col_main_street <- gsub("st", "st", Complete_Dataset_Final_Copy_v3.0$col_main_street)






#write.csv(Complete_Dataset_Final_Copy_v3.0, "Complete_Dataset_Final_Copy_v4.0.csv")

#clean AtStreet column

Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("off ramp to", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("on ramp", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("off", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("ramp", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("east of", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("north of", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("south of", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("west of", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("sb", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("nb", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("wb", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("eb", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("eastbound and westbound", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("eastbound", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("westbound", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("northbound", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("southbound", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("north", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("south", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("west", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("from", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("exit", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("onto allen", "allen", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don valley parkway to gardiner", "don valley parkway to gardiner", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to  allen", "allen", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to don valley parkway south", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to don valley parkway", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to  don valley parkway", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to gardiner on", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to  gardiner", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to gardiner", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to lake shore blvd east", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to lake shore blvd e", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to lake shore blvd", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to lake shoreast", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to lake shore/", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to lake shore", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("in collector lanes", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("in collector", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to collector", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("collectors", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("collector", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("in expressway", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("in express lanes", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("in express", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("expressway", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("express", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("of park lawn", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("hwy 401ast", "hwy 401", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("highway 401ast", "hwy 401", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lake shoreast", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("sherbourneast", "sherbourne", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("yongeast", "yonge", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lawrenceast", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("at park lawn", "park lawn", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("the", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("to\\s(.*)", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("^on\\s(.*)", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("soutbound", "", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("sparidge", "spanbridge", Complete_Dataset_Final_Copy_v3.0$col_at_street)

Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("bay on", "bay st", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("bayview/bloor on", "bayview/bloor", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("bayview/ bloor on", "bayview/bloor", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("bayview bloor", "bayview/bloor", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("bayview on", "bayview/bloor", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("bloor danforth viaduct", "bloor viaduct", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("bloow viaduct", "bloor viaduct", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("brown line on", "brown's line", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("colbourne lodge", "colborne lodge", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("danforth on", "bayview/bloor", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don mills rd. on", "don mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don mills n on", "don mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don mills n", "don mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don mills  on", "don mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don mills on", "don mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("millsrth", "mills", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don valley parkway on", "don valley parkway", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("don roadway on", "don roadway", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("dundas on", "dundas st e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("eglinton on", "eglinton ave e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("hwy 401 on", "hwy 401", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("eastern on", "eastern", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("eglinton ave e on", "eglinton ave e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("eglinton e", "eglinton ave e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("gardiner   don valley parkway", "gardiner expressway", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("gardiner expy", "gardiner expressway", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("grand on", "grand ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("highway 427 on", "hwy 427", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("hwy 427 s", "hwy 427", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("hwy 427 on", "hwy 427", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("hwy. 427", "hwy 427", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("hwy. 401", "hwy 401", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("islington in", "islington ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("islington on", "islington ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("islington lanes", "islington ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("islington s", "islington ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("jameson on", "jameson ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("jarvis on", "jarvis st", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("kingway", "south kingsway", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("kingsway on", "south kingsway", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("kipiling ave", "kipiling ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("kipling lanes", "kipling ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("kipling in", "kipling ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("kipling on", "kipling ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("shore\\((.*)\\)", "\\1", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("kipling s", "kipling ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)

Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lake shore and", "lake shore blvd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lake shore blvd e on", "lake shore blvd e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lake shore   park lawn", "park lawn rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lawerence", "lawrence", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lawrence st. e. on", "lawrence ave e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lawrence ave. w on", "lawrence ave w", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lawrence east on", "lawrence ave e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lawrence on", "lawrence ave e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("lower jarvis on", "lower jarvis", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("new bruinswick", "new brunswick way", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("newfonudland dr", "newfoundland dr", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("new foundland", "newfoundland dr", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("newfoundland drive", "newfoundland dr", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("park lawn on", "park lawn rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("park lawn  lake shore", "park lawn rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("queen on", "queen st e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("queen st e bridge", "queen st e", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("rees on", "rees st", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("royal york s", "royal york rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("shepaprd", "sheppard ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("spadina on", "spadina ave", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("wynford on", "wynford dr", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("york millls", "york mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("york mills e on", "york mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("york mills on", "york mills rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("york on", "york", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("yonge/bay/york on", "yonge/bay/york", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("yonge and lake shore", "yonge st", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("rd drive", "rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("rdon", "rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("rd dr", "rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("rdive", "rd", Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("rd.", "rd",Complete_Dataset_Final_Copy_v3.0$col_at_street)
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("dr.", "dr",Complete_Dataset_Final_Copy_v3.0$col_at_street)      
Complete_Dataset_Final_Copy_v3.0$col_at_street <- gsub("st.", "st",Complete_Dataset_Final_Copy_v3.0$col_at_street)


#write.csv(Complete_Dataset_Final_Copy_v3.0, "Complete_Dataset_Final_Copy_v3.1.csv")

#Clean col_from_street column 
Complete_Dataset_Final_Copy_v3.0$col_from_street <- gsub("west of", "", Complete_Dataset_Final_Copy_v3.0$col_from_street)
Complete_Dataset_Final_Copy_v3.0$col_from_street <- gsub("east of", "", Complete_Dataset_Final_Copy_v3.0$col_from_street)
Complete_Dataset_Final_Copy_v3.0$col_from_street <- gsub("north of", "", Complete_Dataset_Final_Copy_v3.0$col_from_street)
Complete_Dataset_Final_Copy_v3.0$col_from_street <- gsub("rd.", "rd", Complete_Dataset_Final_Copy_v3.0$col_from_street)

#clean col_to_street column
Complete_Dataset_Final_Copy_v3.0$col_to_street <- gsub("west of", "", Complete_Dataset_Final_Copy_v3.0$col_to_street)
Complete_Dataset_Final_Copy_v3.0$col_to_street <- gsub("rdive", "rd", Complete_Dataset_Final_Copy_v3.0$col_to_street)
Complete_Dataset_Final_Copy_v3.0$col_to_street <- gsub("rd.", "rd", Complete_Dataset_Final_Copy_v3.0$col_to_street)
Complete_Dataset_Final_Copy_v3.0$col_to_street <- gsub("dr", "dr", Complete_Dataset_Final_Copy_v3.0$col_to_street)




#edit rows on MainStreet field that contain off-ramp and on-ramp 
Complete_Dataset_Final_Copy_v4.0 <- Complete_Dataset_Final_Copy_v3.0 %>%
mutate(
temp_main_street_final =  
                    ifelse(grepl(pattern = "ramp", EventDescription), NA,
                    ifelse(grepl(pattern = "collectors", EventDescription), NA, 
                    ifelse(grepl(pattern = "rees to bathurst st", col_main_street), NA,
                    ifelse(grepl(pattern = "bathurst to west of fort york", EventDescription), NA,
                    ifelse(grepl(pattern = "harbour at york", EventDescription), "harbour st",
                    ifelse(grepl(pattern = "harbour at bay", EventDescription), "harbour st",
                    ifelse(grepl(pattern = "west of", col_main_a), NA,
                    ifelse(grepl(pattern = "east of", col_main_a), NA,
                    ifelse(grepl(pattern = "south of", col_main_a), NA,
                    ifelse(grepl(pattern = "north of", col_main_a), NA,
                    ifelse(grepl(pattern = "west of", col_main_b), NA,
                    ifelse(grepl(pattern = "south of", col_main_b), NA,
                    ifelse(grepl(pattern = "east of", col_main_b), NA,
                    ifelse(grepl(pattern = "british columbia rd to bathurst st", col_main_street), NA,
                    ifelse(grepl(pattern = "strachan ave. to bathurst st", col_main_street), NA,
                    ifelse(grepl(pattern = "strachan to bathurst st", col_main_street), NA,
                    ifelse(grepl(pattern = "lawrence  black creek", col_main_street), "lawrence ave w",
                    ifelse(grepl(pattern = "pottery  bayview", col_main_street), "pottery road",
                    ifelse(grepl(pattern = "eglinton  don mills", col_main_street), "eglinton ave e",
                    
                    col_main_street))))))))))))))))))),


temp_at_street_final =  ifelse(grepl(pattern = "ramp", EventDescription), "NA",
                        col_at_street),

temp_from_street_final =ifelse(grepl(pattern = "ramp", EventDescription), NA,
                        ifelse(grepl(pattern = "collectors", EventDescription), NA, 
                        ifelse(grepl(pattern = "bathurst to west of fort york", EventDescription), "bathurst st",
                        ifelse(grepl(pattern = "rees to bathurst st", col_main_street), "rees st",
                        ifelse(grepl(pattern = "british columbia rd to bathurst st", col_main_street), "british columbia rd",
                        ifelse(grepl(pattern = "strachan ave. to bathurst st", col_main_street), "strachan ave",
                        ifelse(grepl(pattern = "strachan to bathurst st", col_main_street), "strachan ave",
                        ifelse(grepl(pattern = "isling", col_from_street), "islington ave",
                        ifelse(grepl(pattern = "eglin", col_from_street), "eglinton ave",
                        col_from_street))))))))),

temp_to_street_final =ifelse(grepl(pattern = "ramp", EventDescription), NA,
                      ifelse(grepl(pattern = "collectors", EventDescription), NA,
                      ifelse(grepl(pattern = "bathurst to west of fort york", EventDescription), "fort york blvd",
                       ifelse(grepl(pattern = "rees to bathurst st", col_main_street), "bathurst st",
                       ifelse(grepl(pattern = "british columbia rd to bathurst st", col_main_street), "bathurst st",
                       ifelse(grepl(pattern = "strachan ave. to bathurst st", col_main_street), "bathurst st",
                       ifelse(grepl(pattern = "strachan to bathurst st", col_main_street), "bathurst st",
                       ifelse(grepl(pattern = "rdive", col_main_street), "rd",
                       col_to_street))))))))

)

#write.csv(Complete_Dataset_Final_Copy_v4.0, "Complete_Dataset_Final_Copy_v4.1.csv")

Complete_Dataset_Final_Copy_v5.0 <- Complete_Dataset_Final_Copy_v4.0 %>%
mutate(

  test_01 = sub("ramp to\\s(.*)", "\\1", col_at_3),
  test_02 = sub("to\\s(.*)", "\\1", test_01),
  
  test_03 = ifelse(grepl(pattern = "from", test_02), "NA", test_02)
)

#write.csv(Complete_Dataset_Final_Copy_v5.0, "Complete_Dataset_Final_Copy_v5.0.csv")


Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("southbound", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("northbound", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("eastbound", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("westbound", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("south bound", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("sb", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("nb", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("eb", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("wb", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("north", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("don valley parkway south", "bayview/bloor", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("don mills south", "don mills rd", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("islington south", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("bayview south", "bayview/bloor", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("lake shore blvd west at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("lake shore blvd w at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("lake shore blvd at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("lake shore at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("lake shore w at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("lake shore  at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("lake shore  humber", "", Complete_Dataset_Final_Copy_v5.0$test_03)

Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("gardiner at collectors", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("gardiner w at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("gardiner at", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("east of", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("west of", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("and", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("exit", "", Complete_Dataset_Final_Copy_v5.0$test_03)
Complete_Dataset_Final_Copy_v5.0$test_03 <- gsub("on  don valley parkway", "don valley parkway", Complete_Dataset_Final_Copy_v5.0$test_03)


#write.csv(Complete_Dataset_Final_Copy_v5.0, "Complete_Dataset_Final_Copy_v5.1.csv")

#save final columns
Complete_Dataset_Final_Copy_v6.0 <- Complete_Dataset_Final_Copy_v5.0 %>%
  mutate(
    
    main_street_revised = temp_main_street_final,
    
    at_street_revised = ifelse(!is.na(temp_at_street_final), temp_at_street_final,
                         test_03),
    
    from_street_revised = temp_from_street_final,
    
    to_street_revised = temp_to_street_final
    
    
  )

#write.csv(Complete_Dataset_Final_Copy_v6.0, "Complete_Dataset_Final_Copy_v6.0a.csv")

#Extract only fields with street names
Complete_Dataset_Final_Copy_v6.1 <- Complete_Dataset_Final_Copy_v6.0[28:31]

#Export csv file with v2.0 to include rows with direction, ramp, collector/express
Complete_Dataset_Final_Copy_v7.00 <- bind_cols(Complete_Dataset_Final_Copy_v2.0,Complete_Dataset_Final_Copy_v6.1)

write.csv(Complete_Dataset_Final_Copy_v7.00, "Complete_Dataset_Final_Copy_v7.001.csv")


#############################################################################
#   Merge existing and new fields containing information about street names #
#############################################################################

#Create a new command to combine the fields with MainStreet, AtStreet, FromStreet, ToStreet

Complete_Dataset_Final_Copy_v8.00 <- Complete_Dataset_Final_Copy_v7.00 %>%
mutate(

main_street_final = ifelse(!is.na(MainStreet), MainStreet,
                    ifelse(!is.na(main_street_revised), main_street_revised, NA)),
                  

at_street_final = ifelse(!is.na(AtStreet), AtStreet,
                  ifelse(!is.na(at_street_revised), at_street_revised, NA)),

from_street_final = ifelse(!is.na(FromStreet), FromStreet,
                    ifelse(!is.na(from_street_revised), from_street_revised,
                    NA)),

to_street_final = ifelse(!is.na(ToStreet), ToStreet,
                  ifelse(!is.na(to_street_revised), to_street_revised,
                  NA)),

Incident_Lattitude = I_Lattitude,
Incident_Longitude = I_Longitude

)

#Clean at_street_final column
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvp nb ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvp sb ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvb n ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvp s ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvp ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("sb ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvp 401", "don valley parkway", Complete_Dataset_Final_Copy_v8.00$at_street_final)

Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("hwy 401 e ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy w ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy wb", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy w", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy eb", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy wb", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy collector eb ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy collector e", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardiner expy ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("gardner", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("f g gardiner c e", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("f g gardiner c w", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("f g gardiner xy w", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$at_street_final)

Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("lake shore blvd w browns ln ramp", "browns line", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("lake shore blvd w ramp", "lake shore blvd w", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("spadina ave ramp", "spadina ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("nb ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("bayview ave nb", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("don mills rd nb", "don mills rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("don mills rd sb", "don mills rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("don valley parkway nb", "don valley parkway", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("don valley parkway sb", "don valley parkway", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("horner ave nb", "horner ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("islington ave nb", "islington ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("islington ave sb", "islington ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("kipling ave sb", "kipling ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("kipling ave nb", "kipling ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("spadina ave nb", "spadina ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("spadina ave sb", "spadina ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("wr allen rd nb", "allen rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("eb", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("wb", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("ramp", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)

Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("brown line", "browns line", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("collector n", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("express w", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("collector", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvp nb", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("dvp n", "", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("don valley parkway n", "don valley parkway", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("don valley parkway s", "don valley parkway", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("don mills rd e", "don mills rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("hwy 404 steeles ave e woodbine ave", "hwy 404/woodbine ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("hwy 404 nb", "hwy 404", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("jameson ave lake shore blvd e", "jameson ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("lawrence ave bayview ave", "lawrence ave e", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("lakeshore ave", "lake shore blvd e", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("newfoundland dr", "newfoundland rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("prince edwardviaduct", "bloor viaduct", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("sherbourne lake shore", "sherbourne", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("sheppardave", "sheppard ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("stachan", "strachan", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("stdennis", "st dennis dr", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("stdium", "stadium", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("spadina ave lake shore blvd w", "spadina ave", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("park side", "parkside dr", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("w r allen rd s", "allen rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("w r allen rd", "allen rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("wr allen rd", "allen rd", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("browns ln nb", "browns line", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("kipiling", "kipling", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("eastrn", "eastern", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("queen ste", "queen st e", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("leaside bdge", "leaside bridge", Complete_Dataset_Final_Copy_v8.00$at_street_final)
Complete_Dataset_Final_Copy_v8.00$at_street_final <- gsub("bayview ave bloor st", "bayview/bloor", Complete_Dataset_Final_Copy_v8.00$at_street_final)

#clean main_street_final
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("dvp nb ramp", "", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("gardiner expy wb", "", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("gardiner expy e spadina ave ramp", "spadina ave", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("gardiner expy collector wb", "", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("queensway e gardiner ramp", "queensway", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("spadina ave sb lake shore blvd w ramp", "spadina ave", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("don mills rd nb", "don mills rd", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("don mills rd sb", "don mills rd", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("don valley parkway nb", "don valley parkway", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("islington ave sb", "islington ave", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("wr allen rd nb", "allen rd",Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("wr allen rd", "allen rd", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("bayview ave / bloor st e", "bayview/bloor", Complete_Dataset_Final_Copy_v8.00$main_street_final)
Complete_Dataset_Final_Copy_v8.00$main_street_final <- gsub("bayview ave bloor st", "bayview/bloor", Complete_Dataset_Final_Copy_v8.00$main_street_final)

#clean from_street_final
Complete_Dataset_Final_Copy_v8.00$from_street_final <- gsub("gardner expressway", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$from_street_final) 

#clean to_street_final
Complete_Dataset_Final_Copy_v8.00$to_street_final <- gsub("gardner expressway", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$to_street_final)
Complete_Dataset_Final_Copy_v8.00$to_street_final <- gsub("jarvis exit", "jarvis st", Complete_Dataset_Final_Copy_v8.00$to_street_final)
Complete_Dataset_Final_Copy_v8.00$to_street_final <- gsub("gardiner expy eb", "gardiner expy", Complete_Dataset_Final_Copy_v8.00$to_street_final)

write.csv(Complete_Dataset_Final_Copy_v8.00, "2016 04 06 Complete_Dataset_Final_Copy_v8.00.csv")






























