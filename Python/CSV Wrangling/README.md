#CSV Wrangling

##clean_csv.py

Functions are interperated then run in the shell interactively

###def clean():
- Main function, calls all other helper function
- Forces all characters in df['EventDescripion'], df['OffsetDirection'], and df['AtStreet'] to lower case for easier string parsing
- Replaces any fields in the same columns that are np.nan with a blank string
- Creates df['Respondents'], df['Offset'], and df['Street2'] columns based on info in df['EventDescription'] using corresponding helper functions
- Returns a float corresponding to the percent of rows that were successfully parsed

###def respondents():
- Returns type of emergency services were at the scene

###def offset():
- Returns df['OffsetDirection'] if it exists
- Otherwise, extracts offset information from 

###def atStreeet():
- Returns df['AtStreet'] if it exists
- Otherwise:
  - Use str.split() on the substring of df['EventDescription'] after the df['offset'], this returns the list of words after the offset
  - Take the first two elements of this list, will either be ['roadname','type'] (ex: ['bay','st']) or ['road', 'name'] (ex: ['york', 'mills']), doesn't matter we don't need the strings to be exact for geotagging (more on this later)
  - Concatenate these two list elements into one string and drop the semicolon that often occurs at the end of the second element

###def dropDir():

###def levCol():

###def latLong():


