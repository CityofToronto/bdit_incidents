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

###def atStreeet():

###def dropDir():

###def levCol():

###def latLong():


