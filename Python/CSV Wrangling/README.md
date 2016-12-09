#CSV Wrangling

Relevant orly cover to start: http://imgur.com/a/KMAJc

##clean_csv.py

Functions are interperated then run in the shell interactively

###`def clean(df):`
- Main function, calls all other helper function
- Forces all characters in `df['EventDescripion']`, `df['OffsetDirection']`, and `df['AtStreet']` to lower case for easier string parsing
- Replaces any fields in the same columns that are np.nan with a blank string
- Creates `df['Respondents']`, `df['Offset']`, and `df['Street2']` columns based on info in `df['EventDescription']` using corresponding helper functions
- Saves `df` as a csv file for easy evaluation of how successful information extraction was
- Returns a float corresponding to the percent of rows that were successfully parsed

###`def respondents(df):`
- Returns type of emergency services were at the scene

###`def offset(df):`
- Returns `df['OffsetDirection']` if it exists
- Otherwise, extracts offset information from 

###`def atStreeet(df):`
- Returns `df['AtStreet']` if it exists
- Otherwise:
  - Use `str.split()` on the substring of `df['EventDescription']` after the `df['offset']`, this returns the list of words after the offset
  - Take the first two elements of this list, will either be `['roadname','type']` (ex: `['bay','st']`) or `['road', 'name']` (ex: `['york', 'mills']`), doesn't matter we don't need the strings to be exact for geotagging (more on this later)
  - Concatenate these two list elements into one string and drop the semicolon that often occurs at the end of the second element

###`def dropDir(df):`
- Addresses the lack of consistency in road names, some contain directions and others don't
- `str.split()` is called on `df['Street2']`, if the last element of the resulting list is a cardinal direction it is dropped from the street name

###`def levCol(df,string):`
- returns the levenshtein ratio of every entry in `df['Street2']` and `string` as a `pd.Series`
- helper function for `latLong(df)`

###`def latLong(df):`
- Loops through `df['Lattitude']` 
- Does nothing if there is a value there
- If there is no value:
  - create a `df['Lev']` column with the `string` argument as `df['Street2']`
  - create a `dfFilt` which is a Dataframe that:
    - `df['Lev'] > 0.65'`
      - *Note: 0.65 was chosen through a slightly less than rigerous trial and error analysis* 
    - `np.isnan(df['I_Lattitude']) == False`
    - `df['Offset'] == df['Offset'][i]`
  - Set `df['I_Lattitude'][i]` and `df['I_Longitude'][i]` as a random pair from `dfFilt`
    
- Room for improvement here as the current implementation is O(n) complexity and takes ~10h to run

##clean_csv_part2.py

Just a continuation of `clean_csv.py`, I got a bit excited with the Lat/Long tagging and wanted to get that done 

###`def clean2(df)`
- Same as `clean()`, calls helper functions

###`def nLanes(df)`
- Extracts number of lanes blocked from `df['EventDescription']`
- Returns as type `int`, 99 implies all lanes, 89 ramp, 79 shoulder

###`def incType(df)`
- Categorizes incidents into maintenance, stopped, and collision

