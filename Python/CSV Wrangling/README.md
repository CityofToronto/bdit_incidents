#CSV Wrangling

##clean_csv.py

Functions are interperated then run in the shell interactively

###`def clean(df):`
- Main function, calls all other helper function
- Forces all characters in `df['EventDescripion']`, `df['OffsetDirection']`, and `df['AtStreet']` to lower case for easier string parsing
- Replaces any fields in the same columns that are np.nan with a blank string
- Creates `df['Respondents']`, `df['Offset']`, and `df['Street2']` columns based on info in `df['EventDescription']` using corresponding helper functions
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
- Loops through `df['Lattitude']` and does nothing if there is a value there
- If there is no value:
  - 
- Room for improvement here as the current implementation is O(n) complexity and takes ~10h to run


