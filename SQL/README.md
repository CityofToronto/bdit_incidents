# SQL

All work is done inside the incidents schema. There exist the following tables:
- incidents.bt → bluetooth to TMC reference table
- incidents.mvp_temp → all incident data (mvp = minimum viable product)
- incidents.mvp_subset → 20 rows of mvp_temp for experimenting
- incidents.tmc → copy of gis.inrix_tmc_tor

## Files:

### create_bt_table_len_column.sql
- creates a column in the `bt` table corresponding to the length of each TMC in metres

### create_bt_table.sql
- Creates the `bt` table as a subset of `incidents.tmc` where `roadtype = 'HWY'`
- Further processed in QGIS to remove MTO highways

### create_tmc_column_works.sql
- Assigns a value to the tmc column
- Finds the segment in `incidents.tmc` table that is closest to each incident and going in the same direction

### create_mvp_geom_column.sql
- Takes lat/long and converts to postgis point in new column

### correct_direction.sql
- Removes any irregularities from the `eventdirection` column for the purpose of tagging incidents to TMCs

### raph_query.sql
- ?






