ALTER TABLE incidents.mvp_2016 ADD COLUMN "Delay" double precision;
UPDATE incidents.mvp_2016
SET "Delay" = (SELECT "Delay" 
	FROM incidents.py_out 
	WHERE incidents.mvp_2016."ID" = incidents.py_out."ID");