ALTER TABLE incidents.mvp_2016 ADD COLUMN tmc character varying;
UPDATE incidents.mvp_2016 SET tmc = a.tmc
FROM(
SELECT
DISTINCT ON (incidents.mvp_2016."ID") incidents.mvp_2016."ID", incidents.tmc.tmc
FROM incidents.tmc, incidents.mvp_2016
WHERE incidents.tmc.direction_corrected = incidents.mvp_2016."EventDirection"
ORDER BY incidents.mvp_2016."ID", ST_DISTANCE(incidents.mvp_2016.geom,incidents.tmc.geom)) as a
WHERE a."ID" = incidents.mvp_2016."ID";

