UPDATE incidents.mvp_temp SET tmc = a.tmc
FROM(
SELECT
DISTINCT ON (incidents.mvp_temp.id) incidents.mvp_temp.id, incidents.tmc.tmc
FROM incidents.tmc, incidents.mvp_temp
WHERE incidents.tmc.direction_corrected = incidents.mvp_temp.eventdirection
ORDER BY incidents.mvp_temp.id, ST_DISTANCE(incidents.mvp_temp.geom,incidents.tmc.geom)) as a
WHERE a.id = incidents.mvp_temp.id;

