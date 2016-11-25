ALTER TABLE incidents.mvp_subset ADD COLUMN geom GEOMETRY(POINT, 4326);
UPDATE incidents.mvp_subset SET geom = ST_SetSRID(ST_MakePoint(i_longitude,i_lattitude),4326);