ALTER TABLE incidents.mvp_2016 ADD COLUMN geom GEOMETRY(POINT, 4326);
UPDATE incidents.mvp_2016 SET geom = ST_SetSRID(ST_MakePoint("I_Longitude","I_Lattitude"),4326);