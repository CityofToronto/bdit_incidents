ALTER TABLE incidents.tmc
ALTER COLUMN geom type Geometry(MultiLinestring, 4326) USING geom::Geometry(MultiLinestring,4326)