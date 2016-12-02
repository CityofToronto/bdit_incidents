CREATE TABLE incidents.bt AS
(SELECT tmc, roadname, geom
 FROM incidents.tmc
 WHERE road_type = 'HWY')