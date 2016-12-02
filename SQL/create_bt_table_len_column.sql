UPDATE incidents.bt set len = ST_LENGTH(ST_TRANSFORM(geom,26917))
