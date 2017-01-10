UPDATE incidents.mvp_temp
SET mir = a.mir
FROM(
SELECT * 
FROM incidents.id_mir) AS a
WHERE a.id = incidents.mvp_temp.id; 