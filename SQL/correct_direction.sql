UPDATE incidents.mvp_temp SET eventdirection =
CASE WHEN eventdirection LIKE '%NB%' THEN 'NB'
     WHEN eventdirection LIKE '%SB%' THEN 'SB'
     WHEN eventdirection LIKE '%EB%' THEN 'EB'
     WHEN eventdirection LIKE '%WB%' THEN 'WB' 
     ELSE 'NA' END;