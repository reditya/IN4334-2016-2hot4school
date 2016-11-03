SELECT p.id, p.branch, p.project, SUM(p.r_value > 0), SUM(p.r_value < 0), SUM(p.r_value > 1), SUM(p.r_value < -1), SUM(p.r_value), SUM(p.r_value > 0) > 0 && SUM(p.r_value < 0) > 0
FROM approval p
WHERE p.r_type = "Code-Review"
AND NOT p.r_email = "NA"
GROUP BY p.id, p.branch, p.project