DELETE FROM rel_reviews WHERE true;

INSERT INTO rel_reviews
SELECT DISTINCT p.id, p.branch, p.project, p.createdOn, p.r_email, p.r_value, MAX(p.r_grantedOn)
FROM approval p
WHERE
	NOT p.r_email = "NA"
	AND p.r_type = "Code-Review"
	AND (p.r_value > 1 OR p.r_value < -1)
GROUP BY p.id, p.branch, p.project, p.r_email