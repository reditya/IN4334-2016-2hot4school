DELETE FROM rel_reviews WHERE true;

INSERT INTO rel_reviews
SELECT DISTINCT p.id, p.branch, p.project, p.createdOn, p.r_email, p.r_value, MAX(p.r_grantedOn)
FROM approval_new p
GROUP BY p.id, p.branch, p.project, p.r_email;

DELETE FROM reviewqueue
WHERE true;

INSERT INTO reviewqueue
SELECT p.id, p.branch, p.project, p.r_email, p.createdOn, p.r_grantedOn, (SELECT count(*)
			FROM rel_reviews pp
			WHERE p.r_email = pp.r_email
			AND pp.createdOn < p.createdOn
			AND pp.r_grantedOn > p.createdOn)
FROM rel_reviews p;