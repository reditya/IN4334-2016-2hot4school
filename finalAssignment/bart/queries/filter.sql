INSERT INTO approval_new
SELECT a.*
FROM approval a, filtered_data d
WHERE a.id = d.id AND
		a.branch = d.branch AND
		a.project = d.project