INSERT INTO patch_writer
SELECT DISTINCT id, branch, project, createdOn, s_email, status, null, null
FROM approval;

DELETE FROM patch_writer
WHERE s_email is NULL or s_email = "NA";