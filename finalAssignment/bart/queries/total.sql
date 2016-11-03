DELETE FROM patch_writer WHERE true;

INSERT INTO patch_writer
SELECT DISTINCT id, branch, project, createdOn, s_email, status, null, null
FROM approval_new;

DELETE FROM patch_writer
WHERE s_email is NULL or s_email = "NA";

INSERT INTO patch_writer
SELECT p.id, p.branch, p.project, p.createdOn, p.s_email, p.status, (SELECT count(*) FROM patch_writer p2 WHERE p2.createdOn < p.createdOn AND p2.s_email = p.s_email), null
FROM patch_writer p;

DELETE FROM patch_writer
WHERE num IS NULL;

INSERT INTO patch_writer
SELECT p.id, p.branch, p.project, p.createdOn, p.s_email, p.status, p.num, (SELECT count(*) FROM patch_writer p2 WHERE p2.createdOn < p.createdOn AND p2.s_email = p.s_email AND p2.status = "MERGED")
FROM patch_writer p;

DELETE FROM patch_writer
WHERE accepted IS NULL
