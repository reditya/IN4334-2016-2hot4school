INSERT INTO patch_writer
SELECT p.id, p.branch, p.project, p.createdOn, p.s_email, p.status, (SELECT count(*) FROM patch_writer p2 WHERE p2.createdOn < p.createdOn AND p2.s_email = p.s_email), null
FROM patch_writer p;

DELETE FROM patch_writer
WHERE num IS NULL
