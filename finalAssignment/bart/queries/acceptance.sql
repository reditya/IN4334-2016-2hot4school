INSERT INTO patch_writer
SELECT p.id, p.branch, p.project, p.createdOn, p.s_email, p.status, p.num, (SELECT count(*) FROM patch_writer p2 WHERE p2.createdOn < p.createdOn AND p2.s_email = p.s_email AND p.status = "MERGED")
FROM patch_writer p
WHERE NOT p.status = "NEW";

DELETE FROM patch_writer
WHERE accepted IS NULL
