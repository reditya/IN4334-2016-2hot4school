DELETE FROM approval_new
WHERE true;

INSERT INTO approval_new
SELECT a.* FROM approval a, filtered_data f
WHERE a.id=f.id AND a.branch = f.branch AND a.project = f.project