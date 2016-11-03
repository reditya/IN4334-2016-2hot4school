DELETE FROM rqueue
WHERE true;

INSERT INTO rqueue
SELECT DISTINCT r.id, r.branch, r.project,
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r1 = rq.r_email),
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r2 = rq.r_email), 
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r3 = rq.r_email), 
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r4 = rq.r_email), 
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r5 = rq.r_email), 
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r6 = rq.r_email), 
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r7 = rq.r_email), 
	(SELECT rq.queue FROM reviewqueue rq WHERE rq.id = o.id AND rq.branch = o.branch AND o.project = rq.project AND o.r8 = rq.r_email)
FROM reviewqueue r, organization o 
WHERE r.id = o.id AND r.branch = o.branch AND o.project = r.project