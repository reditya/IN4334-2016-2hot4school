-- helmiriawan@student.tudelft.nl
--
-- Definition:
-- accepted				-> merged or got at least two +2 votes
-- rejected				-> no +2 vote with at least one -2 vote
-- time					-> the difference between created time and granted time of second +2 vote (accepted with at least two +2) or between created time and granted time of first +2 vote (accepted with only one +2 or rejected)
-- reviewer activity	-> total number of previous review (range between -2 and +2)


--------------------------------------------------------------------------------------------------------------------------------
--- Preparation
--------------------------------------------------------------------------------------------------------------------------------


-- Clean up the data
-- Remove these:
-- 	- double quotes
--	- records with incomplete data
-- 	- double/triple comma in the end of the row


-- Create table approval
CREATE TABLE HR.approval (
	id			VARCHAR2(100),
	branch		VARCHAR2(50),
	createdOn	INTEGER,
	r_email		VARCHAR2(200),
	r_name		VARCHAR2(200),
	r_username	VARCHAR2(200),
	r_grantedOn	INTEGER,
	r_type		VARCHAR2(50),
	r_value		INTEGER,
	s_email		VARCHAR2(200),
	s_name		VARCHAR2(200),
	s_username	VARCHAR2(200),
	lastUpdated	INTEGER,
	project		VARCHAR2(200),
	status		VARCHAR2(50)
);


-- Create index
CREATE INDEX identifier ON approval (id, branch, project); 
CREATE INDEX idx_createdOn ON approval (createdOn);
CREATE INDEX idx_r_grantedOn ON approval (r_grantedOn);
CREATE INDEX idx_r_email ON approval (r_email);


-- Import the csv to table


-- Clean up data
delete from 
	approval 
where 
	status is null;
commit;


--------------------------------------------------------------------------------------------------------------------------------
--- Data extraction
--------------------------------------------------------------------------------------------------------------------------------


-- core-reviewer activity
create table 
	reviewer_activity 
as(
	select
		patch,
		max(case when r_rank = 1 then r_email end) as r1_email,
		/*max(case when r_rank = 1 then r_grantedon end) as r1_grantedon,*/
		max(case when r_rank = 1 then r_activity end) as r1_activity,
		max(case when r_rank = 2 then r_email end) as r2_email,
		/*max(case when r_rank = 2 then r_grantedon end) as r2_grantedon,*/
		max(case when r_rank = 2 then r_activity end) as r2_activity
	from (
		select
			a.patch,
			a.r_email,
			a.r_grantedon,
			coalesce(b.r_activity, 0) r_activity,
			rank() over (partition by a.patch order by a.r_grantedon, b.r_activity) as r_rank
		from (
			select													-- all review
				id || '|' || branch || '|' || project as patch,
				r_email,
				r_grantedon
			from
				approval
			where
				lower(r_type) like 'code-review'
				and (r_value > 1 or r_value < -1)			
		) a
		left join (
			select													-- all reviewer activity per review
				a.id || '|' || a.branch || '|' || a.project as patch,
				a.r_email,
				a.r_grantedon,
				count(distinct(b.id || '|' || b.branch || '|' || b.project)) as r_activity
			from
				approval a
			left join
				approval b
				on 
					b.r_email = a.r_email
					and b.r_grantedon < a.r_grantedon
			where
				lower(a.r_type) like 'code-review'
				and (a.r_value > 1 or a.r_value < -1)
				and lower(b.r_type) like 'code-review'
			group by
				a.id,
				a.branch,
				a.project,
				a.r_email,
				a.r_grantedon
		) b
			on a.patch = b.patch
			and a.r_email = b.r_email
			and a.r_grantedon = b.r_grantedon
		order by
			a.r_grantedon
	)
	group by
		patch
	/*order by
		patch*/
);


-- time and positivity
create table 
	time_positivity
as(
	select
		a.patch,
		b.createdon,
		b.p_createdon,
		/*case
			when a.positivity = 1 and c.patch is not null then c.r_email
			when a.positivity = 1 and b.patch is not null then b.r_email
			when a.positivity = -1 and b.patch is not null then b.r_email
		end r_email,*/
		case
			when a.positivity = 1 and c.patch is not null then c.r_grantedon	
			when a.positivity = 1 and b.patch is not null then b.r_grantedon	
			when a.positivity = -1 and b.patch is not null then b.r_grantedon	
		end r_grantedon,
		case
			when a.positivity = 1 and c.patch is not null then c.p_r_grantedon
			when a.positivity = 1 and b.patch is not null then b.p_r_grantedon
			when a.positivity = -1 and b.patch is not null then b.p_r_grantedon
		end p_r_grantedon,
		case
			when a.positivity = 1 and c.patch is not null then c.timeinsecond	-- measured based on second review
			when a.positivity = 1 and b.patch is not null then b.timeinsecond	-- measured based on first review
			when a.positivity = -1 and b.patch is not null then b.timeinsecond	-- measured based on first review
		end timeinsecond,
		a.positivity
	from (
		select											-- positivity
		  patch,
		  case
			when lower(status) like 'merged' then 1
			when unique_review = 1 and n_positive > 1 then 1
			when unique_review = 1 and n_negative > 0 then -1
		  end as positivity
		from (
		  select
			patch,
			status,
			count(distinct(r_value)) as unique_review,
			sum(case when r_value = 2 then cnt end) as n_positive,
			sum(case when r_value = -2 then cnt end) as n_negative,
			sum(cnt) as n_total
		  from (
			select
			  id || '|' || branch || '|' || project as patch,
			  status,
			  r_value,
			  count(*) as cnt
			from
			  approval
			where
			  lower(r_type) like 'code-review'
			  and (r_value > 1 or r_value < -1)
			group by
			  id,
			  branch,
			  project,
			  status,
			  r_value
			order by
			  id,
			  branch,
			  project,
			  r_value
		  )
		  group by
			patch,
			status
		  order by
			patch
		)
	) a
	left join (												-- required time of first review
		select
			patch,
			createdon,
			--r_email,
			r_grantedon,
			p_createdon,
			p_r_grantedon,
			round((p_r_grantedon - p_createdon)*24*60*60) as timeinsecond
		from (
			select
				id || '|' || branch || '|' || project as patch,
				createdon,
				--r_email,
				r_grantedon,
				to_date('1970-01-01','yyyy-mm-dd hh24:mi:ss') + numtodsinterval(createdon,'second') p_createdon,
				to_date('1970-01-01','yyyy-mm-dd hh24:mi:ss') + numtodsinterval(r_grantedon,'second') p_r_grantedon,
				rank() over (partition by id, branch, project order by r_grantedon) r_rank
			from
				approval
			where
				lower(r_type) like 'code-review'
				and (r_value < -1 or r_value > 1)
			group by
				id,
				branch,
				project,
				createdon,
				--r_email,
				r_grantedon
			order by
				createdon,
				r_grantedon
		)
		where
			r_rank = 1
	) b
		on a.patch = b.patch
	left join (												-- required time of second review
		select
			patch,
			createdon,
			--r_email,
			r_grantedon,
			p_createdon,
			p_r_grantedon,
			round((p_r_grantedon - p_createdon)*24*60*60) as timeinsecond
		from (
			select
				id || '|' || branch || '|' || project as patch,
				createdon,
				--r_email,
				r_grantedon,
				to_date('1970-01-01','yyyy-mm-dd hh24:mi:ss') + numtodsinterval(createdon,'second') p_createdon,
				to_date('1970-01-01','yyyy-mm-dd hh24:mi:ss') + numtodsinterval(r_grantedon,'second') p_r_grantedon,
				rank() over (partition by id, branch, project order by r_grantedon) r_rank
			from
				approval
			where
				lower(r_type) like 'code-review'
				and (r_value < -1 or r_value > 1)
			group by
				id,
				branch,
				project,
				createdon,
				--r_email,
				r_grantedon
			order by
				createdon,
				r_grantedon
		)
		where
			r_rank = 2
	) c
		on a.patch = c.patch
	/*order by
		b.createdon,
		b.r_grantedon*/
);


-- Load the data to pipe delimited file, then change the patch header to ID, BRANCH, and PROJECT