{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_table = 'cohort'
{DEFAULT @work_database_schema = 'CDM_SIM' }
{DEFAULT @x_spec_cohort = 0 }
{DEFAULT @prevalenceCohortId = 0 }
{DEFAULT @tempDB = "scratch.dbo" }
{DEFAULT @test_cohort = "test_cohort" }
{DEFAULT @ageLimit = 0}
{DEFAULT @upperAgeLimit = 120}
{DEFAULT @gender = c(8507, 8532)}
{DEFAULT @race = 0}
{DEFAULT @ethnicity = 0}
{DEFAULT @startDate = '19000101' }
{DEFAULT @endDate = '21000101' }
{DEFAULT @baseSampleSize = 150000 }
{DEFAULT @xSpecSampleSize = 1500 }
{DEFAULT @mainPopnCohort = 0 }
{DEFAULT @mainPopnCohortStartDay = 0 }
{DEFAULT @mainPopnCohortEndDay = 0 }
{DEFAULT @exclCohort = 0 }
{DEFAULT @visitLength = 0 }
{DEFAULT @visitType = c(9201) }
{DEFAULT @firstCut = FALSE }

DROP TABLE IF EXISTS #persons;
DROP TABLE IF EXISTS #visits;
DROP TABLE IF EXISTS #finalCohort;

{@mainPopnCohort == 0} ? { -- create cohort using prevalence cohort and visit table - else use population cohort specified
with #persons as ( --subset a random set of subjects
  select *
  from (
    select person_id, row_number() over (order by random()) rn
    from @cdm_database_schema.person)
  order by rn
  limit 10 * @baseSampleSize),
#visits as ( --and a random visit from each subject selected
select distinct v.person_id, first_value(visit_start_date)
over (partition by v.person_id order by RANDOM() rows between unbounded preceding and unbounded following) visit_start_date
from @cdm_database_schema.visit_occurrence v
join @cdm_database_schema.observation_period o
  on v.person_id = o.person_id
    and v.visit_start_date >= o.observation_period_start_date
    and dateadd(d, 365, v.visit_start_date) <= o.observation_period_end_date
where v.person_id in (
  select person_id
  from #persons))
select *
into #finalCohort
from (
  select @prevalenceCohortId as cohort_definition_id, person_id as subject_id, visit_start_date as cohort_start_date,
    dateadd(day, 1, visit_start_date) as cohort_end_date
  from #visits
  where person_id not in ( --noncases from those not in prevalence cohort
    select subject_id
    from @cohort_database_schema.@cohort_database_table
    where cohort_definition_id = @prevalenceCohortId)
  union
  select c.* --cases from those also in prevalence cohort
  from #visits v
  join @cohort_database_schema.@cohort_database_table c
    on v.person_id = c.subject_id
  where c.cohort_definition_id = @prevalenceCohortId
)
;}

DROP TABLE IF EXISTS #cohort_person;

SELECT *
INTO #cohort_person
FROM (
	SELECT co.*,
		p.*,
		row_number() OVER (
			ORDER BY NewId()
			) rn
	from @cohort_database_schema.@cohort_database_table co
	JOIN @cdm_database_schema.person p
		ON co.subject_id = p.person_id
			AND year(COHORT_START_DATE) - year_of_birth >= @ageLimit
			AND year(COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
			AND gender_concept_id IN (@gender)
			{@race != 0} ? {AND race_concept_id in (@race)}
      {@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
	WHERE cohort_definition_id = @x_spec_cohort
		AND co.COHORT_START_DATE >= cast('@startDate' AS DATE)
		AND co.COHORT_START_DATE <= cast('@endDate' AS DATE)
	) pos;


DROP TABLE IF EXISTS @work_database_schema.@test_cohort;

select CAST(0 AS BIGINT) as COHORT_DEFINITION_ID, person_id as SUBJECT_ID,
	dateadd(day, 0, visit_start_date) COHORT_START_DATE,
	dateadd(day, 1, visit_start_date) COHORT_END_DATE
INTO @work_database_schema.@test_cohort
from (select co.subject_id as person_id, FIRST_VALUE(v.visit_start_date) OVER (PARTITION BY v.person_id ORDER BY NewId()) visit_start_date,
					row_number() over (order by NewId()) rn
    	from {@mainPopnCohort == 0} ? {#finalCohort}
    	     {@mainPopnCohort != 0} ? {@cohort_database_schema.@cohort_database_table} co
				join @cdm_database_schema.visit_occurrence v
				  on v.person_id = co.subject_id
					and v.visit_concept_id in (@visitType)
					and v.visit_start_date >= dateadd(day, @mainPopnCohortStartDay, co.COHORT_START_DATE)
					and v.visit_start_date <= dateadd(day, @mainPopnCohortEndDay, co.COHORT_START_DATE)
					and v.visit_start_date >= cast('@startDate' AS DATE)
			  and v.visit_start_date <= cast('@endDate' AS DATE)
		   join (
		  select person_id,
			datediff(day, min(observation_period_start_date), min(observation_period_end_date)) lenPd,
			min(observation_period_start_date) observation_period_start_date,
			min(observation_period_end_date) observation_period_end_date,
			count(observation_period_id) cntPd
		  from @cdm_database_schema.observation_period
		  group by person_id) obs2
		  on v.person_id = obs2.person_id
			and v.visit_start_date >= obs2.observation_period_start_date
			and v.visit_start_date <= obs2.observation_period_end_date
			and lenPd >= 730
			and cntPd = 1
				join @cdm_database_schema.person p
				  on co.subject_id = p.person_id
					and  year(co.COHORT_START_DATE) - year_of_birth >= @ageLimit
					and year(co.COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
					and gender_concept_id in (@gender)
					{@race != 0} ? {AND race_concept_id in (@race)}
		{@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
					{@exclCohort != 0} ? {
	  left join @cohort_database_schema.@cohort_database_table excl
		on v.person_id = excl.subject_id
		  and v.visit_start_date = excl.COHORT_START_DATE
	  }
				where co.cohort_definition_id =
					{@mainPopnCohort == 0} ? {@prevalenceCohortId}
	        {@mainPopnCohort != 0} ? {@mainPopnCohort}
{@exclCohort != 0} ? {
					and excl.subject_id is NULL
}
) negs
  where rn <= cast('@baseSampleSize' as bigint)
union --add in some xSpec subjects due to PLP constraint - will be ignored in analysis
  select 0 as COHORT_DEFINITION_ID, SUBJECT_ID, cp.COHORT_START_DATE COHORT_START_DATE,
	dateadd(day, 1, cp.COHORT_START_DATE) COHORT_END_DATE
  from #cohort_person cp
  join @cdm_database_schema.observation_period o
	on cp.SUBJECT_ID = o.person_id
	  and cp.COHORT_START_DATE >= o.observation_period_start_date
	  and cp.COHORT_START_DATE <= o.observation_period_end_date
  where rn <= @xSpecSampleSize
  union
  select @x_spec_cohort as COHORT_DEFINITION_ID, SUBJECT_ID, cp.COHORT_START_DATE COHORT_START_DATE,
	dateadd(day, 1, cp.COHORT_START_DATE) COHORT_END_DATE
  from #cohort_person cp
  join @cdm_database_schema.observation_period o
	on cp.SUBJECT_ID = o.person_id
	  and cp.COHORT_START_DATE >= o.observation_period_start_date
	  and cp.COHORT_START_DATE <= o.observation_period_end_date
  where rn <= @xSpecSampleSize
  ;

TRUNCATE TABLE #cohort_person;
DROP TABLE #cohort_person;

