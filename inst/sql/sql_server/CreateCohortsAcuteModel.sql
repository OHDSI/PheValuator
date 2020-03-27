/************************************************************************
@file CreateCohortsAcuteModel.sql
************************************************************************/

{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_table = 'cohort'
{DEFAULT @x_spec_cohort = 0 }
{DEFAULT @tempDB = "scratch.dbo" }
{DEFAULT @test_cohort = "test_cohort" }
{DEFAULT @ageLimit = 0}
{DEFAULT @upperAgeLimit = 120}
{DEFAULT @gender = c(8507, 8532)}
{DEFAULT @startDate = '19000101' }
{DEFAULT @endDate = '21000101' }
{DEFAULT @baseSampleSize = 150000 }
{DEFAULT @xSpecSampleSize = 1500 }
{DEFAULT @mainPopnCohort = 0 }
{DEFAULT @mainPopnCohortStartDay = 0 }
{DEFAULT @mainPopnCohortEndDay = 0 }
{DEFAULT @exclCohort = 0 }
{DEFAULT @visitLength = 1 }

IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;

select *
into #cohort_person
from (select co.*, p.*,
	  row_number() over (order by NewId()) rn
	from @cohort_database_schema.@cohort_database_table co
	join @cdm_database_schema.person p
	  on co.subject_id = p.person_id
		and  year(COHORT_START_DATE) - year_of_birth >= @ageLimit
		and year(COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
		and gender_concept_id in (@gender)
	join @cdm_database_schema.observation_period o
	  on co.subject_id = o.person_id
	    and co.COHORT_START_DATE >= o.observation_period_start_date
		and co.COHORT_START_DATE <= o.observation_period_end_date
	where cohort_definition_id = @x_spec_cohort
	  and o.observation_period_start_date >= cast('@startDate' AS DATE)
	  and o.observation_period_start_date <= cast('@endDate' AS DATE)) pos
;

IF OBJECT_ID('tempdb..#eligibles', 'U') IS NOT NULL
	DROP TABLE #eligibles;

SELECT visit_occurrence.person_id,
	minObsStart,
	minObsEnd,
	count(visit_occurrence_id) countVis,
	min(visit_start_date) minDate
INTO #eligibles
FROM @cdm_database_schema.visit_occurrence
JOIN (
	SELECT person_id,
		min(observation_period_start_date) minObsStart,
		min(observation_period_end_date) minObsEnd
	FROM @cdm_database_schema.observation_period
	GROUP BY person_id
	) obs
	ON visit_occurrence.person_id = obs.person_id
		AND visit_start_date >= dateadd(d, 365, minObsStart)
		AND visit_start_date <= dateadd(d, - 30, minObsEnd)
		AND visit_concept_id IN (9201) --in-patient only
		AND datediff(day, visit_start_date, visit_end_date) >= @visitLength
GROUP BY visit_occurrence.person_id,
	minObsStart,
	minObsEnd
HAVING minObsStart >= cast('@startDate' AS DATE)
	AND minObsStart <= cast('@endDate' AS DATE);


IF OBJECT_ID('@tempDB.@test_cohort', 'U') IS NOT NULL
	DROP TABLE @tempDB.@test_cohort;

CREATE TABLE @tempDB.@test_cohort (
  cohort_definition_id bigint NOT NULL,
  subject_id bigint NOT NULL,
  cohort_start_date date,
  cohort_end_date date);

insert into @tempDB.@test_cohort (COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE)
 (select 0 as COHORT_DEFINITION_ID, person_id as SUBJECT_ID, dateadd(day, 0, visit_start_date) COHORT_START_DATE,
            dateadd(day, 1, visit_start_date) COHORT_END_DATE
      from (select
				{@mainPopnCohort == 0} ? {
					v.person_id, minDate as visit_start_date,
						row_number() over (order by NewId()) rn
					from @cdm_database_schema.visit_occurrence v
					join @cdm_database_schema.person p
					  on v.person_id = p.person_id
						and year(visit_start_date) - year_of_birth >= @ageLimit
						and year(visit_start_date) - year_of_birth <= @upperAgeLimit
						and gender_concept_id in (@gender)
					join #eligibles v5 --include only subjects with a visit in their record and within date range
						on v.person_id = v5.person_id
						  and v5.minDate = v.visit_start_date
					where 1 = 1
						{@exclCohort != 0} ? {and v.person_id not in (
													select subject_id
													from @cohort_database_schema.@cohort_database_table
													where COHORT_DEFINITION_ID = @exclCohort)}
					group by v.person_id, minDate)}
				{@mainPopnCohort != 0} ? {
					co.subject_id as person_id, v.visit_start_date,
						row_number() over (order by NewId()) rn
					from @cohort_database_schema.@cohort_database_table co
					join @cdm_database_schema.visit_occurrence v
					  on v.person_id = co.subject_id
					    and v.visit_concept_id in (9201) --in-patient only
					    and v.visit_start_date >= dateadd(day, @mainPopnCohortStartDay, co.COHORT_START_DATE)
					    and v.visit_start_date <= dateadd(day, @mainPopnCohortEndDay, co.COHORT_START_DATE)
					    and v.visit_start_date >= cast('@startDate' AS DATE)
		          and v.visit_start_date <= cast('@endDate' AS DATE)
					join @cdm_database_schema.person p
					  on co.subject_id = p.person_id
						and  year(co.COHORT_START_DATE) - year_of_birth >= @ageLimit
						and year(co.COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
						and gender_concept_id in (@gender)
					join #eligibles v5 --include only subjects with a visit in their record and within date range
						on co.subject_id = v5.person_id
					where co.cohort_definition_id = @mainPopnCohort
						{@exclCohort != 0} ? {and co.subject_id not in (
													select subject_id
													from @cohort_database_schema.@cohort_database_table
													where COHORT_DEFINITION_ID = @exclCohort)}
						)} negs
      where rn <= cast('@baseSampleSize' as bigint)
    union
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
      );
