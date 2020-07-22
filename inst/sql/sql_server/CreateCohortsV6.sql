/************************************************************************
@file CreateCohorts.sql
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
{DEFAULT @visitLength = 0 }
{DEFAULT @visitType = c(9201) }
{DEFAULT @firstCut = FALSE }


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
	    and datediff(day, o.observation_period_start_date, co.COHORT_START_DATE) >= 365
	where cohort_definition_id = @x_spec_cohort
	  and co.COHORT_START_DATE >= cast('@startDate' AS DATE)
	  and co.COHORT_START_DATE <= cast('@endDate' AS DATE)) pos
;

IF OBJECT_ID('tempdb..#eligibles', 'U') IS NOT NULL
	DROP TABLE #eligibles;

select visit_occurrence.person_id,
    observation_period_start_date,
    observation_period_end_date
into #eligibles
from @cdm_database_schema.visit_occurrence
join (
  select person_id,
    datediff(day, min(observation_period_start_date), min(observation_period_end_date)) lenPd,
    min(observation_period_start_date) observation_period_start_date,
    min(observation_period_end_date) observation_period_end_date,
    count(observation_period_id) cntPd
  from @cdm_database_schema.observation_period
  group by person_id) obs
  on visit_occurrence.person_id = obs.person_id
    and lenPd >= 730
    and cntPd = 1
group by visit_occurrence.person_id,
        observation_period_start_date,
        observation_period_end_date
having observation_period_start_date >= cast('@startDate' AS DATE)
		and observation_period_end_date <= cast('@endDate' AS DATE);

IF OBJECT_ID('@tempDB.@test_cohort', 'U') IS NOT NULL
	DROP TABLE @tempDB.@test_cohort;

CREATE TABLE @tempDB.@test_cohort (
  cohort_definition_id bigint NOT NULL,
  subject_id bigint NOT NULL,
  cohort_start_date date,
  cohort_end_date date);

insert into @tempDB.@test_cohort (COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE)
 (select 0 as COHORT_DEFINITION_ID,
              person_id as SUBJECT_ID,
              dateadd(day, 0, observation_period_start_date) COHORT_START_DATE,
              dateadd(day, 1, observation_period_start_date) COHORT_END_DATE
      from (select
				{@mainPopnCohort == 0} ? {
					p.person_id,
					observation_period_start_date,
					row_number() over (order by NewId()) rn
					from @cdm_database_schema.person p
					join #eligibles v5 --include only subjects with a visit in their record and within date range
						on p.person_id = v5.person_id
						and year(observation_period_start_date) - year_of_birth >= @ageLimit
						and year(observation_period_start_date) - year_of_birth <= @upperAgeLimit
						and gender_concept_id in (@gender)
					where 1 = 1
						{@exclCohort != 0} ? {and p.person_id not in (
													select subject_id
													from @cohort_database_schema.@cohort_database_table
													where COHORT_DEFINITION_ID = @exclCohort)}
					)}
				{@mainPopnCohort != 0} ? {
					p.person_id, co.cohort_start_date observation_period_start_date,
						row_number() over (order by NewId()) rn
					from @cohort_database_schema.@cohort_database_table co
					join @cdm_database_schema.person p
					  on co.subject_id = p.person_id
					join #eligibles v5 --include only subjects with a visit in their record and within date range
						on co.subject_id = v5.person_id
						and co.cohort_start_date >= observation_period_start_date
						and co.cohort_start_date <= observation_period_end_date
					where co.cohort_definition_id = @mainPopnCohort
						and year(co.cohort_start_date) - year_of_birth >= @ageLimit
						and year(co.cohort_start_date) - year_of_birth <= @upperAgeLimit
						and gender_concept_id in (@gender)
						{@exclCohort != 0} ? {and co.subject_id not in (
													select subject_id
													from @cohort_database_schema.@cohort_database_table
													where COHORT_DEFINITION_ID = @exclCohort)}
						)} negs
      where rn <= cast('@baseSampleSize' as bigint)
    union
      select 0 as COHORT_DEFINITION_ID, SUBJECT_ID, o.observation_period_start_date COHORT_START_DATE,
        dateadd(day, 1, o.observation_period_start_date) COHORT_END_DATE
      from #cohort_person cp
      join @cdm_database_schema.observation_period o
        on cp.SUBJECT_ID = o.person_id
          and cp.COHORT_START_DATE >= o.observation_period_start_date
          and cp.COHORT_START_DATE <= o.observation_period_end_date
      where rn <= @xSpecSampleSize
      union
      select @x_spec_cohort as COHORT_DEFINITION_ID, SUBJECT_ID, o.observation_period_start_date COHORT_START_DATE,
        dateadd(day, 1, o.observation_period_start_date) COHORT_END_DATE
      from #cohort_person cp
      join @cdm_database_schema.observation_period o
        on cp.SUBJECT_ID = o.person_id
          and cp.COHORT_START_DATE >= o.observation_period_start_date
          and cp.COHORT_START_DATE <= o.observation_period_end_date
      where rn <= @xSpecSampleSize
      );
