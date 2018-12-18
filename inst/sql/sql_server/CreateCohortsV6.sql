/************************************************************************
@file CreateCohorts.sql
************************************************************************/

{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_table = 'cohort'
{DEFAULT @x_spec_cohort = 0 }
{DEFAULT @tempDB = "scratch.dbo" }
{DEFAULT @test_cohort = "test_cohort" }
{DEFAULT @ageLimit = 20}
{DEFAULT @upperAgeLimit = 120}
{DEFAULT @startDate = '19000101' }
{DEFAULT @endDate = '21000101' }
{DEFAULT @baseSampleSize = 150000 }
{DEFAULT @xSpecSampleSize = 1500 }
{DEFAULT @noise = 150 }
{DEFAULT @mainPopnCohort = 0 }
{DEFAULT @exclCohort = 0 }
{DEFAULT @lookback = 0 }


IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;

select *
into #cohort_person
from (select co.*, p.*,
	  row_number() over (order by ((co.subject_id*month(COHORT_START_DATE)) % 123)*((year(COHORT_START_DATE)*day(COHORT_START_DATE)) % 123)) rn
	from @cohort_database_schema.@cohort_database_table co
	join @cdm_database_schema.person p
	  on co.subject_id = p.person_id
		and  year(COHORT_START_DATE) - year_of_birth >= @ageLimit and year(COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
	join @cdm_database_schema.observation_period o
	  on co.subject_id = o.person_id
	    and co.COHORT_START_DATE between o.observation_period_start_date and o.observation_period_end_date
	    and datediff(day, o.observation_period_start_date, co.COHORT_START_DATE) >= 365
	where cohort_definition_id = @x_spec_cohort
	  and COHORT_START_DATE between cast(@startDate as varchar) and cast(@endDate as varchar)) pos
;

IF OBJECT_ID('tempdb..#eligibles', 'U') IS NOT NULL
	DROP TABLE #eligibles;

select visit_occurrence.person_id,
   count(visit_occurrence_id) countVis,
   min(visit_start_date) minDate
into #eligibles
from @cdm_database_schema.visit_occurrence
join (
  select person_id, count(observation_period_id) cntPd,
    min(datediff(day, observation_period_start_date, observation_period_end_date)) lenPd
  from @cdm_database_schema.observation_period
  group by person_id) obs
  on visit_occurrence.person_id = obs.person_id
    and cntPd = 1
    and lenPd >= 730
group by visit_occurrence.person_id
having count(visit_occurrence_id) >= 5
	and min(visit_start_date) between cast(@startDate as varchar) and cast(@endDate as varchar);

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
					v.person_id, min(visit_start_date) as visit_start_date,
						row_number() over (order by ((v.person_id*month(min(visit_start_date))) % 123)*((year(min(visit_start_date))*day(min(visit_start_date))) % 123)) rn
					from @cdm_database_schema.visit_occurrence v
					join @cdm_database_schema.person p
					  on v.person_id = p.person_id
						and  year(visit_start_date) - year_of_birth >= @ageLimit and year(visit_start_date) - year_of_birth <= @upperAgeLimit
					join #eligibles v5 --include only subjects with at least 5 visits in their record and within date range
						on v.person_id = v5.person_id
					where 1 = 1
						{@exclCohort != 0} ? {and v.person_id not in (
													select subject_id
													from @cohort_database_schema.@cohort_database_table
													where COHORT_DEFINITION_ID = @exclCohort)}
					group by v.person_id)}
				{@mainPopnCohort != 0} ? {
					co.subject_id as person_id, co.COHORT_START_DATE as visit_start_date,
						row_number() over (order by ((co.subject_id*month(co.COHORT_START_DATE)) % 123)*((year(co.COHORT_START_DATE)*day(co.COHORT_START_DATE)) % 123)) rn
					from @cohort_database_schema.@cohort_database_table co
					join @cdm_database_schema.person p
					  on co.subject_id = p.person_id
						and  year(co.COHORT_START_DATE) - year_of_birth >= @ageLimit and year(co.COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
					join #eligibles v5 --include only subjects with at least 5 visits in their record and within date range
						on co.subject_id = v5.person_id
					where co.cohort_definition_id = @mainPopnCohort
						{@exclCohort != 0} ? {and co.subject_id not in (
													select subject_id
													from @cohort_database_schema.@cohort_database_table
													where COHORT_DEFINITION_ID = @exclCohort)}

						)} negs
      where rn <= @baseSampleSize
    union
      select 0 as COHORT_DEFINITION_ID, SUBJECT_ID, o.observation_period_start_date COHORT_START_DATE,
        dateadd(day, 1, o.observation_period_start_date) COHORT_END_DATE
      from #cohort_person cp
      join @cdm_database_schema.observation_period o
        on cp.SUBJECT_ID = o.person_id
          and cp.COHORT_START_DATE between o.observation_period_start_date and o.observation_period_end_date
      where rn <= @xSpecSampleSize
      union
      select @x_spec_cohort as COHORT_DEFINITION_ID, SUBJECT_ID, o.observation_period_start_date COHORT_START_DATE,
        dateadd(day, 1, o.observation_period_start_date) COHORT_END_DATE
      from #cohort_person cp
      join @cdm_database_schema.observation_period o
        on cp.SUBJECT_ID = o.person_id
          and cp.COHORT_START_DATE between o.observation_period_start_date and o.observation_period_end_date
      where rn <= @xSpecSampleSize
	  union
	  select 0 as COHORT_DEFINITION_ID, SUBJECT_ID, dateadd(day, 0, COHORT_START_DATE) COHORT_START_DATE,
	      dateadd(day, 1, COHORT_START_DATE) COHORT_END_DATE
      from (select top @noise *
			from @cohort_database_schema.@cohort_database_table
			where COHORT_DEFINITION_ID = @exclCohort
				and COHORT_START_DATE between cast(@startDate as varchar) and cast(@endDate as varchar)) noise -- add a little noise to the base cohort
      );
