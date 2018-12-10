/************************************************************************
@file CreateCohorts.sql
************************************************************************/

{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @x_spec_cohort = 0 }
{DEFAULT @ageLimit = 20}
{DEFAULT @upperAgeLimit = 120}
{DEFAULT @baseSampleSize = 150000 }
{DEFAULT @xSpecSampleSize = 1500 }
{DEFAULT @mainPopnCohort = 0 }


IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;

select *
into #cohort_person
from (select *,
	  row_number() over (order by ((co.subject_id*month(COHORT_START_DATE)) % 123)*((year(COHORT_START_DATE)*day(COHORT_START_DATE)) % 123)) rn
	from @cdm_database_schema.ohdsi_results.cohort co
	join @cdm_database_schema.dbo.person p 
	  on co.subject_id = p.person_id
		and  year(COHORT_START_DATE) - year_of_birth >= @ageLimit and year(COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
	where cohort_definition_id = @x_spec_cohort
	  and substring(cast(subject_id as varchar), 5, 1) not in ('5')) pos
;
				  

IF OBJECT_ID('scratch.dbo.test_cohort', 'U') IS NOT NULL
	DROP TABLE scratch.dbo.test_cohort;
	
select *
into scratch.dbo.test_cohort
from (select 0 as COHORT_DEFINITION_ID, person_id as SUBJECT_ID, dateadd(day, 0, visit_start_date) COHORT_START_DATE, dateadd(day, 1, visit_start_date) COHORT_END_DATE
      from (select 
				{@mainPopnCohort == 0} ? {
					v.person_id, min(visit_start_date) as visit_start_date,
						row_number() over (order by ((v.person_id*month(min(visit_start_date))) % 123)*((year(min(visit_start_date))*day(min(visit_start_date))) % 123)) rn
					from @cdm_database_schema.dbo.visit_occurrence v
					join @cdm_database_schema.dbo.person p 
					  on v.person_id = p.person_id
						and  year(visit_start_date) - year_of_birth >= @ageLimit and year(visit_start_date) - year_of_birth <= @upperAgeLimit
					where substring(cast(v.person_id as varchar), 5, 1) in ('5')
					group by v.person_id)}			
				{@mainPopnCohort != 0} ? {
					co.subject_id as person_id, co.COHORT_START_DATE as visit_start_date,
						row_number() over (order by ((co.subject_id*month(co.COHORT_START_DATE)) % 123)*((year(co.COHORT_START_DATE)*day(co.COHORT_START_DATE)) % 123)) rn
					from @cdm_database_schema.ohdsi_results.cohort co
					join @cdm_database_schema.dbo.person p 
					  on co.subject_id = p.person_id
						and  year(co.COHORT_START_DATE) - year_of_birth >= @ageLimit and year(co.COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
					where co.cohort_definition_id = @mainPopnCohort
						and substring(cast(co.subject_id as varchar), 5, 1) in ('5'))} negs			
      where rn <= 0.5 * @baseSampleSize --use 70% of either full database or specific base cohort - NOTE: 100% of 0% outcome base doesn't work
	  union
	  select 0 as COHORT_DEFINITION_ID, person_id as SUBJECT_ID, dateadd(day, 0, visit_start_date) COHORT_START_DATE, dateadd(day, 1, visit_start_date) COHORT_END_DATE
	        from (select 
					v.person_id, min(visit_start_date) as visit_start_date,
						row_number() over (order by ((v.person_id*month(min(visit_start_date))) % 123)*((year(min(visit_start_date))*day(min(visit_start_date))) % 123)) rn
					from @cdm_database_schema.dbo.visit_occurrence v
					join @cdm_database_schema.dbo.person p 
					  on v.person_id = p.person_id
						and  year(visit_start_date) - year_of_birth >= @ageLimit and year(visit_start_date) - year_of_birth <= @upperAgeLimit
					where substring(cast(v.person_id as varchar), 5, 1) in ('5')
					group by v.person_id) negs			
      where rn <= 0.5 * @baseSampleSize -- use 30% of full database
      union 
      select 0 as COHORT_DEFINITION_ID, SUBJECT_ID, dateadd(day, 0, COHORT_START_DATE) COHORT_START_DATE, dateadd(day, 1, COHORT_START_DATE) COHORT_END_DATE
      from #cohort_person
      where rn <= @xSpecSampleSize
      union 
      select @x_spec_cohort as COHORT_DEFINITION_ID, SUBJECT_ID, dateadd(day, 0, COHORT_START_DATE) COHORT_START_DATE, dateadd(day, 1, COHORT_START_DATE) COHORT_END_DATE
      from #cohort_person
      where rn <= @xSpecSampleSize
      ) allSub
;
