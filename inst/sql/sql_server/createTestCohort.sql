/************************************************************************
@file createTestCohort.sql
************************************************************************/

{DEFAULT @cntPos = 1000}
{DEFAULT @testCohort = 0}
{DEFAULT @cdmDatabaseSchema = NULL}
{DEFAULT @fullCohort = NULL}
{DEFAULT @testingCohort = NULL}
{DEFAULT @cntNeg = 30000}
{DEFAULT @exclCohort = 0}
{DEFAULT @tableToUse = ''}


IF OBJECT_ID('tempdb..#cohortToUse', 'U') IS NOT NULL
	DROP TABLE #cohortToUse;

select top @cntPos *
into #cohortToUse
from (
  select top @cntPos *
  {@tableToUse == ''} ? {
  from @cdmDatabaseSchema.ohdsi_results.cohort
  where cohort_definition_id = @testCohort}
  {@tableToUse != ''} ? {
  from @tableToUse}
  order by ((subject_id*month(cohort_start_date)) % 123)*((year(cohort_start_date))*day(cohort_start_date)) % 123) a

IF OBJECT_ID('@fullCohort', 'U') IS NOT NULL
	DROP TABLE @fullCohort;

select *
into @fullCohort
from (
  select 0 as cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
  from #cohortToUse
  union
  select 1 as cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
  from #cohortToUse
  union
  select top @cntNeg 0 as cohort_definition_id, subject_id, cohort_start_date, dateadd(day, 1, cohort_start_date) cohort_end_date
  from (
    select person_id subject_id, min(visit_start_date) cohort_start_date
    from @cdmDatabaseSchema.dbo.visit_occurrence
    where person_id not in (
      select subject_id
      from @cdmDatabaseSchema.ohdsi_results.cohort
      where cohort_definition_id in (@testCohort, @exclCohort))
    group by person_id) b
  order by ((subject_id*month(cohort_start_date)) % 123)*((year(cohort_start_date))*day(cohort_start_date)) % 123
     ) a

IF OBJECT_ID('@testingCohort', 'U') IS NOT NULL
	DROP TABLE @testingCohort;
     
select *
into @testingCohort
from (
  select top @truePos cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
  from @fullCohort
  where cohort_definition_id = 1
  union
  select top @falsePos 1 as cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
  from @fullCohort
  where cohort_definition_id = 0
    and subject_id not in (
      select subject_id
      from @fullCohort
      where cohort_definition_id = 1)) a

