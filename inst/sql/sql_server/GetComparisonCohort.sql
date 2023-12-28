--SELECT subject_id,
--	cohort_start_date comparison_cohort_start_date, cohort_end_date comparison_cohort_end_date
--FROM @cohort_database_schema.@cohort_table
--WHERE cohort_definition_id = @cohort_id;

{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_table = 'cohort' }
{DEFAULT @cohort_id = 0 }
{DEFAULT @inclusionEvaluationCohortId = 0 }
{DEFAULT @inclusionEvaluationDaysFromStart = 0 }
{DEFAULT @inclusionEvaluationDaysFromEnd = 0 }
{DEFAULT @duringInclusionEvaluationOnly}
{DEFAULT @caseFirstOccurrenceOnly = TRUE}

--if using first occurrence only subset the cohort to the first occurrence per subject
{@caseFirstOccurrenceOnly == TRUE} ?
{with tempCohort as (
  select cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
  from (
    select *, row_number() over (PARTITION BY subject_id order by cohort_start_date) rn
    from @cohort_database_schema.@cohort_table
    where cohort_definition_id = @cohort_id) a
    where rn = 1)}

--else allow all occurrences to go through to main sql
{@caseFirstOccurrenceOnly == FALSE} ?
{with tempCohort as (
  select *
  from @cohort_database_schema.@cohort_table
  where cohort_definition_id = @cohort_id)}

SELECT subject_id, cohort_start_date comparison_cohort_start_date, cohort_end_date comparison_cohort_end_date
  FROM (
    select c1.*, row_number() over (PARTITION BY c1.subject_id order by c1.cohort_start_date) rn
    from tempCohort c1

--if using an inclusion cohort only select the rows that match within the specified cohort start/end dates and then take the first row of the remaining
{@inclusionEvaluationCohortId != 0} ?
   {join @cohort_database_schema.@cohort_table c2
      on c1.subject_id = c2.subject_id
      and c1.cohort_start_date >= dateadd(day, @inclusionEvaluationDaysFromStart, c2.cohort_start_date)

      {@duringInclusionEvaluationOnly == FALSE} ? {--no limit to the ending date designated by inclusionEvaluationDaysFromEnd
      and c1.cohort_start_date <= dateadd(day, @inclusionEvaluationDaysFromEnd, c2.cohort_start_date)}

      {@duringInclusionEvaluationOnly == TRUE} ? {--limit ending date to min value of days offset and end of cohort
      and c1.cohort_start_date <=
        CASE
          WHEN dateadd(day, @inclusionEvaluationDaysFromEnd, c2.cohort_start_date) > c2.cohort_end_date THEN c2.cohort_end_date
          ELSE dateadd(day, @inclusionEvaluationDaysFromEnd, c2.cohort_start_date)
        END}

      and c2.cohort_definition_id = @inclusionEvaluationCohortId}
    ) a;

--where rn = 1;


