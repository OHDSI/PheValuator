{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_table = 'cohort'
{DEFAULT @work_database_schema = 'CDM_SIM' }
{DEFAULT @x_spec_cohort = 0 }
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
{DEFAULT @daysFromxSpec = 0 }

{@daysFromxSpec != 0} ? {
DROP TABLE IF EXISTS #new_xspec;

SELECT @x_spec_cohort as cohort_definition_id, subject_id, minVisit as cohort_start_date, dateadd(day, 1, minVisit) cohort_end_date
into #new_xspec
FROM (select distinct cohort_definition_id, subject_id, cohort_start_date, minVisit
    from (
      SELECT co.cohort_definition_id, co.subject_id, co.cohort_start_date,
        min(v.visit_start_date) OVER (partition by co.subject_id, co.cohort_start_date) minVisit
    	FROM @cohort_database_schema.@cohort_database_table co
    	JOIN @cdm_database_schema.observation_period o
    	  on co.subject_id = o.person_id
    		AND COHORT_START_DATE >= o.observation_period_start_date
    		and dateadd(day, 365, COHORT_START_DATE) <= o.observation_period_end_date
    	join @cdm_database_schema.visit_occurrence v
    	 on co.subject_id = v.person_id
        and v.visit_start_date >= dateadd(day, 1, co.cohort_start_date)
        and v.visit_start_date <= dateadd(day, @daysFromxSpec, co.cohort_start_date)
    	WHERE cohort_definition_id = @x_spec_cohort) pos);
}

DROP TABLE IF EXISTS #cohort_person;

SELECT *
into #cohort_person
FROM (SELECT co.*, p.*,
	  row_number() over (order by NewId()) rn
{@daysFromxSpec != 0} ? {FROM #new_xspec co}
{@daysFromxSpec == 0} ? {FROM @cohort_database_schema.@cohort_database_table co}
	JOIN @cdm_database_schema.person p
	  on co.subject_id = p.person_id
		AND  year(COHORT_START_DATE) - year_of_birth >= @ageLimit
		AND year(COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
		AND gender_concept_id in (@gender)
		{@race != 0} ? {AND race_concept_id in (@race)}
    {@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
	WHERE cohort_definition_id = @x_spec_cohort
	  AND co.COHORT_START_DATE >= cast('@startDate' AS DATE)
	  AND co.COHORT_START_DATE <= cast('@endDate' AS DATE)) pos
;

DROP TABLE IF EXISTS @work_database_schema.@test_cohort;

SELECT CAST(0 AS BIGINT) as COHORT_DEFINITION_ID,
	person_id as SUBJECT_ID,
	dateadd(day, 0, visit_start_date) COHORT_START_DATE,
	dateadd(day, 1, visit_start_date) COHORT_END_DATE
INTO @work_database_schema.@test_cohort
      FROM (SELECT
				{@mainPopnCohort == 0} ? {
					v.person_id, visit_start_date,
						row_number() over (order by NewId()) rn
					FROM @cdm_database_schema.visit_occurrence v
	        JOIN @cdm_database_schema.observation_period obs
	          on v.person_id = obs.person_id
	            AND v.visit_start_date >= dateadd(d, 365, obs.observation_period_start_date)
		          AND v.visit_start_date <= dateadd(d, -30, obs.observation_period_end_date)
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
          JOIN @cdm_database_schema.person p
					  on v.person_id = p.person_id
						AND year(visit_start_date) - year_of_birth >= @ageLimit
						AND year(visit_start_date) - year_of_birth <= @upperAgeLimit
						AND gender_concept_id in (@gender)
						{@race != 0} ? {AND race_concept_id in (@race)}
		        {@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
					WHERE visit_start_date >= cast('@startDate' AS DATE)
						AND visit_start_date <= cast('@endDate' AS DATE)
						AND visit_concept_id IN (@visitType)
		        AND datediff(day, visit_start_date, visit_end_date) >= @visitLength
{@firstCut} ? {AND 11*(9*(v.visit_occurrence_id/9)/11) = v.visit_occurrence_id}
{@exclCohort != 0} ? {AND v.person_id not in (
													SELECT subject_id
													FROM @cohort_database_schema.@cohort_database_table
													WHERE COHORT_DEFINITION_ID = @exclCohort)}
					)}
				{@mainPopnCohort != 0} ? {
					co.subject_id as person_id, v.visit_start_date,
						row_number() over (order by NewId()) rn
					FROM @cohort_database_schema.@cohort_database_table co
					JOIN @cdm_database_schema.visit_occurrence v
					  on v.person_id = co.subject_id
					    AND v.visit_concept_id in (@visitType)
		          AND datediff(day, visit_start_date, visit_end_date) >= @visitLength
					    AND v.visit_start_date >= dateadd(day, @mainPopnCohortStartDay, co.COHORT_START_DATE)
					    AND v.visit_start_date <= dateadd(day, @mainPopnCohortEndDay, co.COHORT_START_DATE)
					    AND v.visit_start_date >= cast('@startDate' AS DATE)
		          AND v.visit_start_date <= cast('@endDate' AS DATE)
					JOIN @cdm_database_schema.person p
					  on co.subject_id = p.person_id
						AND  year(co.COHORT_START_DATE) - year_of_birth >= @ageLimit
						AND year(co.COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
						AND gender_concept_id in (@gender)
						{@race != 0} ? {AND race_concept_id in (@race)}
		        {@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
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
					WHERE co.cohort_definition_id = @mainPopnCohort
						{@exclCohort != 0} ? {AND co.subject_id not in (
													SELECT subject_id
													FROM @cohort_database_schema.@cohort_database_table
													WHERE COHORT_DEFINITION_ID = @exclCohort)}
						)} negs
      WHERE rn <= cast('@baseSampleSize' as bigint)
    UNION
      SELECT 0 as COHORT_DEFINITION_ID, SUBJECT_ID, cp.COHORT_START_DATE COHORT_START_DATE,
        dateadd(day, 1, cp.COHORT_START_DATE) COHORT_END_DATE
      FROM #cohort_person cp
      JOIN @cdm_database_schema.observation_period o
        on cp.SUBJECT_ID = o.person_id
          AND cp.COHORT_START_DATE >= o.observation_period_start_date
          AND cp.COHORT_START_DATE <= o.observation_period_end_date
      WHERE rn <= @xSpecSampleSize
      UNION
      SELECT @x_spec_cohort as COHORT_DEFINITION_ID, SUBJECT_ID, cp.COHORT_START_DATE COHORT_START_DATE,
        dateadd(day, 1, cp.COHORT_START_DATE) COHORT_END_DATE
      FROM #cohort_person cp
      JOIN @cdm_database_schema.observation_period o
        on cp.SUBJECT_ID = o.person_id
          AND cp.COHORT_START_DATE >= o.observation_period_start_date
          AND cp.COHORT_START_DATE <= o.observation_period_end_date
      WHERE rn <= @xSpecSampleSize
      ;

{@daysFromxSpec != 0} ? {
  TRUNCATE TABLE #new_xspec;
  DROP TABLE #new_xspec;
}

TRUNCATE TABLE #cohort_person;
DROP TABLE #cohort_person;
