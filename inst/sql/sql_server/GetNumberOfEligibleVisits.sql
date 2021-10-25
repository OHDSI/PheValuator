SELECT COUNT_BIG(*)
FROM @cdm_database_schema.visit_occurrence v
JOIN @cdm_database_schema.observation_period obs
	ON v.person_id = obs.person_id
		AND v.visit_start_date >= dateadd(d, 365, obs.observation_period_start_date)
		AND v.visit_start_date <= dateadd(d, - 30, obs.observation_period_end_date)
JOIN @cdm_database_schema.person p
	ON v.person_id = p.person_id
		AND year(visit_start_date) - year_of_birth >= @ageLimit
		AND year(visit_start_date) - year_of_birth <= @upperAgeLimit
		AND gender_concept_id IN (@gender)
		{@race != 0} ? {AND race_concept_id in (@race)}
    {@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
WHERE visit_start_date >= cast('@startDate' AS DATE)
	AND visit_start_date <= cast('@endDate' AS DATE)
	AND visit_concept_id IN (@visitType)
	AND datediff(day, visit_start_date, visit_end_date) >= @visitLength
{@exclCohort != 0 } ? {
	AND v.person_id NOT IN (
		SELECT subject_id
		FROM @cohort_database_schema.@cohort_database_table
		WHERE COHORT_DEFINITION_ID = @exclCohort
		)
}
;
