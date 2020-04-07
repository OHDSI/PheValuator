SELECT subject_id,
	cohort_start_date xsens_cohort_start_date,
	observation_period_start_date cohort_start_date,
	DATEDIFF(DAY, observation_period_start_date, cohort_start_date) AS days_to_xsens
FROM @cohort_database_schema.@cohort_table
JOIN @cdm_database_schema.observation_period
	ON subject_id = person_id
		AND cohort_start_date >= observation_period_start_date
		AND cohort_start_date <= observation_period_end_date
WHERE cohort_definition_id = @cohort_id;
