SELECT subject_id,
	cohort_start_date comparison_cohort_start_date
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @cohort_id;
