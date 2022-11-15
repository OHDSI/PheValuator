INSERT INTO @cohort_database_schema.@cohort_table (
	cohort_definition_id, 
	subject_id, 
	cohort_start_date, 
	cohort_end_date
	)
SELECT CAST(@cohort_definition_id AS INT) AS cohort_definition_id,
	v.person_id,
	visit_start_date,
	COALESCE(visit_end_date, visit_start_date)
FROM @cdm_database_schema.visit_occurrence v
;
