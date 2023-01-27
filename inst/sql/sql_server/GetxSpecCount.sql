{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_table = 'cohort'
{DEFAULT @x_spec_cohort = 0 }
{DEFAULT @ageLimit = 0}
{DEFAULT @upperAgeLimit = 120}
{DEFAULT @gender = c(8507, 8532)}
{DEFAULT @race = 0}
{DEFAULT @ethnicity = 0}
{DEFAULT @startDate = '19000101' }
{DEFAULT @endDate = '21000101' }
{DEFAULT @daysFromxSpec = 0 }


select count(distinct subject_id)
from (select co.*, p.*,
	  row_number() over (order by NewId()) rn
	from @cohort_database_schema.@cohort_database_table co
	join @cdm_database_schema.person p
	  on co.subject_id = p.person_id
		and  year(COHORT_START_DATE) - year_of_birth >= @ageLimit
		and year(COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
		and gender_concept_id in (@gender)
		{@race != 0} ? {AND race_concept_id in (@race)}
    {@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
	join @cdm_database_schema.observation_period o
	  on co.subject_id = o.person_id
	    and co.COHORT_START_DATE >= o.observation_period_start_date
		and co.COHORT_START_DATE <= o.observation_period_end_date
	    and datediff(day, o.observation_period_start_date, co.COHORT_START_DATE) >= 365
	{@daysFromxSpec != 0} ? {
	join @cdm_database_schema.visit_occurrence v
    	 on co.subject_id = v.person_id
        and v.visit_start_date >= dateadd(day, 1, co.cohort_start_date)
        and v.visit_start_date <= dateadd(day, @daysFromxSpec, co.cohort_start_date)}
	where cohort_definition_id = @x_spec_cohort
	  and co.COHORT_START_DATE >= cast('@startDate' AS DATE)
	  and co.COHORT_START_DATE <= cast('@endDate' AS DATE)) pos
;



