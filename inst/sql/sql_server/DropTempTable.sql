/************************************************************************
@file dropTempTable.sql
************************************************************************/

{DEFAULT @test_cohort = "test_cohort" }
{DEFAULT @tempDB = "scratch.dbo" }

IF OBJECT_ID('@tempDB.@test_cohort', 'U') IS NOT NULL
	DROP TABLE @tempDB.@test_cohort;


