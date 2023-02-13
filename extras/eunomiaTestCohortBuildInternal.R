library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()

if (grepl("testthat", getwd())) {
  cohortPath <- "cohorts"
} else {
  cohortPath <- file.path("tests", "testthat", "cohorts")
}

creationFile <- file.path(cohortPath, "CohortsToCreate.csv")
cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = creationFile,
  sqlFolder = cohortPath,
  jsonFolder = cohortPath,
  cohortFileNameValue = c("cohortId")
)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "cohort")

CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortTableNames = cohortTableNames,
  cohortDatabaseSchema = "main",
  incremental = FALSE
)


CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTableNames = cohortTableNames,
  cohortDefinitionSet = cohortDefinitionSet,
  incremental = FALSE
)


