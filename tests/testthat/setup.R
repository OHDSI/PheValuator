onCran <- function() {
  notCran <- Sys.getenv("NOT_CRAN")
  if (identical(notCran, "")) {
    !interactive()
  } else {
    !isTRUE(as.logical(notCran))
  }
}

runIntegrationTests <- !onCran() &&
  identical(tolower(Sys.getenv("PHEVALUATOR_RUN_INTEGRATION_TESTS")), "true")

if (runIntegrationTests) {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()

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

  withr::defer(
    {
      unlink(connectionDetails$server())
    },
    testthat::teardown_env()
  )
}
