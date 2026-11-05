# Extracted from test_EndToEnd.R:96

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PheValuator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(PheValuator)
library(testthat)

# test -------------------------------------------------------------------------
folder <- tempfile("Phevaluator")
dir.create(folder)
on.exit(unlink(folder, recursive = TRUE))
databaseId <- "Eunomia"
cdmDatabaseSchema <- "main"
cohortDatabaseSchema <- "main"
cohortTable <- "cohort"
workDatabaseSchema <- "main"
xSpecCohort <- 1001
daysFromxSpec <- 9999
excludedCovariateConceptIds <- c()
xSensCohort <- 2
prevalenceCohort <- 2
condition <- "any condition"
connection <- connect(connectionDetails)
CovSettingsAcute <- createDefaultCovariateSettings(
    excludedCovariateConceptIds = excludedCovariateConceptIds,
    addDescendantsToExclude = TRUE,
    startDayWindow1 = 0,
    endDayWindow1 = 10,
    startDayWindow2 = 11,
    endDayWindow2 = 20,
    startDayWindow3 = 21,
    endDayWindow3 = 9999
  )
CovSettingsAcute <- createDefaultCovariateSettings(
    excludedCovariateConceptIds = excludedCovariateConceptIds,
    addDescendantsToExclude = TRUE,
    startDayWindow1 = 0,
    endDayWindow1 = 9999,
    startDayWindow2 = 0,
    endDayWindow2 = 9999,
    startDayWindow3 = 0,
    endDayWindow3 = 9999
  )
CohortArgsAcute <- createCreateEvaluationCohortArgs(
    xSpecCohortId = xSpecCohort,
    daysFromxSpec = daysFromxSpec,
    xSensCohortId = xSensCohort,
    prevalenceCohortId = prevalenceCohort,
    modelBaseSampleSize = 15000,
    xSpecCohortSize = 5000,
    covariateSettings = CovSettingsAcute,
    baseSampleSize = 2000000,
    startDate = "19000101",
    endDate = "21000101",
    minimumOffsetFromStart = 0,
    minimumOffsetFromEnd = 0,
    saveEvaluationCohortPlpData = FALSE,
    excludeModelFromEvaluation = TRUE
  )
conditionAlgTestArgs <- createTestPhenotypeAlgorithmArgs(
    cutPoints = c("EV"),
    phenotypeCohortId = 5, #6
    washoutPeriod = 0
  )
analysis1 <- createPheValuatorAnalysis(
    analysisId = 1,
    description = "5",
    createEvaluationCohortArgs = CohortArgsAcute,
    testPhenotypeAlgorithmArgs = conditionAlgTestArgs
  )
pheValuatorAnalysisList <- list(analysis1)
savePheValuatorAnalysisList(pheValuatorAnalysisList, file.path(folder, "pheValuatorAnalysisSettings.json"))
pheValuatorAnalysisList <- loadPheValuatorAnalysisList(file.path(folder, "pheValuatorAnalysisSettings.json"))
referenceTable <- runPheValuatorAnalyses(
    phenotype = condition,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    workDatabaseSchema = workDatabaseSchema,
    databaseId = cdmDatabaseSchema,
    outputFolder = folder,
    pheValuatorAnalysisList = pheValuatorAnalysisList
  )
