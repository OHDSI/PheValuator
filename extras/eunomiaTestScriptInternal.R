library(PheValuator)
library(testthat)
#library(Eunomia)

  folder <- "P:/temp/Phevaluator"
  dir.create(folder, showWarnings = FALSE)
  #on.exit(unlink(folder, recursive = TRUE))

  databaseId <- "Eunomia"
  cdmDatabaseSchema <- "main"
  cohortDatabaseSchema <- "main"
  cohortTable <- "cohort"
  workDatabaseSchema <- "main"
  xSpecCohort <- 1001
  daysFromxSpec <- 0
  excludedCovariateConceptIds <- c()
  xSensCohort <- 2
  prevalenceCohort <- 1001

  #connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- connect(connectionDetails)

  # Create analysis settings ---------------------------------------------------
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
    saveEvaluationCohortPlpData = FALSE,
    excludeModelFromEvaluation = TRUE
  )

  conditionAlgTestArgs <- createTestPhenotypeAlgorithmArgs(
    cutPoints = c("EV", 0.05),
    phenotypeCohortId = 6,
    washoutPeriod = 0
  )

  analysis1 <- createPheValuatorAnalysis(
    analysisId = 1,
    description = "6",
    createEvaluationCohortArgs = CohortArgsAcute,
    testPhenotypeAlgorithmArgs = conditionAlgTestArgs
  )


  pheValuatorAnalysisList <- list(analysis1)

  savePheValuatorAnalysisList(pheValuatorAnalysisList, file.path(folder, "pheValuatorAnalysisSettings.json"))

  # Run analysis ---------------------------------------------------------------
  pheValuatorAnalysisList <- loadPheValuatorAnalysisList(file.path(folder, "pheValuatorAnalysisSettings.json"))

  referenceTable <- runPheValuatorAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    workDatabaseSchema = workDatabaseSchema,
    outputFolder = folder,
    pheValuatorAnalysisList = pheValuatorAnalysisList
  )

a <- readRDS("P:/temp/Phevaluator/EvaluationCohort_e1/evaluationCohort_main.rds")$prediction
