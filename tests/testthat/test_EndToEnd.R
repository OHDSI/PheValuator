test_that("TestPhenotype - test PheValuator end to end", {
  Sys.setenv(EUNOMIA_DATA_FOLDER = tempdir())
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- connect(connectionDetails)

  andromedaTempFolder <- tempdir() #change to a spot where you have some disk space for temporary files


  options(andromedaTempFolder = andromedaTempFolder)

  folder <- tempdir() #change to a spot where you have significant free space

  #databases are in the format:
  databaseId <- "Eunomia"
  databaseDetails <- list(databaseId = databaseId,
                           cdmDatabaseSchema = "main",
                           cohortDatabaseSchema = "main",
                           cohortTable = "cohort",
                           workDatabaseSchema = "main",
                           connectionDetails = getEunomiaConnectionDetails())


  dbList <- list(databaseDetails) #insert the db's you want to run in this list

  cohorts <- createCohortSet(connectionDetails)

  #create the 4 cohorts below needed for the analysis
  xSpecCohort <- 1001
  excludedCovariateConceptIds <- c()
  xSensCohort <- 2
  prevalenceCohort <- 2
  evaluationPopulationCohortId <- 5

  # Create analysis settings ----------------------------------------------------------------------------------------
  CovSettingsAcute <- createDefaultAcuteCovariateSettings(excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                          addDescendantsToExclude = TRUE,
                                                          startDayWindow1 = 0,
                                                          endDayWindow1 = 10,
                                                          startDayWindow2 = 11,
                                                          endDayWindow2 = 20,
                                                          startDayWindow3 = 21,
                                                          endDayWindow3 = 9999)

  CohortArgsAcute <- createCreateEvaluationCohortArgs(xSpecCohortId = xSpecCohort,
                                                      xSensCohortId = xSensCohort,
                                                      prevalenceCohortId = prevalenceCohort,
                                                      evaluationPopulationCohortId = evaluationPopulationCohortId,
                                                      evaluationPopulationCohortIdStartDay = 0,
                                                      evaluationPopulationCohortIdEndDay = 0,
                                                      modelBaseSampleSize = 15000,
                                                      xSpecCohortSize = 5000,
                                                      covariateSettings = CovSettingsAcute,
                                                      baseSampleSize = 2000000,
                                                      startDate = "19000101", #can change these dates to suit your database
                                                      endDate = "21000101",
                                                      saveEvaluationCohortPlpData = FALSE,
                                                      excludeModelFromEvaluation = FALSE)


  ##### First phenotype algorithm to test ##############
  conditionAlgTestArgs <- createTestPhenotypeAlgorithmArgs(cutPoints = c("EV"),
                                                           phenotypeCohortId =  6, #CHANGE THIS: 1st phenotype algorithm to test
                                                           washoutPeriod = 0) #CHANGE THIS: to how many continuous observation days prior to index (e.g., 365)

  analysis1 <- createPheValuatorAnalysis(analysisId = 1,
                                         description = "6", #CHANGE THIS: to a good name for your phenotype algorithm to test
                                         createEvaluationCohortArgs = CohortArgsAcute,
                                         testPhenotypeAlgorithmArgs = conditionAlgTestArgs)


  pheValuatorAnalysisList <- list(analysis1) #add any additional phenotypes to test to this list, e.g., analysis3

  savePheValuatorAnalysisList(pheValuatorAnalysisList, file.path(folder, "pheValuatorAnalysisSettings.json"))

  for (dbUp in 1:length(dbList)) {

    pheValuatorAnalysisList <- loadPheValuatorAnalysisList(file.path(folder, "pheValuatorAnalysisSettings.json"))

    outFolder <- file.path(folder, paste0(dbList[[dbUp]]$databaseId))

    referenceTable <- runPheValuatorAnalyses(connectionDetails = dbList[[dbUp]]$connectionDetails,
                                             cdmDatabaseSchema = dbList[[dbUp]]$cdmDatabaseSchema,
                                             cohortDatabaseSchema = dbList[[dbUp]]$cohortDatabaseSchema,
                                             cohortTable = dbList[[dbUp]]$cohortTable,
                                             workDatabaseSchema = dbList[[dbUp]]$workDatabaseSchema,
                                             outputFolder = outFolder,
                                             pheValuatorAnalysisList = pheValuatorAnalysisList)

  }

  expect_true(file.exists(file.path(folder, databaseId, "TestResults_a1.rds")))

  outputFile <- readRDS(file.path(folder, databaseId, "TestResults_a1.rds"))

  expect_true(as.numeric(outputFile$sensitivity[[1]]) >= 0.1 & as.numeric(outputFile$sensitivity[[1]]) <= 0.9)

})
