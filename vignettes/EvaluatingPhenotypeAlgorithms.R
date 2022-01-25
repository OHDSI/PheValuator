## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
library(PheValuator)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  options(andromedaTempFolder = "c:/temp2/ff") #place to store large temporary files
#  
#  CovSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(201254),
#                                                addDescendantsToExclude = TRUE,
#                                                startDayWindow1 = 0,
#                                                endDayWindow1 = 30,
#                                                startDayWindow2 = 31,
#                                                endDayWindow2 = 180,
#                                                startDayWindow3 = 181,
#                                                endDayWindow3 = 365)
#  

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  options(andromedaTempFolder = "c:/temp2/ff") #place to store large temporary files
#  
#  CovSettings <- createDefaultCovariateSettings(startDayWindow1 = 0,
#                                                endDayWindow1 = 10,
#                                                startDayWindow2 = 11,
#                                                endDayWindow2 = 20,
#                                                startDayWindow3 = 21,
#                                                endDayWindow3 = 30)
#  

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  CohortArgs <- createCreateEvaluationCohortArgs(xSpecCohortId = 1769699,
#                                                 xSensCohortId = 1770120,
#                                                 prevalenceCohortId = 1770119,
#                                                 evaluationPopulationCohortId = 1778258,
#                                                 covariateSettings = CovSettings)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  #First phenotype algorithm to test
#  conditionAlg1TestArgs <- createTestPhenotypeAlgorithmArgs(phenotypeCohortId =  1778259)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  analysis1 <- createPheValuatorAnalysis(analysisId = 1,
#                                         description = "[PheValuator] Type 2 Diabetes Mellitus
#                                                      (prevalent)",
#                                         createEvaluationCohortArgs = CohortArgs,
#                                         testPhenotypeAlgorithmArgs = conditionAlg1TestArgs)
#  

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  #Second phenotype algorithm to test
#  conditionAlg2TestArgs <- createTestPhenotypeAlgorithmArgs(phenotypeCohortId = 1778260,
#                                                            washoutPeriod = 0)
#  
#  analysis2 <- createPheValuatorAnalysis(analysisId = 2,
#                                         description = "[PheValuator] Type 2 diabetes mellitus
#                                                      with second code 31-365 days after index",
#                                         createEvaluationCohortArgs = CohortArgs,
#                                         testPhenotypeAlgorithmArgs = conditionAlg2TestArgs)
#  
#  #save the analyses
#  pheValuatorAnalysisList <- list(analysis1, analysis2)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  #create database connection details
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                               server = "localhost/ohdsi",
#                                               user = "joe",
#                                               password = "supersecret")
#  
#  #run the PheValuator process
#  referenceTable <- runPheValuatorAnalyses(connectionDetails = connectionDetails,
#                                           cdmDatabaseSchema = "yourCDMSchema",
#                                           cohortDatabaseSchema = "yourCohortSchema",
#                                           cohortTable = "yourCohortTableSchema",
#                                           workDatabaseSchema = "yourWritableSchema",
#                                           outputFolder = "yourOutputFolderSchema",
#                                           pheValuatorAnalysisList = pheValuatorAnalysisList)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  #view the results of the phenotype evaluation
#  View(summarizePheValuatorAnalyses(referenceTable, "yourOutputFolderSchema"))
#  write.csv(phenotypeResults, "c:/phenotyping/diabetes_results.csv", row.names = FALSE)

