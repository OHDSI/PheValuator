## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
library(PheValuator)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  options(fftempdir = "c:/temp/ff") #place to store large temporary files
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenoTest <- createEvaluationCohort(connectionDetails = connectionDetails,
#                                     xSpecCohortId = 1769699,
#                                     xSensCohortId = 1770120,
#                                     prevalenceCohortId = 1770119,
#                                     cdmDatabaseSchema = "my_cdm_data",
#                                     cohortDatabaseSchema = "my_results",
#                                     cohortTable  = "cohort",
#                                     workDatabaseSchema = "scratch.dbo",
#                                     covariateSettings =
#                                      createDefaultChronicCovariateSettings(
#                                       excludedCovariateConceptIds = c(201826),
#                                       addDescendantsToExclude = TRUE),
#                                     baseSampleSize = 2000000,
#                                     lowerAgeLimit = 18,
#                                     upperAgeLimit = 90,
#                                     gender = c(8507, 8532),
#                                     startDate = "20101010",
#                                     endDate = "21000101",
#                                     cdmVersion = "5",
#                                     outFolder = "c:/phenotyping",
#                                     evaluationCohortId = "diabetes",
#                                     removeSubjectsWithFutureDates = TRUE,
#                                     saveEvaluationCohortPlpData = FALSE,
#                                     modelType = "chronic")
#  

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  options(fftempdir = "c:/temp/ff") #place to store large temporary files
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenotypeResults <- testPhenotypeAlgorithm(connectionDetails,
#                                     cutPoints = c("EV"),
#                                     outFolder = "c:/phenotyping",
#                                     evaluationCohortId = "diabetes",
#                                     phenotypeCohortId = 7142,
#                                     cdmDatabaseSchema = "my_cdm_data",
#                                     cohortDatabaseSchema = "my_results",
#                                     cohortTable  = "cohort",
#                                     washoutPeriod = 365)
#  

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  
#        write.csv(phenotypeResults, "c:/phenotyping/diabetes_results.csv", row.names = FALSE)
#  

