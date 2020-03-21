## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
library(PheValuator)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  cohortSetReference <- read.csv("c:/myCohortFile.csv")
#  

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  options(fftempdir = "c:/temp/ff") #place to store large temporary files
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenoTest <- createChronicPhenotypeModel(connectionDetails = connectionDetails,
#                  cdmDatabaseSchema = "my_cdm_data",
#                  databaseId = "TestDB",
#                  cohortDatabaseSchema = "my_results",
#                  cohortDatabaseTable = "cohort",
#                  outDatabaseSchema = "scratch.dbo", #a database schema with write access
#                  modelOutputFileName = "Train_10XDM_MyCDM_18-62_20190101",
#                  evaluationOutputFileName = "Eval_10XDM_MyCDM_18-62_20190101",
#                  conditionName = "Diabetes",
#                  xSpecCohort = 1769699,
#                  xSensCohort = 1770120,
#                  prevalenceCohort = 1770120,
#                  excludedConcepts = c(201820),
#                  includedCovariateIds = c(),
#                  addDescendantsToExclude = TRUE,
#                  mainPopulationCohort = 0, #use the entire subject population
#                  baseSampleSize = 2000000,
#                  lowerAgeLimit = 18,
#                  upperAgeLimit = 90,
#                  startDays = 0, #from the start of the subject's record
#                  endDays = 10000, #to the end of the subject's record
#                  gender = c(8507, 8532),
#                  startDate = "19000101",
#                  endDate = "21000101",
#                  checkDates = TRUE,
#                  outFolder = "c:/phenotyping",
#                  savePlpData = FALSE, #will preserve disk space
#                  createModel = TRUE, #will create a model
#                  createEvaluationCohort = TRUE, #will create an evaluation cohort
#                  cohortDefinitionsToTest = cohortSetReference)
#  

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
#  options(fftempdir = "c:/temp/ff") #place to store large temporary files
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenoTest <- createAcutePhenotypeModel(connectionDetails = connectionDetails,
#                  cdmDatabaseSchema = "my_cdm_data",
#                  databaseId = "TestDB",
#                  cohortDatabaseSchema = "my_results",
#                  cohortDatabaseTable = "cohort",
#                  outDatabaseSchema = "scratch.dbo", #a database schema with write access
#                  modelOutputFileName = "Train_Pneumonia_MyCDM_18-62_20190101",
#                  evaluationOutputFileName = "Eval_Pneumonia_MyCDM_18-62_20190101",
#                  conditionName = "Pneumonia",
#                  xSpecCohort = 1769699,
#                  xSensCohort = 1770120,
#                  prevalenceCohort = 1770120,
#                  excludedConcepts = c(255848),
#                  includedCovariateIds = c(),
#                  addDescendantsToExclude = TRUE,
#                  mainPopulationCohort = 0, #use the entire subject population
#                  baseSampleSize = 2000000,
#                  lowerAgeLimit = 18,
#                  upperAgeLimit = 90,
#                  startDays = 0, #from the start of the subject's record
#                  endDays = 7, #to the end of the subject's record
#                  visitLength = 3,
#                  gender = c(8507, 8532),
#                  startDate = "19000101",
#                  endDate = "21000101",
#                  checkDates = TRUE,
#                  outFolder = "c:/phenotyping",
#                  savePlpData = FALSE, #will preserve disk space
#                  createModel = TRUE, #will create a model
#                  createEvaluationCohort = TRUE, #will create an evaluation cohort
#                  cohortDefinitionsToTest = cohortSetReference)
#  

