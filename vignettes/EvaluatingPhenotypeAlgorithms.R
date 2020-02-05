## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
library(PheValuator)

## ----tidy=FALSE,eval=FALSE-----------------------------------------------
#  options(fftempdir = "c:/temp/ff") #place to store large temporary files
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenoTest <- PheValuator::createPhenotypeModel(connectionDetails = connectionDetails,
#                             xSpecCohort = 1769699,
#                             cdmDatabaseSchema = "my_cdm_data",
#                             cohortDatabaseSchema = "my_results",
#                             cohortDatabaseTable = "cohort",
#                             outDatabaseSchema = "scratch.dbo", #a database schema with write access
#                             modelOutputFileName = "Train_10XDM_MyCDM_18-62_20190101",
#                             xSensCohort = 1770120, #the xSens cohort
#                             prevalenceCohort = 1770119, #the cohort for prevalence determination
#                             excludedConcepts = c(201820),
#                             addDescendantsToExclude = TRUE,
#                             mainPopulationCohort = 0, #use the entire subject population
#                             lowerAgeLimit = 18,
#                             upperAgeLimit = 90,
#                             startDate = "20100101",
#                             endDate = "20171231",
#                             outFolder = "c:/phenotyping")

## ----tidy=FALSE,eval=FALSE-----------------------------------------------
#  options(fftempdir = "c:/temp/ff") #place to store large temporary files
#  
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  evalCohort <- PheValuator::createEvaluationCohort(connectionDetails = connectionDetails,
#                                xSpecCohort = 1769699,
#                                cdmDatabaseSchema = "my_cdm_data",
#                                cohortDatabaseSchema = "my_results",
#                                cohortDatabaseTable = "cohort",
#                                outDatabaseSchema = "scratch.dbo",
#                                evaluationOutputFileName = "Eval_10XDM_MyCDM_18-62_20190101",
#                                modelOutputFileName = "Train_10XDM_MyCDM_18-62_20190101",
#                                mainPopulationCohort = 0,
#                                lowerAgeLimit = 18,
#                                upperAgeLimit = 90,
#                                startDate = "20100101",
#                                endDate = "20171231"
#                                outFolder = "c:/phenotyping")

## ----tidy=FALSE,eval=FALSE-----------------------------------------------
#  options(fftempdir = "c:/temp/ff") #place to store large temporary files
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenoResult <- PheValuator::testPhenotypeAlgorithm(connectionDetails = connectionDetails,
#                 cutPoints = c("EV"),
#                 evaluationOutputFileName = "c:/phenotyping/lr_results_Eval_10X_DM_MyCDM.rds",
#                 phenotypeCohortId = 1769702,
#                 cdmShortName = "myCDM",
#                 phenotypeText = "All Diabetes by Phenotype 1 X In-patient, 1st Position",
#                 order = 1,
#                 modelText = "Diabetes Mellitus xSpec Model - 10 X T2DM",
#                 xSpecCohort = 1769699,
#                 xSensCohort = 1770120,
#                 prevalenceCohort = 1770119,
#                 cohortDatabaseSchema = "my_results",
#                 cohortTable = "cohort")

