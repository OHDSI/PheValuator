## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
library(PheValuator)

## ----tidy=FALSE,eval=FALSE-----------------------------------------------
#  setwd("c:/phenotyping")
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenoTest <- PheValuator::createPhenoModel(connectionDetails = connectionDetails,
#                             xSpecCohort = 1769699,
#                             cdmDatabaseSchema = "my_cdm_data",
#                             cohortDatabaseSchema = "my_results",
#                             cohortDatabaseTable = "cohort",
#                             outDatabaseSchema = "scratch.dbo", #a database schema with write access
#                             trainOutFile = "PheVal_10X_DM_train",
#                             exclCohort = 1770120, #the xSens cohort
#                             prevCohort = 1770119, #the cohort for prevalence determination
#                             estPPV = 0.75,
#                             modelAnalysisId = "20181206V1",
#                             excludedConcepts = c(201820),
#                             cdmShortName = "myCDM",
#                             mainPopnCohort = 0, #use the entire subject population
#                             lowerAgeLimit = 18,
#                             upperAgeLimit = 90,
#                             startDate = "20100101",
#                             endDate = "20171231")

## ----tidy=FALSE,eval=FALSE-----------------------------------------------
#  setwd("c:/phenotyping")
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  evalCohort <- PheValuator::createEvalCohort(connectionDetails = connectionDetails,
#                                xSpecCohort = 1769699,
#                                cdmDatabaseSchema = "my_cdm_data",
#                                cohortDatabaseSchema = "my_results",
#                                cohortDatabaseTable = "cohort",
#                                outDatabaseSchema = "scratch.dbo",
#                                testOutFile = "PheVal_10X_DM_eval",
#                                trainOutFile = "PheVal_10X_DM_train",
#                                estPPV = 0.75,
#                                modelAnalysisId = "20181206V1",
#                                evalAnalysisId = "20181206V1",
#                                cdmShortName = "myCDM",
#                                mainPopnCohort = 0,
#                                lowerAgeLimit = 18,
#                                upperAgeLimit = 90,
#                                startDate = "20100101",
#                                endDate = "20171231")

## ----tidy=FALSE,eval=FALSE-----------------------------------------------
#  setwd("c:/phenotyping")
#  
#  connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                                server = "localhost/ohdsi",
#                                                user = "joe",
#                                                password = "supersecret")
#  
#  phenoResult <- PheValuator::testPhenotype(connectionDetails = connectionDetails,
#                 cutPoints = c(0.1, 0.2, 0.3, 0.4, 0.5, "EV", 0.6, 0.7, 0.8, 0.9),
#                 resultsFileName = "c:/phenotyping/lr_results_PheVal_10X_DM_eval_myCDM_ePPV0.75_20181206V1.rds",
#                 cohortPheno = 1769702,
#                 phenText = "All Diabetes by Phenotype 1 X In-patient, 1st Position",
#                 order = 1,
#                 testText = "Diabetes Mellitus xSpec Model - 10 X T2DM",
#                 cohortDatabaseSchema = "my_results",
#                 cohortTable = "cohort",
#                 estPPV = 0.75,
#                 cdmShortName = "myCDM")

