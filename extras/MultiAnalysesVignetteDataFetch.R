library(PheValuator)

options(fftempdir = "s:/FFtemp")
connectionDetails <- createConnectionDetails(dbms = "pdw",
                                             server = Sys.getenv("PDW_SERVER"),
                                             port = Sys.getenv("PDW_PORT"))
oracleTempSchema <- NULL

# MDCR settings
databaseId <- "MDCR"
cdmDatabaseSchema <- "CDM_IBM_MDCR_V1104.dbo"
cohortDatabaseSchema <- "CDM_IBM_MDCR_V1104.ohdsi_results"
cohortTable <- "cohort"
workDatabaseSchema <- "scratch.dbo"

# Phenotype settings
xSpecCohortId <- 14584 # Pneumonia with second inpatient diagnose by specialist
excludedCovariateConceptIds <- c(255848) # Pneumonia
xSensCohortId <- 8893 # Any pneumonia ocurrence
prevalenceCohortId <- xSensCohortId
cohortToEvaluateId <- 14713 # Inpatient pneumonia
evaluationCohortFolder <- "s:/temp/PheValuatorOut"
modelId <- "Pneumonia"
evaluationCohortId <- modelId


pneunomoniaCohortArgs <- createCreateEvaluationCohortArgs(xSpecCohortId = 14584,
                                                          xSensCohortId = 8893,
                                                          modelType = "chronic")

pneumoniaAlg1TestArgs <- createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                          phenotypeCohortId = 14713)



# modelType = "chronic"
# covariateSettings <- PheValuator:::createDefaultChronicCovariateSettings(excludedCovariateConceptIds = excludedCovariateConceptIds,
#                                                                          addDescendantsToExclude = TRUE,
#                                                                          startDays = -9999,
#                                                                          endDays = 9999)

modelType = "acute"
covariateSettings <- PheValuator:::createDefaultAcuteCovariateSettings(excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                       addDescendantsToExclude = TRUE,
                                                                       startDays = 0,
                                                                       endDays = 30)

createEvaluationCohort(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortTable,
                       workDatabaseSchema = workDatabaseSchema,
                       xSpecCohortId = xSpecCohortId,
                       xSensCohortId = xSensCohortId,
                       covariateSettings = covariateSettings,
                       outFolder = evaluationCohortFolder,
                       modelType = modelType,
                       baseSampleSize = 10000)

results <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  phenotypeCohortId = cohortToEvaluateId,
                                  outFolder = evaluationCohortFolder,
                                  cutPoints = c(0.5, "EV"),
                                  washoutPeriod = 365)

results

attr(results, "misses")

x <- readRDS(file.path(evaluationCohortFolder, "modelPneumonia.rds"))
y <- x$model$varImp
