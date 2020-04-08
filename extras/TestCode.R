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
xSensCohortId <- 14945#8893 # Any pneumonia ocurrence
prevalenceCohortId <- xSensCohortId
cohortToEvaluateId <- 14713 # Inpatient pneumonia
evaluationCohortFolder <- "s:/temp/PheValuatorOut"
modelId <- "Pneumonia"
evaluationCohortId <- modelId


modelType = "chronic"
covariateSettings <- PheValuator:::createDefaultChronicCovariateSettings(excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                         addDescendantsToExclude = TRUE,
                                                                         startDays = -9999,
                                                                         endDays = 9999)

# modelType = "acute"
# covariateSettings <- PheValuator:::createDefaultAcuteCovariateSettings(excludedCovariateConceptIds = excludedCovariateConceptIds,
#                                                                        addDescendantsToExclude = TRUE,
#                                                                        startDays = 0,
#                                                                        endDays = 30)

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
                       baseSampleSize = 2e6)

results <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  phenotypeCohortId = cohortToEvaluateId,
                                  outFolder = evaluationCohortFolder,
                                  cutPoints = c(0.5, "EV"),
                                  washoutPeriod = 365)

str(results)


misses <- attr(results, "misses")
