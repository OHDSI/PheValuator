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
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  phenotypeCohortId = cohortToEvaluateId,
                                  outFolder = evaluationCohortFolder,
                                  cutPoints = c(0.5, "EV"),
                                  washoutPeriod = 365)

results
misses <- attr(results, "misses")

#Evaluating xSpec cohort
results <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  phenotypeCohortId = xSpecCohortId,
                                  outFolder = evaluationCohortFolder,
                                  cutPoints = c(0.5, "EV"),
                                  washoutPeriod = 365)
results

#Evaluating xSens cohort
results <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  phenotypeCohortId = xSensCohortId,
                                  outFolder = evaluationCohortFolder,
                                  cutPoints = c(0.5, "EV"),
                                  washoutPeriod = 365)
results

connection <- connect(connectionDetails)
sql <- "SELECT * FROM @cohort_database_schema.@cohort_table WHERE cohort_definition_id = @cohort_id AND subject_id = @subject_id;"
renderTranslateQuerySql(connection, sql, cohort_database_schema = cohortDatabaseSchema, cohort_table = cohortTable, cohort_id = xSensCohortId, subject_id = 258869504)

misses <- attr(results, "misses")
misses[misses$subjectId ==  258869504, ]
cohort <- misses[misses$miss == "FN", c("subjectId", "cohortStartDate")]
cohort$cohortDefinitionId <- 999999
cohort$cohortEndDate <- cohort$cohortStartDate
cds <- "scratch.dbo"
ct <- "mschuemi_temp"
insertTable(connection = connection,
            tableName = paste(cds, ct, sep = "."),
            data = cohort,
            camelCaseToSnakeCase = TRUE,
            dropTableIfExists = TRUE,
            createTable = TRUE,
            tempTable = FALSE)
CohortDiagnostics::launchCohortExplorer(connectionDetails = connectionDetails,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortDatabaseSchema = cds,
                                        cohortTable = ct,
                                        cohortId = 999999)


x <- readRDS(file.path(evaluationCohortFolder, "model_main.rds"))
y <- x$model$varImp
View(y)
