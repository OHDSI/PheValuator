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

# Cut Point Sensitivity Sensitivity (95% CI)   PPV         PPV (95% CI) Specificity Specificity (95% CI)   NPV         NPV (95% CI) True Pos. False Pos. True Neg. False Neg. Estimated Prevalence F1 Score
# 1 EmpirCP0.5 (0.5)       0.129 0.129 (0.119, 0.139) 0.763 0.763 (0.731, 0.795)       0.972 0.972 (0.968, 0.976) 0.612 0.612 (0.602, 0.622)       522        162      5581       3532                41.38    0.221
# 2   Expected Value       0.122 0.122 (0.112, 0.132) 0.736 0.736 (0.703, 0.769)       0.968 0.968 (0.963, 0.973) 0.603 0.603 (0.593, 0.613)       504        180      5495       3618                42.07    0.209

#Evaluating xSpec cohort
results <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  phenotypeCohortId = xSpecCohortId,
                                  outFolder = evaluationCohortFolder,
                                  cutPoints = c(0.5, "EV"),
                                  washoutPeriod = 365)
results
# Cut Point Sensitivity Sensitivity (95% CI)   PPV         PPV (95% CI) Specificity Specificity (95% CI)   NPV         NPV (95% CI) True Pos. False Pos. True Neg. False Neg. Estimated Prevalence F1 Score
# 1 EmpirCP0.5 (0.5)       0.005 0.005 (0.003, 0.007) 0.978 0.978 (0.917, 1.039)       0.999 0.999 (0.998, 1.000) 0.587 0.587 (0.577, 0.597)        22          0      5743       4032                41.38     0.01
# 2   Expected Value       0.005 0.005 (0.003, 0.007) 0.917 0.917 (0.802, 1.032)       0.999 0.999 (0.998, 1.000) 0.580 0.580 (0.570, 0.590)        21          1      5673       4102                42.08     0.01

#Evaluating xSens cohort
results <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  phenotypeCohortId = xSensCohortId,
                                  outFolder = evaluationCohortFolder,
                                  cutPoints = c(0.5, "EV"),
                                  washoutPeriod = 365)
results
# Cut Point Sensitivity Sensitivity (95% CI)   PPV         PPV (95% CI) Specificity Specificity (95% CI)   NPV         NPV (95% CI) True Pos. False Pos. True Neg. False Neg. Estimated Prevalence F1 Score
# 1 EmpirCP0.5 (0.5)       0.075 0.075 (0.067, 0.083) 0.759 0.759 (0.717, 0.801)       0.983 0.983 (0.980, 0.986) 0.601 0.601 (0.591, 0.611)       304         96      5647       3750                41.38    0.137
# 2   Expected Value       0.071 0.071 (0.063, 0.079) 0.733 0.733 (0.690, 0.776)       0.981 0.981 (0.977, 0.985) 0.592 0.592 (0.582, 0.602)       294        106      5568       3829                42.08    0.129

x <- readRDS(file.path(evaluationCohortFolder, "model_main.rds"))
y <- x$model$varImp
View(y)
