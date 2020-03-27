# Create analysis settings ----------------------------------------------------------------------------------------
pneumoniaCovSettingsChronic <- createDefaultChronicCovariateSettings(excludedCovariateConceptIds = c(255848),
                                                                     addDescendantsToExclude = TRUE)

pneunomoniaCohortArgsChronic <- createCreateEvaluationCohortArgs(xSpecCohortId = 14584,
                                                                 xSensCohortId = 8893,
                                                                 covariateSettings = pneumoniaCovSettingsChronic,
                                                                 baseSampleSize = 10000,
                                                                 modelType = "chronic")

pneumoniaAlg1TestArgs <- createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                          phenotypeCohortId = 14713)

analysis1 <- createPheValuatorAnalysis(analysisId = 1,
                                       description = "Pneumonia alg 1 as chronic outcome",
                                       createEvaluationCohortArgs = pneunomoniaCohortArgsChronic,
                                       testPhenotypeAlgorithmArgs = pneumoniaAlg1TestArgs)

pneumoniaCovSettingsAcute <- createDefaultAcuteCovariateSettings(excludedCovariateConceptIds = c(255848),
                                                                 addDescendantsToExclude = TRUE)

pneunomoniaCohortArgsAcute <- createCreateEvaluationCohortArgs(xSpecCohortId = 14584,
                                                               xSensCohortId = 8893,
                                                               covariateSettings = pneumoniaCovSettingsAcute,
                                                               baseSampleSize = 10000,
                                                               modelType = "acute")

analysis2 <- createPheValuatorAnalysis(analysisId = 2,
                                       description = "Pneumonia alg 1 as acute outcome",
                                       createEvaluationCohortArgs = pneunomoniaCohortArgsAcute,
                                       testPhenotypeAlgorithmArgs = pneumoniaAlg1TestArgs)

pneumoniaAlg2TestArgs <- createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                          phenotypeCohortId = 99999)

analysis3 <- createPheValuatorAnalysis(analysisId = 3,
                                       description = "Pneumonia alg 2 as chronic outcome",
                                       createEvaluationCohortArgs = pneunomoniaCohortArgsChronic,
                                       testPhenotypeAlgorithmArgs = pneumoniaAlg2TestArgs)

analysis4 <- createPheValuatorAnalysis(analysisId = 4,
                                       description = "Pneumonia alg 2 as acute outcome",
                                       createEvaluationCohortArgs = pneunomoniaCohortArgsAcute,
                                       testPhenotypeAlgorithmArgs = pneumoniaAlg2TestArgs)

pheValuatorAnalysisList <- list(analysis1, analysis2, analysis3, analysis4)
savePheValuatorAnalysisList(pheValuatorAnalysisList, file.path(folder, "pheValuatorAnalysisSettings.json"))

# Run saved analysis settings -------------------------------------------------------------------
library(PheValuator)

options(fftempdir = "s:/FFtemp")
connectionDetails <- createConnectionDetails(dbms = "pdw",
                                             server = Sys.getenv("PDW_SERVER"),
                                             port = Sys.getenv("PDW_PORT"))
oracleTempSchema <- NULL

folder <- "s:/temp/PheValuatorVignette2"

# MDCR settings
databaseId <- "MDCR"
cdmDatabaseSchema <- "CDM_IBM_MDCR_V1104.dbo"
cohortDatabaseSchema <- "CDM_IBM_MDCR_V1104.ohdsi_results"
cohortTable <- "cohort"
workDatabaseSchema <- "scratch.dbo"

pheValuatorAnalysisList <- loadPheValuatorAnalysisList(file.path(folder, "pheValuatorAnalysisSettings.json"))

referenceTable <- runPheValuatorAnalyses(connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         workDatabaseSchema = workDatabaseSchema,
                                         outputFolder = folder,
                                         pheValuatorAnalysisList = pheValuatorAnalysisList)

summarizePheValuatorAnalyses(referenceTable, folder)
# Cut Point Sensitivity Sensitivity (95% CI)   PPV         PPV (95% CI) Specificity Specificity (95% CI)   NPV         NPV (95% CI)
# 1 Expected Value       0.557 0.557 (0.529, 0.585) 0.525 0.525 (0.498, 0.552)       0.930 0.930 (0.925, 0.935) 0.938 0.938 (0.933, 0.943)
# 2 Expected Value       0.544 0.544 (0.516, 0.572) 0.527 0.527 (0.499, 0.555)       0.933 0.933 (0.928, 0.938) 0.937 0.937 (0.932, 0.942)
# 3             EV        <NA>                 <NA>  <NA>                 <NA>        <NA>                 <NA>  <NA>                 <NA>
#   4             EV        <NA>                 <NA>  <NA>                 <NA>        <NA>                 <NA>  <NA>                 <NA>
#   True Pos. False Pos. True Neg. False Neg. Estimated Prevalence F1 Score analysisId                        description
# 1       672        608      8142        535                12.12    0.541          1 Pneumonia alg 1 as chronic outcome
# 2       657        590      8173        550                12.11    0.535          2   Pneumonia alg 1 as acute outcome
# 3        NA         NA        NA         NA                   NA       NA          3 Pneumonia alg 2 as chronic outcome
# 4        NA         NA        NA         NA                   NA       NA          4   Pneumonia alg 2 as acute outcome
