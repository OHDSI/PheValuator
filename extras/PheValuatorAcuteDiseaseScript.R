library(PheValuator)
outFolder <- "d:/ProjectData/PheValuator/myocardialInfarctionV2" #replace with folder name

xSpecCohort <- 692 #replace with generated xSpec cohort ID
xSensCohort <- 693 #replace with generated xSens cohort ID
prevalenceCohort <- 691 #replace with generated prevalence cohort ID
evaluationPopulationCohortId <- 690 #replace with generated evaluation population cohort ID

# Create analysis settings ----------------------------------------------------------------------------------------
CovSettings <- createDefaultAcuteCovariateSettings(excludedCovariateConceptIds = c(), #add excluded concept ids if needed
                                                        addDescendantsToExclude = TRUE,
                                                        startDayWindow1 = 0, #change the feature windows if needed
                                                        endDayWindow1 = 10,
                                                        startDayWindow2 = 11,
                                                        endDayWindow2 = 20,
                                                        startDayWindow3 = 21,
                                                        endDayWindow3 = 30)

CohortArgs <- createCreateEvaluationCohortArgs(xSpecCohortId = xSpecCohort,
                                                    xSensCohortId = xSensCohort,
                                                    prevalenceCohortId = prevalenceCohort,
                                                    evaluationPopulationCohortId = evaluationPopulationCohortId,
                                                    covariateSettings = CovSettings,
                                                    lowerAgeLimit = 18,
                                                    upperAgeLimit = 120,
                                                    startDate = "20100101",
                                                    endDate = "21000101")

#################################
AlgTestArgs <- createTestPhenotypeAlgorithmArgs(phenotypeCohortId = xSpecCohort)

analysis1 <- createPheValuatorAnalysis(analysisId = 1,
                                       description = "xSpec",
                                       createEvaluationCohortArgs = CohortArgs,
                                       testPhenotypeAlgorithmArgs = AlgTestArgs)

#################################
AlgTestArgs <- createTestPhenotypeAlgorithmArgs(phenotypeCohortId = prevalenceCohort)

analysis2 <- createPheValuatorAnalysis(analysisId = 2,
                                       description = "Prevalence",
                                       createEvaluationCohortArgs = CohortArgs,
                                       testPhenotypeAlgorithmArgs = AlgTestArgs)

#################################
AlgTestArgs <- createTestPhenotypeAlgorithmArgs(phenotypeCohortId = 689) #change to first phenotype algorithm cohort ID to test

analysis3 <- createPheValuatorAnalysis(analysisId = 3,
                                       description = "[460] Chronic MI Inpatient 1st Position", #change to name of phenotype algorithm cohort
                                       createEvaluationCohortArgs = CohortArgs,
                                       testPhenotypeAlgorithmArgs = AlgTestArgs)

#################################
AlgTestArgs <- createTestPhenotypeAlgorithmArgs(phenotypeCohortId = 699) #change to second phenotype algorithm cohort ID to test

analysis4 <- createPheValuatorAnalysis(analysisId = 4,
                                       description = "[460] MI (for Cutrona)", #change to name as above
                                       createEvaluationCohortArgs = CohortArgs,
                                       testPhenotypeAlgorithmArgs = AlgTestArgs)

#can add/remove cohorts to test by copying/deleting createTestPhenotypeAlgorithmArgs() and createPheValuatorAnalysis() as above

pheValuatorAnalysisList <- list(analysis1, analysis2, analysis3, analysis4) #add/remove analyses

CCAE_OHDA_RSSpec <- list(databaseId = "CCAE_OHDA_RS", #change to your CDM of choice
                         cdmDatabaseSchema = "cdm_truven_ccae_v1479", 
                         cohortDatabaseSchema = "results_truven_ccae_v1479", 
                         cohortTable = "cohort", 
                         workDatabaseSchema = "scratch_jswerdel",
                         connectionDetails = createConnectionDetails(dbms = "redshift", 
                                                                     connectionString =  paste("jdbc:redshift://ohda-prod-1.cldcoxyrkflo.us-east-1.redshift.amazonaws.com:5439/truven_ccae?ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory", sep=""), 
                                                                     user = user, #change to your user name
                                                                     password = pword)) #change to your password


referenceTable <- runPheValuatorAnalyses(connectionDetails = CCAE_OHDA_RSSpec$connectionDetails,
                                         cdmDatabaseSchema = CCAE_OHDA_RSSpec$cdmDatabaseSchema,
                                         cohortDatabaseSchema = CCAE_OHDA_RSSpec$cohortDatabaseSchema,
                                         cohortTable = CCAE_OHDA_RSSpec$cohortTable,
                                         workDatabaseSchema = CCAE_OHDA_RSSpec$workDatabaseSchema,
                                         outputFolder = outFolder,
                                         pheValuatorAnalysisList = pheValuatorAnalysisList)

savePheValuatorAnalysisList(pheValuatorAnalysisList, file.path(outFolder, "pheValuatorAnalysisSettings.json"))

View(summarizePheValuatorAnalyses(referenceTable, outFolder), paste0("Results", CCAE_OHDA_RSSpec$databaseId))

