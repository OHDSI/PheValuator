

createPheValuatorAnalysisSettings <- function(outputFileName) {
  
  pneumoniaCovSettingsAcute <- PheValuator::createDefaultAcuteCovariateSettings(excludedCovariateConceptIds = c(45770911,
                                                                                                                4001167,
                                                                                                                4050869,
                                                                                                                4049965,
                                                                                                                36712839,
                                                                                                                252552,
                                                                                                                255848),
                                                                                addDescendantsToExclude = TRUE)
  
  ##########################
  pneumoniaCovSettingsAcute[[1]]$longTermStartDays <- -7
  pneumoniaCovSettingsAcute[[1]]$endDays <- -1
  
  pneumoniaCovSettingsAcute[[2]]$shortTermStartDays <- 0
  pneumoniaCovSettingsAcute[[2]]$endDays <- 5
  
  pneumoniaCovSettingsAcute[[3]]$mediumTermStartDays <- 6
  pneumoniaCovSettingsAcute[[3]]$endDays <- 30
  
  #########################
  pneumoniaLevel1xSpecCohortArgsAcute <- PheValuator::createCreateEvaluationCohortArgs(xSpecCohortId = 14599,
                                                                                       xSensCohortId = 15198,
                                                                                       covariateSettings = pneumoniaCovSettingsAcute,
                                                                                       baseSampleSize = 2000000,
                                                                                       lowerAgeLimit = 18,
                                                                                       visitType = c(9201),
                                                                                       visitLength = 0,
                                                                                       startDate = "20100101",
                                                                                       endDate = "21100101",
                                                                                       modelType = "acute")
  
  pneumoniaLevel2xSpecCohortArgsAcute <- PheValuator::createCreateEvaluationCohortArgs(xSpecCohortId = 15215,
                                                                                       xSensCohortId = 15198,
                                                                                       covariateSettings = pneumoniaCovSettingsAcute,
                                                                                       baseSampleSize = 2000000,
                                                                                       lowerAgeLimit = 18,
                                                                                       visitType = c(9201),
                                                                                       visitLength = 0,
                                                                                       startDate = "20100101",
                                                                                       endDate = "21100101",
                                                                                       modelType = "acute")
  
  pneumoniaLevel3xSpecCohortArgsAcute <- PheValuator::createCreateEvaluationCohortArgs(xSpecCohortId = 15179,
                                                                                       xSensCohortId = 15198,
                                                                                       covariateSettings = pneumoniaCovSettingsAcute,
                                                                                       baseSampleSize = 2000000,
                                                                                       lowerAgeLimit = 18,
                                                                                       visitType = c(9201, 9202, 581477),
                                                                                       visitLength = 0,
                                                                                       startDate = "20100101",
                                                                                       endDate = "21100101",
                                                                                       modelType = "acute")
  
  #############################
  pneumoniaLevel1xSpecTestArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                                                phenotypeCohortId = 14599,
                                                                                washoutPeriod = 0)
  
  pneumoniaLevel2xSpecTestArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                                                phenotypeCohortId = 15215,
                                                                                washoutPeriod = 0)
  
  pneumoniaLevel3xSpecTestArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                                                phenotypeCohortId = 15179,
                                                                                washoutPeriod = 0)
  
  pneumoniaxSensTestArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                                          phenotypeCohortId = 15198,
                                                                          washoutPeriod = 0)
  
  pneumoniaAllOccTestArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(cutPoints = "EV",
                                                                           phenotypeCohortId = 15217,
                                                                           washoutPeriod = 0)
  
  ###########################
  analysis1 <- PheValuator::createPheValuatorAnalysis(analysisId = 1,
                                                      description = "Model: Pneumonia Level 1 xSpec, PA:Level 1 xSpec",
                                                      createEvaluationCohortArgs = pneumoniaLevel1xSpecCohortArgsAcute,
                                                      testPhenotypeAlgorithmArgs = pneumoniaLevel1xSpecTestArgs)
  
  
  analysis2 <- PheValuator::createPheValuatorAnalysis(analysisId = 2,
                                                      description = "Model: Pneumonia Level 1 xSpec, PA:Level 2 xSpec",
                                                      createEvaluationCohortArgs = pneumoniaLevel1xSpecCohortArgsAcute,
                                                      testPhenotypeAlgorithmArgs = pneumoniaLevel2xSpecTestArgs)
  
  analysis3 <- PheValuator::createPheValuatorAnalysis(analysisId = 3,
                                                      description = "Model: Pneumonia Level 1 xSpec, PA:Level 3 xSpec",
                                                      createEvaluationCohortArgs = pneumoniaLevel1xSpecCohortArgsAcute,
                                                      testPhenotypeAlgorithmArgs = pneumoniaLevel3xSpecTestArgs)
  
  analysis4 <- PheValuator::createPheValuatorAnalysis(analysisId = 4,
                                                      description = "Model: Pneumonia Level 1 xSpec, PA:xSens",
                                                      createEvaluationCohortArgs = pneumoniaLevel1xSpecCohortArgsAcute,
                                                      testPhenotypeAlgorithmArgs = pneumoniaxSensTestArgs)
  
  analysis5 <- PheValuator::createPheValuatorAnalysis(analysisId = 5,
                                                      description = "Model: Pneumonia Level 1 xSpec, PA:All Occurrences",
                                                      createEvaluationCohortArgs = pneumoniaLevel1xSpecCohortArgsAcute,
                                                      testPhenotypeAlgorithmArgs = pneumoniaAllOccTestArgs)
  
  ###########################
  analysis11 <- PheValuator::createPheValuatorAnalysis(analysisId = 11,
                                                       description = "Model: Pneumonia Level 2 xSpec, PA:Level 1 xSpec",
                                                       createEvaluationCohortArgs = pneumoniaLevel2xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaLevel1xSpecTestArgs)
  
  
  analysis21 <- PheValuator::createPheValuatorAnalysis(analysisId = 21,
                                                       description = "Model: Pneumonia Level 2 xSpec, PA:Level 2 xSpec",
                                                       createEvaluationCohortArgs = pneumoniaLevel2xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaLevel2xSpecTestArgs)
  
  analysis31 <- PheValuator::createPheValuatorAnalysis(analysisId = 31,
                                                       description = "Model: Pneumonia Level 2 xSpec, PA:Level 3 xSpec",
                                                       createEvaluationCohortArgs = pneumoniaLevel2xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaLevel3xSpecTestArgs)
  
  analysis41 <- PheValuator::createPheValuatorAnalysis(analysisId = 41,
                                                       description = "Model: Pneumonia Level 2 xSpec, PA:xSens",
                                                       createEvaluationCohortArgs = pneumoniaLevel2xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaxSensTestArgs)
  
  analysis51 <- PheValuator::createPheValuatorAnalysis(analysisId = 51,
                                                       description = "Model: Pneumonia Level 2 xSpec, PA:All Occurrences",
                                                       createEvaluationCohortArgs = pneumoniaLevel2xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaAllOccTestArgs)
  
  ###########################
  analysis12 <- PheValuator::createPheValuatorAnalysis(analysisId = 12,
                                                       description = "Model: Pneumonia Level 3 xSpec, PA:Level 1 xSpec",
                                                       createEvaluationCohortArgs = pneumoniaLevel3xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaLevel1xSpecTestArgs)
  
  
  analysis22 <- PheValuator::createPheValuatorAnalysis(analysisId = 22,
                                                       description = "Model: Pneumonia Level 3 xSpec, PA:Level 2 xSpec",
                                                       createEvaluationCohortArgs = pneumoniaLevel3xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaLevel2xSpecTestArgs)
  
  analysis32 <- PheValuator::createPheValuatorAnalysis(analysisId = 32,
                                                       description = "Model: Pneumonia Level 3 xSpec, PA:Level 3 xSpec",
                                                       createEvaluationCohortArgs = pneumoniaLevel3xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaLevel3xSpecTestArgs)
  
  analysis42 <- PheValuator::createPheValuatorAnalysis(analysisId = 42,
                                                       description = "Model: Pneumonia Level 3 xSpec, PA:xSens",
                                                       createEvaluationCohortArgs = pneumoniaLevel3xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaxSensTestArgs)
  
  analysis52 <- PheValuator::createPheValuatorAnalysis(analysisId = 52,
                                                       description = "Model: Pneumonia Level 3 xSpec, PA:All Occurrences",
                                                       createEvaluationCohortArgs = pneumoniaLevel3xSpecCohortArgsAcute,
                                                       testPhenotypeAlgorithmArgs = pneumoniaAllOccTestArgs)
  ##############################
  pheValuatorAnalysisList <- list(analysis1, analysis2, analysis3, analysis4, analysis5,
                                  analysis11, analysis21, analysis31, analysis41, analysis51,
                                  analysis12, analysis22, analysis32, analysis42, analysis52)
  
  PheValuator::savePheValuatorAnalysisList(pheValuatorAnalysisList, outputFileName)
}