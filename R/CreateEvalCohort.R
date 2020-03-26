# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of PheValuator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.createEvaluationCohort <- function(connectionDetails,
                                    xSpecCohortId,
                                    xSensCohortId,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    workDatabaseSchema,
                                    covariateSettings,
                                    mainPopulationCohortId = 0,
                                    mainPopulationCohortIdStartDay = 0,
                                    mainPopulationCohortIdEndDay = 0,
                                    baseSampleSize = 2e+06,
                                    lowerAgeLimit = 0,
                                    upperAgeLimit = 120,
                                    visitLength = 3,
                                    gender = c(8507, 8532),
                                    startDate = "19001010",
                                    endDate = "21000101",
                                    cdmVersion = "5",
                                    outFolder = getwd(),
                                    modelId = "main",
                                    evaluationCohortId = "main",
                                    excludeModelFromEvaluation = TRUE,
                                    savePlpData = FALSE,
                                    modelType = "chronic") {

  if (savePlpData == TRUE) {
    evaluationCohortPlpDataFileName <- file.path(outFolder, sprintf("evaluationCohortPlpData_%s", evaluationCohortId))
    if (file.exists(evaluationCohortPlpDataFileName))
      stop("The savePlpData argument is set to TRUE, but ", evaluationCohortPlpDataFileName, " already exists")
  }
  evaluationCohortFileName <- file.path(outFolder, sprintf("evaluationCohort_%s.rds", evaluationCohortId))
  if (file.exists(evaluationCohortFileName)) {
    ParallelLogger::logInfo("Skipping creation of ", evaluationCohortFileName, " because it already exists")
    return()
  }

  # determine the model file to use to apply to the evaluation cohort
  modelFileName <- file.path(outFolder, sprintf("model%s.rds", modelId))
  ParallelLogger::logInfo("Reading model from ", modelFileName)
  if (!file.exists(modelFileName))
    stop("Model output file (", modelFileName, ") does not exist")
  lrResults <- readRDS(modelFileName)

  # get subjects used in model file to exclude from evaluation cohort
  exclSubjectList <- c(lrResults$prediction$subjectId)

  testCohort <- gsub(".",
                     "",
                     (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                     fixed = TRUE)  #unique new cohort name to use

  connection <- DatabaseConnector::connect(connectionDetails)

  if (modelType == "acute") {
    sqlFilename <- "CreateCohortsAcuteEvaluation.sql"
  } else {
    sqlFilename <- "CreateCohortsV6.sql"
  }
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = sqlFilename,
                                           packageName = "PheValuator",
                                           dbms = connection@dbms,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_database_table = cohortTable,
                                           x_spec_cohort = xSpecCohortId,
                                           tempDB = workDatabaseSchema,
                                           test_cohort = testCohort,
                                           exclCohort = 0,
                                           ageLimit = lowerAgeLimit,
                                           upperAgeLimit = upperAgeLimit,
                                           gender = gender,
                                           startDate = startDate,
                                           endDate = endDate,
                                           baseSampleSize = format(baseSampleSize, scientific = FALSE),
                                           xSpecSampleSize = 100,
                                           mainPopnCohort = mainPopulationCohortId,
                                           mainPopnCohortStartDay = mainPopulationCohortIdStartDay,
                                           mainPopnCohortEndDay = mainPopulationCohortIdEndDay,
                                           visitLength = visitLength)
  ParallelLogger::logInfo("Creating evaluation cohort on server")
  DatabaseConnector::executeSql(connection = connection, sql)

  DatabaseConnector::disconnect(connection)

  # will only use the covariates with non-zero betas
  lrNonZeroCovs <- c(lrResults$model$varImp$covariateId[lrResults$model$varImp$covariateValue != 0])
  if (is(covariateSettings, "covariateSettings"))
    covariateSettings <- list(covariateSettings)
  for (listUp in 1:length(covariateSettings)) {
    covariateSettings[[listUp]]$includedCovariateIds <- c(lrNonZeroCovs)
  }

  plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                cohortId = 0,
                                                outcomeIds = xSpecCohortId,
                                                outcomeDatabaseSchema = workDatabaseSchema,
                                                outcomeTable = testCohort,
                                                cohortDatabaseSchema = workDatabaseSchema,
                                                cohortTable = testCohort,
                                                cdmVersion = cdmVersion,
                                                washoutPeriod = 0,
                                                covariateSettings = covariateSettings)

  if (excludeModelFromEvaluation == TRUE) {
    # remove subjects in evaluation cohort that were in model cohort
    excl <- data.frame(plpData$cohorts$rowId[plpData$cohorts$subjectId %in% c(exclSubjectList)])
    xSpec <- c(plpData$outcomes$rowId)  #those with outcome need to be left in
    excl <- subset(excl, !(excl[, 1] %in% c(xSpec)))
    plpData$cohorts <- plpData$cohorts[!(plpData$cohorts$rowId %in% c(excl[, 1])), ]
  }

  if (savePlpData == TRUE) {
    ParallelLogger::logInfo("Saving evaluation cohort PLP data to: ", evaluationCohortPlpDataFileName)
    PatientLevelPrediction::savePlpData(plpData, evaluationCohortPlpDataFileName)
  }

  population <- PatientLevelPrediction::createStudyPopulation(plpData,
                                                              population = NULL,
                                                              outcomeId = xSpecCohortId,
                                                              firstExposureOnly = FALSE,
                                                              washoutPeriod = 0,
                                                              removeSubjectsWithPriorOutcome = TRUE,
                                                              priorOutcomeLookback = 1,
                                                              riskWindowStart = 0,
                                                              requireTimeAtRisk = FALSE,
                                                              minTimeAtRisk = 0,
                                                              addExposureDaysToStart = FALSE,
                                                              riskWindowEnd = 1,
                                                              addExposureDaysToEnd = T)

  # apply the model to the evaluation cohort
  appResults <- PatientLevelPrediction::applyModel(population, plpData, lrResults$model)
  pred <- appResults$prediction

  # pull in the xSens cohort
  sql <- "SELECT subject_id,
    cohort_start_date,
    observation_period_start_date
  FROM @cohort_database_schema.@cohort_table
  JOIN @cdm_database_schema.observation_period
    ON subject_id = person_id
      AND cohort_start_date >= observation_period_start_date
      AND cohort_start_date <= observation_period_end_date
  WHERE cohort_definition_id = @cohort_id;"
  sql <- SqlRender::render(sql = sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_id = xSensCohortId)
  sql <- SqlRender::translate(sql, connection@dbms)
  xSensPopn <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
  # add the start dates from the xSens cohort to the evaluation cohort to be able to apply washout
  # criteria during evaluation
  finalPopn <- merge(pred, xSensPopn, all.x = TRUE)
  finalPopn$daysToXSens <- as.integer(finalPopn$cohortStartDate - finalPopn$observationPeriodStartDate)

  # add other parameters to the input settings list
  appResults$PheValuator$inputSetting$startDays <- covariateSettings[[1]]$longTermStartDays
  appResults$PheValuator$inputSetting$endDays <- covariateSettings[[1]]$endDays
  appResults$PheValuator$inputSetting$visitLength <- visitLength
  appResults$PheValuator$inputSetting$xSpecCohortId <- xSpecCohortId
  appResults$PheValuator$inputSetting$xSensCohortId <- xSensCohortId
  appResults$PheValuator$inputSetting$lowerAgeLimit <- lowerAgeLimit
  appResults$PheValuator$inputSetting$upperAgeLimit <- upperAgeLimit
  appResults$PheValuator$inputSetting$gender <- paste(unlist(gender), collapse = ", ")
  appResults$PheValuator$inputSetting$startDate <- startDate
  appResults$PheValuator$inputSetting$endDate <- endDate
  appResults$PheValuator$inputSetting$mainPopulationCohortId <- mainPopulationCohortId
  appResults$PheValuator$inputSetting$modelType <- modelType
  appResults$PheValuator$inputSetting$excludeModelFromEvaluation <- excludeModelFromEvaluation

  appResults$PheValuator$modelperformanceEvaluation <- lrResults$performanceEvaluation

  # save the full data set to the model
  appResults$prediction <- finalPopn

  ParallelLogger::logInfo("Saving evaluation cohort to: ", evaluationCohortFileName)
  saveRDS(appResults, evaluationCohortFileName)

  # remove temp cohort table
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "DropTempTable.sql",
                                           packageName = "PheValuator",
                                           dbms = connection@dbms,
                                           tempDB = workDatabaseSchema,
                                           test_cohort = testCohort)
  DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = FALSE, reportOverallTime = FALSE)
}
