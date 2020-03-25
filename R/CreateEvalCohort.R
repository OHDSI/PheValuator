# @file createEvaluationCohort.R
#
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

#' Create the evaluation cohort
#'
#' @description
#' Create the evaluation cohort
#'
#' @details
#' Creates the evaluation cohort and applies a diagnostic prediction model for determination of a
#' probability for the health outcome of interest
#'
#' @param connectionDetails                connectionDetails created using the function
#'                                         createConnectionDetails in the DatabaseConnector package
#' @param xSpecCohortId                    The number of the "extremely specific (xSpec)" cohort
#'                                         definition id in the cohort table (for noisy positives)
#' @param xSensCohortId                    The number of the "extremely sensitive (xSens)" cohort
#'                                         definition id in the cohort table (for noisy negatives)
#' @param cdmDatabaseSchema                The name of the database schema that contains the OMOP CDM
#'                                         instance. Requires read permissions to this database. On SQL
#'                                         Server, this should specifiy both the database and the
#'                                         schema, so for example 'cdm_instance.dbo'.
#' @param cohortDatabaseSchema             The name of the database schema that is the location where
#'                                         the cohort data used to define the at risk cohort is
#'                                         available. Requires read permissions to this database.
#' @param cohortTable                      The tablename that contains the at risk cohort. The
#'                                         expectation is cohortTable has format of COHORT table:
#'                                         cohort_concept_id, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param workDatabaseSchema               The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available.
#'                                         Requires read permissions to this database.
#' @param covariateSettings                A covariateSettings object as generated using
#'                                         createCovariateSettings()
#' @param evaluationOutputFileName         A string designation for the evaluation cohort file
#' @param modelOutputFileName              A string designation for the training model file
#' @param mainPopulationCohortId           The number of the cohort to be used as a base population for
#'                                         the model (default=NULL)
#' @param mainPopulationCohortIdStartDay   The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to begin including visits (default=0)
#' @param mainPopulationCohortIdEndDay     The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to end including visits (default=0)
#' @param baseSampleSize                   The maximum number of subjects in the evaluation cohort
#'                                         (default=2M)
#' @param lowerAgeLimit                    The lower age for subjects in the model (default=NULL)
#' @param upperAgeLimit                    The upper age for subjects in the model (default=NULL)
#' @param visitLength                      The minimum length of index visit (default=3)
#' @param gender                           The gender(s) to be included (default c(8507, 8532))
#' @param startDate                        The starting date for including subjects in the model
#'                                         (default=NULL)
#' @param endDate                          The ending date for including subjects in the model
#'                                         (default=NULL)
#' @param cdmVersion                       The CDM version of the database (default=5)
#' @param outFolder                        The folder where the output files will be written
#'                                         (default=working directory)
#' @param excludeModelFromEvaluation       Should subjects used in the model be excluded from the
#'                                         evaluation cohort (default = TRUE)
#' @param savePlpData                      Should large PLP data file be saved (default=FALSE)
#' @param modelType                        The type of health outcome in the model either "acute" or
#'                                         "chronic" (Default = "chronic")
#'
#' @importFrom stats runif
#'
#' @export
createEvaluationCohort <- function(connectionDetails,
                                   xSpecCohortId,
                                   xSensCohortId,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   workDatabaseSchema,
                                   covariateSettings = covariateSettings,
                                   evaluationOutputFileName,
                                   modelOutputFileName,
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
                                   excludeModelFromEvaluation = TRUE,
                                   savePlpData = FALSE,
                                   modelType = "chronic") {

  options(error = NULL)
  options(scipen = 999)

  # error checking for input
  if (modelType != "chronic" & modelType != "acute")
    stop("...modelType must be acute or chronic")
  if (length(connectionDetails) == 0)
    stop("...must supply a connection string")
  if (xSpecCohortId == "")
    stop("...must have an xSpec cohort id (e.g., 1234)")
  if (xSensCohortId == "")
    stop("...must have an xSens cohort id (e.g., 1234)")
  if (cdmDatabaseSchema == "")
    stop("....must have a defined CDM schema (e.g., \"YourCDM.YourCDMSchema\")")
  if (cohortDatabaseSchema == "")
    stop("....must have a defined Cohort schema (e.g., \"YourCDM.YourCohortSchema\")")
  if (cohortTable == "")
    stop("....must have a defined Cohort table (e.g., \"cohort\")")
  if (workDatabaseSchema == "")
    stop("....must have a defined Out Database schema (e.g., \"scratch.dbo\")")
  if (evaluationOutputFileName == "")
    stop("....must have a defined model file name (e.g., \"test_10XDiabetes\")")
  if (modelOutputFileName == "")
    stop("....must have a defined evaluation cohort file name (e.g., \"train_10XDiabetes\")")
  if (modelOutputFileName == evaluationOutputFileName)
    stop("....evaluationOutputFileName cannot be the same as the modelOutputFileName")


  workFolder <- outFolder

  conn <- DatabaseConnector::connect(connectionDetails)

  # determine the model file to use to apply to the evaluation cohort
  resultsFileName <- file.path(workFolder, paste(modelOutputFileName, ".rds", sep = ""))
  writeLines(paste("\n...reading ", resultsFileName, sep = ""))

  if (!file.exists(resultsFileName))
    stop(paste(".....Model Output file (", resultsFileName, ") does not exist"))

  lr_results <- readRDS(resultsFileName)

  # get subjects used in model file to exclude from evaluation cohort
  exclSubjectList <- c(lr_results$prediction$subjectId)

  if (modelType == "acute") {
    # create the evaluation cohort
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "CreateCohortsAcuteEvaluation.sql",
                                                package = "PheValuator"))
  } else {
    # create the evaluation cohort
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "CreateCohortsV6.sql",
                                                package = "PheValuator"))
  }

  test_cohort <- gsub(".",
                      "",
                      (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                      fixed = TRUE)  #unique new cohort name to use

  sql <- SqlRender::render(sqlScript,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_database_table = cohortTable,
                           x_spec_cohort = xSpecCohortId,
                           tempDB = workDatabaseSchema,
                           test_cohort = test_cohort,
                           exclCohort = 0,
                           ageLimit = lowerAgeLimit,
                           upperAgeLimit = upperAgeLimit,
                           gender = gender,
                           startDate = startDate,
                           endDate = endDate,
                           baseSampleSize = baseSampleSize,
                           xSpecSampleSize = 100,
                           mainPopnCohort = mainPopulationCohortId,
                           mainPopnCohortStartDay = mainPopulationCohortIdStartDay,
                           mainPopnCohortEndDay = mainPopulationCohortIdEndDay,
                           visitLength = visitLength)

  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(connection = conn, sql)

  # will only use the covariates with non-zero betas
  lrNonZeroCovs <- c(lr_results$model$varImp$covariateId[lr_results$model$varImp$covariateValue !=
    0])
  if (is(covariateSettings, "covariateSettings"))
    covariateSettings <- list(covariateSettings)

  for (listUp in 1:length(covariateSettings)) {
    covariateSettings[[listUp]]$includedCovariateIds <- c(lrNonZeroCovs)
  }

  plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                cdmDatabaseSchema = paste(cdmDatabaseSchema,
                                                                          sep = ""),
                                                cohortId = 0,
                                                outcomeIds = xSpecCohortId,
                                                outcomeDatabaseSchema = workDatabaseSchema,
                                                outcomeTable = test_cohort,
                                                cohortDatabaseSchema = workDatabaseSchema,
                                                cohortTable = test_cohort,
                                                cdmVersion = cdmVersion,
                                                washoutPeriod = 0,
                                                covariateSettings = covariateSettings)

  summary(plpData)

  if (excludeModelFromEvaluation == TRUE) {
    # remove subjects in evaluation cohort that were in model cohort
    excl <- data.frame(plpData$cohorts$rowId[plpData$cohorts$subjectId %in% c(exclSubjectList)])
    xSpec <- c(plpData$outcomes$rowId)  #those with outcome need to be left in
    excl <- subset(excl, !(excl[, 1] %in% c(xSpec)))

    plpData$cohorts <- plpData$cohorts[!(plpData$cohorts$rowId %in% c(excl[, 1])), ]
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
  appResults <- PatientLevelPrediction::applyModel(population, plpData, lr_results$model)

  pred <- appResults$prediction

  # pull in the xSens cohort
  sql <- paste0("select subject_id, cohort_start_date, observation_period_start_date ",
                "from ",
                cohortDatabaseSchema,
                ".",
                cohortTable,
                " co ",
                "join ",
                cdmDatabaseSchema,
                ".observation_period op ",
                "on co.subject_id = op.person_id ",
                "and cohort_start_date between observation_period_start_date and observation_period_end_date ",
                "where cohort_definition_id = ",
                xSensCohortId)

  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  # add the start dates from the xSens cohort to the evaluation cohort to be able to apply washout
  # criteria during evaluation
  xSensPopn <- DatabaseConnector::querySql(connection = conn, sql)
  finalPopn <- merge(pred, xSensPopn, by.x = "subjectId", by.y = "SUBJECT_ID", all.x = TRUE)
  finalPopn$daysToXSens <- as.integer(finalPopn$COHORT_START_DATE - finalPopn$OBSERVATION_PERIOD_START_DATE)

  # add other parameters to the input settings list
  appResults$PheValuator$inputSetting$startDays <- covariateSettings$longTermStartDays
  appResults$PheValuator$inputSetting$endDays <- covariateSettings$endDays
  appResults$PheValuator$inputSetting$visitLength <- visitLength
  appResults$PheValuator$inputSetting$xSpecCohortId <- xSpecCohortId
  appResults$PheValuator$inputSetting$xSensCohortId <- xSensCohortId
  appResults$PheValuator$inputSetting$lowerAgeLimit <- lowerAgeLimit
  appResults$PheValuator$inputSetting$upperAgeLimit <- upperAgeLimit
  appResults$PheValuator$inputSetting$gender <- paste(unlist(gender), collapse = ", ")
  appResults$PheValuator$inputSetting$startDate <- startDate
  appResults$PheValuator$inputSetting$endDate <- endDate
  appResults$PheValuator$inputSetting$modelOutputFileName <- modelOutputFileName
  appResults$PheValuator$inputSetting$mainPopulationCohortId <- mainPopulationCohortId
  appResults$PheValuator$inputSetting$modelType <- modelType
  appResults$PheValuator$inputSetting$excludeModelFromEvaluation <- excludeModelFromEvaluation

  appResults$PheValuator$modelperformanceEvaluation <- lr_results$performanceEvaluation

  # save the full data set to the model
  appResults$prediction <- finalPopn

  resultsFileName <- file.path(workFolder, paste(evaluationOutputFileName, ".rds", sep = ""))

  writeLines(paste0("\nSaving PLP Evaluation Results to: ", resultsFileName))


  # save the plp data - to be used for phenotype evaluation
  saveRDS(appResults, resultsFileName)

  if (savePlpData == TRUE) {
    plpDataFileName <- file.path(workFolder, paste(evaluationOutputFileName, sep = ""))

    writeLines(paste0("\nSaving PLP Data to: ", plpDataFileName))
    savePlpData(plpData, plpDataFileName)
  }

  # remove temp cohort table
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "DropTempTable.sql",
                                              package = "PheValuator"))
  sql <- SqlRender::render(sqlScript, tempDB = workDatabaseSchema, test_cohort = test_cohort)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  capture.output(conn <- DatabaseConnector::connect(connectionDetails), file = NULL)
  DatabaseConnector::executeSql(connection = conn, sql)

  DatabaseConnector::disconnect(conn)
}
