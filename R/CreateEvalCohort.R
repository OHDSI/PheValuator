# @file createEvaluationCohort.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

#' createEvaluationCohort
#'
#' @description
#' Create the evaluation cohort
#'
#' @details
#' Creates the evaluation cohort and applies a diagnostic prediction model for determination of a
#' probability for the health outcome of interest
#'
#' @param connectionDetails      connectionDetails created using the function createConnectionDetails
#'                               in the DatabaseConnector package.
#' @param xSpecCohort            The number of the "extremely specific (xSpec)" cohort definition id in
#'                               the cohort table (for noisy positives)
#' @param cdmDatabaseSchema      The name of the database schema that contains the OMOP CDM instance.
#'                               Requires read permissions to this database. On SQL Server, this should
#'                               specifiy both the database and the schema, so for example
#'                               'cdm_instance.dbo'.
#' @param cohortDatabaseSchema   The name of the database schema that is the location where the cohort
#'                               data used to define the at risk cohort is available. Requires read
#'                               permissions to this database.
#' @param cohortDatabaseTable    The tablename that contains the at risk cohort. The expectation is
#'                               cohortTable has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                               COHORT_START_DATE, COHORT_END_DATE.
#' @param outDatabaseSchema      The name of the database schema that is the location where the data
#'                               used to define the outcome cohorts is available. Requires read
#'                               permissions to this database.
#' @param evaluationOutputFileName  A string designation for the evaluation cohort file
#' @param modelOutputFileName    A string designation for the training model file
#' @param mainPopulationCohort   The number of the cohort to be used as a base population for the model
#'                               (default=NULL)
#' @param lowerAgeLimit          The lower age for subjects in the model (default=NULL)
#' @param upperAgeLimit          The upper age for subjects in the model (default=NULL)
#' @param startDays              The days to include prior to the cohort start date (default=-10000)
#' @param endDays                The days to include after the cohort start date (default=10000)
#' @param gender                 The gender(s) to be included (default c(8507, 8532))
#' @param startDate              The starting date for including subjects in the model (default=NULL)
#' @param endDate                The ending date for including subjects in the model (default=NULL)
#' @param cdmVersion             The CDM version of the database (default=5)
#' @param outFolder              The folder where the output files will be written (default=working directory)
#'
#' @importFrom stats runif
#'
#' @export
createEvaluationCohort <- function(connectionDetails,
                             xSpecCohort,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             cohortDatabaseTable,
                             outDatabaseSchema,
                             evaluationOutputFileName,
                             modelOutputFileName,
                             mainPopulationCohort = 0,
                             lowerAgeLimit = 0,
                             upperAgeLimit = 120,
                             startDays = -10000,
                             endDays = 10000,
                             gender = c(8507, 8532),
                             startDate = "19001010",
                             endDate = "21000101",
                             cdmVersion = "5",
                             outFolder = getwd()) {

  options(error = NULL)

  # error checking for input
  if (length(connectionDetails) == 0)
    stop("...must supply a connection string")
  if (xSpecCohort == "")
    stop("...must have an xSpec cohort id (e.g., 1234)")
  if (cdmDatabaseSchema == "")
    stop("....must have a defined CDM schema (e.g., \"YourCDM.YourCDMSchema\")")
  if (cohortDatabaseSchema == "")
    stop("....must have a defined Cohort schema (e.g., \"YourCDM.YourCohortSchema\")")
  if (cohortDatabaseTable == "")
    stop("....must have a defined Cohort table (e.g., \"cohort\")")
  if (outDatabaseSchema == "")
    stop("....must have a defined Out Database schema (e.g., \"scratch.dbo\")")
  if (evaluationOutputFileName == "")
    stop("....must have a defined model file name (e.g., \"test_10XDiabetes\")")
  if (modelOutputFileName == "")
    stop("....must have a defined evaluation cohort file name (e.g., \"train_10XDiabetes\")")
  if (modelOutputFileName == evaluationOutputFileName)
    stop("....evaluationOutputFileName cannot be the same as the modelOutputFileName")

  writeLines(paste("xSpecCohort ", xSpecCohort))
  writeLines(paste("cdmDatabaseSchema ", cdmDatabaseSchema))
  writeLines(paste("cohortDatabaseSchema ", cohortDatabaseSchema))
  writeLines(paste("cohortDatabaseTable ", cohortDatabaseTable))
  writeLines(paste("outDatabaseSchema ", outDatabaseSchema))
  writeLines(paste("evaluationOutputFileName ", evaluationOutputFileName))
  writeLines(paste("modelOutputFileName ", modelOutputFileName))
  writeLines(paste("mainPopulationCohort ", mainPopulationCohort))
  writeLines(paste("lowerAgeLimit ", lowerAgeLimit))
  writeLines(paste("upperAgeLimit ", upperAgeLimit))
  writeLines(paste("startDays ", startDays))
  writeLines(paste("endDays ", endDays))
  writeLines(paste("gender ", gender))
  writeLines(paste("startDate ", startDate))
  writeLines(paste("endDate ", endDate))
  writeLines(paste("cdmVersion ", cdmVersion))
  writeLines(paste("outFolder ", outFolder))

  workFolder <- outFolder

  conn <- DatabaseConnector::connect(connectionDetails)

  # create the evaluation cohort
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "CreateCohortsV6.sql",
                                              package = "PheValuator"))

  test_cohort <- gsub(".",
                      "",
                      (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                      fixed = TRUE)  #unique new cohort name to use

  sql <- SqlRender::render(sqlScript,
                              cdm_database_schema = cdmDatabaseSchema,
                              cohort_database_schema = cohortDatabaseSchema,
                              cohort_database_table = cohortDatabaseTable,
                              x_spec_cohort = xSpecCohort,
                              tempDB = outDatabaseSchema,
                              test_cohort = test_cohort,
                              exclCohort = 0,
                              ageLimit = lowerAgeLimit,
                              upperAgeLimit = upperAgeLimit,
                              gender = gender,
                              startDate = startDate,
                              endDate = endDate,
                              baseSampleSize = 2e+06,
                              xSpecSampleSize = 100,
                              mainPopnCohort = mainPopulationCohort,
                              lookback = 0)  #when applying the model start from the first visit for all subjects

  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(conn = conn, sql)

  # determine the model file to use to apply to the evaluation cohort
  resultsFileName <- file.path(workFolder, paste(modelOutputFileName,
                                                 ".rds",
                                                 sep = ""))
  writeLines(paste("\n...reading ", resultsFileName, sep = ""))

  if (!file.exists(resultsFileName))
    stop(paste(".....Model Output file (", resultsFileName, ") does not exist"))

  lr_results <- readRDS(resultsFileName)

  # will only use the covariates with non-zero betas
  lrNonZeroCovs <- c(lr_results$model$varImp$covariateId[lr_results$model$varImp$covariateValue !=
    0])
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                  useDemographicsAgeGroup = TRUE,
                                                                  useDemographicsRace = TRUE,
                                                                  useDemographicsEthnicity = TRUE,
                                                                  useDemographicsPostObservationTime = TRUE,
                                                                  useConditionOccurrenceLongTerm = TRUE,
                                                                  useConditionOccurrencePrimaryInpatientLongTerm = TRUE,
                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                  useDrugExposureLongTerm = TRUE,
                                                                  useDrugEraLongTerm = TRUE,
                                                                  useDrugGroupEraLongTerm = TRUE,
                                                                  useProcedureOccurrenceLongTerm = TRUE,
                                                                  useDeviceExposureLongTerm = TRUE,
                                                                  useMeasurementLongTerm = TRUE,
                                                                  useMeasurementValueLongTerm = TRUE,
                                                                  useMeasurementRangeGroupLongTerm = TRUE,
                                                                  useObservationLongTerm = TRUE,
                                                                  useDistinctConditionCountLongTerm = TRUE,
                                                                  useDistinctIngredientCountLongTerm = TRUE,
                                                                  useDistinctProcedureCountLongTerm = TRUE,
                                                                  useDistinctMeasurementCountLongTerm = TRUE,
                                                                  useVisitCountLongTerm = TRUE,
                                                                  useVisitConceptCountLongTerm = TRUE,
                                                                  longTermStartDays = startDays,
                                                                  endDays = endDays,
                                                                  includedCovariateConceptIds = c(),
                                                                  addDescendantsToInclude = TRUE,
                                                                  excludedCovariateConceptIds = c(),
                                                                  addDescendantsToExclude = TRUE,
                                                                  includedCovariateIds = c())

  covariateSettings$includedCovariateIds <- lrNonZeroCovs

  plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                cdmDatabaseSchema = paste(cdmDatabaseSchema,
                                                                          sep = ""),
                                                cohortId = 0,
                                                outcomeIds = xSpecCohort,
                                                outcomeDatabaseSchema = outDatabaseSchema,
                                                outcomeTable = test_cohort,
                                                cohortDatabaseSchema = outDatabaseSchema,
                                                cohortTable = test_cohort,
                                                cdmVersion = cdmVersion,
                                                washoutPeriod = 0,
                                                covariateSettings = covariateSettings)

  summary(plpData)

  population <- PatientLevelPrediction::createStudyPopulation(plpData,
                                                              population = NULL,
                                                              outcomeId = xSpecCohort,
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
  resultsFileName <- file.path(workFolder, paste(evaluationOutputFileName,
                                                 ".rds",
                                                 sep = ""))

  writeLines(paste0("\nSaving PLP Evaluation Results to: ",resultsFileName))


  # save the plp data - to be used for phenotype evaluation
  saveRDS(appResults, resultsFileName)

  # remove temp cohort table
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "DropTempTable.sql",
                                              package = "PheValuator"))
  sql <- SqlRender::render(sqlScript, tempDB = outDatabaseSchema, test_cohort = test_cohort)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(conn = conn, sql)

  DatabaseConnector::disconnect(conn)
}
