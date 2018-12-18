# @file createEvalCohort.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' createEvalCohort
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
#' @param testOutFile            A string designation for the evaluation cohort file
#' @param trainOutFile           A string designation for the training model file
#' @param estPPV                 A value between 0 and 1 as an estimate for the positive predictive
#'                               value for the exclCohort
#' @param modelAnalysisId        The string previously used for designating the name for the model
#'                               files
#' @param evalAnalysisId         Another string for designating the name for the evaluation files
#' @param cdmShortName           A short name for the current database (CDM)
#' @param mainPopnCohort         The number of the cohort to be used as a base population for the model
#'                               (default=NULL)
#' @param lowerAgeLimit          The lower age for subjects in the model (default=NULL)
#' @param upperAgeLimit          The upper age for subjects in the model (default=NULL)
#' @param startDate              The starting date for including subjects in the model (default=NULL)
#' @param endDate                The ending date for including subjects in the model (default=NULL)
#'
#' @importFrom stats runif
#'
#' @export
createEvalCohort <- function(connectionDetails = list(),
                             xSpecCohort = "",
                             cdmDatabaseSchema = "",
                             cohortDatabaseSchema = "",
                             cohortDatabaseTable = "",
                             outDatabaseSchema = "",
                             testOutFile = "",
                             trainOutFile = "",
                             estPPV = 1,
                             modelAnalysisId = "",
                             evalAnalysisId = "1",
                             cdmShortName = "CDM",
                             mainPopnCohort = 0,
                             lowerAgeLimit = 0,
                             upperAgeLimit = 120,
                             startDate = "19001010",
                             endDate = "21000101") {

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
  if (testOutFile == "")
    stop("....must have a defined training file name (e.g., \"test_10XDiabetes\")")
  if (trainOutFile == "")
    stop("....must have a defined training file name (e.g., \"train_10XDiabetes\")")

  writeLines(paste("xSpecCohort ", xSpecCohort))
  writeLines(paste("cdmDatabaseSchema ", cdmDatabaseSchema))
  writeLines(paste("cohortDatabaseSchema ", cohortDatabaseSchema))
  writeLines(paste("cohortDatabaseTable ", cohortDatabaseTable))
  writeLines(paste("outDatabaseSchema ", outDatabaseSchema))
  writeLines(paste("testOutFile ", testOutFile))
  writeLines(paste("trainOutFile ", trainOutFile))
  writeLines(paste("modelAnalysisId ", modelAnalysisId))
  writeLines(paste("evalAnalysisId ", evalAnalysisId))
  writeLines(paste("cdmShortName ", cdmShortName))
  writeLines(paste("estPPV ", estPPV))
  writeLines(paste("mainPopnCohort ", mainPopnCohort))
  writeLines(paste("lowerAgeLimit ", lowerAgeLimit))
  writeLines(paste("upperAgeLimit ", upperAgeLimit))
  writeLines(paste("startDate ", startDate))
  writeLines(paste("endDate ", endDate))

  workFolder <- getwd()

  conn <- DatabaseConnector::connect(connectionDetails)

  # create the evaluation cohort
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "CreateCohortsV6.sql",
                                              package = "PheValuator"))

  test_cohort <- gsub(".",
                      "",
                      (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                      fixed = TRUE)  #unique new cohort name to use

  sql <- SqlRender::renderSql(sqlScript,
                              cdm_database_schema = cdmDatabaseSchema,
                              cohort_database_schema = cohortDatabaseSchema,
                              cohort_database_table = cohortDatabaseTable,
                              x_spec_cohort = xSpecCohort,
                              tempDB = outDatabaseSchema,
                              test_cohort = test_cohort,
                              exclCohort = 0,
                              ageLimit = lowerAgeLimit,
                              upperAgeLimit = upperAgeLimit,
                              startDate = startDate,
                              endDate = endDate,
                              baseSampleSize = 1e+06,
                              xSpecSampleSize = 1,
                              noise = 0,
                              mainPopnCohort = mainPopnCohort,
                              lookback = 0)  #when applying the model start from the first visit for all subjects

  sql <- SqlRender::translateSql(sql$sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(conn = conn, sql$sql)

  # determine the model file to use to apply to the evaluation cohort
  resultsFileName <- file.path(workFolder, paste("lr_results_",
                                                 trainOutFile,
                                                 "_",
                                                 cdmShortName,
                                                 "_ePPV",
                                                 estPPV,
                                                 "_",
                                                 modelAnalysisId,
                                                 ".rds",
                                                 sep = ""))
  writeLines(paste("\n...reading ", resultsFileName, sep = ""))

  if (!file.exists(resultsFileName))
    stop(paste(".....Results file (", resultsFileName, ") does not exist"))

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
                                                                  useDrugEraStartLongTerm = TRUE,
                                                                  useDrugGroupEraLongTerm = TRUE,
                                                                  useDrugGroupEraStartLongTerm = TRUE,
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
                                                                  longTermStartDays = -10000,
                                                                  endDays = 10000,
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
                                                cdmVersion = 5,
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
  resultsFileName <- file.path(workFolder, paste("lr_results_",
                                                 testOutFile,
                                                 "_",
                                                 cdmShortName,
                                                 "_ePPV",
                                                 estPPV,
                                                 "_",
                                                 evalAnalysisId,
                                                 ".rds",
                                                 sep = ""))
  print(resultsFileName)

  # save the plp data - to be used for phenotype evaluation
  saveRDS(appResults, resultsFileName)

  # remove temp cohort table
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "DropTempTable.sql",
                                              package = "PheValuator"))
  sql <- SqlRender::renderSql(sqlScript, tempDB = outDatabaseSchema, test_cohort = test_cohort)
  sql <- SqlRender::translateSql(sql$sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(conn = conn, sql$sql)

  DatabaseConnector::disconnect(conn)
}
