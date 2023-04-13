# Copyright 2022 Observational Health Data Sciences and Informatics
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
#' Fits a diagnostic prediction model, and uses it to create an evaluation cohort with
#' probabilities for the health outcome of interest.
#'
#' @param connectionDetails                connectionDetails created using the function
#'                                         createConnectionDetails in the DatabaseConnector package.
#' @param oracleTempSchema    DEPRECATED: use \code{tempEmulationSchema} instead.
#' @param tempEmulationSchema Some database platforms like Oracle and Impala do not truly support temp tables. To
#'                            emulate temp tables, provide a schema with write privileges where temp tables
#'                            can be created.
#' @param phenotype                        Name of the phenotype for analysis
#' @param analysisName                     Name of the analysis
#' @param runDateTime                      Starting date and time of the PheValuator run
#' @param databaseId                       Name of the database in the analysis
#' @param xSpecCohortId                    The number of the "extremely specific (xSpec)" cohort
#'                                         definition id in the cohort table (for noisy positives).
#' @param daysFromxSpec                    Number of days allowed from xSpec condition until analyzed visit
#' @param xSensCohortId                    The number of the "extremely sensitive (xSens)" cohort
#'                                         definition id in the cohort table (for noisy negatives).
#' @param prevalenceCohortId               The number of the cohort definition id to determine the
#'                                         disease prevalence.
#' @param xSpecCohortSize                  The recommended xSpec sample size to use in model (default = NULL)
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
#'                                         a table can be created and afterwards removed.
#'                                         Requires write permissions to this database.
#' @param covariateSettings                A covariateSettings object as generated using
#'                                         createCovariateSettings().
#' @param modelPopulationCohortId          The number of the cohort to be used as a base population for
#'                                         the model. If set to 0, the entire database population will be
#'                                         used.
#' @param modelPopulationCohortIdStartDay   The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to begin including visits.
#' @param modelPopulationCohortIdEndDay     The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to end including visits.
#' @param inclusionEvaluationCohortId      The number of the cohort of the population to be used to designate which visits
#'                                         are eligible to be in the evaluation cohort
#' @param inclusionEvaluationDaysFromStart The number of days from the cohort start date of the inclusionEvaluationCohortId
#'                                         to start eligible included visits
#' @param inclusionEvaluationDaysFromEnd   The number of days from the cohort end date of the inclusionEvaluationCohortId
#'                                         to end eligible included visits
#' @param exclusionEvaluationCohortId      The number of the cohort of the population to be used to designate which visits
#'                                         are NOT eligible to be in the evaluation cohort
#' @param exclusionEvaluationDaysFromStart The number of days from the cohort start date of the exclusionEvaluationCohortId
#'                                         to start ineligible included visits
#' @param exclusionEvaluationDaysFromEnd   The number of days from the cohort end date of the exclusionEvaluationCohortId
#'                                         to end ineligible included visits
#' @param minimumOffsetFromStart           Minimum number of days to offset for the analysis visit from the start of the observation period
#' @param minimumOffsetFromEnd             Minimum number of days to offset for the analysis visit from the end of the observation period
#' @param modelBaseSampleSize              The number of non-xSpec subjects to include in the model
#' @param baseSampleSize                   The maximum number of subjects in the evaluation cohort.
#' @param lowerAgeLimit                    The lower age for subjects in the model.
#' @param upperAgeLimit                    The upper age for subjects in the model.
#' @param visitLength                      The minimum length of index visit for acute outcomes.
#' @param visitType                        The concept_id for the visit type.
#' @param gender                           The gender(s) to be included.
#' @param race                             The race(s) to be included.
#' @param ethnicity                        The ethnicity(s) to be included.
#' @param startDate                        The starting date for including subjects in the model.
#' @param endDate                          The ending date for including subjects in the model.
#' @param falsePositiveNegativeSubjects    Number of subjects to include for evaluating false positives and negatives
#' @param cdmVersion                       The CDM version of the database.
#' @param outFolder                        The folder where the output files will be written.
#' @param exportFolder                     The folder where the csv output files will be written.
#' @param modelId                          A string used to generate the file names for this model.
#' @param evaluationCohortId               A string used to generate the file names for this evaluation cohort.
#' @param excludeModelFromEvaluation       Should subjects used in the model be excluded from the evaluation cohort?
#' @param removeSubjectsWithFutureDates    For buggy data with data in the future: ignore subjects with
#'                                         dates in the future?
#' @param saveEvaluationCohortPlpData      Should the large PLP file for the evaluation cohort be saved? To be
#'                                         used for debugging purposes.
#'
#' @export
createEvaluationCohort <- function(connectionDetails,
                                   oracleTempSchema = NULL,
                                   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                   phenotype,
                                   analysisName,
                                   runDateTime,
                                   databaseId,
                                   xSpecCohortId,
                                   daysFromxSpec = 0,
                                   xSensCohortId,
                                   prevalenceCohortId,
                                   xSpecCohortSize = 5000,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   workDatabaseSchema,
                                   covariateSettings = createDefaultCovariateSettings(
                                     excludedCovariateConceptIds = c(),
                                     addDescendantsToExclude = TRUE
                                   ),
                                   modelPopulationCohortId = 0,
                                   modelPopulationCohortIdStartDay = 0,
                                   modelPopulationCohortIdEndDay = 0,
                                   inclusionEvaluationCohortId = 0,
                                   inclusionEvaluationDaysFromStart = 0,
                                   inclusionEvaluationDaysFromEnd = 0,
                                   exclusionEvaluationCohortId = 0,
                                   exclusionEvaluationDaysFromStart = 0,
                                   exclusionEvaluationDaysFromEnd = 0,
                                   minimumOffsetFromStart = 365,
                                   minimumOffsetFromEnd = 365,
                                   modelBaseSampleSize = 25000,
                                   baseSampleSize = 2e+06,
                                   lowerAgeLimit = 0,
                                   upperAgeLimit = 120,
                                   visitLength = 0,
                                   visitType = c(9201, 9202, 9203, 262, 581477),
                                   gender = c(8507, 8532),
                                   race = 0,
                                   ethnicity = 0,
                                   startDate = "19001010",
                                   endDate = "21000101",
                                   falsePositiveNegativeSubjects = 10,
                                   cdmVersion = "5",
                                   outFolder = getwd(),
                                   exportFolder,
                                   modelId = "main",
                                   evaluationCohortId = "main",
                                   excludeModelFromEvaluation = FALSE,
                                   removeSubjectsWithFutureDates = TRUE,
                                   saveEvaluationCohortPlpData = FALSE) {
  if (length(connectionDetails) == 0) {
    stop("Must supply a connection string")
  }
  if (xSpecCohortId == "") {
    stop("Must have an xSpec cohort id (e.g., 1234)")
  }
  if (xSensCohortId == "") {
    stop("Must have an xSens cohort id (e.g., 1235)")
  }
  if (prevalenceCohortId == "") {
    stop("Must have an prevalence cohort (prevCohort) (e.g., 1235)")
  }
  if (cdmDatabaseSchema == "") {
    stop(".Must have a defined CDM schema (e.g., \"YourCDM.YourCDMSchema\")")
  }
  if (cohortDatabaseSchema == "") {
    stop(".Must have a defined Cohort schema ((e.g., \"YourCDM.YourCohortSchema\")")
  }
  if (cohortTable == "") {
    stop(".Must have a defined Cohort table (e.g., \"cohort\")")
  }
  if (workDatabaseSchema == "") {
    stop(".Must have a defined Out Database schema (e.g., \"scratch.dbo\")")
  }
  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warning("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.")
    tempEmulationSchema <- oracleTempSchema
  }
  if (!file.exists(outFolder)) {
    dir.create(outFolder, recursive = TRUE)
  }
  start <- Sys.time()
  .createPhenotypeModel(
    connectionDetails = connectionDetails,
    phenotype = phenotype,
    analysisName = analysisName,
    runDateTime = runDateTime,
    databaseId = databaseId,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    workDatabaseSchema = workDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    xSpecCohortId = xSpecCohortId,
    daysFromxSpec = daysFromxSpec,
    xSensCohortId = xSensCohortId,
    prevalenceCohortId = prevalenceCohortId,
    xSpecCohortSize = xSpecCohortSize,
    covariateSettings = covariateSettings,
    modelPopulationCohortId = modelPopulationCohortId,
    modelPopulationCohortIdStartDay = modelPopulationCohortIdStartDay,
    modelPopulationCohortIdEndDay = modelPopulationCohortIdEndDay,
    modelBaseSampleSize = modelBaseSampleSize,
    minimumOffsetFromStart = minimumOffsetFromStart,
    minimumOffsetFromEnd = minimumOffsetFromEnd,
    lowerAgeLimit = lowerAgeLimit,
    upperAgeLimit = upperAgeLimit,
    visitLength = visitLength,
    visitType = c(visitType),
    gender = gender,
    race = race,
    ethnicity = ethnicity,
    startDate = startDate,
    endDate = endDate,
    removeSubjectsWithFutureDates = removeSubjectsWithFutureDates,
    cdmVersion = cdmVersion,
    outFolder = outFolder,
    exportFolder = exportFolder,
    modelId = modelId
  )

  .createEvaluationCohort(
    connectionDetails = connectionDetails,
    phenotype = phenotype,
    analysisName = analysisName,
    runDateTime = runDateTime,
    databaseId = databaseId,
    xSpecCohortId = xSpecCohortId,
    xSensCohortId = xSensCohortId,
    prevalenceCohortId = prevalenceCohortId,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    workDatabaseSchema = workDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    covariateSettings = covariateSettings,
    inclusionEvaluationCohortId = inclusionEvaluationCohortId,
    inclusionEvaluationDaysFromStart = inclusionEvaluationDaysFromStart,
    inclusionEvaluationDaysFromEnd = inclusionEvaluationDaysFromEnd,
    exclusionEvaluationCohortId = exclusionEvaluationCohortId,
    exclusionEvaluationDaysFromStart = exclusionEvaluationDaysFromStart,
    exclusionEvaluationDaysFromEnd = exclusionEvaluationDaysFromEnd,
    minimumOffsetFromStart = minimumOffsetFromStart,
    minimumOffsetFromEnd = minimumOffsetFromEnd,
    baseSampleSize = baseSampleSize,
    lowerAgeLimit = lowerAgeLimit,
    upperAgeLimit = upperAgeLimit,
    visitLength = visitLength,
    visitType = c(visitType),
    gender = gender,
    race = race,
    ethnicity = ethnicity,
    startDate = startDate,
    endDate = endDate,
    falsePositiveNegativeSubjects = falsePositiveNegativeSubjects,
    cdmVersion = cdmVersion,
    outFolder = outFolder,
    exportFolder = exportFolder,
    modelId = modelId,
    evaluationCohortId = evaluationCohortId,
    excludeModelFromEvaluation = excludeModelFromEvaluation,
    savePlpData = saveEvaluationCohortPlpData
  )
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Creating evaluation cohort took ", signif(delta, 3), " ", attr(delta, "units"))
}
