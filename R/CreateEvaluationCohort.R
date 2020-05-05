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
#' Fits a diagnostic pretiction model, and uses it to create an evaluation cohort with
#' probabilities for the health outcome of interest.
#'
#' @param connectionDetails                connectionDetails created using the function
#'                                         createConnectionDetails in the DatabaseConnector package.
#' @param oracleTempSchema	               A schema where temp tables can be created in Oracle.
#' @param xSpecCohortId                    The number of the "extremely specific (xSpec)" cohort
#'                                         definition id in the cohort table (for noisy positives).
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
#' @param mainPopulationCohortId           The number of the cohort to be used as a base population for
#'                                         the model. If set to 0, the entire database population will be
#'                                         used.
#' @param mainPopulationCohortIdStartDay   The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to begin including visits.
#' @param mainPopulationCohortIdEndDay     The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to end including visits.
#' @param baseSampleSize                   The maximum number of subjects in the evaluation cohort.
#' @param lowerAgeLimit                    The lower age for subjects in the model.
#' @param upperAgeLimit                    The upper age for subjects in the model.
#' @param visitLength                      The minimum length of index visit for acute outcomes.
#' @param visitType                        The concept_id for the visit type.
#' @param gender                           The gender(s) to be included.
#' @param startDate                        The starting date for including subjects in the model.
#' @param endDate                          The ending date for including subjects in the model.
#' @param cdmVersion                       The CDM version of the database.
#' @param outFolder                        The folder where the output files will be written.
#' @param modelId                          A string used to generate the file names for this model.
#' @param evaluationCohortId               A string used to generate the file names for this evaluation cohort.
#' @param excludeModelFromEvaluation       Should subjects used in the model be excluded from the evaluation cohort?
#' @param removeSubjectsWithFutureDates    For buggy data with data in the future: ignore subjects with
#'                                         dates in the future?
#' @param saveEvaluationCohortPlpData      Should the large PLP file for the evaluation cohort be saved? To be
#'                                         used for debugging purposes.
#' @param modelType                        The type of health outcome in the model either "acute" or
#'                                         "chronic".
#'
#' @export
createEvaluationCohort <- function(connectionDetails,
                                   oracleTempSchema = NULL,
                                   xSpecCohortId,
                                   xSensCohortId,
                                   prevalenceCohortId = xSensCohortId,
                                   xSpecCohortSize = NULL,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   workDatabaseSchema,
                                   covariateSettings = createDefaultChronicCovariateSettings(excludedCovariateConceptIds = c(),
                                                                                             addDescendantsToExclude = FALSE),
                                   mainPopulationCohortId = 0,
                                   mainPopulationCohortIdStartDay = 0,
                                   mainPopulationCohortIdEndDay = 0,
                                   baseSampleSize = 2e+06,
                                   lowerAgeLimit = 0,
                                   upperAgeLimit = 120,
                                   visitLength = 3,
                                   visitType = c(9201),
                                   gender = c(8507, 8532),
                                   startDate = "19001010",
                                   endDate = "21000101",
                                   cdmVersion = "5",
                                   outFolder = getwd(),
                                   modelId = "main",
                                   evaluationCohortId = "main",
                                   excludeModelFromEvaluation = TRUE,
                                   removeSubjectsWithFutureDates = TRUE,
                                   saveEvaluationCohortPlpData = FALSE,
                                   modelType = "chronic") {
  if (modelType != "chronic" & modelType != "acute")
    stop("ModelType must be acute or chronic")
  if (length(connectionDetails) == 0)
    stop("Must supply a connection string")
  if (xSpecCohortId == "")
    stop("Must have an xSpec cohort id (e.g., 1234)")
  if (xSensCohortId == "")
    stop("Must have an xSens cohort id (e.g., 1235)")
  if (prevalenceCohortId == "")
    stop("Must have an prevalence cohort (prevCohort) (e.g., 1235)")
  if (cdmDatabaseSchema == "")
    stop(".Must have a defined CDM schema (e.g., \"YourCDM.YourCDMSchema\")")
  if (cohortDatabaseSchema == "")
    stop(".Must have a defined Cohort schema ((e.g., \"YourCDM.YourCohortSchema\")")
  if (cohortTable == "")
    stop(".Must have a defined Cohort table (e.g., \"cohort\")")
  if (workDatabaseSchema == "")
    stop(".Must have a defined Out Database schema (e.g., \"scratch.dbo\")")

  if (!file.exists(outFolder)) {
    dir.create(outFolder, recursive = TRUE)
  }
  start <-  Sys.time()
  .createPhenotypeModel(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = cohortTable,
                        workDatabaseSchema = workDatabaseSchema,
                        xSpecCohortId = xSpecCohortId,
                        xSensCohortId = xSensCohortId,
                        prevalenceCohortId = prevalenceCohortId,
                        xSpecCohortSize = xSpecCohortSize,
                        covariateSettings = covariateSettings,
                        mainPopulationCohortId = mainPopulationCohortId,
                        mainPopulationCohortIdStartDay = mainPopulationCohortIdStartDay,
                        mainPopulationCohortIdEndDay = mainPopulationCohortIdEndDay,
                        lowerAgeLimit = lowerAgeLimit,
                        upperAgeLimit = upperAgeLimit,
                        visitLength = visitLength,
                        visitType = c(visitType),
                        gender = gender,
                        startDate = startDate,
                        endDate = endDate,
                        removeSubjectsWithFutureDates = removeSubjectsWithFutureDates,
                        cdmVersion = cdmVersion,
                        outFolder = outFolder,
                        modelId = evaluationCohortId,
                        modelType = modelType)

  .createEvaluationCohort(connectionDetails = connectionDetails,
                          xSpecCohortId = xSpecCohortId,
                          xSensCohortId = xSensCohortId,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortTable = cohortTable,
                          workDatabaseSchema = workDatabaseSchema,
                          covariateSettings = covariateSettings,
                          mainPopulationCohortId = mainPopulationCohortId,
                          mainPopulationCohortIdStartDay = mainPopulationCohortIdStartDay,
                          mainPopulationCohortIdEndDay = mainPopulationCohortIdEndDay,
                          baseSampleSize = baseSampleSize,
                          lowerAgeLimit = lowerAgeLimit,
                          upperAgeLimit = upperAgeLimit,
                          visitLength = visitLength,
                          visitType = c(visitType),
                          gender = gender,
                          startDate = startDate,
                          endDate = endDate,
                          cdmVersion = cdmVersion,
                          outFolder = outFolder,
                          modelId = evaluationCohortId,
                          evaluationCohortId = evaluationCohortId,
                          excludeModelFromEvaluation = TRUE,
                          savePlpData = saveEvaluationCohortPlpData,
                          modelType = modelType)
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Creating evaluation cohort took ", signif(delta, 3), " ", attr(delta, "units"))
}
