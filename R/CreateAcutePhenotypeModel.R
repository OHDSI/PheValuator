
# @file CreateAcutePhenotypeModel.R
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

#' Create the phenotype model for acute diseases
#'
#' @description
#' Create the phenotype model for acute diseases
#'
#' @details
#' Function to run through the complete phenotype evaluation process from model building to developing an
#' evaluation cohort to using the evaluation cohort to determine the performance characteristics of phenotype algorithms
#' for acute health conditions.
#'
#' @param connectionDetails      connectionDetails created using the function createConnectionDetails
#'                               in the DatabaseConnector package.
#' @param cdmDatabaseSchema      The name of the database schema that contains the OMOP CDM instance.
#'                               Requires read permissions to this database. On SQL Server, this should
#'                               specifiy both the database and the schema, so for example
#'                               'cdm_instance.dbo'.
#' @param databaseId             Short name for the database (default="TestDB")
#' @param cohortDatabaseSchema   The name of the database schema that is the location where the cohort
#'                               data used to define the at risk cohort is available. Requires read
#'                               permissions to this database.
#' @param cohortDatabaseTable    The tablename that contains the at risk cohort. The expectation is
#'                               cohortTable has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                               COHORT_START_DATE, COHORT_END_DATE.
#' @param outDatabaseSchema      The name of a database schema where the user has write capability.  A
#'                               temporary cohort table will be created here.
#' @param modelOutputFileName    A string designation for the training model file
#' @param evaluationOutputFileName  A string designation for the evaluation cohort file
#' @param conditionName          A short name for the health outcome of interest, e.g., "Hypertension" (default = "HOI")
#' @param xSpecCohort            The number of the "extremely specific (xSpec)" cohort definition id in
#'                               the cohort table (for noisy positives)
#' @param xSensCohort            The number of the "extremely sensitive (xSens)" cohort definition id
#'                               in the cohort table (used to exclude subjects from the base population)
#' @param prevalenceCohort       The number of the cohort definition id to determine the disease prevalence,
#'                               (default=xSensCohort)
#' @param excludedConcepts       A list of conceptIds to exclude from featureExtraction.  These should include all
#'                               concept_ids that were used to define the xSpec model (default=NULL)
#' @param includedCovariateIds   A list of covariate IDs that should be restricted to.
#' @param addDescendantsToExclude        Should descendants of excluded concepts also be excluded? (default=FALSE)
#' @param mainPopulationCohort   The number of the cohort ID to be used as a base population for the model
#'                               (default=NULL)
#' @param baseSampleSize         The maximum number of subjects in the evaluation cohort (default=2M)
#' @param lowerAgeLimit          The lower age for subjects in the model (default=NULL)
#' @param upperAgeLimit          The upper age for subjects in the model (default=NULL)
#' @param startDays              The days to include prior to the cohort start date (default=0)
#' @param endDays                The days to include after the cohort start date (default=7)
#' @param visitLength            The minimum length of index visit for noisy negative comparison (default=3)
#' @param gender                 The gender(s) to be included (default c(8507, 8532))
#' @param startDate              The starting date for including subjects in the model (default=NULL)
#' @param endDate                The ending date for including subjects in the model (default=NULL)
#' @param checkDates             Should dates be checked to remove future dates (default=TRUE)
#' @param cdmVersion             The CDM version of the database (default=5)
#' @param outFolder              The folder where the output files will be written (default=working directory)
#' @param savePlpData            Should large PLP data file be saved (default=FALSE)
#' @param createModel            Run the function to create the diagnostic predictive model (default=TRUE)
#' @param createEvaluationCohort Run the function to create the evaluation cohort (default=TRUE)
#' @param cohortDefinitionsToTest A dataframe with cohorts to evaluate. Leave blank to not test any cohort definitions (default=Null)
#' with the format: atlasId - The cohort ID in ATLAS; atlasName - The full name of the cohort; cohortId - The cohort ID to use in the package. Usually the same as the cohort ID in ATLAS;
#' name - A short name for the cohort, to use to create file names. Do not use special characters; washoutPeriod - The mininum required continuous observation time prior to index date for subjects within the cohort to test
#'
#' @importFrom stats runif
#'
#' @export
createAcutePhenotypeModel <- function(connectionDetails,
                                      cdmDatabaseSchema,
                                      databaseId = "TestDB",
                                      cohortDatabaseSchema,
                                      cohortDatabaseTable,
                                      outDatabaseSchema,
                                      modelOutputFileName = "train",
                                      evaluationOutputFileName = "eval",
                                      conditionName = "HOI",
                                      xSpecCohort,
                                      xSensCohort,
                                      prevalenceCohort = xSensCohort,
                                      excludedConcepts = c(),
                                      includedCovariateIds = c(),
                                      addDescendantsToExclude = FALSE,
                                      mainPopulationCohort = 0,
                                      baseSampleSize = 2000000,
                                      lowerAgeLimit = 0,
                                      upperAgeLimit = 120,
                                      startDays = 0,
                                      endDays = 7,
                                      visitLength = 3,
                                      gender = c(8507, 8532),
                                      startDate = "19000101",
                                      endDate = "21000101",
                                      checkDates = TRUE,
                                      cdmVersion = "5",
                                      outFolder = getwd(),
                                      savePlpData = F,
                                      createModel = TRUE,
                                      createEvaluationCohort = TRUE,
                                      cohortDefinitionsToTest = NULL) {

  options(error = NULL)
  options(scipen=999)


  if(createModel == TRUE) {
    errorCheck(callingProgram = "createPhenotypeModel",
               connectionDetails = connectionDetails,
               cdmDatabaseSchema = cdmDatabaseSchema,
               cohortDatabaseSchema = cohortDatabaseSchema,
               cohortDatabaseTable = cohortDatabaseTable,
               outDatabaseSchema = outDatabaseSchema,
               modelOutputFileName = modelOutputFileName,
               evaluationOutputFileName = evaluationOutputFileName,
               xSpecCohort = xSpecCohort,
               xSensCohort = xSensCohort,
               prevalenceCohort = prevalenceCohort,
               excludedConcepts = excludedConcepts,
               mainPopulationCohort = mainPopulationCohort,
               outFolder = outFolder,
               cohortDefinitionsToTest = cohortDefinitionsToTest)

    model <- createPhenotypeModel(connectionDetails = connectionDetails,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortDatabaseTable = cohortDatabaseTable,
                                  outDatabaseSchema = outDatabaseSchema,
                                  modelOutputFileName = modelOutputFileName,
                                  xSpecCohort = xSpecCohort,
                                  xSensCohort = xSensCohort,
                                  prevalenceCohort = prevalenceCohort,
                                  excludedConcepts = c(excludedConcepts),
                                  includedCovariateIds = c(includedCovariateIds),
                                  addDescendantsToExclude = addDescendantsToExclude,
                                  mainPopulationCohort = mainPopulationCohort,
                                  lowerAgeLimit = lowerAgeLimit,
                                  upperAgeLimit = upperAgeLimit,
                                  startDays = startDays,
                                  endDays = endDays,
                                  visitLength = visitLength,
                                  gender = c(gender),
                                  startDate = startDate ,
                                  endDate = endDate,
                                  checkDates = checkDates,
                                  cdmVersion = cdmVersion,
                                  outFolder = outFolder,
                                  modelType = 'acute')
  }

  if(createEvaluationCohort == TRUE) {
    errorCheck(callingProgram = "createEvaluationCohort",
               connectionDetails = connectionDetails,
               cdmDatabaseSchema = cdmDatabaseSchema,
               cohortDatabaseSchema = cohortDatabaseSchema,
               cohortDatabaseTable = cohortDatabaseTable,
               outDatabaseSchema = outDatabaseSchema,
               modelOutputFileName = modelOutputFileName,
               evaluationOutputFileName = evaluationOutputFileName,
               xSpecCohort = xSpecCohort,
               xSensCohort = xSensCohort,
               prevalenceCohort = prevalenceCohort,
               excludedConcepts = excludedConcepts,
               mainPopulationCohort = mainPopulationCohort,
               outFolder = outFolder,
               cohortDefinitionsToTest = cohortDefinitionsToTest)

    evalCohort <- createEvaluationCohort(connectionDetails = connectionDetails,
                                         xSpecCohort = xSpecCohort,
                                         xSensCohort = xSensCohort,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortDatabaseTable = cohortDatabaseTable,
                                         outDatabaseSchema = outDatabaseSchema,
                                         evaluationOutputFileName = evaluationOutputFileName,
                                         modelOutputFileName = modelOutputFileName,
                                         mainPopulationCohort = mainPopulationCohort,
                                         baseSampleSize = baseSampleSize,
                                         lowerAgeLimit = lowerAgeLimit,
                                         upperAgeLimit = upperAgeLimit,
                                         startDays = startDays,
                                         endDays = endDays,
                                         gender = c(gender),
                                         startDate = startDate,
                                         endDate = endDate,
                                         cdmVersion = cdmVersion,
                                         outFolder = outFolder,
                                         savePlpData = savePlpData,
                                         modelType = 'acute')
  }

  if(!is.null(cohortDefinitionsToTest)) {
    errorCheck(callingProgram = "testPhenosFromFile",
               connectionDetails = connectionDetails,
               cdmDatabaseSchema = cdmDatabaseSchema,
               cohortDatabaseSchema = cohortDatabaseSchema,
               cohortDatabaseTable = cohortDatabaseTable,
               outDatabaseSchema = outDatabaseSchema,
               modelOutputFileName = modelOutputFileName,
               evaluationOutputFileName = evaluationOutputFileName,
               xSpecCohort = xSpecCohort,
               xSensCohort = xSensCohort,
               prevalenceCohort = prevalenceCohort,
               excludedConcepts = excludedConcepts,
               mainPopulationCohort = mainPopulationCohort,
               outFolder = outFolder,
               cohortDefinitionsToTest = cohortDefinitionsToTest)

    cohortsToTest <- testPhenosFromFile(connectionDetails = connectionDetails,
                                        evaluationOutputFileName = evaluationOutputFileName,
                                        databaseId = databaseId,
                                        conditionName = conditionName,
                                        xSpecCohort = xSpecCohort,
                                        xSensCohort = xSensCohort,
                                        prevalenceCohort = prevalenceCohort,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        cohortDatabaseTable = cohortDatabaseTable,
                                        cohortDefinitionsToTest = cohortDefinitionsToTest,
                                        outFolder = outFolder,
                                        modelType = 'acute')
  }

}

