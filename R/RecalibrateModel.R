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
#' Recalibrates a PheValuator model file
#'
#' @details
#' Recalibrates a PheValuator model file based on a the prevalence calculated by the prevalence cohort
#'
#' @param connectionDetails                connectionDetails created using the function
#'                                         createConnectionDetails in the DatabaseConnector package.
#' @param xSpecCohortId                    The number of the "extremely specific (xSpec)" cohort
#'                                         definition id in the cohort table (for noisy positives).
#' @param prevalenceCohortId               The number of the cohort definition id to determine the
#'                                         disease prevalence.
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
#' @param mainPopulationCohortId           The number of the cohort to be used as a base population for
#'                                         the model. If set to 0, the entire database population will be
#'                                         used.
#' @param mainPopulationCohortIdStartDay   The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to begin including visits.
#' @param mainPopulationCohortIdEndDay     The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to end including visits.
#' @param lowerAgeLimit                    The lower age for subjects in the model.
#' @param upperAgeLimit                    The upper age for subjects in the model.
#' @param gender                           The gender(s) to be included.
#' @param race                             The race(s) to be included.
#' @param ethnicity                        The ethnicity(s) to be included.
#' @param startDate                        The starting date for including subjects in the model.
#' @param endDate                          The ending date for including subjects in the model.
#' @param cdmVersion                       The CDM version of the database.
#' @param modelId                          A string used to generate the file names for this model.
#' @param evaluationCohortId               A string used to generate the file names for this evaluation cohort.
#' @param modelFileDirectory               Directory name of the RDS model file to recalibrate
#' @param destinationModelFileDirectory    Directory name to put the recalibrated model file
#' @param evalFileDirectory                Directory name of the RDS evaluation file to get cohort
#' @param destinationEvalFileDirectory     Directory name to put the cohort RDS file
#' @param removeSubjectsWithFutureDates    For buggy data with data in the future: ignore subjects with
#'                                         dates in the future?
#'
#' @export
recalibrateModel <- function(connectionDetails,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             cohortTable,
                             xSpecCohortId,
                             prevalenceCohortId,
                             mainPopulationCohortId = 0,
                             mainPopulationCohortIdStartDay = 0,
                             mainPopulationCohortIdEndDay = 0,
                             lowerAgeLimit = 0,
                             upperAgeLimit = 120,
                             gender = c(8507, 8532),
                             race = 0,
                             ethnicity = 0,
                             startDate = "19000101",
                             endDate = "21000101",
                             cdmVersion = "5",
                             modelId = "main",
                             evaluationCohortId = "main",
                             modelFileDirectory = NULL,
                             destinationModelFileDirectory = NULL,
                             evalFileDirectory = NULL,
                             destinationEvalFileDirectory = NULL,
                             removeSubjectsWithFutureDates = TRUE) {

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  modelFileName <- file.path(modelFileDirectory, sprintf("model_%s.rds", modelId))

  evaluationCohortFileName <- file.path(evalFileDirectory, sprintf("evaluationCohort_%s.rds", evaluationCohortId))

  if(!is.null(modelFileDirectory)) {
    if(!file.exists(modelFileName)) {
      stop(paste0(modelFileName, " does not exist."))
    }

    if(is.null(destinationModelFileDirectory)) {
      stop(paste0("...must provide a model destination."))
    }
  }

  if(!is.null(evalFileDirectory)) {
    if(!file.exists(evaluationCohortFileName)) {
      stop(paste0(evaluationCohortFileName, " does not exist."))
    }

    if(is.null(destinationEvalFileDirectory)) {
      stop(paste0("...must provide a evaluation file destination."))
    }
  }

  if(!is.null(modelFileDirectory)) { #if an available model file is provided perform recalibration
    if (prevalenceCohortId >= 1) {
      # determine population prevalence for correct xSpec/noisy negative popn ratio
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "getPopnPrev.sql",
                                               packageName = "PheValuator",
                                               dbms = connection@dbms,
                                               cdm_database_schema = cdmDatabaseSchema,
                                               cohort_database_schema = cohortDatabaseSchema,
                                               cohort_database_table = cohortTable,
                                               lowerAgeLimit = lowerAgeLimit,
                                               upperAgeLimit = upperAgeLimit,
                                               gender = gender,
                                               race = race,
                                               ethnicity = ethnicity,
                                               startDate = startDate,
                                               endDate = endDate,
                                               mainPopnCohort = mainPopulationCohortId,
                                               prevCohort = prevalenceCohortId,
                                               removeSubjectsWithFutureDates = removeSubjectsWithFutureDates)
      popPrev <- as.numeric(DatabaseConnector::querySql(connection = connection, sql))
    } else {popPrev <- prevalenceCohortId}

    if (popPrev == 0)
      stop("Unable to calculate the expected prevalence, possibly an error with prevalence cohort id")

    ParallelLogger::logInfo(sprintf("Estimated population prevalence is %0.2f%%", 100 * popPrev))

    lrResults <- readRDS(modelFileName)

    originalModelPrevalence <- lrResults$PheValuator$runTimeValues$truePrevalencePopulation
    originalModelYIntercept <- lrResults$PheValuator$runTimeValues$recalibratedYIntercept

    #re-calibrate model
    prevToUseOdds <- originalModelPrevalence/(1 - originalModelPrevalence) #uses prevalence for original model
    popPrevOdds <- popPrev/(1 - popPrev) #uses actual prevalence
    modelYIntercept <- lrResults$model$model$coefficients[1]
    delta <- log(prevToUseOdds) - log(popPrevOdds)
    yIntercept <- as.numeric(lrResults$model$model$coefficients[1])
    lrResults$model$model$coefficients[1] <- as.numeric(yIntercept - delta)  # Equation (7) in King and Zeng (2001)
    lrResults$model$predict <- PatientLevelPrediction:::createTransform(lrResults$model)

    lrResults$PheValuator$runTimeValues$truePrevalencePopulation <- popPrev
    lrResults$PheValuator$runTimeValues$prevalenceModel <- originalModelPrevalence
    lrResults$PheValuator$runTimeValues$modelYIntercept <- originalModelYIntercept
    lrResults$PheValuator$runTimeValues$recalibratedYIntercept <- lrResults$model$model$coefficients[1]


    dir.create(destinationModelFileDirectory, showWarnings = FALSE)

    evaluationFullDirectory <- file.path(destinationModelFileDirectory, "EvaluationCohort_e1")
    dir.create(evaluationFullDirectory, showWarnings = FALSE)

    destinationModelFileName <- file.path(evaluationFullDirectory, sprintf("model_%s.rds", modelId))

    ParallelLogger::logInfo("Saving recalibrated model summary to ", destinationModelFileName)
    saveRDS(lrResults, destinationModelFileName)
  }

  #handle evaluation cohort if provided
  if(!is.null(evalFileDirectory)) {
    #create an RDS file with the provided evaluation cohort
    evalData <- readRDS(evaluationCohortFileName)
    cohortData <- data.frame(evalData$prediction[,c(4,1,2,10)])
    names(cohortData)[1] <- "cohortDefinitionId"
    cohortData$cohortEndDate <- cohortData$cohortStartDate + 1

    outcomeCohort <- cohortData[cohortData$outcomeCount == 1,]
    outcomeCohort$cohortDefinitionId <- xSpecCohortId

    fullCohortData <- rbind(cohortData, outcomeCohort)
    fullCohortData <- fullCohortData[,c(1,2,3,5)]

    dir.create(destinationEvalFileDirectory, showWarnings = FALSE)
    evaluationFullDirectory <- file.path(destinationEvalFileDirectory, "EvaluationCohort_e1")
    dir.create(evaluationFullDirectory, showWarnings = FALSE)

    ParallelLogger::logInfo("Saving cohort file to ", file.path(evaluationFullDirectory, "evaluationCohortSubjects.rds"))
    saveRDS(fullCohortData, file.path(evaluationFullDirectory, "evaluationCohortSubjects.rds"))
  }

}
