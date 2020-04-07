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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses.
#'
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the
#'                                       schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema	             A schema where temp tables can be created in Oracle.
#' @param cohortDatabaseSchema           The name of the database schema that is the location where
#'                                       the cohort data used to define the at risk cohort is
#'                                       available. Requires read permissions to this database.
#' @param cohortTable                    The tablename that contains the at risk cohort. The
#'                                       expectation is cohortTable has format of COHORT table:
#'                                       cohort_concept_id, SUBJECT_ID, COHORT_START_DATE,
#'                                       COHORT_END_DATE.
#' @param workDatabaseSchema             The name of the database schema that is the location where
#'                                       a table can be created and afterwards removed.
#'                                       Requires write permissions to this database.
#' @param cdmVersion                     Define the OMOP CDM version used: currently supports "5".
#' @param outputFolder                   Name of the folder where all the outputs will written to.
#' @param pheValuatorAnalysisList        A list of objects of type \code{pheValuatorAnalysis} as created using
#'                                       the \code{\link{createPheValuatorAnalysis}} function.
#'
#' @return
#' A data frame specifiying where the constructed evaluation cohort and phenotype evaluation results can be found
#' in the local file system.
#'
#' @export
runPheValuatorAnalyses <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   oracleTempSchema = NULL,
                                   cohortDatabaseSchema = cdmDatabaseSchema,
                                   cohortTable = "cohort",
                                   workDatabaseSchema = cdmDatabaseSchema,
                                   cdmVersion = 5,
                                   outputFolder,
                                   pheValuatorAnalysisList) {
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  referenceTable <- createReferenceTable(pheValuatorAnalysisList)
  saveRDS(referenceTable, file.path(outputFolder, "reference.rds"))

  ParallelLogger::logInfo("Generating evaluation cohorts")
  evaluationCohortFolders <- unique(referenceTable$evaluationCohortFolder)
  evaluationCohortFolders <- evaluationCohortFolders[!file.exists(file.path(outputFolder, evaluationCohortFolders, "evaluationCohort_main.rds"))]
  if (length(evaluationCohortFolders) > 0) {
    createTask <- function(evaluationCohortFolder) {
      analysisId <- referenceTable$analysisId[referenceTable$evaluationCohortFolder == evaluationCohortFolder][1]
      matched <- ParallelLogger::matchInList(pheValuatorAnalysisList, list(analysisId = analysisId))
      args <- matched[[1]]$createEvaluationCohortArgs
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- cdmDatabaseSchema
      args$oracleTempSchema <- oracleTempSchema
      args$cohortDatabaseSchema <- cohortDatabaseSchema
      args$cohortTable <- cohortTable
      args$workDatabaseSchema <- workDatabaseSchema
      args$cdmVersion <- cdmVersion
      args$outFolder <- file.path(outputFolder, evaluationCohortFolder)
      task <- list(args = args)
      return(task)
    }
    tasks <- lapply(evaluationCohortFolders, createTask)
    lapply(tasks, doCreateEvaluationCohort)
  }

  ParallelLogger::logInfo("Evaluating phenotypes")
  resultsFiles <- unique(referenceTable$resultsFile)
  resultsFiles <- resultsFiles[!file.exists(file.path(outputFolder, resultsFiles))]
  if (length(resultsFiles) > 0) {
    createTask <- function(resultsFile) {
      analysisId <- referenceTable$analysisId[referenceTable$resultsFile == resultsFile]
      matched <- ParallelLogger::matchInList(pheValuatorAnalysisList, list(analysisId = analysisId))
      args <- matched[[1]]$testPhenotypeAlgorithmArgs
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- cdmDatabaseSchema
      args$cohortDatabaseSchema <- cohortDatabaseSchema
      args$cohortTable <- cohortTable
      args$outFolder <- file.path(outputFolder,
                                 referenceTable$evaluationCohortFolder[referenceTable$analysisId == analysisId])
      task <- list(args = args,
                   fileName = file.path(outputFolder, resultsFile))
      return(task)
    }
    tasks <- lapply(resultsFiles, createTask)
    lapply(tasks, doTestPhenotypeAlgorithm)
  }
  invisible(referenceTable)
}

doCreateEvaluationCohort <- function(task) {
  do.call("createEvaluationCohort", task$args)
}

doTestPhenotypeAlgorithm <- function(task) {
  result <- do.call("testPhenotypeAlgorithm", task$args)
  saveRDS(result, task$fileName)
}

createReferenceTable <- function(pheValuatorAnalysisList) {
  referenceTable <- data.frame(analysisId = unlist(ParallelLogger::selectFromList(pheValuatorAnalysisList, "analysisId")),
                               description = unlist(ParallelLogger::selectFromList(pheValuatorAnalysisList, "description")))
  evalCohortArgs <- ParallelLogger::selectFromList(pheValuatorAnalysisList, "createEvaluationCohortArgs")
  uniqueEvalCohortArgs <- unique(evalCohortArgs)
  referenceTable$evaluationCohortFolder <- ""
  for (evalCohortId in 1:length(uniqueEvalCohortArgs)) {
    uniqueArgs <- uniqueEvalCohortArgs[[evalCohortId]]
    matched <- ParallelLogger::matchInList(pheValuatorAnalysisList, uniqueArgs)
    analysisIds <- unlist(ParallelLogger::selectFromList(matched, "analysisId"))
    referenceTable$evaluationCohortFolder[match(analysisIds, referenceTable$analysisId)] <- sprintf("EvaluationCohort_e%s", evalCohortId)
  }
  referenceTable$resultsFile <- sprintf("TestResults_a%s.rds", referenceTable$analysisId)
  return(referenceTable)
}

#' Summarize results of PheValuator analyses
#'
#' @param referenceTable  A reference table as created using \code{\link{runPheValuatorAnalyses}}.
#' @param outputFolder    The output folder used when calling \code{\link{runPheValuatorAnalyses}}.
#'
#' @return
#' A data frame of resuts.
#'
#' @export
summarizePheValuatorAnalyses <- function(referenceTable, outputFolder) {
  getResults <- function(referenceRow) {
    resultsRows <- readRDS(file.path(outputFolder, referenceRow$resultsFile))
    resultsRows$analysisId <- referenceRow$analysisId
    resultsRows$description <- referenceRow$description
    return(resultsRows)
  }
  results <- lapply(split(referenceTable, referenceTable$analysisId), getResults)
  results <- suppressWarnings(dplyr::bind_rows(results))
  return(results)
}


