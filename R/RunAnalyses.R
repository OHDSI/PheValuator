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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses.
#'
#' @param phenotype                      Name of the phenotype for analysis
#' @param cohortDefinitionSet            Data.frame of cohorts must include columns cohortId, cohortName, json, sql - should include all
#'                                       cohort definitions needed to replicate PheValuator analysis
#' @param analysisName                   Name of the analysis
#' @param runDateTime                    Starting date and time of the PheValuator run
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the
#'                                       schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema    DEPRECATED: use \code{tempEmulationSchema} instead.
#' @param tempEmulationSchema Some database platforms like Oracle and Impala do not truly support temp tables. To
#'                            emulate temp tables, provide a schema with write privileges where temp tables
#'                            can be created.
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
#' @param databaseId                     Name of the database in the analysis
#' @param cdmVersion                     Define the OMOP CDM version used: currently supports "5".
#' @param outputFolder                   Name of the folder where all the outputs will be written to.
#' @param priorModelToUse                folder where a previously developed model to use in analysis will be found
#' @param pheValuatorAnalysisList        A list of objects of type \code{pheValuatorAnalysis} as created using
#'                                       the \code{\link{createPheValuatorAnalysis}} function.
#'
#' @return
#' A data frame specifying where the constructed evaluation cohort and phenotype evaluation results can be found
#' in the local file system.
#'
#' @export
runPheValuatorAnalyses <- function(phenotype,
                                   cohortDefinitionSet = data.frame(),
                                   analysisName = "Main",
                                   runDateTime = format(Sys.time(), "%b %d %Y %X"),
                                   connectionDetails,
                                   oracleTempSchema = NULL,
                                   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema = cdmDatabaseSchema,
                                   cohortTable = "cohort",
                                   workDatabaseSchema = cdmDatabaseSchema,
                                   databaseId = cdmDatabaseSchema,
                                   cdmVersion = 5,
                                   outputFolder,
                                   priorModelToUse = NULL,
                                   pheValuatorAnalysisList) {
  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warning("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.")
    tempEmulationSchema <- oracleTempSchema
  }
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  ParallelLogger::logInfo("\nBeginning PheValuator analysis for phenotype: ", phenotype, "\n")

  #create export folder for csv output
  exportFolder <- file.path(outputFolder, "exportFolder")
  dir.create(exportFolder, showWarnings = FALSE)

  referenceTable <- createReferenceTable(pheValuatorAnalysisList)
  saveRDS(referenceTable, file.path(outputFolder, "reference.rds"))

  df <- NULL
  df$phenotype <- phenotype
  df$analysisName <- analysisName
  df$databaseId <- databaseId
  df$runDateTime <- runDateTime

  if(nrow(cohortDefinitionSet) != 0) {
    df <- cbind(df, data.frame(cohortDefinitionSet[,c(1:3)]))
  }

  if(!is.null(colnames(df))) {
    colnames(df) <- SqlRender::camelCaseToSnakeCase(colnames(df))
  }

  cohortFile <- file.path(exportFolder, "pv_cohort_definition_set.csv")
  if(!file.exists(cohortFile)) {
    ParallelLogger::logInfo("\nsaving ", cohortFile, "\n")
    write.csv(df, cohortFile, row.names = FALSE)
  } else {
    ParallelLogger::logInfo("\n", cohortFile, " exists...not overwriting.\n")
  }

  ParallelLogger::logInfo("Generating evaluation cohorts")
  evaluationCohortFolders <- unique(referenceTable$evaluationCohortFolder)
  # evaluationCohortFolders <- evaluationCohortFolders[!file.exists(file.path(outputFolder, evaluationCohortFolders, "evaluationCohort_main.rds"))]
  # if (length(evaluationCohortFolders) > 0) {
  createTask <- function(evaluationCohortFolder) {
    analysisId <- referenceTable$analysisId[referenceTable$evaluationCohortFolder == evaluationCohortFolder][1]
    matched <- ParallelLogger::matchInList(pheValuatorAnalysisList, list(analysisId = analysisId))
    args <- matched[[1]]$createEvaluationCohortArgs
    args$phenotype <- phenotype
    args$analysisName <- analysisName
    args$runDateTime <- runDateTime
    args$databaseId <- databaseId
    args$connectionDetails <- connectionDetails
    args$cdmDatabaseSchema <- cdmDatabaseSchema
    args$tempEmulationSchema <- getOption("sqlRenderTempEmulationSchema")
    args$cohortDatabaseSchema <- cohortDatabaseSchema
    args$cohortTable <- cohortTable
    args$workDatabaseSchema <- workDatabaseSchema
    args$cdmVersion <- cdmVersion
    args$outFolder <- file.path(outputFolder, evaluationCohortFolder)
    if(!is.null(priorModelToUse)) {
      args$priorModelToUse <- file.path(priorModelToUse, evaluationCohortFolder)
    } else {
      args$priorModelToUse <- NULL
    }

    if(!is.null(priorModelToUse)) { #previously created model to use in the analysis
      args$priorModelToUse <- file.path(priorModelToUse, evaluationCohortFolder)
    } else {
      args$priorModelToUse <- NULL
    }

    args$exportFolder <- exportFolder
    task <- list(args = args)
    return(task)
  }
  tasks <- lapply(evaluationCohortFolders, createTask)
  lapply(tasks, doCreateEvaluationCohort)
  # }

  ParallelLogger::logInfo("Evaluating phenotypes")
  resultsFiles <- unique(referenceTable$resultsFile)
  resultsFiles <- resultsFiles[!file.exists(file.path(outputFolder, resultsFiles))]
  if (length(resultsFiles) > 0) {
    createEvalTask <- function(resultsFile) {
      analysisId <- referenceTable$analysisId[referenceTable$resultsFile == resultsFile]
      matched <- ParallelLogger::matchInList(pheValuatorAnalysisList, list(analysisId = analysisId))
      args <- matched[[1]]$testPhenotypeAlgorithmArgs
      args$phenotype <- phenotype
      args$analysisName <- analysisName
      args$runDateTime <- runDateTime
      args$databaseId <- databaseId
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- cdmDatabaseSchema
      args$cohortDatabaseSchema <- cohortDatabaseSchema
      args$cohortTable <- cohortTable
      args$outFolder <- file.path(
        outputFolder,
        referenceTable$evaluationCohortFolder[referenceTable$analysisId == analysisId]
      )
      args$exportFolder <- exportFolder
      task <- list(
        args = args,
        fileName = file.path(outputFolder, resultsFile)
      )
      return(task)
    }
    tasks <- lapply(resultsFiles, createEvalTask)
    lapply(tasks, doTestPhenotypeAlgorithm)
  }


  output <- data.frame(summarizePheValuatorAnalyses(referenceTable, outputFolder))
  names(output)[names(output)=="analysisId"] <- "analysisIdResults"
  colnames(output) <- SqlRender::camelCaseToSnakeCase(colnames(output))

  if(ncol(output) > 10) {
    ParallelLogger::logInfo("Saving phenotype algorithm evaluation results to ", exportFolder)
    write.csv(output, file.path(exportFolder, "pv_algorithm_performance_results.csv"), row.names = FALSE)
  } else {
    ParallelLogger::logInfo("Not saving phenotype algorithm evaluation results - too few outcomes.")
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
  referenceTable <- data.frame(
    analysisId = unlist(ParallelLogger::selectFromList(pheValuatorAnalysisList, "analysisId")),
    description = unlist(ParallelLogger::selectFromList(pheValuatorAnalysisList, "description"))
  )
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
#' A data frame of results.
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
