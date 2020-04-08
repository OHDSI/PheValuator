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

#' Export the PheValuator results to CSV files
#'
#' @param outputFolder                   Name of the folder where all the outputs will written to.
#' @param pheValuatorAnalysisList        A list of objects of type \code{pheValuatorAnalysis} as created using
#'                                       the \code{\link{createPheValuatorAnalysis}} function.
#' @param packageName         The name of the package containing the cohort definitions. Can be left NULL if
#'                            \code{baseUrl} and \code{cohortSetReference} have been specified.
#' @param cohortToCreateFile  The location of the cohortToCreate file within the package. Is ignored if \code{cohortSetReference} is specified.
#' @param baseUrl             The base URL for the WebApi instance, for example:
#'                            "http://server.org:80/WebAPI". Can be left NULL if no cohort meta-data needs to be exported.
#' @param cohortSetReference  A data frame with four columns, as described in the details. Can be left NULL if
#'                            \code{packageName} and \code{cohortToCreateFile} have been specified.
#' @param minCellCount                The minimum cell count for fields contains person counts or fractions.
#'
#' @details
#' Currently three ways of executing this function are supported, which determines how the meta-data on the cohorts is retrieved:
#'
#' \enumerate{
#'   \item Embedded in a study package, assuming the cohort definitions are stored in that package using the \code{ROhdsiWebApi::insertCohortDefinitionSetInPackage}. In this case, the \code{packageName} and \code{cohortToCreateFile} should be populated.
#'   \item Using a WebApi interface to retrieve the cohort definitions. In this case, the \code{baseUrl} and \code{cohortSetReference} should be populated.
#'   \item Skipping meta-data on the cohorts altogether. In this case, only the \code{cohortSetReference} needs to be populated.
#' }
#'
#' The \code{cohortSetReference} argument must be a data frame with  the following columns:
#' \describe{
#' \item{atlasId}{The cohort ID in ATLAS.}
#' \item{atlasName}{The full name of the cohort. This will be shown in the Shiny app.}
#' \item{cohortId}{The cohort ID to use in the package. Usually the same as the cohort ID in ATLAS.}
#' \item{name}{A short name for the cohort, to use to create file names. do not use special characters.}
#' }
#' @export
exportPheValuatorResults <- function(outputFolder,
                                     exportFolder = file.path(outputFolder, "export"),
                                     pheValuatorAnalysisList,
                                     packageName = NULL,
                                     cohortToCreateFile = "settings/CohortsToCreate.csv",
                                     baseUrl = NULL,
                                     cohortSetReference = NULL,
                                     minCellCount = 5) {
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder)
  }

  cohortIds <- extractCohortIds(pheValuatorAnalysisList)

  if (is.null(packageName)) {
    cohorts <- loadCohortsFromWebApi(baseUrl = baseUrl,
                                     cohortSetReference = cohortSetReference,
                                     cohortIds = cohortIds)
  } else {
    cohorts <- loadCohortsFromPackage(packageName = packageName,
                                      cohortToCreateFile = cohortToCreateFile,
                                      cohortIds = cohortIds)
  }
  writeToCsv(cohorts, file.path(exportFolder, "cohort.csv"))

  ParallelLogger::logInfo("Saving database metadata")
  database <- tibble::tibble(databaseId = databaseId,
                             databaseName = databaseName,
                             description = databaseDescription)
  writeToCsv(database, file.path(exportFolder, "database.csv"))

  ParallelLogger::logInfo("Saving analysis specifications")
  tempFileName <- tempfile()
  pheValuatorAnalysisToRow <- function(pheValuatorAnalysis) {
    ParallelLogger::saveSettingsToJson(pheValuatorAnalysis, tempFileName)
    row <- tibble::tibble(analysisId = pheValuatorAnalysis$analysisId,
                          description = pheValuatorAnalysis$description,
                          definition = readChar(tempFileName, file.info(tempFileName)$size))
    return(row)
  }
  pheValuatorAnalysis <- lapply(pheValuatorAnalysisList, pheValuatorAnalysisToRow)
  pheValuatorAnalysis <- dplyr::bind_rows(pheValuatorAnalysis)
  pheValuatorAnalysis <- unique(pheValuatorAnalysis)
  unlink(tempFileName)
  writeToCsv(pheValuatorAnalysis, file.path(exportFolder, "phevaluator_analysis.csv"))

  ParallelLogger::logInfo("Saving PheValuator results")
  referenceTable <- readRDS(file.path(outputFolder, "reference.rds"))
  analysisSummary <- summarizePheValuatorAnalyses(referenceTable, outputFolder)
  results <- analysisSummary[, c("analysisId", "truePositives", "falsePositives", "trueNegatives", "falseNegatives")]
  results$databaseId <- rep(databaseId, nrow(results))
  if (nrow(results) > 0) {
    results <- enforceMinCellValue(results, fieldName = "truePositives", minValues = minCellCount)
    results <- enforceMinCellValue(results, fieldName = "falsePositives", minValues = minCellCount)
    results <- enforceMinCellValue(results, fieldName = "trueNegatives", minValues = minCellCount)
    results <- enforceMinCellValue(results, fieldName = "falseNegatives", minValues = minCellCount)
  }
  writeToCsv(results, file.path(exportFolder, "phevaluator_result.csv"))

  # Add all to zip file -------------------------------------------------------------------------------
  ParallelLogger::logInfo("Adding results to zip file")
  zipName <- file.path(exportFolder, paste0("PheValuatorResults_", databaseId, ".zip"))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

writeToCsv <- function(data, fileName, incremental = FALSE, ...) {
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  readr::write_csv(data, fileName)
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- as.vector(!is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0)
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    ParallelLogger::logInfo("   censoring ",
                            sum(toCensor),
                            " values (",
                            percent,
                            "%) from ",
                            fieldName,
                            " because value below minimum")
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

loadCohortsFromPackage <- function(packageName, cohortToCreateFile, cohortIds) {
  pathToCsv <- system.file(cohortToCreateFile, package = packageName)
  cohorts <- readr::read_csv(pathToCsv, col_types = readr::cols())
  cohorts$atlasId <- NULL
  if (!is.null(cohortIds)) {
    cohorts <- cohorts[cohorts$cohortId %in% cohortIds, ]
  }
  if ("atlasName" %in% colnames(cohorts)) {
    cohorts <- dplyr::rename(cohorts, cohortName = "name", cohortFullName = "atlasName")
  } else {
    cohorts <- dplyr::rename(cohorts, cohortName = "name", cohortFullName = "fullName")
  }

  getSql <- function(name) {
    pathToSql <- system.file("sql", "sql_server", paste0(name, ".sql"), package = packageName)
    sql <- readChar(pathToSql, file.info(pathToSql)$size)
    return(sql)
  }
  cohorts$sql <- sapply(cohorts$cohortName, getSql)
  getJson <- function(name) {
    pathToJson <- system.file("cohorts", paste0(name, ".json"), package = packageName)
    json <- readChar(pathToJson, file.info(pathToJson)$size)
    return(json)
  }
  cohorts$json <- sapply(cohorts$cohortName, getJson)
  return(cohorts)
}

loadCohortsFromWebApi <- function(baseUrl,
                                  cohortSetReference,
                                  cohortIds = NULL,
                                  generateStats = TRUE) {
  cohorts <- cohortSetReference
  if (!is.null(cohortIds)) {
    cohorts <- cohorts[cohorts$cohortId %in% cohortIds, ]
  }
  cohorts <- dplyr::rename(cohorts, cohortName = "name", cohortFullName = "atlasName")
  ParallelLogger::logInfo("Retrieving cohort definitions from WebAPI")
  if (!is.null(baseUrl)) {
    if (!isInstalled("ROhdsiWebApi")) {
      stop(" ROhdsiWebApi is required but is not installed. Please install it first")
    }
    for (i in 1:nrow(cohorts)) {
      ParallelLogger::logInfo("- Retrieving definitions for cohort ", cohorts$cohortFullName[i])
      cohortExpression <-  ROhdsiWebApi::getCohortDefinitionExpression(definitionId = cohorts$atlasId[i],
                                                                       baseUrl = baseUrl)
      cohorts$json[i] <- cohortExpression$expression
      cohorts$sql[i] <- ROhdsiWebApi::getCohortDefinitionSql(definitionId = cohorts$atlasId[i],
                                                             baseUrl = baseUrl,
                                                             generateStats = generateStats)
    }
  }
  cohorts$atlasId <- NULL
  return(cohorts)
}

isInstalled <- function(pkg, version = 0) {
  installedVersion <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  return(!is.na(installedVersion) && installedVersion >= version)
}

extractCohortIds <- function(pheValuatorAnalysisList) {
  cohortIds <- c()
  for (i in 1:length(pheValuatorAnalysisList)) {
    if (is.list(pheValuatorAnalysisList[[i]]) &&
        length(pheValuatorAnalysisList[[i]]) > 0) {
      cohortIds <- c(cohortIds, extractCohortIds(pheValuatorAnalysisList[[i]]))
    } else if (grepl("CohortId$", names(pheValuatorAnalysisList)[i])[[1]]) {
      cohortIds <- c(cohortIds, pheValuatorAnalysisList[[i]])
    }
  }
  cohortIds <- unique(cohortIds)
  cohortIds <- cohortIds[cohortIds != 0]
  return(cohortIds)
}
