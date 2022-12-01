# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of examplePackage
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

#' Execute PheValuator
#'
#' @details
#' This function executes PheValuator.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cdmVersion                     Define the OMOP CDM version used: currently supports "5".
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the cohorts used in this study.
#' @param workDatabaseSchema   The name of the database schema that is the location where
#'                             a table can be created and afterwards removed.
#'                             Requires write permissions to this database.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param databaseName         The full name of the database (e.g. 'Medicare Claims
#'                             Synthetic Public Use Files (SynPUFs)').
#' @param databaseDescription  A short description (several sentences) of the database.
#' @param createCohorts        Create the cohortTable table with the exposure and outcome cohorts?
#' @param runPheValuator       Run PheValuator?
#' @param exportResults        Export the PheValuator results?
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmVersion,
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    workDatabaseSchema = cohortDatabaseSchema,
                    oracleTempSchema = cohortDatabaseSchema,
                    outputFolder,
                    databaseId = "Unknown",
                    databaseName = "Unknown",
                    databaseDescription = "Unknown",
                    createCohorts = TRUE,
                    runPheValuator = TRUE,
                    exportResults = TRUE,
                    minCellCount = 5) {
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  if (!is.null(getOption("fftempdir")) && !file.exists(getOption("fftempdir"))) {
    warning("fftempdir '", getOption("fftempdir"), "' not found. Attempting to create folder")
    dir.create(getOption("fftempdir"), recursive = TRUE)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "phevaluatorLog.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER"))
  
  if (createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    connection <- DatabaseConnector::connect(connectionDetails)
    .createCohorts(connection = connection,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTable = cohortTable,
                   oracleTempSchema = oracleTempSchema,
                   outputFolder = outputFolder)
    DatabaseConnector::disconnect(connection)
  }
  
  if (runPheValuator) {
    ParallelLogger::logInfo("Running PheValuator")
    pathToJson <- system.file("settings", "pheValuatorAnalysisList.json", package = "examplePackage")
    pheValuatorAnalysisList <- PheValuator::loadPheValuatorAnalysisList(pathToJson)
    PheValuator::runPheValuatorAnalyses(connectionDetails = connectionDetails,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        oracleTempSchema = oracleTempSchema,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        workDatabaseSchema = workDatabaseSchema,
                                        cohortTable = cohortTable,
                                        outputFolder = outputFolder,
                                        pheValuatorAnalysisList = pheValuatorAnalysisList,
                                        cdmVersion = cdmVersion)
  }
  
  if (exportResults) {
    ParallelLogger::logInfo("Exporting PheValuator results")
    pathToJson <- system.file("settings", "pheValuatorAnalysisList.json", package = "examplePackage")
    pheValuatorAnalysisList <- PheValuator::loadPheValuatorAnalysisList(pathToJson)
    PheValuator::exportPheValuatorResults(outputFolder = outputFolder,
                                          exportFolder = file.path(outputFolder, "export"),
                                          pheValuatorAnalysisList = pheValuatorAnalysisList,
                                          packageName = "examplePackage",
                                          cohortToCreateFile = "settings/CohortsToCreate.csv",
                                          minCellCount = minCellCount)
    
  }
}
