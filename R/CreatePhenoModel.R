# @file createPhenotypeModel.R
#
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

#' Create the phenotype model
#'
#' @description
#' Create the phenotype model
#'
#' @details
#' Function to create a diagnostic prediction model for a health outcome of interest using the xSpec
#' cohort.  The model may be applied to the evaluation cohort to determine probabilities for each
#' subject for the health outcome of interest.
#'
#' @param connectionDetails      connectionDetails created using the function createConnectionDetails
#'                               in the DatabaseConnector package.
#' @param cdmDatabaseSchema      The name of the database schema that contains the OMOP CDM instance.
#'                               Requires read permissions to this database. On SQL Server, this should
#'                               specifiy both the database and the schema, so for example
#'                               'cdm_instance.dbo'.
#' @param cohortDatabaseSchema   The name of the database schema that is the location where the cohort
#'                               data used to define the at risk cohort is available. Requires read
#'                               permissions to this database.
#' @param cohortTable            The tablename that contains the at risk cohort. The expectation is
#'                               cohortTable has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                               COHORT_START_DATE, COHORT_END_DATE.
#' @param workDatabaseSchema      The name of a database schema where the user has write capability.  A
#'                               temporary cohort table will be created here.
#' @param modelOutputFileName    A string designation for the training model file
#' @param xSpecCohortId            The number of the "extremely specific (xSpec)" cohort definition id in
#'                               the cohort table (for noisy positives)
#' @param xSensCohortId            The number of the "extremely sensitive (xSens)" cohort definition id
#'                               in the cohort table (used to exclude subjects from the base population)
#' @param prevalenceCohortId       The number of the cohort definition id to determine the disease prevalence,
#'                               (default=xSensCohortId)
#' @param covariateSettings       A covariateSettings object as generated using createCovariateSettings()
#' @param mainPopulationCohortId   The number of the cohort ID to be used as a base population for the model
#'                               (default=NULL)
#' @param mainPopulationCohortIdStartDay The number of days relative to the mainPopulationCohortId cohort start date
#'                              to begin including visits (default=0)
#' @param mainPopulationCohortIdEndDay   The number of days relative to the mainPopulationCohortId cohort start date
#'                              to end including visits (default=0)
#' @param lowerAgeLimit          The lower age for subjects in the model (default=NULL)
#' @param upperAgeLimit          The upper age for subjects in the model (default=NULL)
#' @param visitLength            The minimum length of index visit for noisy negative comparison (default=3)
#' @param gender                 The gender(s) to be included (default c(8507, 8532))
#' @param startDate              The starting date for including subjects in the model (default=NULL)
#' @param endDate                The ending date for including subjects in the model (default=NULL)
#' @param removeSubjectsWithFutureDates             Should dates be checked to remove future dates (default=TRUE)
#' @param cdmVersion             The CDM version of the database (default=5)
#' @param outFolder              The folder where the output files will be written (default=working directory)
#' @param modelType              The type of health outcome in the model either "acute" or "chronic" (Default = "chronic")
#'
#' @importFrom stats runif
#'
#' @export
createPhenotypeModel <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 workDatabaseSchema,
                                 modelOutputFileName = "train",
                                 xSpecCohortId,
                                 xSensCohortId,
                                 prevalenceCohortId = xSensCohortId,
                                 covariateSettings,
                                 mainPopulationCohortId = 0,
                                 mainPopulationCohortIdStartDay = 0,
                                 mainPopulationCohortIdEndDay = 0,
                                 lowerAgeLimit = 0,
                                 upperAgeLimit = 120,
                                 visitLength = 3,
                                 gender = c(8507, 8532),
                                 startDate = "19000101",
                                 endDate = "21000101",
                                 removeSubjectsWithFutureDates = TRUE,
                                 cdmVersion = "5",
                                 outFolder = getwd(),
                                 modelType = "chronic") {

  options(error = NULL)
  options(scipen=999)

  # error checking for input
  if (modelType != "chronic" & modelType != "acute")
    stop("...modelType must be acute or chronic")
  if (length(connectionDetails) == 0)
    stop("...must supply a connection string")
  if (xSpecCohortId == "")
    stop("...must have an xSpec cohort id (e.g., 1234)")
  if (xSensCohortId == "")
    stop("...must have an xSens cohort id (e.g., 1235)")
  if (prevalenceCohortId == "")
    stop("...must have an prevalence cohort (prevCohort) (e.g., 1235)")
  if (cdmDatabaseSchema == "")
    stop("....must have a defined CDM schema (e.g., \"YourCDM.YourCDMSchema\")")
  if (cohortDatabaseSchema == "")
    stop("....must have a defined Cohort schema ((e.g., \"YourCDM.YourCohortSchema\")")
  if (cohortTable == "")
    stop("....must have a defined Cohort table (e.g., \"cohort\")")
  if (workDatabaseSchema == "")
    stop("....must have a defined Out Database schema (e.g., \"scratch.dbo\")")
  if (modelOutputFileName == "")
    stop("....must have a defined training file name (e.g., \"train_10XDiabetes\")")

  workFolder <- outFolder

  conn <- DatabaseConnector::connect(connectionDetails)

  # determine population prevalence for correct xSpec/noisy negative popn ratio
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "getPopnPrev.sql",
                                              package = "PheValuator"))

  sql <- SqlRender::render(sqlScript,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_database_table = cohortTable,
                           lowerAgeLimit = lowerAgeLimit,
                           upperAgeLimit = upperAgeLimit,
                           gender = gender,
                           startDate = startDate,
                           endDate = endDate,
                           prevCohort = prevalenceCohortId,
                           removeSubjectsWithFutureDates = removeSubjectsWithFutureDates)

  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  popPrev <- DatabaseConnector::querySql(conn = conn, sql)


  if (popPrev == 0)
    stop("...unable to calculate the expected prevalence, possibly an error with prevalence cohort id")

  writeLines(paste("\nEstimated Population Prevalence: ", popPrev, sep = ""))

  # set reasonable model populations - for fast, but accurate models
  if (popPrev >= 0.3) {
    xspecSize <- 4000  #use large xSpec size for higher prevalence values
  } else if (popPrev >= 0.2) {
    xspecSize <- 3000
  } else if (popPrev >= 0.1) {
    xspecSize <- 2000
  } else {
    xspecSize <- 1500  #use smaller xSpec size for lower prevalence values
  }

  # set the number of nosiy negatives in the model either from the prevalence or to 500K max
  baseSampleSize <- min(as.integer(xspecSize/popPrev), 500000)  #use 500,000 as largest base sample


  if(modelType == "acute") {
    # sql script to create a temporary cohort table for predictive modeling
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "CreateCohortsAcuteModel.sql",
                                                package = "PheValuator"))

  } else {
    # sql script to create a temporary cohort table for predictive modeling
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "CreateCohortsV6.sql",
                                                package = "PheValuator"))

  }

  baseSample <- baseSampleSize
  plpDataFile <- file.path(workFolder, paste("plpData_",
                                             modelOutputFileName,
                                             sep = ""))
  resultsFileName <- file.path(workFolder, paste(modelOutputFileName,
                                                 ".rds",
                                                 sep = ""))
  resultsDirName <- file.path(workFolder, paste(modelOutputFileName,
                                                sep = ""))

  if (!file.exists(plpDataFile)) {
    # only pull the plp data if it doesn't already exist create a unique name for the temporary cohort
    test_cohort <- gsub(".",
                        "",
                        (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                        fixed = TRUE)

    # create the temporary cohort
    sql <- SqlRender::render(sqlScript,
                             cdm_database_schema = cdmDatabaseSchema,
                             cohort_database_schema = cohortDatabaseSchema,
                             cohort_database_table = cohortTable,
                             x_spec_cohort = xSpecCohortId,
                             tempDB = workDatabaseSchema,
                             test_cohort = test_cohort,
                             exclCohort = xSensCohortId,
                             ageLimit = lowerAgeLimit,
                             upperAgeLimit = upperAgeLimit,
                             gender = gender,
                             startDate = startDate,
                             endDate = endDate,
                             baseSampleSize = baseSample,
                             xSpecSampleSize = xspecSize,
                             mainPopnCohort = mainPopulationCohortId,
                             mainPopnCohortStartDay = mainPopulationCohortIdStartDay,
                             mainPopnCohortEndDay = mainPopulationCohortIdEndDay,
                             visitLength = visitLength)

    sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

    DatabaseConnector::executeSql(conn = conn, sql)

    # pull the features for the model
    plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                  cdmDatabaseSchema = paste(cdmDatabaseSchema,
                                                                            sep = ""),
                                                  cohortId = 0,
                                                  outcomeIds = xSpecCohortId,
                                                  outcomeDatabaseSchema = workDatabaseSchema,
                                                  outcomeTable = test_cohort,
                                                  cohortDatabaseSchema = workDatabaseSchema,
                                                  cohortTable = test_cohort,
                                                  cdmVersion = cdmVersion,
                                                  washoutPeriod = 0,
                                                  covariateSettings = covariateSettings)

    summary(plpData)
    writeLines(paste0("\nSaving PLP Data to: ",plpDataFile))
    PatientLevelPrediction::savePlpData(plpData, plpDataFile)

    # remove temp cohort table
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "DropTempTable.sql",
                                                package = "PheValuator"))
    sql <- SqlRender::render(sqlScript, tempDB = workDatabaseSchema, test_cohort = test_cohort)
    sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

    DatabaseConnector::executeSql(conn = conn, sql)
  } else {
    writeLines(paste("...Loading ", plpDataFile, " from existing directory", sep = ""))
    plpData <- PatientLevelPrediction::loadPlpData(plpDataFile)
  }

  if (!file.exists(resultsFileName)) {
    # only create the model if it doesn't exist
    population <- PatientLevelPrediction::createStudyPopulation(plpData,
                                                                population = NULL,
                                                                outcomeId = xSpecCohortId,
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

    modelSettings <- PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01, seed = 5)

    lr_results <- PatientLevelPrediction::runPlp(population,
                                                 plpData,
                                                 modelSettings = modelSettings,
                                                 testSplit = "person",
                                                 testFraction = 0.25,
                                                 splitSeed = 5,
                                                 nfold = 3,
                                                 savePlpData = F, savePlpResult = F,
                                                 savePlpPlots = F, saveEvaluation = F, )

    lr_results$PheValuator$inputSetting$xSpecCohortId <- xSpecCohortId
    lr_results$PheValuator$inputSetting$xSensCohortId <- xSensCohortId
    lr_results$PheValuator$inputSetting$prevalenceCohortId <- prevalenceCohortId
    lr_results$PheValuator$inputSetting$mainPopulationCohortId <- mainPopulationCohortId
    lr_results$PheValuator$inputSetting$lowerAgeLimit <- lowerAgeLimit
    lr_results$PheValuator$inputSetting$upperAgeLimit <- upperAgeLimit
    lr_results$PheValuator$inputSetting$startDays <- covariateSettings$longTermStartDays
    lr_results$PheValuator$inputSetting$endDays <- covariateSettings$endDays
    lr_results$PheValuator$inputSetting$visitLength <- visitLength
    lr_results$PheValuator$inputSetting$gender <- paste(unlist(gender), collapse=', ')
    lr_results$PheValuator$inputSetting$startDate <- startDate
    lr_results$PheValuator$inputSetting$endDate <- endDate
    lr_results$PheValuator$inputSetting$modelType <- modelType

    print(resultsFileName)
    saveRDS(lr_results, resultsFileName)

    print(resultsDirName)
    PatientLevelPrediction::savePlpResult(lr_results, resultsDirName)
  } else {
    writeLines(paste("\n...Loading ", resultsFileName, " from existing file", sep = ""))
    lr_results <- readRDS(resultsFileName)
  }

  capture.output(conn <- DatabaseConnector::connect(connectionDetails), file=NULL)
  DatabaseConnector::disconnect(conn)
}

