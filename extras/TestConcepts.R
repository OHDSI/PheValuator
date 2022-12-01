# @file TestConcepts.R
#
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

#' testConcepts
#'
#' @description
#' Test the concept sets used in the models
#'
#' @details
#' Function to examine the concepts in the xSpec cohort compared to the noisy negative cohort. This
#' will give a sense of the comrrectnes/completeness of the concept set used for the model
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
#' @param outDatabaseSchema      The name of a database schema where the user has write capability.  A
#'                               temporary cohort table will be created here.
#' @param testOutFile            A string designation for the test model file
#' @param exclCohort             The number of the "extremely sensitive (xSens)" cohort definition id
#'                               in the cohort table (used to estimate population prevalence and to
#'                               exclude subjects from the noisy positives)
#' @param inclConcepts           A list of concepts that were included in the definition
#' @param modelAnalysisId        Another string for designating the name for the model files
#' @param cdmShortName           A short name for the current database (CDM)
#' @param mainPopnCohort         The number of the cohort to be used as a base population for the model
#'                               (default=NULL)
#' @param lowerAgeLimit          The lower age for subjects in the model (default=NULL)
#' @param upperAgeLimit          The upper age for subjects in the model (default=NULL)
#' @param gender                 The gender(s) to be included (default c(8507, 8532))
#' @param startDate              The starting date for including subjects in the model (default=NULL)
#' @param endDate                The ending date for including subjects in the model (default=NULL)
#'
#' @importFrom stats runif
#' @importFrom dplyr summarize
#'
#' @export
testConcepts <- function(connectionDetails = list(),
                         xSpecCohort = "",
                         cdmDatabaseSchema = "",
                         cohortDatabaseSchema = "",
                         cohortDatabaseTable = "",
                         outDatabaseSchema = "",
                         testOutFile = "",
                         exclCohort = "",
                         modelAnalysisId = "1",
                         inclConcepts = c(),
                         cdmShortName = "CDM",
                         mainPopnCohort = 0,
                         lowerAgeLimit = 0,
                         upperAgeLimit = 120,
                         gender = c(8507, 8532),
                         startDate = "19000101",
                         endDate = "21000101") {

  options(error = NULL)

  # error checking for input
  if (length(connectionDetails) == 0)
    stop("...must supply a connection string")
  if (xSpecCohort == "")
    stop("...must have an xSpec cohort id (e.g., 1234)")
  if (exclCohort == "")
    stop("...must have an exclusion cohort (exclCohort) (e.g., 1235)")
  if (cdmDatabaseSchema == "")
    stop("....must have a defined CDM schema (e.g., \"YourCDM.YourCDMSchema\")")
  if (cohortDatabaseSchema == "")
    stop("....must have a defined Cohort schema ((e.g., \"YourCDM.YourCohortSchema\")")
  if (cohortDatabaseTable == "")
    stop("....must have a defined Cohort table (e.g., \"cohort\")")
  if (outDatabaseSchema == "")
    stop("....must have a defined Out Database schema (e.g., \"scratch.dbo\")")
  if (testOutFile == "")
    stop("....must have a defined test file name (e.g., \"test_10XDiabetes\")")

  writeLines(paste("xSpecCohort ", xSpecCohort))
  writeLines(paste("cdmDatabaseSchema ", cdmDatabaseSchema))
  writeLines(paste("cohortDatabaseSchema ", cohortDatabaseSchema))
  writeLines(paste("cohortDatabaseTable ", cohortDatabaseTable))
  writeLines(paste("outDatabaseSchema ", outDatabaseSchema))
  writeLines(paste("testOutFile ", testOutFile))
  writeLines(paste("exclCohort ", exclCohort))
  writeLines(paste("modelAnalysisId ", modelAnalysisId))
  writeLines(paste("inclConcepts ", c(inclConcepts)))
  writeLines(paste("cdmShortName ", cdmShortName))
  writeLines(paste("mainPopnCohort ", mainPopnCohort))
  writeLines(paste("lowerAgeLimit ", lowerAgeLimit))
  writeLines(paste("upperAgeLimit ", upperAgeLimit))
  writeLines(paste("gender ", gender))
  writeLines(paste("startDate ", startDate))
  writeLines(paste("endDate ", endDate))

  workFolder <- getwd()

  conn <- DatabaseConnector::connect(connectionDetails)

  xspecSize <- 1e+05  #use xSpec sample size

  # set the number of nosiy negatives in the model either from the prevalence or to 500K max
  baseSampleSize <- 1e+05

  # sql script to create a temporary cohort table for predictive modeling
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "CreateCohortsV6.sql",
                                              package = "PheValuator"))

  covariateSettings <- FeatureExtraction::createCovariateSettings(useConditionOccurrenceLongTerm = TRUE,
                                                                  longTermStartDays = -10000,
                                                                  endDays = 10000,
                                                                  includedCovariateConceptIds = c(),
                                                                  addDescendantsToInclude = TRUE,
                                                                  excludedCovariateConceptIds = c(),
                                                                  addDescendantsToExclude = TRUE,
                                                                  includedCovariateIds = c())

  baseSample <- baseSampleSize
  plpDataFile <- file.path(workFolder, paste("plpData_",
                                             testOutFile,
                                             "_",
                                             cdmShortName,
                                             "_",
                                             modelAnalysisId,
                                             sep = ""))

  if (!file.exists(plpDataFile)) {
    # only pull the plp data if it doesn't already exist create a unique name for the temporary cohort
    test_cohort <- gsub(".",
                        "",
                        (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                        fixed = TRUE)

    # create the temporary cohort
    sql <- SqlRender::renderSql(sqlScript,
                                cdm_database_schema = cdmDatabaseSchema,
                                cohort_database_schema = cohortDatabaseSchema,
                                cohort_database_table = cohortDatabaseTable,
                                x_spec_cohort = xSpecCohort,
                                tempDB = outDatabaseSchema,
                                test_cohort = test_cohort,
                                exclCohort = exclCohort,
                                ageLimit = lowerAgeLimit,
                                upperAgeLimit = upperAgeLimit,
                                gender = gender,
                                startDate = startDate,
                                endDate = endDate,
                                baseSampleSize = baseSample,
                                xSpecSampleSize = xspecSize,
                                noise = 0,
                                mainPopnCohort = mainPopnCohort,
                                lookback = -1095)
    # for the xSpec model include 1095 days before first dx

    sql <- SqlRender::translateSql(sql$sql, targetDialect = connectionDetails$dbms)

    DatabaseConnector::executeSql(conn = conn, sql$sql)

    # pull the features for the model
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
    print(plpDataFile)
    PatientLevelPrediction::savePlpData(plpData, plpDataFile)

    # remove temp cohort table
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "DropTempTable.sql",
                                                package = "PheValuator"))
    sql <- SqlRender::renderSql(sqlScript, tempDB = outDatabaseSchema, test_cohort = test_cohort)
    sql <- SqlRender::translateSql(sql$sql, targetDialect = connectionDetails$dbms)

    DatabaseConnector::executeSql(conn = conn, sql$sql)
  } else {
    writeLines(paste("...Loading ", plpDataFile, " from existing directory", sep = ""))
    plpData <- PatientLevelPrediction::loadPlpData(plpDataFile)
  }

  # produce the output for analyzing the concept set
  covDataRef <- data.frame(plpData$covariateRef)
  covDataAll <- merge(data.frame(plpData$covariates), covDataRef, by = "covariateId")
  covDataAll <- merge(covDataAll, data.frame(plpData$cohorts), by = "rowId")

  subjAll <- c(plpData$cohorts$subjectId)
  subjOut <- c(plpData$cohorts$subjectId[plpData$cohorts$rowId %in% plpData$outcomes$rowId])
  subjNoOut <- c(plpData$cohorts$subjectId[!(plpData$cohorts$rowId %in% plpData$outcomes$rowId)])

  data0 <- covDataAll[covDataAll$subjectId %in% c(subjNoOut), ]
  data1 <- covDataAll[covDataAll$subjectId %in% c(subjOut), ]
  cov0 <- dplyr::summarize(group_by(data0, as.character(covariateId)), n())
  cov1 <- dplyr::summarize(group_by(data1, as.character(covariateId)), n())

  names(cov0)[1] <- "covariateId"
  names(cov0)[2] <- "n"
  cov0$prop <- cov0$n/length(subjNoOut)

  names(cov1)[1] <- "covariateId"
  names(cov1)[2] <- "n"
  cov1$prop <- cov1$n/length(subjOut)

  conceptCompare <- merge(cov1, cov0, by = "covariateId", all.x = TRUE)
  conceptCompare$prop.y[is.na(conceptCompare$prop.y)] <- 1e-15
  conceptCompare$n.y[is.na(conceptCompare$n.y)] <- 0
  conceptCompare$pr <- conceptCompare$prop.x/conceptCompare$prop.y
  conceptCompare <- conceptCompare[conceptCompare$n.x >= 10, ]
  conceptCompare <- merge(conceptCompare, covDataRef, by = "covariateId")
  conceptCompare$inConceptList <- !is.na(stringr::str_locate(paste(inclConcepts, collapse = ", "),
                                                             as.character(conceptCompare$conceptId))[1])
  conceptCompare$name <- substr(conceptCompare$covariateName,
                                stringr::str_locate(conceptCompare$covariateName, ":")[1] + 2,
                                length(conceptCompare$covariateName))
  names(conceptCompare)[names(conceptCompare) == "n.x"] <- "countxSpec"
  names(conceptCompare)[names(conceptCompare) == "prop.x"] <- "proportionxSpec"
  names(conceptCompare)[names(conceptCompare) == "n.y"] <- "countNeg"
  names(conceptCompare)[names(conceptCompare) == "prop.y"] <- "proportionNeg"
  conceptCompare <- conceptCompare[, c(9, 11, 10, 6, 2, 3, 4, 5)]
  conceptCompare <- conceptCompare[with(conceptCompare, order(-pr)), ]

  outFile <- file.path(getwd(), paste("conceptCompare_",
                                      testOutFile,
                                      "_",
                                      cdmShortName,
                                      "_",
                                      modelAnalysisId,
                                      ".csv",
                                      sep = ""))
  write.csv(conceptCompare, outFile, row.names = FALSE)
  writeLines(paste("\nConcept Comparison File: ", outFile, sep = ""))

  DatabaseConnector::disconnect(conn)
}

