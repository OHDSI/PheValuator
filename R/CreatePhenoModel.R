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

.createPhenotypeModel <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  workDatabaseSchema,
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
                                  modelId = "main",
                                  modelType = "chronic") {
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

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
                                           startDate = startDate,
                                           endDate = endDate,
                                           prevCohort = prevalenceCohortId,
                                           removeSubjectsWithFutureDates = removeSubjectsWithFutureDates)
  popPrev <- DatabaseConnector::querySql(connection = connection, sql)

  if (popPrev == 0)
    stop("Unable to calculate the expected prevalence, possibly an error with prevalence cohort id")

  ParallelLogger::logInfo(sprintf("Estimated population prevalence is %0.1f%%", 100 * popPrev))

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

  # set the number of noisy negatives in the model either from the prevalence or to 500K max
  baseSampleSize <- min(as.integer(xspecSize/popPrev), 5e+05)  #use 500,000 as largest base sample

  plpDataFile <- file.path(outFolder, sprintf("plpData_%s", modelId))
  modelFileName <- file.path(outFolder, sprintf("model_%s.rds", modelId))
  plpResultsFileName <- file.path(outFolder, sprintf("plpResults_%s", modelId))

  if (!file.exists(plpDataFile)) {
    # only pull the plp data if it doesn't already exist create a unique name for the temporary cohort
    testCohort <- gsub(".",
                       "",
                       (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                       fixed = TRUE)
    if (modelType == "acute") {
      sqlFileName <- "CreateCohortsAcuteModel.sql"
    } else {
      sqlFileName <- "CreateCohortsV6.sql"
    }
    ParallelLogger::logInfo("Subsetting and sampling cohorts")
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = sqlFileName,
                                             packageName = "PheValuator",
                                             dbms = connection@dbms,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_database_table = cohortTable,
                                             x_spec_cohort = xSpecCohortId,
                                             tempDB = workDatabaseSchema,
                                             test_cohort = testCohort,
                                             exclCohort = xSensCohortId,
                                             ageLimit = lowerAgeLimit,
                                             upperAgeLimit = upperAgeLimit,
                                             gender = gender,
                                             startDate = startDate,
                                             endDate = endDate,
                                             baseSampleSize = baseSampleSize,
                                             xSpecSampleSize = xspecSize,
                                             mainPopnCohort = mainPopulationCohortId,
                                             mainPopnCohortStartDay = mainPopulationCohortIdStartDay,
                                             mainPopnCohortEndDay = mainPopulationCohortIdEndDay,
                                             visitLength = visitLength)
    DatabaseConnector::executeSql(connection = connection, sql)

    plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  cohortId = 0,
                                                  outcomeIds = xSpecCohortId,
                                                  outcomeDatabaseSchema = workDatabaseSchema,
                                                  outcomeTable = testCohort,
                                                  cohortDatabaseSchema = workDatabaseSchema,
                                                  cohortTable = testCohort,
                                                  cdmVersion = cdmVersion,
                                                  washoutPeriod = 0,
                                                  covariateSettings = covariateSettings)

    # summary(plpData)
    ParallelLogger::logInfo("Saving PLP Data to: ", plpDataFile)
    PatientLevelPrediction::savePlpData(plpData, plpDataFile)

    # remove temp cohort table
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "DropTempTable.sql",
                                             packageName = "PheValuator",
                                             dbms = connection@dbms,
                                             tempDB = workDatabaseSchema,
                                             test_cohort = testCohort)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = FALSE, reportOverallTime = FALSE)
  } else {
    ParallelLogger::logInfo("Loading ", plpDataFile, " from existing directory")
    plpData <- PatientLevelPrediction::loadPlpData(plpDataFile)
  }

  if (!file.exists(modelFileName)) {
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
                                                                addExposureDaysToEnd = TRUE)

    modelSettings <- PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01, seed = 5)

    lrResults <- PatientLevelPrediction::runPlp(population,
                                                plpData,
                                                modelSettings = modelSettings,
                                                testSplit = "person",
                                                testFraction = 0.25,
                                                splitSeed = 5,
                                                nfold = 3,
                                                savePlpData = FALSE,
                                                savePlpResult = FALSE,
                                                savePlpPlots = FALSE,
                                                saveEvaluation = FALSE)

    lrResults$PheValuator$inputSetting$xSpecCohortId <- xSpecCohortId
    lrResults$PheValuator$inputSetting$xSensCohortId <- xSensCohortId
    lrResults$PheValuator$inputSetting$prevalenceCohortId <- prevalenceCohortId
    lrResults$PheValuator$inputSetting$mainPopulationCohortId <- mainPopulationCohortId
    lrResults$PheValuator$inputSetting$lowerAgeLimit <- lowerAgeLimit
    lrResults$PheValuator$inputSetting$upperAgeLimit <- upperAgeLimit
    lrResults$PheValuator$inputSetting$startDays <- covariateSettings$longTermStartDays
    lrResults$PheValuator$inputSetting$endDays <- covariateSettings$endDays
    lrResults$PheValuator$inputSetting$visitLength <- visitLength
    lrResults$PheValuator$inputSetting$gender <- paste(unlist(gender), collapse = ", ")
    lrResults$PheValuator$inputSetting$startDate <- startDate
    lrResults$PheValuator$inputSetting$endDate <- endDate
    lrResults$PheValuator$inputSetting$modelType <- modelType

    ParallelLogger::logInfo("Saving model summary to ", modelFileName)
    saveRDS(lrResults, modelFileName)

    ParallelLogger::logInfo("Saving PLP results to ", plpResultsFileName)
    PatientLevelPrediction::savePlpResult(lrResults, plpResultsFileName)
  } else {
    ParallelLogger::logInfo("Skipping creation of ", modelFileName, " because it already exists")
  }
}

