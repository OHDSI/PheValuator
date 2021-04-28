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
                                  xSpecCohortSize = NULL,
                                  covariateSettings,
                                  modelProportion = 0.05,
                                  mainPopulationCohortId = 0,
                                  mainPopulationCohortIdStartDay = 0,
                                  mainPopulationCohortIdEndDay = 0,
                                  lowerAgeLimit = 0,
                                  upperAgeLimit = 120,
                                  visitLength = 3,
                                  visitType = c(9201),
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


  plpDataFile <- file.path(outFolder, sprintf("plpData_%s", modelId))
  modelFileName <- file.path(outFolder, sprintf("model_%s.rds", modelId))
  plpResultsFileName <- file.path(outFolder, sprintf("plpResults_%s", modelId))

  #get xSpec subjects to create a model
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "GetxSpecCount.sql",
                                           packageName = "PheValuator",
                                           dbms = connection@dbms,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_database_table = cohortTable,
                                           x_spec_cohort = xSpecCohortId,
                                           ageLimit = lowerAgeLimit,
                                           upperAgeLimit = upperAgeLimit,
                                           gender = gender,
                                           startDate = startDate,
                                           endDate = endDate)

  xSpecCount <- as.numeric(DatabaseConnector::querySql(connection = connection, sql = sql))
  if (xSpecCount < 200) {
    ParallelLogger::logInfo("Too few subjects in xSpec to produce model. (Outcome count = ", xSpecCount, ")")
    ParallelLogger::logInfo("Saving null model summary to ", modelFileName)
    lrResults <- NULL
    lrResults$errorMessage <- "Error: Too few outcomes to produce model"
    saveRDS(lrResults, modelFileName)
  } else {

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

    if (!is.null(xSpecCohortSize)) { #pre-specified xSpec cohort size
      xspecSize = xSpecCohortSize
    } else {
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
    }

    if (xspecSize > xSpecCount) {xspecSize <- xSpecCount} #set xSpec size to either what was specified or to maximum available

    prevToUse <- as.numeric(modelProportion) #set the proportion for model building - to be re-calibrated

    # set the number of noisy negatives in the model either from the prevalence or to 1500K max
    baseSampleSize <- min(c(as.integer(xspecSize/prevToUse), as.integer(format(1.5e+06, scientific = FALSE))))  #use 1,500,000 as largest base sample

    if (baseSampleSize != as.integer(xspecSize/prevToUse)) {
      xspecSize <- as.integer(baseSampleSize * prevToUse)} #set final xSpec size for low prevalence values if necessary

    ParallelLogger::logInfo(sprintf("Using xSpec size of: %i", xspecSize))
    ParallelLogger::logInfo(sprintf("Using base sample size of: %i", baseSampleSize))

    baseSampleSize <- baseSampleSize - xspecSize #adjust for adding xSpec

    if (!file.exists(plpDataFile)) {
      # only pull the plp data if it doesn't already exist create a unique name for the temporary cohort table
      testCohort <- paste0("test_model_",xSpecCohortId, "_", paste(sample(c(letters, 0:9), 8), collapse = ""))
      if (modelType == "acute") {
        #first check number of eligible visits in db
        sql <- SqlRender::loadRenderTranslateSql("GetNumberOfEligibleVisits.sql",
                                                 packageName = "PheValuator",
                                                 dbms = connectionDetails$dbms,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 cohort_database_table = cohortTable,
                                                 ageLimit = lowerAgeLimit,
                                                 upperAgeLimit = upperAgeLimit,
                                                 gender = gender,
                                                 startDate = startDate,
                                                 endDate = endDate,
                                                 visitType = visitType,
                                                 visitLength = visitLength,
                                                 exclCohort = xSensCohortId)
        cntVisits <- DatabaseConnector::querySql(connection = connection, sql)

        #if number of visits is over 100M reduce down by factor of 12 to increase processing speed
        if (cntVisits > 100000000) {
          firstCut <- TRUE
        } else {
          firstCut <- FALSE
        }
        sqlFileName <- "CreateCohortsAcuteModel.sql"
      } else {
        firstCut <- FALSE
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
                                               baseSampleSize = format(baseSampleSize, scientific = FALSE),
                                               xSpecSampleSize = xspecSize,
                                               mainPopnCohort = mainPopulationCohortId,
                                               mainPopnCohortStartDay = mainPopulationCohortIdStartDay,
                                               mainPopnCohortEndDay = mainPopulationCohortIdEndDay,
                                               visitLength = visitLength,
                                               visitType = c(visitType),
                                               firstCut = firstCut)
      DatabaseConnector::executeSql(connection = connection, sql)

      ParallelLogger::logInfo("Getting data for prediction model from server")
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
      ParallelLogger::logInfo("Fitting predictive model")
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

      #test population file to see if the model process can proceed - based on population counts
      if (sum(population$outcomeCount) < 200) { #too few outcomes to produce viable model
        ParallelLogger::logInfo("Too few outcomes to produce model. (Outcome count = ", sum(population$outcomeCount), ")")
        ParallelLogger::logInfo("Saving null model summary to ", modelFileName)
        lrResults <- NULL
        lrResults$errorMessage <- "Error: Too few outcomes to produce model"
        saveRDS(lrResults, modelFileName)

      } else {
        modelSettings <- PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01, seed = 5)

        tryCatch({
          lrResults <- PatientLevelPrediction::runPlp(population,
                                                      plpData,
                                                      modelSettings = modelSettings,
                                                      testSplit = "stratified",
                                                      testFraction = 0.25,
                                                      splitSeed = 5,
                                                      nfold = 3,
                                                      savePlpData = FALSE,
                                                      savePlpResult = FALSE,
                                                      savePlpPlots = FALSE,
                                                      saveEvaluation = FALSE,
                                                      saveDirectory = outFolder)

          PatientLevelPrediction::runPlp

          #re-calibrate model
          prevToUseOdds <- prevToUse/(1 - prevToUse) #uses prevalence for model building
          popPrevOdds <- popPrev/(1 - popPrev) #uses actual prevalence
          modelYIntercept <- lrResults$model$model$coefficients[1]
          delta <- log(prevToUseOdds) - log(popPrevOdds)
          yIntercept <- as.numeric(lrResults$model$model$coefficients[1])
          lrResults$model$model$coefficients[1] <- as.numeric(yIntercept - delta)  # Equation (7) in King and Zeng (2001)
          lrResults$model$predict <- PatientLevelPrediction:::createTransform(lrResults$model)

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
          lrResults$PheValuator$runTimeValues$truePrevalencePopulation <- popPrev
          lrResults$PheValuator$runTimeValues$prevalenceModel <- prevToUse
          lrResults$PheValuator$runTimeValues$modelYIntercept <- modelYIntercept
          lrResults$PheValuator$runTimeValues$recalibratedYIntercept <- lrResults$model$model$coefficients[1]

          ParallelLogger::logInfo("Saving model summary to ", modelFileName)
          saveRDS(lrResults, modelFileName)

          ParallelLogger::logInfo("Saving PLP results to ", plpResultsFileName)
          PatientLevelPrediction::savePlpResult(lrResults, plpResultsFileName)
        },
        error = function(e) {message("error found: ")
          message(e)
          ParallelLogger::logInfo("Saving null model summary to ", modelFileName)
          lrResults <- NULL
          lrResults$errorMessage <- paste0("Error: ", e)
          saveRDS(lrResults, modelFileName)
        })
      }
    } else {
      ParallelLogger::logInfo("Skipping creation of ", modelFileName, " because it already exists")
    }
  }
}

