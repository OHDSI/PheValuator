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

.createPhenotypeModel <- function(connectionDetails,
                                  phenotype,
                                  analysisName,
                                  runDateTime,
                                  databaseId,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  workDatabaseSchema,
                                  tempEmulationSchema,
                                  xSpecCohortId,
                                  daysFromxSpec = 0,
                                  xSensCohortId,
                                  prevalenceCohortId,
                                  xSpecCohortSize = 5000,
                                  covariateSettings,
                                  modelPopulationCohortId = 0,
                                  modelPopulationCohortIdStartDay = 0,
                                  modelPopulationCohortIdEndDay = 0,
                                  priorModelToUse = NULL,
                                  minimumOffsetFromStart = 365,
                                  minimumOffsetFromEnd = 365,
                                  modelBaseSampleSize = 15000,
                                  lowerAgeLimit = 0,
                                  upperAgeLimit = 120,
                                  visitLength = 0,
                                  visitType = c(9201, 9202, 9203, 262, 581477),
                                  gender = c(8507, 8532),
                                  race = 0,
                                  ethnicity = 0,
                                  startDate = "19000101",
                                  endDate = "21000101",
                                  removeSubjectsWithFutureDates = TRUE,
                                  cdmVersion = "5",
                                  outFolder = getwd(),
                                  exportFolder,
                                  randomVisitTable = "",
                                  modelId = "main") {

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  plpDataFile <- file.path(outFolder, sprintf("plpData_%s", modelId))
  modelFileName <- file.path(outFolder, sprintf("model_%s.rds", modelId))
  plpResultsFileName <- file.path(outFolder, sprintf("plpResults_%s", modelId))

  if(!is.null(priorModelToUse)) {
    priorModelToUse <- file.path(priorModelToUse, sprintf("model_%s.rds", modelId))
  }

  visitType <- c(9201, 9202, 9203, 262, 581477) # model is run is all inpatient/outpatient visits

  if (!file.exists(modelFileName) & is.null(priorModelToUse)) {
    # get xSpec subjects to create a model
    sql <- SqlRender::loadRenderTranslateSql(
      sqlFilename = "GetxSpecCount.sql",
      packageName = "PheValuator",
      dbms = connectionDetails$dbms,
      cdm_database_schema = cdmDatabaseSchema,
      cohort_database_schema = cohortDatabaseSchema,
      cohort_database_table = cohortTable,
      x_spec_cohort = xSpecCohortId,
      ageLimit = lowerAgeLimit,
      upperAgeLimit = upperAgeLimit,
      gender = gender,
      race = race,
      ethnicity = ethnicity,
      startDate = startDate,
      endDate = endDate,
      daysFromxSpec = daysFromxSpec
    )

    xSpecCount <- as.numeric(DatabaseConnector::querySql(connection = connection, sql = sql))

    if (xSpecCount < 100) { #set to 100 as minimum
      ParallelLogger::logInfo("Too few subjects in xSpec to produce model. (Outcome count = ", xSpecCount, ")")
      ParallelLogger::logInfo("Saving null model summary to ", modelFileName)
      lrResults <- NULL
      lrResults$errorMessage <- "Error: Too few outcomes to produce model"
      saveRDS(lrResults, modelFileName)
    } else {
      if (prevalenceCohortId >= 1) {
        # determine population prevalence for correct xSpec/noisy negative popn ratio
        sql <- SqlRender::loadRenderTranslateSql(
          sqlFilename = "getPopnPrev.sql",
          packageName = "PheValuator",
          dbms = connectionDetails$dbms,
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
          mainPopnCohort = modelPopulationCohortId,
          prevCohort = prevalenceCohortId,
          removeSubjectsWithFutureDates = removeSubjectsWithFutureDates
        )

        popPrev <- as.numeric(DatabaseConnector::querySql(connection = connection, sql))
      } else {
        popPrev <- prevalenceCohortId
      }

      if (is.na(popPrev)) {popPrev <- 0.01} #set default for eunomia data

      if (popPrev == 0) {
        stop("Unable to calculate the expected prevalence, possibly an error with prevalence cohort id")
      }

      ParallelLogger::logInfo(sprintf("Estimated population prevalence is %0.2f%%", 100 * popPrev))

      xspecSize <- min(c(xSpecCohortSize, xSpecCount)) # min value of pre-specified and available xSpec subjects
      baseSampleSize <- modelBaseSampleSize # how many non-cases to include in model development
      prevToUse <- xspecSize / (xspecSize + baseSampleSize) # calculate the prevalence of the model subjects - to be recalibrated

      ParallelLogger::logInfo(sprintf("Using xSpec size of: %i", xspecSize))
      ParallelLogger::logInfo(sprintf("Using base sample size of: %i", (xspecSize + baseSampleSize)))

      if (!file.exists(plpDataFile)) {
        # only pull the plp data if it doesn't already exist create a unique name for the temporary cohort table
        testCohort <- paste0("test_model_", xSpecCohortId, "_", paste(sample(c(letters, 0:9), 8), collapse = ""))

        if (randomVisitTable != "") {
          ParallelLogger::logInfo("Creating model cohort subjects using supplied random visit table: ",
                                  paste0(workDatabaseSchema, ".", randomVisitTable))
        }

        #first check number of eligible visits in db
        sql <- SqlRender::loadRenderTranslateSql("GetNumberOfEligibleVisits.sql",
                                                 packageName = "PheValuator",
                                                 dbms = connectionDetails$dbms,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 cohort_database_table = cohortTable,
                                                 work_database_schema = workDatabaseSchema,
                                                 ageLimit = lowerAgeLimit,
                                                 upperAgeLimit = upperAgeLimit,
                                                 gender = gender,
                                                 race = race,
                                                 ethnicity = ethnicity,
                                                 startDate = startDate,
                                                 endDate = endDate,
                                                 visitType = visitType,
                                                 visitLength = visitLength,
                                                 randomVisitTable = randomVisitTable,
                                                 exclCohort = xSensCohortId
        )
        cntVisits <- DatabaseConnector::querySql(connection = connection, sql)

        # if number of visits is over 100M reduce down by factor of 12 to increase processing speed
        if (cntVisits > 100000000) {
          firstCut <- TRUE
        } else {
          firstCut <- FALSE
        }

        if (file.exists(file.path(outFolder, "modelCohortSubjects.rds"))) {
          # use existing subjects from local file to create a cohort table on server
          cohort <- readRDS(file.path(outFolder, "modelCohortSubjects.rds"))
          ParallelLogger::logInfo("Creating model cohort on server from cohort file")

          tryCatch(
            {
              insertTable(
                connection = connection,
                databaseSchema = workDatabaseSchema,
                tableName = testCohort,
                data = cohort,
                dropTableIfExists = TRUE,
                createTable = TRUE,
                tempTable = FALSE,
                bulkLoad = TRUE,
                progressBar = TRUE,
                camelCaseToSnakeCase = TRUE
              )
            },
            error = function(cond) {
              if (grepl("Bulk load credentials", cond, fixed = TRUE)) {
                message(paste0("...bulk load failed...trying without bulk load...this may be slow"))
                insertTable(
                  connection = connection,
                  databaseSchema = workDatabaseSchema,
                  tableName = testCohort,
                  data = cohort,
                  dropTableIfExists = TRUE,
                  createTable = TRUE,
                  tempTable = FALSE,
                  bulkLoad = FALSE,
                  progressBar = TRUE,
                  camelCaseToSnakeCase = TRUE
                )
              } else {
                stop(cond)
              }
            }
          )
        } else { # otherwise create the model cohort from an sql query

          sqlFileName <- "CreateCohortsAcuteModel.sql"

          if(daysFromxSpec != 0) {
            ParallelLogger::logInfo("Transforming xSpec cohort using daysFromxSpec parameter")
          } else {
            ParallelLogger::logInfo("Using xSpec cohort verbatim (daysFromxSpec = 0)")
          }

          ParallelLogger::logInfo("Subsetting and sampling cohorts")
          sql <- SqlRender::loadRenderTranslateSql(
            sqlFilename = sqlFileName,
            packageName = "PheValuator",
            dbms = connectionDetails$dbms,
            cdm_database_schema = cdmDatabaseSchema,
            cohort_database_schema = cohortDatabaseSchema,
            cohort_database_table = cohortTable,
            x_spec_cohort = xSpecCohortId,
            daysFromxSpec = daysFromxSpec,
            work_database_schema = workDatabaseSchema,
            test_cohort = testCohort,
            exclCohort = xSensCohortId,
            ageLimit = lowerAgeLimit,
            upperAgeLimit = upperAgeLimit,
            gender = gender,
            race = race,
            ethnicity = ethnicity,
            startDate = startDate,
            endDate = endDate,
            baseSampleSize = format(baseSampleSize, scientific = FALSE),
            xSpecSampleSize = xspecSize,
            mainPopnCohort = modelPopulationCohortId,
            mainPopnCohortStartDay = modelPopulationCohortIdStartDay,
            mainPopnCohortEndDay = modelPopulationCohortIdEndDay,
            minimumOffsetFromStart = minimumOffsetFromStart,
            minimumOffsetFromEnd = minimumOffsetFromEnd,
            visitLength = visitLength,
            visitType = c(visitType),
            randomVisitTable = randomVisitTable,
            firstCut = firstCut
          )
          DatabaseConnector::executeSql(connection = connection, sql)
        }

        ParallelLogger::logInfo("Getting data for prediction model from server")
        databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
          connectionDetails = connectionDetails,
          cdmDatabaseSchema = cdmDatabaseSchema,
          cdmDatabaseName = "CDM",
          tempEmulationSchema = tempEmulationSchema,
          cohortDatabaseSchema = workDatabaseSchema,
          cohortTable = testCohort,
          outcomeDatabaseSchema = workDatabaseSchema,
          outcomeTable = testCohort,
          targetId = 0,
          outcomeIds = xSpecCohortId,
          cdmVersion = 5
        )

        restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
          studyStartDate = startDate,
          studyEndDate = endDate,
          firstExposureOnly = F,
          washoutPeriod = 0,
          sampleSize = NULL
        )

        plpData <- PatientLevelPrediction::getPlpData(
          databaseDetails = databaseDetails,
          covariateSettings = covariateSettings,
          restrictPlpDataSettings = restrictPlpDataSettings
        )

        # summary(plpData)
        ParallelLogger::logInfo("Saving PLP Data to: ", plpDataFile)
        PatientLevelPrediction::savePlpData(plpData, plpDataFile)

        # remove temp cohort table
        sql <- SqlRender::loadRenderTranslateSql(
          sqlFilename = "DropTempTable.sql",
          packageName = "PheValuator",
          dbms = connectionDetails$dbms,
          work_database_schema = workDatabaseSchema,
          test_cohort = testCohort
        )
        sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
        DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = FALSE, reportOverallTime = FALSE)
      } else {
        ParallelLogger::logInfo("Loading ", plpDataFile, " from existing directory")
        plpData <- PatientLevelPrediction::loadPlpData(plpDataFile)
      }

      ParallelLogger::logInfo("Fitting predictive model")
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
        binary = T,
        includeAllOutcomes = T,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        requireTimeAtRisk = FALSE,
        minTimeAtRisk = 0,
        riskWindowStart = 0,
        startAnchor = "cohort start",
        riskWindowEnd = 365,
        endAnchor = "cohort start",
        restrictTarToCohortEnd = F
      )

      population <- PatientLevelPrediction::createStudyPopulation(
        plpData = plpData,
        outcomeId = xSpecCohortId,
        populationSettings = populationSettings,
        population = NULL
      )

      # test population file to see if the model process can proceed - based on population counts
      if (sum(population$outcomeCount) < 100) { # too few outcomes to produce viable model
        ParallelLogger::logInfo("Too few outcomes to produce model. (Outcome count = ", sum(population$outcomeCount), ")")
        ParallelLogger::logInfo("Saving null model summary to ", modelFileName)
        lrResults <- NULL
        lrResults$errorMessage <- "Error: Too few outcomes to produce model"
        saveRDS(lrResults, modelFileName)
      } else {
        tryCatch(
          {
            splitSettings <- PatientLevelPrediction::createDefaultSplitSetting(
              type = "stratified", testFraction = 0.25,
              trainFraction = 0.75, splitSeed = 123, nfold = 2
            )
            sampleSettings <- PatientLevelPrediction::createSampleSettings(type = "none")
            featureEngineeringSettings <- PatientLevelPrediction::createFeatureEngineeringSettings(type = "none")
            preprocessSettings <- PatientLevelPrediction::createPreprocessSettings(minFraction = 0.001, normalize = T)
            modelSettings <- PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01, seed = 5)
            logSettings <- PatientLevelPrediction::createLogSettings(verbosity = "INFO", timeStamp = T, logName = "runPlp Log")
            executeSettings <- PatientLevelPrediction::createExecuteSettings(
              runSplitData = TRUE,
              runSampleData = F,
              runfeatureEngineering = F,
              runPreprocessData = TRUE,
              runModelDevelopment = TRUE,
              runCovariateSummary = TRUE
            )


            lrResults <- PatientLevelPrediction::runPlp(
              plpData = plpData,
              outcomeId = xSpecCohortId,
              analysisId = paste0(gsub("[^[:alnum:] ]", "", Sys.time())),
              analysisName = "Study details",
              populationSettings = populationSettings,
              splitSettings = splitSettings,
              sampleSettings = sampleSettings,
              featureEngineeringSettings = featureEngineeringSettings,
              preprocessSettings = preprocessSettings,
              modelSettings = modelSettings,
              logSettings = logSettings,
              executeSettings = executeSettings,
              saveDirectory = outFolder
            )

            # re-calibrate model
            ParallelLogger::logInfo("Recalibrating model")
            prevToUseOdds <- prevToUse / (1 - prevToUse) # uses prevalence for model building
            popPrevOdds <- popPrev / (1 - popPrev) # uses actual prevalence
            modelYIntercept <- lrResults$model$model$coefficients$betas[1]
            delta <- log(prevToUseOdds) - log(popPrevOdds)
            yIntercept <- as.numeric(lrResults$model$model$coefficients$betas[1])

            #save analysis parameters
            lrResults$model$model$coefficients$betas[1] <- as.numeric(yIntercept - delta) # Equation (7) in King and Zeng (2001)

            lrResults$PheValuator$inputSetting$phenotype <- phenotype
            lrResults$PheValuator$inputSetting$analysisName <- analysisName
            lrResults$PheValuator$inputSetting$databaseId <- databaseId
            lrResults$PheValuator$inputSetting$runDateTime <- runDateTime
            lrResults$PheValuator$inputSetting$xSpecCohortId <- xSpecCohortId
            lrResults$PheValuator$inputSetting$daysFromxSpec <- daysFromxSpec
            lrResults$PheValuator$inputSetting$xSpecCohortId <- xSpecCohortId

            lrResults$PheValuator$inputSetting$xSensCohortId <- xSensCohortId
            lrResults$PheValuator$inputSetting$prevalenceCohortId <- prevalenceCohortId
            lrResults$PheValuator$inputSetting$modelPopulationCohortId <- modelPopulationCohortId
            lrResults$PheValuator$inputSetting$modelPopulationCohortIdStartDay = modelPopulationCohortIdStartDay
            lrResults$PheValuator$inputSetting$modelPopulationCohortIdEndDay = modelPopulationCohortIdEndDay
            lrResults$PheValuator$inputSetting$minimumOffsetFromStart = minimumOffsetFromStart
            lrResults$PheValuator$inputSetting$minimumOffsetFromEnd = minimumOffsetFromEnd
            lrResults$PheValuator$inputSetting$lowerAgeLimit <- lowerAgeLimit
            lrResults$PheValuator$inputSetting$upperAgeLimit <- upperAgeLimit

            lrResults$PheValuator$inputSetting$startDayWindow1 <- covariateSettings[[1]]$longTermStartDays
            lrResults$PheValuator$inputSetting$endDayWindow1 <- covariateSettings[[1]]$endDays

            if(length(covariateSettings) > 1) {
              lrResults$PheValuator$inputSetting$startDayWindow2 <- covariateSettings[[2]]$shortTermStartDays
              lrResults$PheValuator$inputSetting$endDayWindow2 <- covariateSettings[[2]]$endDays
            } else {
              lrResults$PheValuator$inputSetting$startDayWindow2 <- NULL
              lrResults$PheValuator$inputSetting$endDayWindow2 <- NULL
            }

            if(length(covariateSettings) > 2) {
              lrResults$PheValuator$inputSetting$startDayWindow3 <- covariateSettings[[3]]$mediumTermStartDays
              lrResults$PheValuator$inputSetting$endDayWindow3 <- covariateSettings[[3]]$endDays
            } else {
              lrResults$PheValuator$inputSetting$startDayWindow3 <- NULL
              lrResults$PheValuator$inputSetting$endDayWindow3 <- NULL
            }

            lrResults$PheValuator$inputSetting$visitType <- paste(unlist(visitType), collapse = ", ")
            lrResults$PheValuator$inputSetting$visitLength <- visitLength
            lrResults$PheValuator$inputSetting$gender <- paste(unlist(gender), collapse = ", ")
            lrResults$PheValuator$inputSetting$race <- paste(unlist(race), collapse = ", ")
            lrResults$PheValuator$inputSetting$ethnicity <- paste(unlist(ethnicity), collapse = ", ")
            lrResults$PheValuator$inputSetting$startDate <- startDate
            lrResults$PheValuator$inputSetting$endDate <- endDate

            lrResults$PheValuator$runTimeValues$truePrevalencePopulation <- popPrev
            lrResults$PheValuator$runTimeValues$prevalenceModel <- prevToUse
            lrResults$PheValuator$runTimeValues$modelYIntercept <- modelYIntercept
            lrResults$PheValuator$runTimeValues$recalibratedYIntercept <- lrResults$model$model$coefficients[1,1]

            ParallelLogger::logInfo("Saving model summary to ", modelFileName)
            saveRDS(lrResults, modelFileName)

            ParallelLogger::logInfo("Saving PLP results to ", plpResultsFileName)
            PatientLevelPrediction::savePlpResult(lrResults, plpResultsFileName)

            ParallelLogger::logInfo("Saving model results to ", exportFolder)

            df <- data.frame(lrResults$PheValuator$inputSetting)
            colnames(df) <- SqlRender::camelCaseToSnakeCase(colnames(df))
            write.csv(df, file.path(exportFolder, "pv_model_input_parameters.csv"), row.names = FALSE)

            df <- NULL
            df$phenotype <- phenotype
            df$analysisName <- analysisName
            df$databaseId <- databaseId
            df$runDateTime <- runDateTime
            df <- cbind(df, data.frame(lrResults$PheValuator$runTimeValues))
            colnames(df) <- SqlRender::camelCaseToSnakeCase(colnames(df))
            write.csv(df, file.path(exportFolder, "pv_model_run_time_values.csv"), row.names = FALSE)

            df <- NULL
            df$phenotype <- phenotype
            df$analysisName <- analysisName
            df$databaseId <- databaseId
            df$runDateTime <- runDateTime
            df <- cbind(df, lrResults$model$covariateImportance[lrResults$model$covariateImportance$covariateValue != 0,c(4,5,2,1,3)])
            colnames(df) <- SqlRender::camelCaseToSnakeCase(colnames(df))
            write.csv(df, file.path(exportFolder, "pv_model_covariates.csv"), row.names = FALSE)

            df <- NULL
            df$phenotype <- phenotype
            df$analysisName <- analysisName
            df$databaseId <- databaseId
            df$runDateTime <- runDateTime
            df <- cbind(df, lrResults$covariateSummary[lrResults$covariateSummary$covariateValue != 0 &
                                                         !(is.na(lrResults$covariateSummary$covariateValue)),c(2,4,27,11,12,8,9,14)])
            #change first character of each column name to lower case
            for(colNum in 1:ncol(df)) {names(df)[colNum] <- paste0(tolower(substr(names(df)[colNum],1,1)),
                                                                   substr(names(df)[colNum],2,nchar(names(df)[colNum])))}
            colnames(df) <- SqlRender::camelCaseToSnakeCase(colnames(df))
            write.csv(df, file.path(exportFolder, "pv_model_covariate_summary.csv"), row.names = FALSE)

            df <- NULL
            df$phenotype <- phenotype
            df$analysisName <- analysisName
            df$databaseId <- databaseId
            df$runDateTime <- runDateTime
            df$evaluation <- unlist(lrResults$performanceEvaluation$evaluationStatistics$evaluation)
            df$metric <- unlist(lrResults$performanceEvaluation$evaluationStatistics$metric)
            df$valuePerformance <- unlist(lrResults$performanceEvaluation$evaluationStatistics$value)
            df <- data.frame(df)
            colnames(df) <- SqlRender::camelCaseToSnakeCase(colnames(df))
            write.csv(df, file.path(exportFolder, "pv_model_performance.csv"),
                      row.names = FALSE)
          },
          error = function(e) {
            message("error found: ")
            message(e)
            ParallelLogger::logInfo("Saving null model summary to ", modelFileName)
            lrResults <- NULL
            lrResults$errorMessage <- paste0("Error: ", e)
            saveRDS(lrResults, modelFileName)
          }
        )
      }
    }
  } else {
    if(!is.null(priorModelToUse)) {
      if(!file.exists(priorModelToUse)){
        stop("Error: priorModelToUse does not exist")
      } else {
        ParallelLogger::logInfo("Using previously created model file ", priorModelToUse)
        fileCopy <- file.copy(file.path(priorModelToUse),
                              file.path(outFolder, sprintf("model_%s.rds", modelId)),
                              copy.date = TRUE, overwrite = TRUE)
      }
    } else {
      ParallelLogger::logInfo("Skipping creation of ", modelFileName, " because it already exists")
    }
  }
}
