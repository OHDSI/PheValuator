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

.createEvaluationCohort <- function(connectionDetails,
                                    xSpecCohortId,
                                    xSensCohortId,
                                    prevalenceCohortId,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    workDatabaseSchema,
                                    tempEmulationSchema,
                                    covariateSettings,
                                    inclusionEvaluationCohortId = 0,
                                    inclusionEvaluationDaysFromStart = 0,
                                    inclusionEvaluationDaysFromEnd = 0,
                                    exclusionEvaluationCohortId = 0,
                                    exclusionEvaluationDaysFromStart = 0,
                                    exclusionEvaluationDaysFromEnd = 0,
                                    baseSampleSize = 2e+06,
                                    lowerAgeLimit = 0,
                                    upperAgeLimit = 120,
                                    visitLength = 0,
                                    visitType = c(9201, 9202, 9203, 262),
                                    gender = c(8507, 8532),
                                    race = 0,
                                    ethnicity = 0,
                                    startDate = "19001010",
                                    endDate = "21000101",
                                    falsePositiveNegativeSubjects = 10,
                                    cdmVersion = "5",
                                    outFolder = getwd(),
                                    modelId = "main",
                                    evaluationCohortId = "main",
                                    excludeModelFromEvaluation = FALSE,
                                    savePlpData = FALSE) {
  if (savePlpData == TRUE) {
    evaluationCohortPlpDataFileName <- file.path(outFolder, sprintf("evaluationCohortPlpData_%s", evaluationCohortId))
    if (file.exists(evaluationCohortPlpDataFileName)) {
      stop("The savePlpData argument is set to TRUE, but ", evaluationCohortPlpDataFileName, " already exists")
    }
  }
  evaluationCohortFileName <- file.path(outFolder, sprintf("evaluationCohort_%s.rds", evaluationCohortId))
  if (file.exists(evaluationCohortFileName)) {
    ParallelLogger::logInfo("Skipping creation of ", evaluationCohortFileName, " because it already exists")
    return()
  }

  # determine the model file to use to apply to the evaluation cohort
  modelFileName <- file.path(outFolder, sprintf("model_%s.rds", modelId))
  ParallelLogger::logInfo("Reading model from ", modelFileName)
  if (!file.exists(modelFileName)) {
    stop("Model output file (", modelFileName, ") does not exist")
  }
  lrResults <- readRDS(modelFileName)

  # test that viable model was created
  if (!is.null(lrResults$errorMessage)) {
    ParallelLogger::logInfo(lrResults$errorMessage, " - Evaluation cohort not created.")
    saveRDS(lrResults, evaluationCohortFileName)
  } else {
    # get subjects used in model file to exclude from evaluation cohort
    exclSubjectList <- c(lrResults$prediction$subjectId)

    testCohort <- gsub(".",
                       "",
                       (paste0("test_eval_", xSpecCohortId, "_", paste(sample(c(letters, 0:9), 8), collapse = ""))),
                       fixed = TRUE
    ) # unique new cohort name to use

    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))

    if (file.exists(file.path(outFolder, "evaluationCohortSubjects.rds"))) {
      # use existing subjects from local file to create a cohort table on server
      cohort <- readRDS(file.path(outFolder, "evaluationCohortSubjects.rds"))
      ParallelLogger::logInfo("Creating evaluation cohort on server from cohort file")

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
    } else { # otherwise create the evaluation cohort from an sql query

      sqlFilename <- "CreateCohortsAcuteEvaluation.sql"

      if (inclusionEvaluationCohortId != 0) {
        ParallelLogger::logInfo("Creating evaluation cohort subjects using visits from cohort Id: ", inclusionEvaluationCohortId)
      }
      if (exclusionEvaluationCohortId != 0) {
        ParallelLogger::logInfo("Creating evaluation cohort subjects excluding visits from cohort Id: ", exclusionEvaluationCohortId)
      }

      if(prevalenceCohortId < 1) {prevalenceCohortId <= 1001}

      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = sqlFilename,
        packageName = "PheValuator",
        dbms = connectionDetails$dbms,
        cdm_database_schema = cdmDatabaseSchema,
        cohort_database_schema = cohortDatabaseSchema,
        cohort_database_table = cohortTable,
        tempEmulationSchema = tempEmulationSchema,
        x_spec_cohort = xSpecCohortId,
        prevalenceCohortId = prevalenceCohortId,
        work_database_schema = workDatabaseSchema,
        test_cohort = testCohort,
        exclCohort = 0,
        ageLimit = lowerAgeLimit,
        upperAgeLimit = upperAgeLimit,
        gender = gender,
        race = race,
        ethnicity = ethnicity,
        startDate = startDate,
        endDate = endDate,
        baseSampleSize = format(baseSampleSize, scientific = FALSE),
        xSpecSampleSize = 100,
        inclusionEvaluationCohortId = inclusionEvaluationCohortId,
        inclusionEvaluationDaysFromStart = inclusionEvaluationDaysFromStart,
        inclusionEvaluationDaysFromEnd = inclusionEvaluationDaysFromEnd,
        exclusionEvaluationCohortId = exclusionEvaluationCohortId,
        exclusionEvaluationDaysFromStart = exclusionEvaluationDaysFromStart,
        exclusionEvaluationDaysFromEnd = exclusionEvaluationDaysFromEnd,
        visitLength = visitLength,
        visitType = c(visitType)
      )

      ParallelLogger::logInfo("Creating evaluation cohort on server from sql")
      DatabaseConnector::executeSql(connection = connection, sql)
    }

    # will only use the covariates with non-zero betas
    lrNonZeroCovs <- c(lrResults$model$covariateImportance$covariateId[lrResults$model$covariateImportance$covariateValue != 0])

    if (is(covariateSettings, "covariateSettings")) {
      covariateSettings <- list(covariateSettings)
    }
    for (listUp in 1:length(covariateSettings)) {
      covariateSettings[[listUp]]$includedCovariateIds <- c(lrNonZeroCovs)
    }

    evaluationCohortPlpDataFileName <- file.path(outFolder, sprintf("evaluationCohortPlpData_%s", evaluationCohortId))
    if (file.exists(evaluationCohortPlpDataFileName)) {
      ParallelLogger::logInfo("Getting evaluation cohort data from existing folder")
      plpData <- PatientLevelPrediction::loadPlpData(evaluationCohortPlpDataFileName)
    } else {
      ParallelLogger::logInfo("Getting evaluation cohort data from server")

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

      suppressWarnings(
        plpData <- PatientLevelPrediction::getPlpData(
          databaseDetails = databaseDetails,
          covariateSettings = covariateSettings,
          restrictPlpDataSettings = restrictPlpDataSettings
        )
      )
    }

    if (excludeModelFromEvaluation == TRUE) {
      # remove subjects in evaluation cohort that were in model cohort
      excl <- data.frame(plpData$cohorts$rowId[plpData$cohorts$subjectId %in% c(exclSubjectList)])
      xSpec <- c(plpData$outcomes$rowId) # those with outcome need to be left in
      excl <- subset(excl, !(excl[, 1] %in% c(xSpec)))
      plpData$cohorts <- plpData$cohorts[!(plpData$cohorts$rowId %in% c(excl[, 1])), ]
    }

    if (savePlpData == TRUE) {
      ParallelLogger::logInfo("Saving evaluation cohort PLP data to: ", evaluationCohortPlpDataFileName)
      PatientLevelPrediction::savePlpData(plpData, evaluationCohortPlpDataFileName)
    }

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

    ParallelLogger::logInfo("Applying predictive model to evaluation cohort")

    # apply the model to the evaluation cohort
    appResults <- NULL


    # appResults$prediction <- PatientLevelPrediction::predictPlp(
    #   plpModel = lrResults$model, plpData = plpData,
    #   population = population
    # )

    ####################
    ParallelLogger::logInfo("Applying predictive model to evaluation cohort using rapid method")

    startTime <- Sys.time()

    # extract the normalization and stuff
    covs <- lrResults$model$covariateImportance[lrResults$model$covariateImportance$covariateValue !=0,]

    intercept <- lrResults$model$model$coefficients$betas[1]

    covariate <- data.frame(
      coefficient = covs$covariateValue,
      covariateId = covs$covariateId)

    norm <- as.data.frame(lrResults$model$preprocessing$tidyCovariates$normFactors)
    covNorm <- merge(covariate, norm, by = 'covariateId', all.x = T)

    allCovData <- as.data.frame(plpData$covariateData$covariates)

    #allCovData <- merge(allCovData, covNorm, by = 'covariateId', all.x = T)

    allCovData <- dplyr::left_join(allCovData, covNorm, by = 'covariateId')

    allCovData$coeffValue <- allCovData$covariateValue*allCovData$coefficient/allCovData$maxValue
    allCovData$coeffValue[is.na(allCovData$coeffValue)] <- 0

    sumData <- aggregate(allCovData$coeffValue, by=list(Category=allCovData$rowId), FUN=sum)

    sumData$x[is.na(sumData$x)] <- 0
    sumData$x <- sumData$x + intercept
    sumData$probability <- 1/(1+exp(-1*sumData$x))
    sumData$probability <- round(sumData$probability, digits = 3)
    names(sumData)[1] <- "rowId"
    names(sumData)[3] <- "value"

    sumData <- sumData[,c(1,3)]
    appResults$prediction <- merge(population, sumData, by = 'rowId')

    ParallelLogger::logInfo("Time to apply model was ", round(difftime(Sys.time(), startTime, units = c("mins")), digits = 3), " min.")
    ###################

    appResults$prediction$value <- round(appResults$prediction$value, digits = 3)

    pred <- appResults$prediction

    # pull in the xSens cohort
    sql <- SqlRender::loadRenderTranslateSql("GetComparisonCohort.sql",
                                             packageName = "PheValuator",
                                             dbms = connectionDetails$dbms,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable,
                                             cohort_id = prevalenceCohortId
    )
    sql <- SqlRender::translate(sql, connectionDetails$dbms)
    comparisonPopn <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
    # add the start dates from the comparison cohort to the evaluation cohort to be able to create
    # dataframe of TP, FP, TN, FN
    finalPopn <- merge(pred, comparisonPopn, all.x = TRUE)

    #determine TP, FP, TN, FN
    fullTestCases <- NULL
    testCases <- finalPopn[is.na(finalPopn$comparisonCohortStartDate) & finalPopn$outcomeCount == 0 & finalPopn$value > 0.8,]
    testCases <- testCases[order(-testCases[,14]),][1:falsePositiveNegativeSubjects,]
    testCases$type <- "FN"
    fullTestCases <- testCases[!is.na(testCases$subjectId),]

    testCases <- finalPopn[!is.na(finalPopn$comparisonCohortStartDate) & finalPopn$outcomeCount == 0 & finalPopn$value < 0.1,]
    testCases <- testCases[order(testCases[,14]),][1:falsePositiveNegativeSubjects,]
    testCases$type <- "FP"
    fullTestCases <- rbind(fullTestCases, testCases[!is.na(testCases$subjectId),])

    testCases <- finalPopn[!is.na(finalPopn$comparisonCohortStartDate) & finalPopn$outcomeCount == 0 & finalPopn$value > 0.8,]
    testCases <- testCases[order(-testCases[,14]),][1:10,]
    testCases$type <- "TP"
    fullTestCases <- rbind(fullTestCases, testCases[!is.na(testCases$subjectId),])

    testCases <- finalPopn[is.na(finalPopn$comparisonCohortStartDate) & finalPopn$outcomeCount == 0 & finalPopn$value < 0.1,]
    testCases <- testCases[order(testCases[,14]),][1:10,]
    testCases$type <- "TN"
    fullTestCases <- rbind(fullTestCases, testCases[!is.na(testCases$subjectId),])

    fullSubjectList <- tibble::tibble(fullTestCases)
    if(nrow(fullTestCases) > 0) { #no test cases found
      subjectCovariates <- data.frame()
      for(subjectUp in 1:nrow(fullTestCases)) {
        rowId <- fullTestCases$rowId[[subjectUp]]
        covs <- allCovData[allCovData$rowId == rowId,]
        modelCovs <- lrResults$model$covariateImportance[lrResults$model$covariateImportance$covariateValue != 0,]
        covs <- merge(covs, modelCovs, by="covariateId", all.x = TRUE)
        covs <- covs[!(is.na(covs$analysisId)),]

        covs$subjectId <- fullTestCases$subjectId[[subjectUp]]
        covs$type <- fullTestCases$type[[subjectUp]]

        if(nrow(subjectCovariates) == 0) {
          subjectCovariates <- covs[!is.na(covs$subjectId),]
        } else {
          subjectCovariates <- rbind(subjectCovariates, covs[!is.na(covs$subjectId),])
        }
      }
    } else {
      ParallelLogger::logInfo("No test cases found")
    }

    # add other parameters to the input settings list
    appResults$PheValuator$inputSetting$startDays <- covariateSettings[[1]]$longTermStartDays
    appResults$PheValuator$inputSetting$endDays <- covariateSettings[[1]]$endDays
    appResults$PheValuator$inputSetting$visitLength <- visitLength
    appResults$PheValuator$inputSetting$xSpecCohortId <- xSpecCohortId
    appResults$PheValuator$inputSetting$xSensCohortId <- xSensCohortId
    appResults$PheValuator$inputSetting$lowerAgeLimit <- lowerAgeLimit
    appResults$PheValuator$inputSetting$upperAgeLimit <- upperAgeLimit
    appResults$PheValuator$inputSetting$gender <- paste(unlist(gender), collapse = ", ")
    appResults$PheValuator$inputSetting$race <- paste(unlist(race), collapse = ", ")
    appResults$PheValuator$inputSetting$ethnicity <- paste(unlist(ethnicity), collapse = ", ")
    appResults$PheValuator$inputSetting$startDate <- startDate
    appResults$PheValuator$inputSetting$endDate <- endDate
    appResults$PheValuator$inputSetting$inclusionEvaluationCohortId <- inclusionEvaluationCohortId
    appResults$PheValuator$inputSetting$inclusionEvaluationDaysFromStart <- inclusionEvaluationDaysFromStart
    appResults$PheValuator$inputSetting$inclusionEvaluationDaysFromEnd <- inclusionEvaluationDaysFromEnd
    appResults$PheValuator$inputSetting$exclusionEvaluationCohortId <- exclusionEvaluationCohortId
    appResults$PheValuator$inputSetting$exclusionEvaluationDaysFromStart <- exclusionEvaluationDaysFromStart
    appResults$PheValuator$inputSetting$exclusionEvaluationDaysFromEnd <- exclusionEvaluationDaysFromEnd
    appResults$PheValuator$inputSetting$excludeModelFromEvaluation <- excludeModelFromEvaluation

    appResults$PheValuator$modelperformanceEvaluation <- lrResults$performanceEvaluation

    if(nrow(fullTestCases) > 0) { #no test cases found
      appResults$PheValuator$testCases <- as.data.frame(fullTestCases)
      appResults$PheValuator$testCaseCovariates <- subjectCovariates[,c(11,12,9,6,7)]
    } else {
      appResults$PheValuator$testCases <- NULL
      appResults$PheValuator$testCaseCovariates <- NULL
    }

    # save the full data set to the model
    appResults$prediction <- finalPopn

    ParallelLogger::logInfo("Saving evaluation cohort to: ", evaluationCohortFileName)
    saveRDS(appResults, evaluationCohortFileName)

    # remove temp cohort table
    sql <- SqlRender::loadRenderTranslateSql(
      sqlFilename = "DropTempTable.sql",
      packageName = "PheValuator",
      dbms = connectionDetails$dbms,
      work_database_schema = workDatabaseSchema,
      test_cohort = testCohort
    )
    DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = FALSE, reportOverallTime = FALSE)
  }
}
