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

.createEvaluationCohort <- function(connectionDetails,
                                    xSpecCohortId,
                                    xSensCohortId,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    workDatabaseSchema,
                                    covariateSettings,
                                    mainPopulationCohortId = 0,
                                    mainPopulationCohortIdStartDay = 0,
                                    mainPopulationCohortIdEndDay = 0,
                                    baseSampleSize = 2e+06,
                                    lowerAgeLimit = 0,
                                    upperAgeLimit = 120,
                                    visitLength = 0,
                                    visitType = c(9201,9202,9203,262),
                                    gender = c(8507, 8532),
                                    race = 0,
                                    ethnicity = 0,
                                    startDate = "19001010",
                                    endDate = "21000101",
                                    cdmVersion = "5",
                                    outFolder = getwd(),
                                    modelId = "main",
                                    evaluationCohortId = "main",
                                    excludeModelFromEvaluation = FALSE,
                                    savePlpData = FALSE,
                                    modelType = "acute") {

  modelType <- "acute" #force to only using acute settings

  if (savePlpData == TRUE) {
    evaluationCohortPlpDataFileName <- file.path(outFolder, sprintf("evaluationCohortPlpData_%s", evaluationCohortId))
    if (file.exists(evaluationCohortPlpDataFileName))
      stop("The savePlpData argument is set to TRUE, but ", evaluationCohortPlpDataFileName, " already exists")
  }
  evaluationCohortFileName <- file.path(outFolder, sprintf("evaluationCohort_%s.rds", evaluationCohortId))
  if (file.exists(evaluationCohortFileName)) {
    ParallelLogger::logInfo("Skipping creation of ", evaluationCohortFileName, " because it already exists")
    return()
  }

  # determine the model file to use to apply to the evaluation cohort
  modelFileName <- file.path(outFolder, sprintf("model_%s.rds", modelId))
  ParallelLogger::logInfo("Reading model from ", modelFileName)
  if (!file.exists(modelFileName))
    stop("Model output file (", modelFileName, ") does not exist")
  lrResults <- readRDS(modelFileName)

  #test that viable model was created
  if (!is.null(lrResults$errorMessage)) {
    ParallelLogger::logInfo(lrResults$errorMessage, " - Evaluation cohort not created.")
    saveRDS(lrResults, evaluationCohortFileName)
  } else {
    # get subjects used in model file to exclude from evaluation cohort
    exclSubjectList <- c(lrResults$prediction$subjectId)

    testCohort <- gsub(".",
                       "",
                       (paste0("test_eval_",xSpecCohortId, "_", paste(sample(c(letters, 0:9), 8), collapse = ""))),
                       fixed = TRUE)  #unique new cohort name to use

    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))

    if(file.exists(file.path(outFolder, "evaluationCohortSubjects.rds"))) {
      #use existing subjects from local file to create a cohort table on server
      cohort <- readRDS(file.path(outFolder, "evaluationCohortSubjects.rds"))
      ParallelLogger::logInfo("Creating evaluation cohort on server from cohort file")

      tryCatch({
        insertTable(
          connection = connection,
          databaseSchema = workDatabaseSchema,
          tableName = testCohort,
          data = cohort,
          dropTableIfExists = TRUE,
          createTable = TRUE,
          tempTable = FALSE,
          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
          bulkLoad = TRUE,
          progressBar = TRUE,
          camelCaseToSnakeCase = TRUE)
      },
      error=function(cond) {
        if(grepl("Bulk load credentials", cond, fixed = TRUE)) {
          message(paste0("...bulk load failed...trying without bulk load...this may be slow"))
          insertTable(
            connection = connection,
            databaseSchema = workDatabaseSchema,
            tableName = testCohort,
            data = cohort,
            dropTableIfExists = TRUE,
            createTable = TRUE,
            tempTable = FALSE,
            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
            bulkLoad = FALSE,
            progressBar = TRUE,
            camelCaseToSnakeCase = TRUE)
        } else {
          stop(cond)
        }
      })

    } else { #otherwise create the evaluation cohort from an sql query
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
                                               race = race,
                                               ethnicity = ethnicity,
                                               startDate = startDate,
                                               endDate = endDate,
                                               visitType = visitType,
                                               visitLength = visitLength,
                                               exclCohort = xSensCohortId)
      cntVisits <- DatabaseConnector::querySql(connection = connection, sql)

      #if number of visits is over 100M reduce down by factor of 12 to increase processing speed
      if (cntVisits > 100000000) {firstCut <- TRUE} else {firstCut <- FALSE}

      sqlFilename <- "CreateCohortsAcuteEvaluation.sql"

      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = sqlFilename,
                                               packageName = "PheValuator",
                                               dbms = connection@dbms,
                                               cdm_database_schema = cdmDatabaseSchema,
                                               cohort_database_schema = cohortDatabaseSchema,
                                               cohort_database_table = cohortTable,
                                               x_spec_cohort = xSpecCohortId,
                                               tempDB = workDatabaseSchema,
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
                                               mainPopnCohort = mainPopulationCohortId,
                                               mainPopnCohortStartDay = mainPopulationCohortIdStartDay,
                                               mainPopnCohortEndDay = mainPopulationCohortIdEndDay,
                                               visitLength = visitLength,
                                               visitType = c(visitType),
                                               firstCut = firstCut)
      ParallelLogger::logInfo("Creating evaluation cohort on server from sql")
      DatabaseConnector::executeSql(connection = connection, sql)
    }

    # will only use the covariates with non-zero betas
    lrNonZeroCovs <- c(lrResults$model$covariateImportance$covariateId[lrResults$model$covariateImportance$covariateValue != 0])

    if (is(covariateSettings, "covariateSettings"))
      covariateSettings <- list(covariateSettings)
    for (listUp in 1:length(covariateSettings)) {
      covariateSettings[[listUp]]$includedCovariateIds <- c(lrNonZeroCovs)
    }

    evaluationCohortPlpDataFileName <- file.path(outFolder, sprintf("evaluationCohortPlpData_%s", evaluationCohortId))
    if (file.exists(evaluationCohortPlpDataFileName)) {
      ParallelLogger::logInfo("Getting evaluation cohort data from existing folder")
      plpData <- PatientLevelPrediction::loadPlpData(evaluationCohortPlpDataFileName)
    } else {
      ParallelLogger::logInfo("Getting evaluation cohort data from server")

      databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                                                       cdmDatabaseName = "CDM",
                                                                       tempEmulationSchema = workDatabaseSchema,
                                                                       cohortDatabaseSchema = workDatabaseSchema,
                                                                       cohortTable = testCohort,
                                                                       outcomeDatabaseSchema = workDatabaseSchema,
                                                                       outcomeTable = testCohort,
                                                                       targetId = 0,
                                                                       outcomeIds = xSpecCohortId,
                                                                       cdmVersion = 5)

      restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(studyStartDate = startDate,
                                                                                       studyEndDate = endDate,
                                                                                       firstExposureOnly = F,
                                                                                       washoutPeriod = 0,
                                                                                       sampleSize = NULL)

      plpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
                                                    covariateSettings = covariateSettings,
                                                    restrictPlpDataSettings = restrictPlpDataSettings)

    }

    if (excludeModelFromEvaluation == TRUE) {
      # remove subjects in evaluation cohort that were in model cohort
      excl <- data.frame(plpData$cohorts$rowId[plpData$cohorts$subjectId %in% c(exclSubjectList)])
      xSpec <- c(plpData$outcomes$rowId)  #those with outcome need to be left in
      excl <- subset(excl, !(excl[, 1] %in% c(xSpec)))
      plpData$cohorts <- plpData$cohorts[!(plpData$cohorts$rowId %in% c(excl[, 1])), ]
    }

    if (savePlpData == TRUE) {
      ParallelLogger::logInfo("Saving evaluation cohort PLP data to: ", evaluationCohortPlpDataFileName)
      PatientLevelPrediction::savePlpData(plpData, evaluationCohortPlpDataFileName)
    }

    populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(binary = T,
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
                                                                                restrictTarToCohortEnd = F)

    population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                                outcomeId = xSpecCohortId,
                                                                populationSettings = populationSettings,
                                                                population = NULL)

    ParallelLogger::logInfo("Applying predictive model to evaluation cohort")

    # apply the model to the evaluation cohort
    appResults <- NULL
    appResults$prediction <- PatientLevelPrediction::predictPlp(plpModel = lrResults$model, plpData = plpData,
                                                                population = population)

    appResults$prediction$value <- round(appResults$prediction$value, digits = 3)

    pred <- appResults$prediction

    # pull in the xSens cohort
    sql <- SqlRender::loadRenderTranslateSql("GetXsensCohort.sql",
                                             packageName = "PheValuator",
                                             dbms = connection@dbms,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_id = xSensCohortId)
    sql <- SqlRender::translate(sql, connection@dbms)
    xSensPopn <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
    # add the start dates from the xSens cohort to the evaluation cohort to be able to apply washout
    # criteria during evaluation
    finalPopn <- merge(pred, xSensPopn, all.x = TRUE)
    finalPopn$daysToXSens <- as.integer(finalPopn$daysToXsens)

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
    appResults$PheValuator$inputSetting$mainPopulationCohortId <- mainPopulationCohortId
    appResults$PheValuator$inputSetting$modelType <- modelType
    appResults$PheValuator$inputSetting$excludeModelFromEvaluation <- excludeModelFromEvaluation

    appResults$PheValuator$modelperformanceEvaluation <- lrResults$performanceEvaluation

    # save the full data set to the model
    appResults$prediction <- finalPopn

    ParallelLogger::logInfo("Saving evaluation cohort to: ", evaluationCohortFileName)
    saveRDS(appResults, evaluationCohortFileName)

    # remove temp cohort table
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "DropTempTable.sql",
                                             packageName = "PheValuator",
                                             dbms = connection@dbms,
                                             tempDB = workDatabaseSchema,
                                             test_cohort = testCohort)
    DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = FALSE, reportOverallTime = FALSE)
  }
}
