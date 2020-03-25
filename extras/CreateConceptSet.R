function() {
  ccaeCDM <- "CDM_IBM_CCAE_V813"
  dodCDM <- "CDM_OPTUM_EXTENDED_DOD_V837"
  panthCDM <- "CDM_OPTUM_PANTHER_V811"
  mdcdCDM <- "CDM_TRUVEN_MDCD_V780"
  mdcrCDM <- "CDM_IBM_MDCR_V814"

  cdm_database_schema <- "CDM_IBM_CCAE_V813.dbo"
  cohort_database_schema <- "CDM_IBM_CCAE_V813.ohdsi_results"
  x_spec_cohort <- 8618
  x_sens_cohort <- 8626
  ageLimit <- 2
  upperAgeLimit <- 17
  estPPV <- 0.75

  trainFile <- "10XJIDNar"
  cdmShortName <- "ccae217"
  analysisId <- "20190207v1"
  newAnalysisId <- "20190207v5"
  gender <- c(8507, 8532)

  workFolder <- getwd()

  connectionDetails <- createConnectionDetails(dbms = "pdw",
                                               server = "JRDUSAPSCTL01",
                                               port = "17001")
  # conn <- DatabaseConnector::connect(connectionDetails)


  # create the evaluation cohort
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "CreateCohortsV6.sql",
                                              package = "PheValuator"))

  test_cohort <- gsub(".",
                      "",
                      (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                      fixed = TRUE)  #unique new cohort name to use

  sql <- SqlRender::renderSql(sqlScript,
                              cdm_database_schema = cdm_database_schema,
                              cohort_database_schema = cohort_database_schema,
                              cohort_database_table = "cohort",
                              x_spec_cohort = x_sens_cohort,
                              tempDB = "scratch.dbo",
                              test_cohort = test_cohort,
                              exclCohort = 0,
                              ageLimit = ageLimit,
                              upperAgeLimit = upperAgeLimit,
                              gender = gender,
                              startDate = "20100101",
                              endDate = "21000101",
                              baseSampleSize = 1500000,
                              xSpecSampleSize = 5e+05,
                              noise = 0,
                              mainPopnCohort = 0,
                              lookback = 0)  #when applying the model start from the first visit for all subjects

  sql <- SqlRender::translateSql(sql$sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(conn = conn, sql$sql)

  # determine the model file to use to apply to the evaluation cohort
  resultsFileName <- file.path(workFolder, paste("lr_results_train_",
                                                 trainFile,
                                                 "_",
                                                 cdmShortName,
                                                 "_ePPV",
                                                 estPPV,
                                                 "_",
                                                 analysisId,
                                                 ".rds",
                                                 sep = ""))

  plpFileName <- file.path(workFolder, paste("plpData_train_",
                                             trainFile,
                                             "_",
                                             cdmShortName,
                                             "_ePPV",
                                             estPPV,
                                             "_",
                                             analysisId,
                                             sep = ""))

  writeLines(paste("\n...reading ", resultsFileName, sep = ""))

  if (!file.exists(resultsFileName))
    stop(paste(".....Results file (", resultsFileName, ") does not exist"))

  lr_results <- readRDS(resultsFileName)

  # will only use the covariates with non-zero betas
  lrNonZeroCovs <- c(lr_results$model$varImp$covariateId[lr_results$model$varImp$covariateValue !=
    0])
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                  useDemographicsAgeGroup = TRUE,
                                                                  useDemographicsRace = TRUE,
                                                                  useDemographicsEthnicity = TRUE,
                                                                  useDemographicsPostObservationTime = TRUE,
                                                                  useConditionOccurrenceLongTerm = TRUE,
                                                                  useConditionOccurrencePrimaryInpatientLongTerm = TRUE,
                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                  useDrugExposureLongTerm = TRUE,
                                                                  useDrugEraLongTerm = TRUE,
                                                                  useDrugEraStartLongTerm = TRUE,
                                                                  useDrugGroupEraLongTerm = TRUE,
                                                                  useDrugGroupEraStartLongTerm = TRUE,
                                                                  useProcedureOccurrenceLongTerm = TRUE,
                                                                  useDeviceExposureLongTerm = TRUE,
                                                                  useMeasurementLongTerm = TRUE,
                                                                  useMeasurementValueLongTerm = TRUE,
                                                                  useMeasurementRangeGroupLongTerm = TRUE,
                                                                  useObservationLongTerm = TRUE,
                                                                  useDistinctConditionCountLongTerm = TRUE,
                                                                  useDistinctIngredientCountLongTerm = TRUE,
                                                                  useDistinctProcedureCountLongTerm = TRUE,
                                                                  useDistinctMeasurementCountLongTerm = TRUE,
                                                                  useVisitCountLongTerm = TRUE,
                                                                  longTermStartDays = -10000,
                                                                  endDays = 10000,
                                                                  includedCovariateConceptIds = c(),
                                                                  addDescendantsToInclude = TRUE,
                                                                  excludedCovariateConceptIds = c(),
                                                                  addDescendantsToExclude = TRUE,
                                                                  includedCovariateIds = c())

  plpData <- loadPlpData(plpFileName)
  conditionList <- data.frame(plpData$covariateRef[plpData$covariateRef$analysisId == 102,
                              ])$covariateId

  covariateSettings$includedCovariateIds <- c(lrNonZeroCovs, conditionList)

  plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                cdmDatabaseSchema = paste(cdm_database_schema,
                                                                          sep = ""),
                                                cohortId = 0,
                                                outcomeIds = x_sens_cohort,
                                                outcomeDatabaseSchema = "scratch.dbo",
                                                outcomeTable = test_cohort,
                                                cohortDatabaseSchema = "scratch.dbo",
                                                cohortTable = test_cohort,
                                                cdmVersion = 5,
                                                washoutPeriod = 0,
                                                covariateSettings = covariateSettings)

  summary(plpData)

  population <- PatientLevelPrediction::createStudyPopulation(plpData,
                                                              population = NULL,
                                                              outcomeId = x_sens_cohort,
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

  # apply the model to the evaluation cohort
  appResults <- PatientLevelPrediction::applyModel(population, plpData, lr_results$model)
  plpFileName <- file.path(workFolder, paste("plpData_TESTeval_",
                                             trainFile,
                                             "_",
                                             cdmShortName,
                                             "_ePPV",
                                             estPPV,
                                             "_",
                                             newAnalysisId,
                                             sep = ""))
  print(plpFileName)

  resultsFileName <- file.path(workFolder, paste("lr_results_TESTeval_",
                                                 trainFile,
                                                 "_",
                                                 cdmShortName,
                                                 "_ePPV",
                                                 estPPV,
                                                 "_",
                                                 newAnalysisId,
                                                 ".rds",
                                                 sep = ""))

  print(resultsFileName)
  # save the plp data - to be used for phenotype evaluation
  savePlpData(plpData, plpFileName)
  saveRDS(appResults, resultsFileName)
  # plpData <- loadPlpData(file.path(workFolder,
  # 'plpData_TESTeval_10XJIDNar_ccae217_ePPV0.75_20190201v1')) appResults <-
  # readRDS(file.path(workFolder, 'lr_results_TESTeval_10XJIDNar_ccae217_ePPV0.75_20190201v1.rds'))

  # remove temp cohort table
  sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                              "DropTempTable.sql",
                                              package = "PheValuator"))
  sql <- SqlRender::renderSql(sqlScript, tempDB = "scratch.dbo", test_cohort = test_cohort)
  sql <- SqlRender::translateSql(sql$sql, targetDialect = connectionDetails$dbms)


  DatabaseConnector::executeSql(conn = conn, sql$sql)

  DatabaseConnector::disconnect(conn)

  covDataRef <- data.table::data.table(data.frame(plpData$covariateRef))
  covData <- data.table::data.table(data.frame(plpData$covariates))
  covDataAll <- merge(covData, covDataRef, by = "covariateId")

  cohortProb <- data.frame(appResults$prediction)
  cohortProb <- data.table::data.table(cohortProb[, (names(cohortProb) %in% c("rowId", "value"))])
  cohortProb$invValue <- 1 - cohortProb$value

  covDataAll <- merge(covDataAll, cohortProb, by = "rowId")
  covDataAll <- data.table::setkey(covDataAll, covariateId)
  covDataAll2 <- data.table::data.table(covDataAll)
  covDataAll2 <- data.table::setkey(covDataAll2, rowId)

  # covSummary <- dplyr::summarize(group_by(covDataAll, as.character(covariateId)), n(), sum(value),
  # mean(value))
  covSummary <- as.data.frame(covDataAll[, j = list(mean(value, na.rm = TRUE),
                                                    sum(value, na.rm = TRUE)), by = covariateId])
  covSummary$n <- round(covSummary$V2/covSummary$V1, 0)

  names(covSummary)[1] <- "covariateId"
  covSummary <- merge(covSummary, covDataRef, by = "covariateId")
  # covSummary$mean <- covSummary$`sum(value)`/covSummary$`n()`
  names(covSummary)[2] <- "mean"
  covSummary102 <- covSummary[covSummary$analysisId == 102 & covSummary$n >= 10 & covSummary$mean >=
    0.2, ]
  covSummary102 <- covSummary102[with(covSummary102, order(-mean)), ]

  {
    tp <- 0
    fp <- 0
    fn <- 0
    prevTp <- 0
    prevFp <- 0
    prevFn <- 0
    removeCnt <- 0
    prevListUp <- c()
    listUp <- c()
    covListUp <- c()
    prevCovListUp <- c()
    for (covUp in 1:nrow(covSummary102)) {
      listUp <- c(listUp, as.numeric(covSummary102$covariateId[covUp]))
      covListUp <- c(covListUp, as.numeric(covSummary102$conceptId[covUp]))

      covName <- as.character(covSummary102$covariateName[covUp])
      covName <- substr(covName, stringr::str_locate(covName, ":")[1] + 2, nchar(covName))

      positives <- unique(covDataAll[.(listUp)]$rowId)
      negatives <- unique(covDataAll2[!(.(positives))]$rowId)

      # negatives <- unique(covDataAll[!(covDataAll$rowId %in% positives)]$rowId)

      tp <- colSums(data.frame(cohortProb$value[cohortProb$rowId %in% positives]))
      fp <- colSums(data.frame(cohortProb$invValue[cohortProb$rowId %in% positives]))

      fn <- colSums(data.frame(cohortProb$value[cohortProb$rowId %in% negatives]))
      tn <- colSums(data.frame(cohortProb$invValue[cohortProb$rowId %in% negatives]))

      sens <- tp/(tp + fn)
      ppv <- tp/(tp + fp)
      spec <- tn/(tn + fp)
      npv <- tn/(tn + fn)

      writeLines(paste(covUp,
                       "\ttp: ",
                       round(tp, 0),
                       "\tfn:",
                       round(fn, 0),
                       "\tsens: ",
                       round(sens, 3),
                       "\tfp: ",
                       round(fp, 0),
                       "\t(",
                       round(covSummary102$mean[covUp], 2),
                       ")",
                       covName,
                       sep = ""))
      priorList <- listUp

      if (((tp - prevTp) < 1 | ((tp - prevTp) <= (fp - prevFp)/100))) {
        listUp <- prevListUp
        covListUp <- prevCovListUp
        tp <- prevTp
        fp <- prevFp
        fn <- prevFn
        sens <- tp/(tp + fn)
        writeLines(paste("Removing:", covName))
        writeLines(paste(covUp - 1,
                         "\ttp: ",
                         round(tp, 0),
                         "\tfn:",
                         round(fn, 0),
                         "\tsens: ",
                         round(sens, 3),
                         "\tfp: ",
                         round(fp, 0),
                         "\n"))
        removeCnt <- removeCnt + 1
      } else {
        prevListUp <- listUp
        prevCovListUp <- covListUp
        prevTp <- tp
        prevFp <- fp
        prevFn <- fn
        removeCnt <- 0
      }

      if (removeCnt == 10) {
        writeLines(paste("Removal limit reached"))
        break
      }
    }
  }

  preFinalList <- data.frame(listUp)
  names(preFinalList)[1] <- "covariateId"
  covSummary102 <- merge(preFinalList, covSummary102, by = "covariateId")
  covSummary102 <- covSummary102[with(covSummary102, order(-n)), ]
  {
    tp <- 0
    fp <- 0
    fn <- 0
    prevTp <- 0
    prevFp <- 0
    prevFn <- 0
    removeCnt <- 0
    prevListUp <- c()
    listUp <- c()
    covListUp <- c()
    prevCovListUp <- c()
    for (covUp in 1:nrow(covSummary102)) {
      listUp <- c(listUp, as.numeric(covSummary102$covariateId[covUp]))
      covListUp <- c(covListUp, as.numeric(covSummary102$conceptId[covUp]))

      covName <- as.character(covSummary102$covariateName[covUp])
      covName <- substr(covName, stringr::str_locate(covName, ":")[1] + 2, nchar(covName))

      positives <- unique(covDataAll[.(listUp)]$rowId)
      negatives <- unique(covDataAll2[!(.(positives))]$rowId)

      tp <- colSums(data.frame(cohortProb$value[cohortProb$rowId %in% positives]))
      fp <- colSums(data.frame(cohortProb$invValue[cohortProb$rowId %in% positives]))

      fn <- colSums(data.frame(cohortProb$value[cohortProb$rowId %in% negatives]))
      tn <- colSums(data.frame(cohortProb$invValue[cohortProb$rowId %in% negatives]))

      sens <- tp/(tp + fn)
      ppv <- tp/(tp + fp)
      spec <- tn/(tn + fp)
      npv <- tn/(tn + fn)

      writeLines(paste(covUp,
                       "\ttp: ",
                       round(tp, 0),
                       "\tfn:",
                       round(fn, 0),
                       "\tsens: ",
                       round(sens, 3),
                       "\tfp: ",
                       round(fp, 0),
                       "\t(",
                       round(covSummary102$mean[covUp], 2),
                       ")",
                       covName,
                       sep = ""))
      priorList <- listUp

      if (((tp - prevTp) < 1 | ((tp - prevTp) <= (fp - prevFp)/100))) {
        listUp <- prevListUp
        covListUp <- prevCovListUp
        tp <- prevTp
        fp <- prevFp
        fn <- prevFn
        sens <- tp/(tp + fn)
        writeLines(paste("Removing:", covName))
        writeLines(paste(covUp - 1,
                         "\ttp: ",
                         round(tp, 0),
                         "\tfn:",
                         round(fn, 0),
                         "\tsens: ",
                         round(sens, 3),
                         "\tfp: ",
                         round(fp, 0),
                         "\n"))
        removeCnt <- removeCnt + 1
      } else {
        prevListUp <- listUp
        prevCovListUp <- covListUp
        prevTp <- tp
        prevFp <- fp
        prevFn <- fn
        removeCnt <- 0
      }

      if (removeCnt == 10) {
        writeLines(paste("Removal limit reached"))
        break
      }
    }
  }

  paste(covListUp, collapse = " ")
  rm(covDataAll, covDataAll2, covData, covDataRef, cohortProb, covSummary)
}
