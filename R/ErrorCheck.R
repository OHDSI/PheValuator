errorCheck <- function(callingProgram,
                       connectionDetails,
                       cdmDatabaseSchema,
                       cohortDatabaseSchema,
                       cohortTable,
                       workDatabaseSchema,
                       modelOutputFileName,
                       evaluationOutputFileName,
                       xSpecCohortId,
                       xSensCohortId,
                       prevalenceCohortId,
                       excludedCovariateConceptIds,
                       mainPopulationCohortId,
                       outFolder,
                       cohortDefinitionsToTest) {

  options(error = NULL)
  options(scipen = 999)

  if (callingProgram == "createPhenotypeModel" | callingProgram == "createEvaluationCohort" | callingProgram ==
    "testPhenosFromFile") {

    if (!file.exists(file.path(outFolder))) {
      stop(paste0("Error: ",
                  file.path(outFolder, " does not exist. \n Select an OutFolder that exists")))
    }

    if (length(connectionDetails) == 0)
      stop("...must supply a connection string")

    if (evaluationOutputFileName == modelOutputFileName)
      stop("...evaluationOutputFileName must be different than modelOutputFileName")

    # test if connection details are valid
    tryCatch(capture.output(conn <- DatabaseConnector::connect(connectionDetails), file = NULL),
             error = function(e) {
        writeLines(paste0("...error from your connection details."))
        message(e)
        stop()
      })

    # get the count of subjects in the phenotype
    sql <- paste("select count(*) from ",
                 paste(cohortDatabaseSchema, ".", cohortTable, sep = ""),
                 " where cohort_definition_id = ",
                 as.character(xSpecCohortId),
                 sep = "")
    cohortCount <- checkCohortCounts(connectionDetails = connectionDetails, sql = sql, action = "q")

    if (cohortCount == 0) {
      stop(paste0("Error: there are no subjects in your xSpec cohort (", xSpecCohortId, ")"))
    }

    # get the count of subjects in the phenotype
    sql <- paste("select count(*) from ",
                 paste(cohortDatabaseSchema, ".", cohortTable, sep = ""),
                 " where cohort_definition_id = ",
                 as.character(xSensCohortId),
                 sep = "")

    cohortCount <- checkCohortCounts(connectionDetails = connectionDetails, sql = sql, action = "q")

    if (cohortCount == 0) {
      stop(paste0("Error: there are no subjects in your xSens cohort (", xSensCohortId, ")"))
    }



    # check cdm schema
    sql <- paste("select count(*) from ",
                 paste(cdmDatabaseSchema, ".cdm_source", sep = ""),
                 sep = "")

    cohortCount <- checkCohortCounts(connectionDetails = connectionDetails, sql = sql, action = "q")


    # check write capability of workDatabaseSchema sql <- paste('select top 10 * ', 'into ',
    # workDatabaseSchema, '.jns_test12345 ', 'from ', cdmDatabaseSchema, '.cdm_source', sep = '')
    # cohortCount <- checkCohortCounts(connectionDetails = connectionDetails, sql = sql, action = 'e')
    # sql <- paste('drop table ', workDatabaseSchema, '.jns_test12345', sep = '') cohortCount <-
    # checkCohortCounts(connectionDetails = connectionDetails, sql = sql, action = 'e')
  }


  if (callingProgram == "createPhenotypeModel" | callingProgram == "createEvaluationCohort") {

    if (mainPopulationCohortId != 0) {
      # get the count of subjects in the phenotype
      sql <- paste("select count(*) from ",
                   paste(cohortDatabaseSchema, ".", cohortTable, sep = ""),
                   " where cohort_definition_id = ",
                   as.character(mainPopulationCohortId),
                   sep = "")
      cohortCount <- checkCohortCounts(connectionDetails = connectionDetails,
                                       sql = sql,
                                       action = "q")
      if (cohortCount == 0) {
        stop(paste0("Error: there are no subjects in your mainPopulationCohortId cohort (",
                    mainPopulationCohortId,
                    ")"))
      }
    }
  }


  if (callingProgram == "createPhenotypeModel") {
    # get the count of subjects in the phenotype
    sql <- paste("select count(*) from ",
                 paste(cohortDatabaseSchema, ".", cohortTable, sep = ""),
                 " where cohort_definition_id = ",
                 as.character(prevalenceCohortId),
                 sep = "")
    cohortCount <- checkCohortCounts(connectionDetails = connectionDetails, sql = sql, action = "q")
    if (cohortCount == 0) {
      stop(paste0("Error: there are no subjects in your prevalence cohort (",
                  prevalenceCohortId,
                  ")"))
    }

    if (length(excludedCovariateConceptIds) == 0) {
      stop(paste0("Error: there are no concepts to exclude from your model."))
    }

    if (file.exists(file.path(outFolder, paste0(modelOutputFileName, ".rds")))) {
      stop(paste0("Error: ",
                  file.path(outFolder, modelOutputFileName),
                  " exists. \n Select a new name for modelOutputFileName or set createModel = FALSE"))
    }

  }

  if (callingProgram == "createEvaluationCohort") {
    if (file.exists(file.path(outFolder, paste0(evaluationOutputFileName)))) {
      stop(paste0("Error: ",
                  file.path(outFolder, evaluationOutputFileName),
                  " exists. \n Select a new name for evaluationOutputFileName or set createEvaluationCohort = FALSE"))
    }
  }


  if (callingProgram == "testPhenosFromFile") {
    if (!file.exists(file.path(outFolder, paste0(evaluationOutputFileName, ".rds")))) {
      stop(paste0("Error: ",
                  file.path(outFolder, evaluationOutputFileName),
                  " does not exist. \n Select a evaluationOutputFileName that exists or set createEvaluationCohort = TRUE"))
    }

    # test if cohortDefinitionsToTest is valid
    for (i in 1:nrow(cohortDefinitionsToTest)) {
      if (!(is.numeric(cohortDefinitionsToTest$atlasId[[i]])) | is.na(cohortDefinitionsToTest$atlasId[[i]]) |
        !(is.numeric(cohortDefinitionsToTest$washoutPeriod[[i]])) | is.na(cohortDefinitionsToTest$washoutPeriod[[i]])) {
        write.csv(cohortDefinitionsToTest)
        stop(paste0("...error in cohortDefinitionsToTest...please check format"))
      }
    }
  }
}

checkCohortCounts <- function(connectionDetails, sql, action) {

  sql <- SqlRender::render(sql = sql)

  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)

  capture.output(conn <- DatabaseConnector::connect(connectionDetails), file = NULL)

  if (action == "q") {
    cohortCounts <- DatabaseConnector::querySql(conn = conn, sql)
  } else if (action == "e") {
    capture.output(cohortCounts <- DatabaseConnector::executeSql(conn = conn, sql), file = NULL)
  }

  capture.output(conn <- DatabaseConnector::connect(connectionDetails), file = NULL)
  DatabaseConnector::disconnect(conn)

  return(cohortCounts)
}
