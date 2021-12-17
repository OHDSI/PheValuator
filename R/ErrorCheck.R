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
  if (callingProgram == "createPhenotypeModel" | callingProgram == "createEvaluationCohort" | callingProgram ==
      "testPhenosFromFile") {

    if (!file.exists(file.path(outFolder))) {
      stop("Error: ", file.path(outFolder, " does not exist. \n Select an OutFolder that exists"))
    }

    if (length(connectionDetails) == 0)
      stop("Must supply a connection string")

    if (evaluationOutputFileName == modelOutputFileName)
      stop("EvaluationOutputFileName must be different than modelOutputFileName")

    # test if connection details are valid
    tryCatch(conn <- DatabaseConnector::connect(connectionDetails),
             error = function(e) {
               writeLines(paste0("Unable to connect using provided connection details"))
               message(e)
               stop()
             })
    on.exit(DatabaseConnector::disconnect(conn))

    # get the count of subjects in the phenotype
    cohortCount <- getCohortCount(connection = conn,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  cohortId = xSpecCohortId)
    if (cohortCount == 0) {
      stop("Error: there are no subjects in your xSpec cohort (", xSpecCohortId, ")")
    }

    # get the count of subjects in the phenotype
    cohortCount <- getCohortCount(connection = conn,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  cohortId = xSensCohortId)
    if (cohortCount == 0) {
      stop("Error: there are no subjects in your xSens cohort (", xSpecCohortId, ")")
    }
  }


  if (callingProgram == "createPhenotypeModel" | callingProgram == "createEvaluationCohort") {
    if (mainPopulationCohortId != 0) {
      # get the count of subjects in the phenotype
      cohortCount <- getCohortCount(connection = conn,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTable = cohortTable,
                                    cohortId = mainPopulationCohortId)
      if (cohortCount == 0) {
        stop("Error: there are no subjects in your mainPopulationCohortId cohort (", mainPopulationCohortId, ")")
      }
    }
  }

  if (callingProgram == "createPhenotypeModel") {
    # get the count of subjects in the phenotype
    cohortCount <- getCohortCount(connection = conn,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  cohortId = prevalenceCohortId)
    if (cohortCount == 0) {
      stop("Error: there are no subjects in your prevalence cohort (", prevalenceCohortId, ")")
    }

    if (length(excludedCovariateConceptIds) == 0) {
      stop("Error: there are no concepts to exclude from your model.")
    }

    if (file.exists(file.path(outFolder, paste0(modelOutputFileName, ".rds")))) {
      stop("Error: ",
           file.path(outFolder, modelOutputFileName),
           " exists. \n Select a new name for modelOutputFileName or set createModel = FALSE")
    }
  }

  if (callingProgram == "createEvaluationCohort") {
    if (file.exists(file.path(outFolder, paste0(evaluationOutputFileName)))) {
      stop("Error: ",
           file.path(outFolder, evaluationOutputFileName),
           " exists. \n Select a new name for evaluationOutputFileName or set createEvaluationCohort = FALSE")
    }
  }

  if (callingProgram == "testPhenosFromFile") {
    if (!file.exists(file.path(outFolder, paste0(evaluationOutputFileName, ".rds")))) {
      stop("Error: ",
           file.path(outFolder, evaluationOutputFileName),
           " does not exist. \n Select a evaluationOutputFileName that exists or set createEvaluationCohort = TRUE")
    }

    # test if cohortDefinitionsToTest is valid
    for (i in 1:nrow(cohortDefinitionsToTest)) {
      if (!(is.numeric(cohortDefinitionsToTest$atlasId[[i]])) | is.na(cohortDefinitionsToTest$atlasId[[i]]) |
          !(is.numeric(cohortDefinitionsToTest$washoutPeriod[[i]])) | is.na(cohortDefinitionsToTest$washoutPeriod[[i]])) {
        write.csv(cohortDefinitionsToTest)
        stop("...error in cohortDefinitionsToTest...please check format")
      }
    }
  }
}

getCohortCount <- function(connection, cohortDatabaseSchema, cohortTable, cohortId) {
  sql <- "SELECT COUNT(*) AS cohort_count
  FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id = @cohort_id;"
  sql <- SqlRender::render(sql = sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable)
  sql <- SqlRender::translate(sql = sql, targetDialect = connection@dbms)
  count <- DatabaseConnector::querySql(connection = connection, sql, snakeCaseToCamelCase = TRUE)
  return(count$cohortCount)
}
