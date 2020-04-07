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

#' Test phenotype algorithms
#'
#' @description
#' Test phenotype algorithms
#'
#' @details
#' This function will perform the phenotype algorithm evaluation using the evaluation cohort returned
#' from createEvalCohort and the phenotype algorithm cohort specified
#'
#' @param connectionDetails          ConnectionDetails created using the function
#'                                   createConnectionDetails in the DatabaseConnector package.
#' @param cutPoints                  A list of threshold predictions for the evaluations.  Include "EV"
#'                                   for the expected value
#' @param outFolder                  The folder where the cohort evaluation output files are written
#' @param evaluationCohortId         A string used to generate the file names for the evaluation cohort.
#' @param cdmDatabaseSchema          The name of the database schema that contains the OMOP CDM
#'                                   instance. Requires read permissions to this database. On SQL
#'                                   Server, this should specifiy both the database and the
#'                                   schema, so for example 'cdm_instance.dbo'.
#' @param cohortDatabaseSchema       The name of the database schema that is the location where the
#'                                   cohort data used to define the at risk cohort is available.
#'                                   Requires read permissions to this database.
#' @param cohortTable                The tablename that contains the at risk cohort. The expectation is
#'                                   cohortTable has format of COHORT table: cohort_concept_id,
#'                                   SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param phenotypeCohortId          The ID of the cohort to evaluate in the specified cohort table.
#' @param washoutPeriod              The mininum required continuous observation time prior to index
#'                                   date for subjects within the cohort to test (Default = 0).
#'
#' @return
#' A dataframe with the results from the phenotype
#' algorithm evaluation.
#'
#' If 0.5 is included as a cutpoint, the data frame will have an attribute called 'misses', a dataframe
#' with a sample of subject ids for TPs, FPs, TNs, and FNs for the 50 percent and over prediction threshold.
#'
#' @export
testPhenotypeAlgorithm <- function(connectionDetails,
                                   cutPoints = c(0.1, 0.2, 0.3, 0.4, 0.5, "EV", 0.6, 0.7, 0.8, 0.9),
                                   outFolder,
                                   evaluationCohortId = "main",
                                   phenotypeCohortId,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   washoutPeriod = 0) {
  if (length(connectionDetails) == 0)
    stop("Must supply a connection string")
  if (cohortDatabaseSchema == "")
    stop("Must have a defined Cohort schema (e.g., \"YourCDM.YourSchema\")")
  if (cohortTable == "")
    stop("Must have a defined Cohort table (e.g., \"cohort\")")
  if (phenotypeCohortId == "")
    stop("Must have a defined Phenotype Cohort ID to test (e.g., 1234)")

  getStandardError <- function(probValue, popnSize) {
    return(sqrt(((probValue * (1 - probValue))/(popnSize + 0.01))))  #ensure no division by 0
  }
  createCI <- function(probValue, stdErr) {
    return(paste0("(",
                  sprintf("%.3f", round(probValue - (1.96 * stdErr), 3)),
                  ", ",
                  sprintf("%.3f", round(probValue + (1.96 * stdErr), 3)),
                  ")"))
  }

  start <-  Sys.time()

  evaluationCohortFileName <- file.path(outFolder, sprintf("evaluationCohort_%s.rds", evaluationCohortId))
  if (!file.exists(evaluationCohortFileName))
    stop(paste("Evaluation cohort file (", evaluationCohortFileName, ") does not exist"))
  ParallelLogger::logInfo("Loading evaluation cohort from ", evaluationCohortFileName)
  evaluationCohort <- readRDS(evaluationCohortFileName)

  results <- data.frame()
  misses <- data.frame()
  xSpecP <- 0.5
  xSpecP2 <- -1
  xSpecP3 <- -1

  modelType <- evaluationCohort$PheValuator$inputSetting$modelType
  if (modelType == "acute") {
    sql <- "SELECT DISTINCT subject_id,
      visit_start_date AS cohort_start_date
    FROM @cohort_database_schema.@cohort_table
    JOIN @cdm_database_schema.visit_occurrence
      ON subject_id = person_id
        and cohort_start_date >= visit_start_date
        and cohort_start_date <= visit_end_date
    WHERE cohort_definition_id = @cohort_id;"
  } else {
    sql <- "SELECT DISTINCT subject_id,
      observation_period_start_date AS cohort_start_date
    FROM @cohort_database_schema.@cohort_table
    JOIN @cdm_database_schema.observation_period
      ON subject_id = person_id
        and cohort_start_date >= observation_period_start_date
        and cohort_start_date <= observation_period_end_date
    WHERE cohort_definition_id = @cohort_id;"
  }

  sql <- SqlRender::render(sql = sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable,
                           cohort_id = phenotypeCohortId)

  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)
  connection <- DatabaseConnector::connect(connectionDetails)
  ParallelLogger::logInfo("Downloading cohort to evaluate. Assuming type is ", modelType, ".")
  phenoPop <- DatabaseConnector::querySql(connection = connection, sql, snakeCaseToCamelCase = TRUE)
  DatabaseConnector::disconnect(connection)

  if (nrow(phenoPop) == 0) {
    warning('Phenotype cohort is empty')
    cutPoints[cutPoints == "EV"] <- "Expected Value"
    return(data.frame('Cut Point' = cutPoints, check.names = FALSE))
  }
  modelAll <- evaluationCohort$prediction[evaluationCohort$prediction$outcomeCount == 0, ]
  if (washoutPeriod >= 0) {
    modelAll <- modelAll[(modelAll$daysToXSens > washoutPeriod | is.na(modelAll$daysToXSens)), ]
  }
  modelAll <- modelAll[order(modelAll$value), ]
  modelAll$rownum <- 1:nrow(modelAll)
  phenoPop$inPhenotype <- rep(TRUE, nrow(phenoPop))
  for (cpUp in 1:length(cutPoints)) {
    # join the phenotype table with the prediction table
    if (modelType == "acute") {
      fullTable <- dplyr::left_join(modelAll,
                                    phenoPop[, c("subjectId", "cohortStartDate", "inPhenotype"),],
                                    by = c("subjectId", "cohortStartDate"))
    } else {
      fullTable <- dplyr::left_join(modelAll,
                                    phenoPop[, c("subjectId", "inPhenotype")],
                                    by = c("subjectId"))
    }
    fullTable$inPhenotype[is.na(fullTable$inPhenotype)] <- FALSE

    # a cut point = 'EV' indicates to calculate the expected values - using the probability to proportion
    # trues and falses
    fullTable$valueOrig <- fullTable$value
    if (cutPoints[[cpUp]] != "EV") {
      # for numeric cutpoints determine the cut point to use
      cutPt <- as.numeric(cutPoints[[cpUp]])
      fullTable$value <- fullTable$value > cutPt
    }
    fullTable$tp <- 0
    fullTable$tn <- 0
    fullTable$fp <- 0
    fullTable$fn <- 0
    fullTable$tp[fullTable$inPhenotype] <- fullTable$value[fullTable$inPhenotype]
    fullTable$tn[!fullTable$inPhenotype] <- 1 - fullTable$value[!fullTable$inPhenotype]
    fullTable$fp[fullTable$inPhenotype] <- 1 - fullTable$value[fullTable$inPhenotype]
    fullTable$fn[!fullTable$inPhenotype] <- fullTable$value[!fullTable$inPhenotype]
    newRow <- NULL

    # the values ('counts') for true and false positives and negatives will be the sum of the respective
    # columns
    truePos <- sum(fullTable$tp)
    trueNeg <- sum(fullTable$tn)
    falsePos <- sum(fullTable$fp)
    falseNeg <- sum(fullTable$fn)

    # capture subject id's of mistakes if requested - only for the 0.5 or xSpecP cutpoint
    if (!is.null(xSpecP)) {
      missesCP <- xSpecP
    } else {
      missesCP <- 0.5
    }
    if (cutPoints[[cpUp]] == missesCP) {
      subjectDF <- fullTable[fullTable$tp == 1, ]
      subjectDF <- subjectDF[order(-subjectDF$valueOrig), ]
      subjectList <- c(subjectDF$subjectId)

      if (length(subjectList) > 0) {
        for (subj in 1:min(5, length(subjectList))) {
          tempMisses <- NULL
          tempMisses$miss <- "TP"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$cohortStartDate <- subjectDF$cohortStartDate[[subj]]
          tempMisses$daysFromObsStart <- subjectDF$daysFromObsStart[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          misses <- rbind(misses, as.data.frame(tempMisses))
        }
      }

      subjectDF <- fullTable[fullTable$fp == 1, ]
      subjectDF <- subjectDF[order(subjectDF$valueOrig), ]
      subjectList <- c(subjectDF$subjectId)
      if (length(subjectList) > 0) {
        for (subj in 1:min(50, length(subjectList))) {
          tempMisses <- NULL
          tempMisses$miss <- "FP"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$cohortStartDate <- subjectDF$cohortStartDate[[subj]]
          tempMisses$daysFromObsStart <- subjectDF$daysFromObsStart[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          if (nrow(misses) == 0) {
            misses <- as.data.frame(tempMisses)
          } else {
            misses <- rbind(misses, as.data.frame(tempMisses))
          }
        }
      }

      subjectDF <- fullTable[fullTable$fn == 1, ]
      subjectDF <- subjectDF[order(-subjectDF$valueOrig), ]
      subjectList <- c(subjectDF$subjectId)
      if (length(subjectList) > 0) {
        for (subj in 1:min(50, length(subjectList))) {
          tempMisses <- NULL
          tempMisses$miss <- "FN"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$cohortStartDate <- subjectDF$cohortStartDate[[subj]]
          tempMisses$daysFromObsStart <- subjectDF$daysFromObsStart[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          if (nrow(misses) == 0) {
            misses <- as.data.frame(tempMisses)
          } else {
            misses <- rbind(misses, as.data.frame(tempMisses))
          }
        }
      }
    }
    if (cutPoints[[cpUp]] == xSpecP) {
      newRow$`Cut Point` <- paste("EmpirCP0.5 (", round(xSpecP, 2), ")", sep = "")
    } else if (cutPoints[[cpUp]] == xSpecP2) {
      newRow$`Cut Point` <- paste("EmpirCP1.0 (", round(xSpecP2, 2), ")", sep = "")
    } else if (cutPoints[[cpUp]] == xSpecP3) {
      newRow$`Cut Point` <- paste("EmpirCP1.5 (", round(xSpecP3, 2), ")", sep = "")
    } else if (cutPoints[[cpUp]] == "EV") {
      newRow$`Cut Point` <- paste("Expected Value", sep = "")
    } else {
      newRow$`Cut Point` <- cutPoints[[cpUp]]
    }

    if (truePos + falsePos == 0) {
      falsePos <- 1
    }  #for cohorts lacking any members - eliminates division by 0 for PPV
    newRow$Sensitivity <- sprintf("%.3f",
                                  round(truePos/(truePos + falseNeg + 0.5),
                                        3))  #add 0.5 to all denominators to avoid division by 0
    if (newRow$Sensitivity > 0.999) {
      newRow$Sensitivity <- as.character(0.999)
    } else {
      newRow$Sensitivity <- as.character(newRow$Sensitivity)
    }

    newRow$`Sensitivity (95% CI)` <- paste0(newRow$Sensitivity,
                                            " ",
                                            createCI(as.numeric(newRow$Sensitivity),
                                                     getStandardError(as.numeric(newRow$Sensitivity), truePos + falseNeg)))

    newRow$PPV <- sprintf("%.3f", round((truePos/(truePos + falsePos + 0.5)), 3))
    if (newRow$PPV > 0.999) {
      newRow$PPV <- as.character(0.999)
    } else {
      newRow$PPV <- as.character(newRow$PPV)
    }

    newRow$`PPV (95% CI)` <- paste0(newRow$PPV,
                                    " ",
                                    createCI(as.numeric(newRow$PPV),
                                             getStandardError(as.numeric(newRow$PPV), truePos + falsePos)))

    newRow$Specificity <- sprintf("%.3f", round((trueNeg/(trueNeg + falsePos + 0.5)), 3))
    if (newRow$Specificity > 0.999) {
      newRow$Specificity <- as.character(0.999)
    } else {
      newRow$Specificity <- as.character(newRow$Spec)
    }

    newRow$`Specificity (95% CI)` <- paste0(newRow$Specificity,
                                            " ",
                                            createCI(as.numeric(newRow$Specificity),
                                                     getStandardError(as.numeric(newRow$Specificity), trueNeg + falsePos)))

    newRow$NPV <- sprintf("%.3f", round((trueNeg/(trueNeg + falseNeg + 0.5)), 3))
    if (newRow$NPV > 0.999) {
      newRow$NPV <- as.character(0.999)
    } else {
      newRow$NPV <- as.character(newRow$NPV)
    }

    newRow$`NPV (95% CI)` <- paste0(newRow$NPV,
                                    " ",
                                    createCI(as.numeric(newRow$NPV),
                                             getStandardError(as.numeric(newRow$NPV), trueNeg + falseNeg)))

    newRow$`True Pos.` <- round(truePos, 0)
    newRow$`False Pos.` <- round(falsePos, 0)
    newRow$`True Neg.` <- round(trueNeg, 0)
    newRow$`False Neg.` <- round(falseNeg, 0)

    newRow$`Estimated Prevalence` <- round(((newRow$`True Pos.` + newRow$`False Neg.`)/(newRow$`True Pos.` + newRow$`False Neg.` + newRow$`False Pos.` + newRow$`True Neg.`)) * 100,
                                           2)

    newRow$`F1 Score` <- round(1/((1/as.numeric(newRow$Sensitivity) + 1/as.numeric(newRow$PPV))/2), 3)
    results <- rbind(results,
                     as.data.frame(newRow, stringsAsFactors = FALSE, check.names = FALSE))
  }
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Testing phenotype algorithm took ", signif(delta, 3), " ", attr(delta, "units"))
  if (nrow(misses) > 0) {
    attr(results, "misses") <- misses
  }
  return(results)
}
