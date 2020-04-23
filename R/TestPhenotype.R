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

  start <-  Sys.time()

  evaluationCohortFileName <- file.path(outFolder, sprintf("evaluationCohort_%s.rds", evaluationCohortId))
  if (!file.exists(evaluationCohortFileName))
    stop(paste("Evaluation cohort file (", evaluationCohortFileName, ") does not exist"))
  ParallelLogger::logInfo("Loading evaluation cohort from ", evaluationCohortFileName)
  evaluationCohort <- readRDS(evaluationCohortFileName)

  #test that viable evaluation cohort was created
  if (!is.null(evaluationCohort$errorMessage)) {
    ParallelLogger::logInfo(evaluationCohort$errorMessage, " - Evaluation cohort not created.")
    results <- tibble::tibble(cutPoint = evaluationCohort$errorMessage)
  } else {

    countsTable <- tibble::tibble()
    misses <- tibble::tibble()
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
      return(tibble::tibble('Cut Point' = cutPoints, check.names = FALSE))
    }
    ParallelLogger::logInfo("Computing evaluation statistics")
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
      if (cutPoints[cpUp] != "EV") {
        # for numeric cutpoints determine the cut point to use
        cutPt <- as.numeric(cutPoints[cpUp])
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

      # capture subject id's of mistakes if requested - only for the 0.5 or xSpecP cutpoint
      if (!is.null(xSpecP)) {
        missesCP <- xSpecP
      } else {
        missesCP <- 0.5
      }
      if (cutPoints[cpUp] == missesCP) {
        subjects <- fullTable[fullTable$tp == 1, ]
        if (nrow(subjects) > 0) {
          subjects <- subjects[order(-subjects$valueOrig), ]
          tempMisses <- subjects[min(5, nrow(subjects)), c("subjectId", "cohortStartDate", "daysFromObsStart", "valueOrig")]
          tempMisses$miss <- "TP"
          misses <- dplyr::bind_rows(misses, tempMisses)
        }
        subjects <- fullTable[fullTable$fp == 1, ]
        if (nrow(subjects) > 0) {
          subjects <- subjects[order(-subjects$valueOrig), ]
          tempMisses <- subjects[min(50, nrow(subjects)), c("subjectId", "cohortStartDate", "daysFromObsStart", "valueOrig")]
          tempMisses$miss <- "FP"
          misses <- dplyr::bind_rows(misses, tempMisses)
        }
        subjects <- fullTable[fullTable$fn == 1, ]
        if (nrow(subjects) > 0) {
          subjects <- subjects[order(-subjects$valueOrig), ]
          tempMisses <- subjects[min(50, nrow(subjects)), c("subjectId", "cohortStartDate", "daysFromObsStart", "valueOrig")]
          tempMisses$miss <- "FN"
          misses <- dplyr::bind_rows(misses, tempMisses)
        }
      }
      newRow <- tibble::tibble(truePositives = sum(fullTable$tp),
                               trueNegatives = sum(fullTable$tn),
                               falsePositives = sum(fullTable$fp),
                               falseNegatives = sum(fullTable$fn))

      if (cutPoints[cpUp] == xSpecP) {
        newRow$cutPoint <- paste("EmpirCP0.5 (", round(xSpecP, 2), ")", sep = "")
      } else if (cutPoints[cpUp] == xSpecP2) {
        newRow$cutPoint <- paste("EmpirCP1.0 (", round(xSpecP2, 2), ")", sep = "")
      } else if (cutPoints[cpUp] == xSpecP3) {
        newRow$cutPoint <- paste("EmpirCP1.5 (", round(xSpecP3, 2), ")", sep = "")
      } else if (cutPoints[cpUp] == "EV") {
        newRow$cutPoint <- paste("Expected Value", sep = "")
      } else {
        newRow$cutPoint <- cutPoints[cpUp]
      }
      countsTable <- dplyr::bind_rows(countsTable, newRow)
    }
    countsTable <- computePerformanceMetricsFromCounts(countsTable)

    # Make pretty results table
    results <- tibble::tibble(cutPoint = countsTable$cutPoint,
                              truePositives = round(countsTable$truePositives, 0),
                              trueNegatives = round(countsTable$trueNegatives, 0),
                              falsePositives = round(countsTable$falsePositives, 0),
                              falseNegatives = round(countsTable$falseNegatives, 0),
                              sensitivity = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$sens, countsTable$sensCi95Lb, countsTable$sensCi95Ub),
                              ppv = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$ppv, countsTable$ppvCi95Lb, countsTable$ppvCi95Ub),
                              specificity = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$spec, countsTable$specCi95Lb, countsTable$specCi95Ub),
                              npv = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$npv, countsTable$npvCi95Lb, countsTable$npvCi95Ub),
                              estimatedPrevalence = sprintf("%0.1f", 100*countsTable$estimatedPrevalence),
                              f1Score = sprintf("%0.3f", countsTable$f1Score))

    if (nrow(misses) > 0) {
      attr(results, "misses") <- misses
    }
    delta <- Sys.time() - start
    ParallelLogger::logInfo("Testing phenotype algorithm took ", signif(delta, 3), " ", attr(delta, "units"))
  }
  return(results)
}

#' Compute performance metrics based on a 2-by-2 counts table
#'
#' @param counts  Counts as created by the \code{\link{testPhenotypeAlgorithm}} function.
#'
#' @return
#' A tibble with the statistics metrics added to the counts table.
#'
#' @export
computePerformanceMetricsFromCounts <- function(counts) {
  # Note: negative counts indicate the cell counts was below the specified minimum for sharing.

  computeSingleProportion <- function(i,x, n) {
    exact <- binom.test(as.integer(x[i]), as.integer(n[i]), conf.level = 0.95)
    return(tibble::tibble(estimate = exact$estimate,
                          ci95Lb = exact$conf.int[1],
                          ci95Ub = exact$conf.int[2]))
  }

  computeProportions <- function(x, n, name) {
    proportions <- lapply(1:length(x), computeSingleProportion, x = abs(x), n = n)
    proportions <- dplyr::bind_rows(proportions)
    names(proportions) <- paste0(name, c("", "Ci95Lb", "Ci95Ub"))
    proportions[x < 0, ] <- -proportions[x < 0, ]
    return(proportions)
  }

  counts <- dplyr::bind_cols(counts,
                             computeProportions(counts$truePositives,
                                                abs(counts$truePositives) + abs(counts$falseNegatives) +
                                                  max(abs(counts$truePositives) + abs(counts$falseNegatives), 1), #prevent division by 0
                                                "sens"))

  counts <- dplyr::bind_cols(counts,
                             computeProportions(counts$truePositives,
                                                abs(counts$truePositives) + abs(counts$falsePositives) +
                                                  max(abs(counts$truePositives) + abs(counts$falsePositives), 1),
                                                "ppv"))

  counts <- dplyr::bind_cols(counts,
                             computeProportions(counts$trueNegatives,
                                                abs(counts$trueNegatives) + abs(counts$falsePositives) +
                                                  max(abs(counts$trueNegatives) + abs(counts$falsePositives), 1),
                                                "spec"))

  counts <- dplyr::bind_cols(counts,
                             computeProportions(counts$trueNegatives,
                                                abs(counts$trueNegatives) + abs(counts$falseNegatives) +
                                                  max(abs(counts$trueNegatives) + abs(counts$falseNegatives), 1),
                                                "npv"))

  counts$estimatedPrevalence <- (abs(counts$truePositives) + abs(counts$falseNegatives)) / (abs(counts$truePositives) + abs(counts$trueNegatives) + abs(counts$falsePositives) + abs(counts$falseNegatives))
  idx <- counts$truePositives < 0 | counts$falseNegatives < 0
  counts$estimatedPrevalence[idx] <- -counts$estimatedPrevalence[idx]

  counts$f1Score <- 1/((1/abs(counts$sens) + 1/abs(counts$ppv))/2)
  return(counts)
}
