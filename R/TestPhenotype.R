# @file TestPhenotypeAlgorithm.R
#
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
#' @description
#' Test phenotype algorithms
#'
#' @details
#' This function will perform the phenotype algorithm evaluation using the evaluation cohort returned
#' from createEvalCohort and the phenotype algorithm cohort specified
#'
#' @param connectionDetails      ConnectionDetails created using the function createConnectionDetails
#'                               in the DatabaseConnector package.
#' @param cutPoints              A list of threshold predictions for the evaluations.  Include "EV" for
#'                               the expected value
#' @param evaluationOutputFileName        The full file name with path for the evaluation file
#' @param phenotypeCohortId      The number of the cohort of the phenotype algorithm to test
#' @param phenotypeText          A string to identify the phenotype algorithm in the output file
#' @param databaseId             A string to identify the CDM tested (Default = NULL)
#' @param order                  The order of this algorithm for sorting in the output file (used when
#'                               there are multiple phenotypes to test)  (Default = 1)
#' @param modelText              Descriptive name for the model (Default = NULL)
#' @param xSpecCohortId            The number of the "extremely specific (xSpec)" cohort definition id in
#'                               the cohort table (for noisy positives) (Default = NULL)
#' @param xSensCohortId            The number of the "extremely sensitive (xSens)" cohort definition id
#'                               in the cohort table (used to exclude subjects from the base population) (Default = NULL)
#' @param prevalenceCohortId       The number of the cohort definition id to determine the disease prevalence,
#'                               (default=xSensCohortId)
#' @param cohortDatabaseSchema   The name of the database schema that is the location where the cohort
#'                               data used to define the at risk cohort is available. Requires read
#'                               permissions to this database.
#' @param cohortTable            The tablename that contains the at risk cohort. The expectation is
#'                               cohortTable has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                               COHORT_START_DATE, COHORT_END_DATE.
#' @param washoutPeriod          The mininum required continuous observation time prior to index date
#'                               for subjects within the cohort to test (Default = 0).
#' @param modelType              The type of health outcome in the model either "acute" or "chronic" (Default = "chronic")
#'
#' @return
#' A list containing 2 dataframes: 1) results - a dataframe with the results from the phenotype
#' algorithm evaluation 2) misses - a dataframe with a sample of subject ids for TPs, FPs, TNs, and FNs
#' for the 50 percent and over prediction threshold (must include 0.5 in cutPoints list)
#'
#' @importFrom data.table :=
#' @export
testPhenotypeAlgorithm <- function(connectionDetails,
                                   cutPoints = c(0.1, 0.2, 0.3, 0.4, 0.5, "EV", 0.6, 0.7, 0.8, 0.9),
                                   evaluationOutputFileName,
                                   phenotypeCohortId,
                                   databaseId = "",
                                   phenotypeText = "",
                                   order = 1,
                                   modelText = "",
                                   xSpecCohortId = "",
                                   xSensCohortId = "",
                                   prevalenceCohortId = "",
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   washoutPeriod = 0,
                                   modelType = "chronic") {

  options(error = NULL)

  getStandardError <- function(probValue, popnSize) {
    return(sqrt(((probValue*(1-probValue))/(popnSize + 0.01)))) #ensure no division by 0
  }
  createCI <- function(probValue, stdErr) {
    return(paste0("(", sprintf("%.3f", round(probValue-(1.96*stdErr),3)),", ",sprintf("%.3f", round(probValue+(1.96*stdErr),3)),")"))
  }

  # error checking for input
  if (modelType != "chronic" & modelType != "acute")
    stop("...modelType must be acute or chronic")
  if (length(connectionDetails) == 0)
    stop("...must supply a connection string")
  if (evaluationOutputFileName == "")
    stop("....must have a defined Evaluation Output File Name (e.g., \"c:/phenotypes/lr_results_10XDiabetes_dod_ePPV1_1.rds\")")
  if (cohortDatabaseSchema == "")
    stop("...must have a defined Cohort schema (e.g., \"YourCDM.YourSchema\")")
  if (cohortTable == "")
    stop("....must have a defined Cohort table (e.g., \"cohort\")")
  if (phenotypeCohortId == "")
    stop(".....must have a defined Phenotype Cohort ID to test (e.g., 1234)")

  results <- data.frame()
  misses <- data.frame()
  xSpecP <- 0.5; xSpecP2 <- -1; xSpecP3 <- -1
  writeLines(paste("\t", Sys.time(), "pheno: ", phenotypeCohortId))

  modelFileName <- "" #set this for now to be null - allows to add it back in later if needed
  if(modelFileName != "") { #if a model was supplied, add the value for the 0.005% xSpec cohort probability to the cut-points
    if(!file.exists(modelFileName)){
      writeLines(paste("...", modelFileName,"does not exist"))
    } else {
      model <- readRDS(modelFileName)
      xSpecP <- as.numeric(quantile(model$prediction$value[model$prediction$outcomeCount == 1], 0.005))
      xSpecP2 <- as.numeric(quantile(model$prediction$value[model$prediction$outcomeCount == 1], 0.01))
      xSpecP3 <- as.numeric(quantile(model$prediction$value[model$prediction$outcomeCount == 1], 0.015))
      #cutPoints <- append(cutPoints, c(xSpecP, xSpecP2, xSpecP3))
    }
  }

  # pull the subjects in the phentype
  if(modelType == "acute") {
    sql <- paste("select distinct subject_id,  cohort_start_date, subject_id  subject_id2 from ",
               paste(cohortDatabaseSchema, ".", cohortTable, sep = ""),
               " where cohort_definition_id = ",
               as.character(phenotypeCohortId),
               sep = "")
  } else {
    sql <- paste("select distinct subject_id, subject_id  subject_id2 from ",
                 paste(cohortDatabaseSchema, ".", cohortTable, sep = ""),
                 " where cohort_definition_id = ",
                 as.character(phenotypeCohortId),
                 sep = "")
  }

  sql <- SqlRender::render(sql = sql)

  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)

  #conn <- DatabaseConnector::connect(connectionDetails)
  capture.output(conn <- DatabaseConnector::connect(connectionDetails), file=NULL)

  PhenoPop <- data.table::data.table(DatabaseConnector::querySql(conn = conn, sql))

  #if (nrow(PhenoPop) == 0)
  # stop(".....phenotype cohort did not return any rows")

  if (!file.exists(evaluationOutputFileName))
    stop(paste(".....Evaluation Output file (", evaluationOutputFileName, ") does not exist"))

  resultsFile <- readRDS(evaluationOutputFileName)
  modelAll <- data.table::data.table(resultsFile$prediction[resultsFile$prediction$outcomeCount == 0,])

  #enforce a washout period if specified
  if(washoutPeriod > 0) {
    modelAll <- modelAll[(modelAll$daysToXSens > washoutPeriod | is.na(modelAll$daysToXSens)),]
  }

  modelAll <- modelAll[order(modelAll$value), ]
  modelAll$rownum <- 1:nrow(modelAll)

  for (cpUp in 1:length(cutPoints)) {
    # join the phenotype table with the prediction table
    if(modelType == "acute") {
      fullTable <- data.table::data.table(merge(modelAll,
                                                PhenoPop,
                                                by.x = c("subjectId", "cohortStartDate"),
                                                by.y = c("SUBJECT_ID2", "COHORT_START_DATE"),
                                                sort = FALSE,
                                                all.x = T))
    } else {
      fullTable <- data.table::data.table(merge(modelAll,
                                                PhenoPop,
                                                by.x = "subjectId",
                                                by.y = "SUBJECT_ID2",
                                                sort = FALSE,
                                                all.x = T))
    }

    fullTable <- fullTable[, `:=`(PN, ifelse(!is.na(SUBJECT_ID),
                                             1,
                                             0))]  #set the flag to 1 for a subject in both tables

    # a cut point = 'EV' indicates to calculate the expected values - using the probability to proportion
    # trues and falses
    fullTable$valueOrig <- fullTable$value
    if (cutPoints[[cpUp]] != "EV") {
      # for numeric cutpoints determine the cut point to use
      cutPt <- as.numeric(cutPoints[[cpUp]])
      fullTable <- fullTable[, `:=`(value, ifelse(value >= cutPt, 1, 0))]
    }

    # set the intial values
    fullTable <- fullTable[, `:=`(c("TP", "TN", "FP", "FN"),
                                  list(value, 1 - value, 1 - value, value))][]

    # change the ones that don't apply to 0 if not found in the phenotype, set true and false positves to
    # 0 (won't add to summation)
    fullTable <- fullTable[PN == 0, `:=`(c("TP", "FP"), list(0, 0))]

    # if found in the phenotype, set true and false negatves to 0 (won't add to summation)
    fullTable <- fullTable[PN == 1, `:=`(c("TN", "FN"), list(0, 0))]

    newRow <- NULL
    newRow$CDM <- as.character(databaseId)
    newRow$`Phenotype Algorithm` <- as.character(phenotypeText)

    # the values ('counts') for true and false positives and negatives will be the sum of the respective
    # columns
    truePos <- sum(fullTable$TP)
    trueNeg <- sum(fullTable$TN)
    falsePos <- sum(fullTable$FP)
    falseNeg <- sum(fullTable$FN)

    # capture subject id's of mistakes if requested - only for the 0.5 or xSpecP cutpoint
    if(!is.null(xSpecP)) {missesCP <- xSpecP} else {missesCP <- 0.5}
    if (cutPoints[[cpUp]] == missesCP) {
      subjectDF <- fullTable[fullTable$TP == 1]
      subjectDF <- subjectDF[order(-subjectDF$valueOrig), ]
      subjectList <- c(subjectDF$subjectId)

      if (length(subjectList) > 0) {
        for (subj in 1:min(5, length(subjectList))) {
          tempMisses <- NULL
          tempMisses$CDM <- as.character(cohortDatabaseSchema)
          tempMisses$cohort <- as.character(phenotypeCohortId)
          tempMisses$miss <- "TP"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$cohortStartDate <- subjectDF$cohortStartDate[[subj]]
          tempMisses$daysFromObsStart <- subjectDF$daysFromObsStart[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          tempMisses$Test_File <- evaluationOutputFileName
          if (nrow(misses) == 0) {
            misses <- as.data.frame(tempMisses)
          } else {
            misses <- rbind(misses, as.data.frame(tempMisses))
          }
        }
      }

      subjectDF <- fullTable[fullTable$FP == 1]
      subjectDF <- subjectDF[order(subjectDF$valueOrig), ]
      subjectList <- c(subjectDF$subjectId)
      if (length(subjectList) > 0) {
        for (subj in 1:min(50, length(subjectList))) {
          tempMisses <- NULL
          tempMisses$CDM <- as.character(cohortDatabaseSchema)
          tempMisses$cohort <- as.character(phenotypeCohortId)
          tempMisses$miss <- "FP"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$cohortStartDate <- subjectDF$cohortStartDate[[subj]]
          tempMisses$daysFromObsStart <- subjectDF$daysFromObsStart[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          tempMisses$Test_File <- evaluationOutputFileName
          if (nrow(misses) == 0) {
            misses <- as.data.frame(tempMisses)
          } else {
            misses <- rbind(misses, as.data.frame(tempMisses))
          }
        }
      }

      subjectDF <- fullTable[fullTable$FN == 1]
      subjectDF <- subjectDF[order(-subjectDF$valueOrig), ]
      subjectList <- c(subjectDF$subjectId)
      if (length(subjectList) > 0) {
        for (subj in 1:min(50, length(subjectList))) {
          tempMisses <- NULL
          tempMisses$CDM <- as.character(cohortDatabaseSchema)
          tempMisses$cohort <- as.character(phenotypeCohortId)
          tempMisses$miss <- "FN"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$cohortStartDate <- subjectDF$cohortStartDate[[subj]]
          tempMisses$daysFromObsStart <- subjectDF$daysFromObsStart[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          tempMisses$Test_File <- evaluationOutputFileName
          if (nrow(misses) == 0) {
            misses <- as.data.frame(tempMisses)
          } else {
            misses <- rbind(misses, as.data.frame(tempMisses))
          }
        }
      }
    }

    if(cutPoints[[cpUp]] == xSpecP) {
      newRow$`Cut Point` <- paste("EmpirCP0.5 (", round(xSpecP,2), ")", sep="")
    } else if(cutPoints[[cpUp]] == xSpecP2) {
      newRow$`Cut Point` <- paste("EmpirCP1.0 (", round(xSpecP2,2), ")", sep="")
    } else if(cutPoints[[cpUp]] == xSpecP3) {
      newRow$`Cut Point` <- paste("EmpirCP1.5 (", round(xSpecP3,2), ")", sep="")
    } else if(cutPoints[[cpUp]] == "EV") {
      newRow$`Cut Point` <- paste("Expected Value", sep="")
    } else {
      newRow$`Cut Point` <- cutPoints[[cpUp]]
    }

    if (truePos + falsePos == 0)
    {
      falsePos <- 1
    }  #for cohorts lacking any members - eliminates division by 0 for PPV
    newRow$Sensitivity <- sprintf("%.3f", round(truePos/(truePos + falseNeg + 0.5),
                                                3))  #add 0.5 to all denominators to avoid division by 0
    if (newRow$Sensitivity > 0.999) {
      newRow$Sensitivity <- as.character(0.999)
    } else {
      newRow$Sensitivity <- as.character(newRow$Sensitivity)
    }

    newRow$`Sensitivity (95% CI)` <- paste0(newRow$Sensitivity, " ",
                                            createCI(as.numeric(newRow$Sensitivity),
                                                     getStandardError(as.numeric(newRow$Sensitivity), truePos+falseNeg)))

    newRow$PPV <- sprintf("%.3f", round((truePos/(truePos + falsePos + 0.5)), 3))
    if (newRow$PPV > 0.999) {
      newRow$PPV <- as.character(0.999)
    } else {
      newRow$PPV <- as.character(newRow$PPV)
    }

    newRow$`PPV (95% CI)` <- paste0(newRow$PPV, " ", createCI(as.numeric(newRow$PPV), getStandardError(as.numeric(newRow$PPV), truePos+falsePos)))

    newRow$Specificity <- sprintf("%.3f", round((trueNeg/(trueNeg + falsePos + 0.5)), 3))
    if (newRow$Specificity > 0.999) {
      newRow$Specificity <- as.character(0.999)
    } else {
      newRow$Specificity <- as.character(newRow$Spec)
    }

    newRow$`Specificity (95% CI)` <- paste0(newRow$Specificity, " ", createCI(as.numeric(newRow$Specificity),
                                                                              getStandardError(as.numeric(newRow$Specificity), trueNeg+falsePos)))

    newRow$NPV <- sprintf("%.3f", round((trueNeg/(trueNeg + falseNeg + 0.5)), 3))
    if (newRow$NPV > 0.999) {
      newRow$NPV <- as.character(0.999)
    } else {
      newRow$NPV <- as.character(newRow$NPV)
    }

    newRow$`NPV (95% CI)` <- paste0(newRow$NPV, " ", createCI(as.numeric(newRow$NPV), getStandardError(as.numeric(newRow$NPV), trueNeg+falseNeg)))

    newRow$`True Pos.` <- round(truePos, 0)
    newRow$`False Pos.` <- round(falsePos, 0)
    newRow$`True Neg.` <- round(trueNeg, 0)
    newRow$`False Neg.` <- round(falseNeg, 0)

    newRow$`Estimated Prevalence` <- round(((newRow$`True Pos.` + newRow$`False Neg.`)/(newRow$`True Pos.` +
                                                                                          newRow$`False Neg.` + newRow$`False Pos.` + newRow$`True Neg.`)) * 100, 2)

    #newRow$LR_Pos <- round(((as.numeric(newRow$Sens))/(1 - as.numeric(newRow$Spec))), 1)
    newRow$`F1 Score` <- round(1/((1/as.numeric(newRow$Sensitivity) + 1/as.numeric(newRow$PPV))/2), 3)

    newRow$`Washout Period` <- as.character(washoutPeriod)

    newRow$`Start Days` <- paste0(resultsFile$PheValuator$inputSetting$startDays, " ")
    newRow$`End Days` <- paste0(resultsFile$PheValuator$inputSetting$endDays, " ")

    newRow$`Phenotype Cohort Id` <- as.character(phenotypeCohortId)

    newRow$`Phenotype Order` <- as.numeric(order)
    newRow$`Model Name` <- as.character(modelText)

    newRow$`xSpec Cohort` <- as.character(xSpecCohortId)
    newRow$`xSens Cohort` <- as.character(xSensCohortId)
    newRow$`Prevalence Cohort` <- as.character(prevalenceCohortId)

    newRow$`Evaluation Output File Name` <- evaluationOutputFileName

    if (nrow(results) == 0) {
      results <- as.data.frame(newRow, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      results <- rbind(results, as.data.frame(newRow, stringsAsFactors = FALSE, check.names = FALSE))
    }
  }
  capture.output(conn <- DatabaseConnector::connect(connectionDetails), file=NULL)
  DatabaseConnector::disconnect(conn)
  return(list(results, misses))
}
