# @file TestPhenotype.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' testPhenotype

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
#' @param resultsFileName        The full file name with path for the evaluation file
#' @param modelFileName          The full file name with path for the model file
#' @param cohortPheno            The number of the cohort of the phenotype algorithm to test
#' @param phenText               A string to identify the phenotype algorithm in the outpuit file
#' @param order                  The order of this algorithm for sorting in the output file (used when
#'                               there are multiple phenotypes to test)
#' @param testText               Descriptive name for the model
#' @param cohortDatabaseSchema   The name of the database schema that is the location where the cohort
#'                               data used to define the at risk cohort is available. Requires read
#'                               permissions to this database.
#' @param cohortTable            The tablename that contains the at risk cohort. The expectation is
#'                               cohortTable has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                               COHORT_START_DATE, COHORT_END_DATE.
#' @param estPPV                 The positive predictive value estimate used in developing the model
#' @param cdmShortName           A short name for the current database (CDM)
#'
#' @return
#' A list containg 2 dataframes: 1) results - a dataframe with the results from the phenotype
#' algorithm evaluation 2) misses - a dataframe with a sample of subject ids for TPs, FPs, TNs, and FNs
#' for the 50 percent and over prediction threshold
#'
#' @importFrom data.table :=
#' @export
testPhenotype <- function(connectionDetails = list(),
                          cutPoints = c(0.1, 0.2, 0.3, 0.4, 0.5, "EV", 0.6, 0.7, 0.8, 0.9),
                          resultsFileName = "",
                          modelFileName = "",
                          cohortPheno = "",
                          phenText = "Test Pheno",
                          order = 1,
                          testText = "Phenotype",
                          cohortDatabaseSchema = "",
                          cohortTable = "",
                          estPPV = 1,
                          cdmShortName = "CDM") {

  options(error = NULL)

  # error checking for input
  if (length(connectionDetails) == 0)
    stop("...must supply a connection string")
  if (resultsFileName == "")
    stop("....must have a defined Result File Name (e.g., \"c:/phenotypes/lr_results_10XDiabetes_dod_ePPV1_1.rds\")")
  if (cohortDatabaseSchema == "")
    stop("...must have a defined Cohort schema (e.g., \"YourCDM.YourSchema\")")
  if (cohortTable == "")
    stop("....must have a defined Cohort table (e.g., \"cohort\")")
  if (cohortPheno == "")
    stop(".....must have a defined Phenotype Cohort ID to test (e.g., 1234)")

  results <- data.frame()
  misses <- data.frame()
  xSpecP <- -1; xSpecP2 <- -1; xSpecP3 <- -1
  writeLines(paste("\t", Sys.time(), "pheno: ", cohortPheno))

  if(modelFileName != "") { #if a model was supplied, add the value for the 0.005% xSpec cohort probability to the cut-points
    if(!file.exists(modelFileName)){
      writeLines(paste("...", modelFileName,"does not exist"))
    } else {
            model <- readRDS(modelFileName)
            xSpecP <- as.numeric(quantile(model$prediction$value[model$prediction$outcomeCount == 1], 0.005))
            xSpecP2 <- as.numeric(quantile(model$prediction$value[model$prediction$outcomeCount == 1], 0.01))
            xSpecP3 <- as.numeric(quantile(model$prediction$value[model$prediction$outcomeCount == 1], 0.015))
            cutPoints <- append(cutPoints, c(xSpecP, xSpecP2, xSpecP3))
            }
  }

  # pull the subjects in the phentype
  sql <- paste("select distinct subject_id,  subject_id  subject_id2 from ",
               paste(cohortDatabaseSchema, ".", cohortTable, sep = ""),
               " where cohort_definition_id = ",
               as.character(cohortPheno),
               sep = "")

  sql <- SqlRender::renderSql(sql = sql)$sql

  sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql

  #conn <- DatabaseConnector::connect(connectionDetails)
  capture.output(conn <- DatabaseConnector::connect(connectionDetails), file='NUL')

  PhenoPop <- data.table::data.table(DatabaseConnector::querySql(conn = conn, sql))

  #if (nrow(PhenoPop) == 0)
   # stop(".....phenotype cohort did not return any rows")

  if (!file.exists(resultsFileName))
    stop(paste(".....Results file (", resultsFileName, ") does not exist"))

  resultsFile <- readRDS(resultsFileName)
  modelAll <- data.table::data.table(resultsFile$prediction)
  modelAll <- modelAll[order(modelAll$value), ]
  modelAll$rownum <- 1:nrow(modelAll)

  for (cpUp in 1:length(cutPoints)) {
    # join the phenotype table with the prediction table
    fullTable <- data.table::data.table(merge(modelAll,
                                              PhenoPop,
                                              by.x = "subjectId",
                                              by.y = "SUBJECT_ID2",
                                              sort = FALSE,
                                              all.x = T))

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
    newRow$CDM <- as.character(cdmShortName)
    newRow$Pheno_Cohort_Name <- as.character(phenText)

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
          tempMisses$cohort <- as.character(cohortPheno)
          tempMisses$miss <- "TP"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          tempMisses$Test_File <- resultsFileName
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
          tempMisses$cohort <- as.character(cohortPheno)
          tempMisses$miss <- "FP"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          tempMisses$Test_File <- resultsFileName
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
          tempMisses$cohort <- as.character(cohortPheno)
          tempMisses$miss <- "FN"
          tempMisses$subjectId <- subjectList[[subj]]
          tempMisses$predValue <- subjectDF$valueOrig[[subj]]
          tempMisses$Test_File <- resultsFileName
          if (nrow(misses) == 0) {
          misses <- as.data.frame(tempMisses)
          } else {
          misses <- rbind(misses, as.data.frame(tempMisses))
          }
        }
      }
    }

    if(cutPoints[[cpUp]] == xSpecP) {
      newRow$Cut_Point <- paste("EmpirCP0.5 (", round(xSpecP,2), ")", sep="")
    } else if(cutPoints[[cpUp]] == xSpecP2) {
      newRow$Cut_Point <- paste("EmpirCP1.0 (", round(xSpecP2,2), ")", sep="")
    } else if(cutPoints[[cpUp]] == xSpecP3) {
      newRow$Cut_Point <- paste("EmpirCP1.5 (", round(xSpecP3,2), ")", sep="")
    } else {
        newRow$Cut_Point <- cutPoints[[cpUp]]
    }

    if (truePos + falsePos == 0)
      {
        falsePos <- 1
      }  #for cohorts lacking any members - eliminates division by 0 for PPV
    newRow$Sens <- round(truePos/(truePos + falseNeg + 0.5),
                         3)  #add 0.5 to all denominators to avoid division by 0
    if (newRow$Sens > 0.999) {
      newRow$Sens <- as.character(0.999)
    } else {
      newRow$Sens <- as.character(newRow$Sens)
    }

    newRow$PPV <- round((truePos/(truePos + falsePos + 0.5)), 3)
    if (newRow$PPV > 0.999) {
      newRow$PPV <- as.character(0.999)
    } else {
      newRow$PPV <- as.character(newRow$PPV)
    }

    newRow$Spec <- round((trueNeg/(trueNeg + falsePos + 0.5)), 3)
    if (newRow$Spec > 0.999) {
      newRow$Spec <- as.character(0.999)
    } else {
      newRow$Spec <- as.character(newRow$Spec)
    }

    newRow$NPV <- round((trueNeg/(trueNeg + falseNeg + 0.5)), 3)
    if (newRow$NPV > 0.999) {
      newRow$NPV <- as.character(0.999)
    } else {
      newRow$NPV <- as.character(newRow$NPV)
    }

    newRow$True_Pos <- round(truePos, 0)
    newRow$False_Pos <- round(falsePos, 0)
    newRow$True_Neg <- round(trueNeg, 0)
    newRow$False_Neg <- round(falseNeg, 0)

    newRow$Est_Prev <- round(((newRow$True_Pos + newRow$False_Neg)/(newRow$True_Pos + newRow$False_Neg + newRow$False_Pos + newRow$True_Neg)) * 100,
                             2)

    newRow$LR_Pos <- round(((as.numeric(newRow$Sens))/(1 - as.numeric(newRow$Spec))), 1)

    newRow$Pheno_Cohort <- as.character(cohortPheno)

    newRow$Prev_PPV <- as.character(estPPV)
    newRow$Pheno_Order <- as.numeric(order)
    newRow$Test_Cohort_Name <- as.character(testText)
    newRow$Test_File <- resultsFileName

    if (nrow(results) == 0) {
      results <- as.data.frame(newRow, stringsAsFactors = FALSE)
    } else {
      results <- rbind(results, as.data.frame(newRow))
    }
  }
  DatabaseConnector::disconnect(conn)
  return(list(results, misses))
}
