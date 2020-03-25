# @file GetPerformanceCharacteristics.R
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

#' testPhenotype

#' @description
#' Get Model Performance Characteristics
#'
#' @details
#' This function will extract the model performance characteristics (e.g., AUC, calibration data) from
#' the .rds output file from the model
#'
#' @param modelFileName          The full file name with path for the model file
#' @param cdmShortName           A short name for the current database (CDM)

#'
#' @return
#' A list containg 2 dataframes: 1) dataframe with the model performance characteristics
#'                               2) dataframe with top 10 positive predictors
#'
#' @export
getPerformanceCharacteristics <- function(modelFileName = "",
                                          cdmShortName = "") {

  options(error = NULL)

  # error checking for input
  if (length(connectionDetails) == 0)
    stop("...must supply a connection string")
  if (modelFileName == "")
    stop("....must have a defined Model File Name (e.g., \"c:/phenotypes/lr_results_10XDiabetes_dod_ePPV1_1.rds\")")

  results <- data.frame()
  preds <- data.frame()

  if (!file.exists(modelFileName))
    stop(paste(".....Model file (", modelFileName, ") does not exist"))

  newRow <- NULL
  newRow$CDM <- as.character(cdmShortName)
  modelFile <- readRDS(modelFileName)
  evalStats <- suppressWarnings(data.frame(modelFile$performanceEvaluation$evaluationStatistics))

  newRow$TrainAuc <- round(as.numeric(as.character(evalStats$Value[evalStats$Eval == "test" &
                            evalStats$Metric == "AUC.auc"])),3)
  newRow$TrainIntercept <- round(as.numeric(as.character(evalStats$Value[evalStats$Eval == "test" &
                            evalStats$Metric == "CalibrationIntercept.Intercept"])),2)
  newRow$TrainSlope <- round(as.numeric(as.character(evalStats$Value[evalStats$Eval == "test" &
                            evalStats$Metric == "CalibrationSlope.Gradient"])),2)

  predStats <- data.frame(modelFile$performanceEvaluation$predictionDistribution)
  newRow$AvgPredProbCase <- round(as.numeric(as.character(predStats$averagePredictedProbability
                                                          [predStats$Eval == "test" & predStats$class == 1])),2)
  newRow$MedianPredProbCase <- round(as.numeric(as.character(predStats$MedianPredictedProbability
                                                          [predStats$Eval == "test" & predStats$class == 1])),2)
  newRow$AvgPredProbNonCase <- round(as.numeric(as.character(predStats$averagePredictedProbability
                                                          [predStats$Eval == "test" & predStats$class == 0])),2)
  newRow$MedianPredProbNonCase <- round(as.numeric(as.character(predStats$MedianPredictedProbability
                                                          [predStats$Eval == "test" & predStats$class == 0])),2)


  newRow$ModelFile <- as.character(modelFileName)

  preds <- data.frame(modelFile$model$varImp[with(modelFile$model$varImp,
                                                  order(-modelFile$model$varImp$covariateValue)),])[1:10,]
  preds <- preds[ , (names(preds) %in% c("CDM", "covariateName", "covariateValue"))]

  preds$CDM <- as.character(cdmShortName)

  preds$ModelFile <- as.character(modelFileName)
  preds$covariateName <- as.character(preds$covariateName)
  preds <- data.frame(preds[,c(3,1,2,4)], stringsAsFactors = FALSE)

    if (nrow(results) == 0) {
      results <- as.data.frame(newRow, stringsAsFactors = FALSE)
    } else {
      results <- rbind(results, as.data.frame(newRow))
    }

  return(list(results, preds))
}
