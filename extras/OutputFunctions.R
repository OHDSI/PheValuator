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

#' Table ready summary of  results from PheValuator analyses
#'
#' @param outputFolder  Name of the folder where all the outputs was written to..
#' @param label         Optional string for the the Data Set column value.
#'
#' @return
#' A data frame of results.
#'
#' @export
prettyPhenotypeOutput <- function(outputFolder, label = NULL) {

  outputTable <- data.frame()
  analysisFile <- PheValuator::loadPheValuatorAnalysisList(file.path(outputFolder, "pheValuatorAnalysisSettings.json"))
  if(dir.exists(outputFolder)) {
    refFile <- readRDS(file.path(outputFolder,"reference.rds"))
    summary <- PheValuator::summarizePheValuatorAnalyses(readRDS(file.path(outputFolder, "reference.rds")), outputFolder)
    if(summary$cutPoint[[1]] == "Error: Too few outcomes to produce model") {
      writeLines(paste0("Skipping ", outputFolder,
                        ": ", summary$cutPoint[[1]]))
    } else {
      for (analysisUp in 1:length(analysisFile)) {
        analysisId <- analysisFile[[analysisUp]]$analysisId
        modelFile <- readRDS(file.path(outputFolder, refFile$evaluationCohortFolder[refFile$analysisId  == analysisId], "model_main.rds"))
        results <- summary[summary$pheValAnalysisID == analysisFile[[analysisUp]]$analysisId,]
        cutPoints <- c(analysisFile[[analysisUp]]$testPhenotypeAlgorithmArgs$cutPoints)
        for (cp in 1:length(cutPoints)) {
          sumUp <- NULL
          sumUp <- results[cp, c(1,2,31,3:16,29,30)]
          if(!(is.null(label))) {sumUp$cdm <- label}
          sumUp$'xSpec Cohort Id' <- analysisFile[[analysisUp]]$createEvaluationCohortArgs$xSpecCohortId
          sumUp$'xSpec Count In Model' <- max(modelFile$covariateSummary$CovariateCountWithOutcome)

          valueCol <- which(colnames(modelFile$performanceEvaluation$evaluationStatistics) == "Value")
          evalCol <- which(colnames(modelFile$performanceEvaluation$evaluationStatistics) == "Eval")

          auc <- which(rownames(modelFile$performanceEvaluation$evaluationStatistics) == "AUC.auc" &
                         modelFile$performanceEvaluation$evaluationStatistics[,evalCol] == "test")
          sumUp$'Test AUC (95% CI)' <- paste0(sprintf("%.3f", as.numeric(modelFile$performanceEvaluation$evaluationStatistics[auc,valueCol])),
                                              " (", sprintf("%.3f", as.numeric(modelFile$performanceEvaluation$evaluationStatistics[auc+1,valueCol])),
                                              ", ", sprintf("%.3f", as.numeric(modelFile$performanceEvaluation$evaluationStatistics[auc+2,valueCol])), ")")
          sumUp$'Output Folder' <- outputFolder
          if(!is.na(sumUp$truePositives)) {
            if (nrow(outputTable) == 0) {
              outputTable <- data.frame(sumUp)
            } else {
              outputTable <- rbind(outputTable, data.frame(sumUp))
            }
          }
        }
      }
    }
  } else {
    writeLines(paste0("Skipping ", outputFolder, "...does not exist"))
  }

  if(nrow(outputTable) > 0) {
    outputTable <- outputTable[with(outputTable, order(xtfrm(pheValAnalysisID), xtfrm(cdm))), ]
    outputTable$cdm <- as.character(outputTable$cdm)
    names(outputTable)[names(outputTable)=="cdm"] <- "Data Set"
    names(outputTable)[names(outputTable)=="cohortId"] <- "Cohort Id"
    names(outputTable)[names(outputTable)=="cohortName"] <- "Cohort Name"
    names(outputTable)[names(outputTable)=="sensitivity95Ci"] <- "Sensitivity (95% CI)"
    names(outputTable)[names(outputTable)=="ppv95Ci"] <- "PPV (95% CI)"
    names(outputTable)[names(outputTable)=="specificity95Ci"] <- "Specificity (95% CI)"
    names(outputTable)[names(outputTable)=="npv95Ci"] <- "NPV (95% CI)"
    names(outputTable)[names(outputTable)=="estimatedPrevalence"] <- "Estimated Prevalence"
    names(outputTable)[names(outputTable)=="f1Score"] <- "F1 Score"
    names(outputTable)[names(outputTable)=="truePositives"] <- "True Positives"
    names(outputTable)[names(outputTable)=="trueNegatives"] <- "True Negatives"
    names(outputTable)[names(outputTable)=="falsePositives"] <- "False Positives"
    names(outputTable)[names(outputTable)=="falseNegatives"] <- "False Negatives"
    names(outputTable)[names(outputTable)=="washoutPeriod"] <- "Washout Period"
    names(outputTable)[names(outputTable)=="splayPrior"] <- "Splay Prior"
    names(outputTable)[names(outputTable)=="splayPost"] <- "Splay Post"
    names(outputTable)[names(outputTable)=="cutPoint"] <- "Cut Point"
    names(outputTable)[names(outputTable)=="runDateTimeGMT"] <- "Run Date/Time (GMT)"
    names(outputTable)[names(outputTable)=="pheValAnalysisID"] <- "Analysis ID"
    names(outputTable)[names(outputTable)=="xSpec.Cohort.Id"] <- "xSpec Cohort Id"
    names(outputTable)[names(outputTable)=="xSpec.Count.In.Model"] <- "xSpec Count In Model"
    names(outputTable)[names(outputTable)=="Test.AUC..95..CI."] <- "Test AUC (95% CI)"
    names(outputTable)[names(outputTable)=="Output.Folder"] <- "Output Folder"
  }
  return(outputTable)
}


