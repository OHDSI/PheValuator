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

#' Create a PheValuator analysis specification
#'
#' @details
#' Create a set of analysis choices, to be used with the \code{\link{runPheValuatorAnalyses}} function.
#'
#' @param analysisId                      An integer that will be used later to refer to this specific
#'                                        set of analysis choices.
#' @param description                     A short description of the analysis.
#' @param createEvaluationCohortArgs      An object representing the arguments to be used when calling
#'                                        the \code{\link{createCreateEvaluationCohortArgs}} function.
#' @param testPhenotypeAlgorithmArgs      Should the \code{\link{createTestPhenotypeAlgorithmArgs}} function be used in this
#'                                        analysis?
#'
#' @export
createPheValuatorAnalysis <- function(analysisId,
                                      description,
                                      createEvaluationCohortArgs,
                                      testPhenotypeAlgorithmArgs) {
  # TODO: add input checks
  analysis <- list()
  for (name in names(formals(createPheValuatorAnalysis))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "pheValuatorAnalysis"
  return(analysis)
}


#' Save a list of phevaluatorAnalysis to file
#'
#' @description
#' Write a list of objects of type \code{pheValuatorAnalysis} to file. The file is in JSON format.
#'
#' @param pheValuatorAnalysisList   The pheValuatorAnalysis list to be written to file
#' @param file                      The name of the file where the results will be written
#'
#' @export
savePheValuatorAnalysisList <- function(pheValuatorAnalysisList, file) {
  stopifnot(is.list(pheValuatorAnalysisList))
  stopifnot(length(pheValuatorAnalysisList) > 0)
  for (i in 1:length(pheValuatorAnalysisList)) {
    stopifnot(class(pheValuatorAnalysisList[[i]]) == "pheValuatorAnalysis")
  }
  ParallelLogger::logTrace("Saving pheValuatorAnalysisList to ", file)
  ParallelLogger::saveSettingsToJson(pheValuatorAnalysisList, file)
}

#' Load a list of phevaluatorAnalysis from file
#'
#' @description
#' Load a list of objects of type \code{pheValuatorAnalysis} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{pheValuatorAnalysis}.
#'
#' @export
loadPheValuatorAnalysisList <- function(file) {
  ParallelLogger::logTrace("Loading phevaluatorAnalysisList from ", file)
  return(ParallelLogger::loadSettingsFromJson(file))
}
