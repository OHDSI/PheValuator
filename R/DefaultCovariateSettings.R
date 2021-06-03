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

#' Create the default covariate settings
#'
#' @description
#' Create the default covariate settings for acute diseases
#'
#' @details
#' Function to create the default covariate settings for acute diseases
#'
#' @param excludedCovariateConceptIds   A list of conceptIds to exclude from featureExtraction.  These
#'                                      should include all concept_ids that were used to define the
#'                                      xSpec model (default=NULL)
#' @param includedCovariateIds          A list of covariate IDs that should be restricted to.
#' @param includedCovariateConceptIds   A list of covariate concept IDs that should be restricted to.
#' @param addDescendantsToExclude       Should descendants of excluded concepts also be excluded?
#'                                      (default=FALSE)
#' @param startDayWindow1              The day to start time window 1 for feature extraction
#' @param endDayWindow1                The day to end time window 1 for feature extraction
#' @param startDayWindow2              The day to start time window 2 for feature extraction
#' @param endDayWindow2                The day to end time window 2 for feature extraction
#' @param startDayWindow3              The day to start time window 3 for feature extraction
#' @param endDayWindow3                The day to end time window 3 for feature extraction #'
#'
#' @export
createDefaultAcuteCovariateSettings <- function(excludedCovariateConceptIds = c(),
                                                includedCovariateIds = c(),
                                                includedCovariateConceptIds = c(),
                                                addDescendantsToExclude = FALSE,
                                                startDayWindow1 = 0,
                                                endDayWindow1 = 9999,
                                                startDayWindow2 = NULL,
                                                endDayWindow2 = NULL,
                                                startDayWindow3 = NULL,
                                                endDayWindow3 = NULL) {

  covariateSettings1 <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                   useDemographicsAgeGroup = TRUE,
                                                                   useDemographicsRace = TRUE,
                                                                   useDemographicsEthnicity = TRUE,
                                                                   useConditionGroupEraLongTerm = TRUE,
                                                                   useDrugGroupEraLongTerm = TRUE,
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
                                                                   useVisitConceptCountLongTerm = TRUE,
                                                                   longTermStartDays = startDayWindow1,
                                                                   endDays = endDayWindow1,
                                                                   includedCovariateConceptIds = c(includedCovariateConceptIds),
                                                                   addDescendantsToInclude = addDescendantsToExclude,
                                                                   excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                   addDescendantsToExclude = addDescendantsToExclude,
                                                                   includedCovariateIds = c(includedCovariateIds))

  if(!(is.null(startDayWindow2))) {
    covariateSettings2 <- FeatureExtraction::createCovariateSettings(useConditionGroupEraShortTerm = TRUE,
                                                                     useDrugGroupEraShortTerm = TRUE,
                                                                     useProcedureOccurrenceShortTerm = TRUE,
                                                                     useDeviceExposureShortTerm = TRUE,
                                                                     useMeasurementShortTerm = TRUE,
                                                                     useMeasurementValueShortTerm = TRUE,
                                                                     useMeasurementRangeGroupShortTerm = TRUE,
                                                                     useObservationShortTerm = TRUE,
                                                                     useDistinctConditionCountShortTerm = TRUE,
                                                                     useDistinctIngredientCountShortTerm = TRUE,
                                                                     useDistinctProcedureCountShortTerm = TRUE,
                                                                     useDistinctMeasurementCountShortTerm = TRUE,
                                                                     useVisitCountShortTerm = TRUE,
                                                                     useVisitConceptCountShortTerm = TRUE,
                                                                     shortTermStartDays = startDayWindow2,
                                                                     endDays = endDayWindow2,
                                                                     includedCovariateConceptIds = c(includedCovariateConceptIds),
                                                                     addDescendantsToInclude = addDescendantsToExclude,
                                                                     excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                     addDescendantsToExclude = addDescendantsToExclude,
                                                                     includedCovariateIds = c(includedCovariateIds))
  }

  if(!(is.null(startDayWindow3))) {
    covariateSettings3 <- FeatureExtraction::createCovariateSettings(useConditionGroupEraMediumTerm = TRUE,
                                                                     useDrugGroupEraMediumTerm = TRUE,
                                                                     useProcedureOccurrenceMediumTerm = TRUE,
                                                                     useDeviceExposureMediumTerm = TRUE,
                                                                     useMeasurementMediumTerm = TRUE,
                                                                     useMeasurementValueMediumTerm = TRUE,
                                                                     useMeasurementRangeGroupMediumTerm = TRUE,
                                                                     useObservationMediumTerm = TRUE,
                                                                     useDistinctConditionCountMediumTerm = TRUE,
                                                                     useDistinctIngredientCountMediumTerm = TRUE,
                                                                     useDistinctProcedureCountMediumTerm = TRUE,
                                                                     useDistinctMeasurementCountMediumTerm = TRUE,
                                                                     useVisitCountMediumTerm = TRUE,
                                                                     useVisitConceptCountMediumTerm = TRUE,
                                                                     mediumTermStartDays = startDayWindow3,
                                                                     endDays = endDayWindow3,
                                                                     includedCovariateConceptIds = c(includedCovariateConceptIds),
                                                                     addDescendantsToInclude = addDescendantsToExclude,
                                                                     excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                     addDescendantsToExclude = addDescendantsToExclude,
                                                                     includedCovariateIds = c(includedCovariateIds))
  }

  if (is.null(startDayWindow1)) {
    stop("The first time window must not be null")
  } else if(is.null(startDayWindow2) & is.null(startDayWindow3)) {
    covariateSettings <- list(covariateSettings1)
  } else if(!(is.null(startDayWindow2)) & is.null(startDayWindow3)) {
    covariateSettings <- list(covariateSettings1, covariateSettings2)
  } else if(!(is.null(startDayWindow3)) & is.null(startDayWindow2)) {
    covariateSettings <- list(covariateSettings1, covariateSettings3)
  } else {
    covariateSettings <- list(covariateSettings1, covariateSettings2, covariateSettings3)
  }

  return(covariateSettings)
}

#' Create the default covariate settings
#'
#' @description
#' Create the default covariate settings for chronic diseases
#'
#' @details
#' Function to create the default covariate settings for chronic diseases
#'
#' @param excludedCovariateConceptIds   A list of conceptIds to exclude from featureExtraction.  These
#'                                      should include all concept_ids that were used to define the
#'                                      xSpec model.
#' @param includedCovariateIds          A list of covariate IDs that should be restricted to.
#' @param addDescendantsToExclude       Should descendants of excluded concepts also be excluded?
#' @param startDays                     The days to include prior to the cohort start date.
#' @param endDays                       The days to include after the cohort start date.
#'
#' @export
createDefaultChronicCovariateSettings <- function(excludedCovariateConceptIds = c(),
                                                  includedCovariateIds = c(),
                                                  addDescendantsToExclude = FALSE,
                                                  startDays = 0,
                                                  endDays = 9999) {

  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                  useDemographicsAgeGroup = TRUE,
                                                                  useDemographicsRace = TRUE,
                                                                  useDemographicsEthnicity = TRUE,
                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                  useDrugGroupEraLongTerm = TRUE,
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
                                                                  useVisitConceptCountLongTerm = TRUE,
                                                                  longTermStartDays = startDays,
                                                                  endDays = endDays,
                                                                  includedCovariateConceptIds = includedCovariateIds,
                                                                  addDescendantsToInclude = addDescendantsToExclude,
                                                                  excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                  addDescendantsToExclude = addDescendantsToExclude,
                                                                  includedCovariateIds = c(includedCovariateIds))

  return(covariateSettings)
}
