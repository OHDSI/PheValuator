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
#' @param addDescendantsToExclude       Should descendants of excluded concepts also be excluded?
#'                                      (default=FALSE)
#' @param startDays                     The days to include prior to the cohort start date (default=0)
#' @param endDays                       The days to include after the cohort start date (default=7)
#'
#' @importFrom stats runif
#'
#' @export
createDefaultAcuteCovariateSettings <- function(excludedCovariateConceptIds = c(),
                                                includedCovariateIds = c(),
                                                addDescendantsToExclude = FALSE,
                                                startDays = 0,
                                                endDays = 7) {

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
                                                                   longTermStartDays = -180,
                                                                   endDays = startDays - 1,
                                                                   includedCovariateConceptIds = includedCovariateIds,
                                                                   addDescendantsToInclude = addDescendantsToExclude,
                                                                   excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                   addDescendantsToExclude = addDescendantsToExclude,
                                                                   includedCovariateIds = c(includedCovariateIds))

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
                                                                   mediumTermStartDays = endDays + 1,
                                                                   endDays = 180,
                                                                   includedCovariateConceptIds = includedCovariateIds,
                                                                   addDescendantsToInclude = addDescendantsToExclude,
                                                                   excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                   addDescendantsToExclude = addDescendantsToExclude,
                                                                   includedCovariateIds = c(includedCovariateIds))

  covariateSettings2 <- FeatureExtraction::createCovariateSettings(useConditionGroupEraShortTerm = TRUE,
                                                                   useDrugGroupEraShortTerm = TRUE,
                                                                   useProcedureOccurrenceShortTerm = TRUE,
                                                                   useDeviceExposureShortTerm = TRUE,
                                                                   useMeasurementShortTerm = TRUE,
                                                                   useMeasurementValueShortTerm = TRUE,
                                                                   useMeasurementRangeGroupShortTerm = TRUE,
                                                                   useObservationShortTerm = TRUE,
                                                                   shortTermStartDays = startDays,
                                                                   endDays = endDays,
                                                                   includedCovariateConceptIds = includedCovariateIds,
                                                                   addDescendantsToInclude = addDescendantsToExclude,
                                                                   excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                                   addDescendantsToExclude = addDescendantsToExclude,
                                                                   includedCovariateIds = c(includedCovariateIds))

  covariateSettings <- list(covariateSettings1, covariateSettings2, covariateSettings3)

  return(covariateSettings)
}

createDefaultChronicCovariateSettings <- function(excludedCovariateConceptIds = c(),
                                                  includedCovariateIds = c(),
                                                  addDescendantsToExclude = FALSE,
                                                  startDays = 0,
                                                  endDays = 7) {

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

