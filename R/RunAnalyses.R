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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses.
#'
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the
#'                                       schema, so for example 'cdm_instance.dbo'.
#' @param cohortDatabaseSchema           The name of the database schema that is the location where
#'                                       the cohort data used to define the at risk cohort is
#'                                       available. Requires read permissions to this database.
#' @param cohortTable                    The tablename that contains the at risk cohort. The
#'                                       expectation is cohortTable has format of COHORT table:
#'                                       cohort_concept_id, SUBJECT_ID, COHORT_START_DATE,
#'                                       COHORT_END_DATE.
#' @param workDatabaseSchema             The name of the database schema that is the location where
#'                                       a table can be created and afterwards removed.
#'                                       Requires write permissions to this database.
#' @param cdmVersion                     Define the OMOP CDM version used: currently support "4" and
#'                                       "5".
#' @param outputFolder                   Name of the folder where all the outputs will written to.
#' @param phevaluatorAnalysisList        A list of objects of type \code{phevaluatorAnalysis} as created using
#'                                       the \code{\link{createPhevaluatorAnalysis}} function.
#'
#' @return
#' A data frame specifiying where the constructed evaluation cohort and phenotype evaluation results can be found
#' in the local file system.
#'
#' @export
runPheValuatorAnalyses <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema = cdmDatabaseSchema,
                                   cohortTable = "cohort",
                                   workDatabaseSchema = cdmDatabaseSchema,
                                   cdmVersion = 5,
                                   outputFolder = "./PhevaluatorOutput",
                                   phevaluatorAnalysisList) {
}
