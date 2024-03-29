# This file has been autogenerated. Do not change by hand.

#' Create a parameter object for the function createEvaluationCohort
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param xSpecCohortId                    The number of the "extremely specific (xSpec)"
#'                                         cohortdefinition id in the cohort table (for noisy
#'                                         positives).
#' @param daysFromxSpec                    Number of days from xSpec condition until analyzed visit
#' @param xSensCohortId                    The number of the "extremely sensitive (xSens)"
#'                                         cohortdefinition id in the cohort table (for noisy
#'                                         negatives).
#' @param prevalenceCohortId               The number of the cohort definition id to determine
#'                                         the disease prevalence.
#' @param caseCohortId                     The number of the cohort definition id to determine cases in the evaluation cohort
#' @param caseFirstOccurrenceOnly          Set to true if only the first occurrence per subject in the case cohort is to be used
#' @param xSpecCohortSize                  Maximum xSpec sample size to use in the analysis
#' @param covariateSettings                A covariateSettings object as generated
#'                                         using createCovariateSettings().
#' @param modelPopulationCohortId          The number of the cohort to be used as a base population for
#'                                         the model. If set to 0, the entire database population will be
#'                                         used.
#' @param modelPopulationCohortIdStartDay   The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to begin including visits.
#' @param modelPopulationCohortIdEndDay     The number of days relative to the mainPopulationCohortId
#'                                         cohort start date to end including visits.
#' @param inclusionEvaluationCohortId      The number of the cohort of the population to be used to designate which visits
#'                                         are are eligible to be in the evaluation cohort
#' @param inclusionEvaluationDaysFromStart The number of days from the cohort start date of the inclusionEvaluationCohortId
#'                                         to start eligible included visits
#' @param inclusionEvaluationDaysFromEnd   The number of days from the cohort start date of the inclusionEvaluationCohortId
#'                                         to end eligible included visits
#' @param duringInclusionEvaluationOnly    Only include visits that are within the cohort start and end dates
#' @param exclusionEvaluationCohortId      The number of the cohort of the population to be used to designate which visits
#'                                         are NOT eligible to be in the evaluation cohort
#' @param exclusionEvaluationDaysFromStart The number of days from the cohort start date of the exclusionEvaluationCohortId
#'                                         to start ineligible included visits
#' @param exclusionEvaluationDaysFromEnd   The number of days from the cohort start date of the exclusionEvaluationCohortId
#'                                         to end ineligible included visits
#' @param minimumOffsetFromStart           Minimum number of days to offset for the analysis visit from the start of the observation period
#' @param minimumOffsetFromEnd             Minimum number of days to offset for the analysis visit from the end of the observation period
#' @param modelBaseSampleSize              The number of non-xSpec subjects to include in the model
#' @param baseSampleSize                   The maximum number of subjects in the evaluation cohort.
#' @param lowerAgeLimit                    The lower age for subjects in the model.
#' @param upperAgeLimit                    The upper age for subjects in the model.
#' @param visitLength                      The minimum length of index visit for acute outcomes.
#' @param visitType                        The concept_id for the visit type.
#' @param gender                           The gender(s) to be included.
#' @param race                             The race(s) to be included.
#' @param ethnicity                        The ethnicity(s) to be included.
#' @param startDate                        The starting date for including subjects in the model.
#' @param endDate                          The ending date for including subjects in the model.
#' @param falsePositiveNegativeSubjects    Number of subjects to include for evaluating false positives and negatives
#' @param modelId                          A string used to generate the file names for this model.
#' @param evaluationCohortId               A string used to generate the file names for this evaluation cohort.
#' @param randomVisitTable                 Table stored in work directory with pre-selected random visits in format of visit_occurrence table
#' @param excludeModelFromEvaluation       Should subjects used in the model be excluded from the
#'                                         evaluation cohort?
#' @param removeSubjectsWithFutureDates    For buggy data with data in the future: ignore subjects
#'                                         with dates in the future?
#' @param saveEvaluationCohortPlpData      Should the large PLP file for the evaluation cohort be
#'                                         saved? To be used for debugging purposes.
#'
#' @export
createCreateEvaluationCohortArgs <- function(xSpecCohortId,
                                             daysFromxSpec = 0,
                                             xSensCohortId = prevalenceCohortId,
                                             prevalenceCohortId,
                                             caseCohortId = prevalenceCohortId,
                                             caseFirstOccurrenceOnly = TRUE,
                                             xSpecCohortSize = 5000,
                                             covariateSettings = createDefaultCovariateSettings(
                                               excludedCovariateConceptIds = c(),
                                               addDescendantsToExclude = TRUE
                                             ),
                                             modelPopulationCohortId = 0,
                                             modelPopulationCohortIdStartDay = 0,
                                             modelPopulationCohortIdEndDay = 0,
                                             inclusionEvaluationCohortId = 0,
                                             inclusionEvaluationDaysFromStart = 0,
                                             inclusionEvaluationDaysFromEnd = 0,
                                             duringInclusionEvaluationOnly = FALSE,
                                             exclusionEvaluationCohortId = 0,
                                             exclusionEvaluationDaysFromStart = 0,
                                             exclusionEvaluationDaysFromEnd = 0,
                                             minimumOffsetFromStart = 0,
                                             minimumOffsetFromEnd = 30,
                                             modelBaseSampleSize = 25000,
                                             baseSampleSize = 2e+06,
                                             lowerAgeLimit = 0,
                                             upperAgeLimit = 120,
                                             visitLength = 0,
                                             visitType = c(9201, 9202, 9203, 581477, 262),
                                             gender = c(8507, 8532),
                                             race = 0,
                                             ethnicity = 0,
                                             startDate = "19001010",
                                             endDate = "21000101",
                                             falsePositiveNegativeSubjects = 10,
                                             modelId = "main",
                                             evaluationCohortId = "main",
                                             randomVisitTable = "",
                                             excludeModelFromEvaluation = FALSE,
                                             removeSubjectsWithFutureDates = TRUE,
                                             saveEvaluationCohortPlpData = FALSE) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createCreateEvaluationCohortArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }
  class(analysis) <- "args"
  return(analysis)
}

#' Create a parameter object for the function testPhenotypeAlgorithm
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param cutPoints           A list of threshold predictions for the evaluations.  Include "EV"for the
#'                            expected value
#' @param phenotypeCohortId   The ID of the cohort to evaluate in the specified cohort table.
#' @param washoutPeriod       The mininum required continuous observation time prior to indexdate for
#'                            subjects within the cohort to test (Default = 0).
#' @param splayPrior          The number of days to allow for test phenotype visit date prior to evaluation date
#' @param splayPost           The number of days to allow for test phenotype visit date after evaluation date
#'
#' @export
createTestPhenotypeAlgorithmArgs <- function(cutPoints = c("EV"),
                                             phenotypeCohortId,
                                             washoutPeriod = 0,
                                             splayPrior = 7,
                                             splayPost = 7) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createTestPhenotypeAlgorithmArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }
  class(analysis) <- "args"
  return(analysis)
}
