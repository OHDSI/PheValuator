  # @file createPhenotypeModel.R
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

  #' createPhenotypeModel
  #'
  #' @description
  #' Create the phenotype model
  #'
  #' @details
  #' Function to create a diagnostic prediction model for a health outcome of interest using the xSpec
  #' cohort.  The model may be applied to the evaluation cohort to determine probabilities for each
  #' subject for the health outcome of interest.
  #'
  #' @param connectionDetails      connectionDetails created using the function createConnectionDetails
  #'                               in the DatabaseConnector package.
  #' @param xSpecCohort            The number of the "extremely specific (xSpec)" cohort definition id in
  #'                               the cohort table (for noisy positives)
  #' @param cdmDatabaseSchema      The name of the database schema that contains the OMOP CDM instance.
  #'                               Requires read permissions to this database. On SQL Server, this should
  #'                               specifiy both the database and the schema, so for example
  #'                               'cdm_instance.dbo'.
  #' @param cohortDatabaseSchema   The name of the database schema that is the location where the cohort
  #'                               data used to define the at risk cohort is available. Requires read
  #'                               permissions to this database.
  #' @param cohortDatabaseTable    The tablename that contains the at risk cohort. The expectation is
  #'                               cohortTable has format of COHORT table: cohort_concept_id, SUBJECT_ID,
  #'                               COHORT_START_DATE, COHORT_END_DATE.
  #' @param outDatabaseSchema      The name of a database schema where the user has write capability.  A
  #'                               temporary cohort table will be created here.
  #' @param modelOutputFileName    A string designation for the training model file
  #' @param xSensCohort            The number of the "extremely sensitive (xSens)" cohort definition id
  #'                               in the cohort table (used to exclude subjects from the base population)
  #' @param prevalenceCohort       The number of the cohort definition id to determine the disease prevalence,
  #'                               (default=xSensCohort)
  #' @param excludedConcepts       A list of conceptIds to exclude from featureExtraction.  These should include all
  #'                               concept_ids that were used to define the xSpec model (default=NULL)
  #' @param addDescendantsToExclude        Should descendants of excluded concepts also be excluded? (default=FALSE)
  #' @param mainPopulationCohort   The number of the cohort ID to be used as a base population for the model
  #'                               (default=NULL)
  #' @param lowerAgeLimit          The lower age for subjects in the model (default=NULL)
  #' @param upperAgeLimit          The upper age for subjects in the model (default=NULL)
  #' @param gender                 The gender(s) to be included (default c(8507, 8532))
  #' @param startDate              The starting date for including subjects in the model (default=NULL)
  #' @param endDate                The ending date for including subjects in the model (default=NULL)
  #' @param cdmVersion             The CDM version of the database (default=5)
  #' @param outFolder              The folder where the output files will be written (default=working directory)
  #'
  #' @importFrom stats runif
  #'
  #' @export
  createPhenotypeModel <- function(connectionDetails,
                               xSpecCohort,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema,
                               cohortDatabaseTable,
                               outDatabaseSchema,
                               modelOutputFileName = "train",
                               xSensCohort,
                               prevalenceCohort = xSensCohort,
                               excludedConcepts = c(),
                               addDescendantsToExclude = FALSE,
                               mainPopulationCohort = 0,
                               lowerAgeLimit = 0,
                               upperAgeLimit = 120,
                               gender = c(8507, 8532),
                               startDate = "19000101",
                               endDate = "21000101",
                               cdmVersion = "5",
                               outFolder = getwd()) {

    options(error = NULL)

    # error checking for input
    if (length(connectionDetails) == 0)
      stop("...must supply a connection string")
    if (xSpecCohort == "")
      stop("...must have an xSpec cohort id (e.g., 1234)")
    if (xSensCohort == "")
      stop("...must have an xSens cohort id (e.g., 1235)")
    if (prevalenceCohort == "")
      stop("...must have an prevalence cohort (prevCohort) (e.g., 1235)")
    if (cdmDatabaseSchema == "")
      stop("....must have a defined CDM schema (e.g., \"YourCDM.YourCDMSchema\")")
    if (cohortDatabaseSchema == "")
      stop("....must have a defined Cohort schema ((e.g., \"YourCDM.YourCohortSchema\")")
    if (cohortDatabaseTable == "")
      stop("....must have a defined Cohort table (e.g., \"cohort\")")
    if (outDatabaseSchema == "")
      stop("....must have a defined Out Database schema (e.g., \"scratch.dbo\")")
    if (modelOutputFileName == "")
      stop("....must have a defined training file name (e.g., \"train_10XDiabetes\")")

    writeLines(paste("xSpecCohort ", xSpecCohort))
    writeLines(paste("cdmDatabaseSchema ", cdmDatabaseSchema))
    writeLines(paste("cohortDatabaseSchema ", cohortDatabaseSchema))
    writeLines(paste("cohortDatabaseTable ", cohortDatabaseTable))
    writeLines(paste("outDatabaseSchema ", outDatabaseSchema))
    writeLines(paste("modelOutputFileName ", modelOutputFileName))
    writeLines(paste("xSensCohort ", xSensCohort))
    writeLines(paste("prevalenceCohort ", prevalenceCohort))
    writeLines(paste("excludedConcepts ", c(excludedConcepts)))
    writeLines(paste("addDescendantsToExclude ", addDescendantsToExclude))
    writeLines(paste("mainPopulationCohort ", mainPopulationCohort))
    writeLines(paste("lowerAgeLimit ", lowerAgeLimit))
    writeLines(paste("upperAgeLimit ", upperAgeLimit))
    writeLines(paste("gender ", gender))
    writeLines(paste("startDate ", startDate))
    writeLines(paste("endDate ", endDate))
    writeLines(paste("cdmVersion ", cdmVersion))
    writeLines(paste("outFolder ", outFolder))

    workFolder <- outFolder

    conn <- DatabaseConnector::connect(connectionDetails)

    # determine population prevalence for correct xSpec/noisy negative popn ratio
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "getPopnPrev.sql",
                                                package = "PheValuator"))

    sql <- SqlRender::render(sqlScript,
                                cdm_database_schema = cdmDatabaseSchema,
                                cohort_database_schema = cohortDatabaseSchema,
                                cohort_database_table = cohortDatabaseTable,
                                lowerAgeLimit = lowerAgeLimit,
                                upperAgeLimit = upperAgeLimit,
                                gender = gender,
                                startDate = startDate,
                                endDate = endDate,
                                prevCohort = prevalenceCohort)

    sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

    popPrev <- DatabaseConnector::querySql(conn = conn, sql)

    if (popPrev == 0)
      stop("...unable to calculate the expected prevalence, possibly an error with prevalence cohort id")

    writeLines(paste("\nEstimated Population Prevalence: ", popPrev, sep = ""))

    # set reasonable model populations - for fast, but accurate models
    if (popPrev >= 0.3) {
      xspecSize <- 4000  #use large xSpec size for higher prevalence values
    } else if (popPrev >= 0.2) {
      xspecSize <- 3000
    } else if (popPrev >= 0.1) {
      xspecSize <- 2000
    } else {
      xspecSize <- 1500  #use smaller xSpec size for lower prevalence values
    }

    # set the number of nosiy negatives in the model either from the prevalence or to 500K max
    baseSampleSize <- min(as.integer(xspecSize/popPrev), 5e+05)  #use 500,000 as largest base sample

    # sql script to create a temporary cohort table for predictive modeling
    sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                "CreateCohortsV6.sql",
                                                package = "PheValuator"))

    covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                    useDemographicsAgeGroup = TRUE,
                                                                    useDemographicsRace = TRUE,
                                                                    useDemographicsEthnicity = TRUE,
                                                                    useConditionOccurrenceLongTerm = TRUE,
                                                                    useConditionOccurrencePrimaryInpatientLongTerm = TRUE,
                                                                    useConditionGroupEraLongTerm = TRUE,
                                                                    useDrugExposureLongTerm = TRUE,
                                                                    useDrugEraLongTerm = TRUE,
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
                                                                    longTermStartDays = -10000,
                                                                    endDays = 10000,
                                                                    includedCovariateConceptIds = c(),
                                                                    addDescendantsToInclude = TRUE,
                                                                    excludedCovariateConceptIds = excludedConcepts,
                                                                    addDescendantsToExclude = addDescendantsToExclude,
                                                                    includedCovariateIds = c())

    baseSample <- baseSampleSize
    plpDataFile <- file.path(workFolder, paste("plpData_",
                                               modelOutputFileName,
                                               sep = ""))
    resultsFileName <- file.path(workFolder, paste(modelOutputFileName,
                                                   ".rds",
                                                   sep = ""))
    resultsDirName <- file.path(workFolder, paste("lr_results_",
                                                  modelOutputFileName,
                                                  sep = ""))

    if (!file.exists(plpDataFile)) {
      # only pull the plp data if it doesn't already exist create a unique name for the temporary cohort
      test_cohort <- gsub(".",
                          "",
                          (paste("test_cohort", runif(1, min = 0, max = 1), sep = "")),
                          fixed = TRUE)

      # create the temporary cohort
      sql <- SqlRender::render(sqlScript,
                                  cdm_database_schema = cdmDatabaseSchema,
                                  cohort_database_schema = cohortDatabaseSchema,
                                  cohort_database_table = cohortDatabaseTable,
                                  x_spec_cohort = xSpecCohort,
                                  tempDB = outDatabaseSchema,
                                  test_cohort = test_cohort,
                                  exclCohort = xSensCohort,
                                  ageLimit = lowerAgeLimit,
                                  upperAgeLimit = upperAgeLimit,
                                  gender = gender,
                                  startDate = startDate,
                                  endDate = endDate,
                                  baseSampleSize = baseSample,
                                  xSpecSampleSize = xspecSize,
                                  mainPopnCohort = mainPopulationCohort,
                                  lookback = -1095)
      # for the xSpec model include 1095 days before first dx

      sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

      DatabaseConnector::executeSql(conn = conn, sql)

      # pull the features for the model
      plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                    cdmDatabaseSchema = paste(cdmDatabaseSchema,
                                                                              sep = ""),
                                                    cohortId = 0,
                                                    outcomeIds = xSpecCohort,
                                                    outcomeDatabaseSchema = outDatabaseSchema,
                                                    outcomeTable = test_cohort,
                                                    cohortDatabaseSchema = outDatabaseSchema,
                                                    cohortTable = test_cohort,
                                                    cdmVersion = cdmVersion,
                                                    washoutPeriod = 0,
                                                    covariateSettings = covariateSettings)

      summary(plpData)
      writeLines(paste0("\nSaving PLP Data to: ",plpDataFile))
      PatientLevelPrediction::savePlpData(plpData, plpDataFile)

      # remove temp cohort table
      sqlScript <- SqlRender::readSql(system.file(paste("sql/", "sql_server", sep = ""),
                                                  "DropTempTable.sql",
                                                  package = "PheValuator"))
      sql <- SqlRender::render(sqlScript, tempDB = outDatabaseSchema, test_cohort = test_cohort)
      sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

      DatabaseConnector::executeSql(conn = conn, sql)
    } else {
      writeLines(paste("...Loading ", plpDataFile, " from existing directory", sep = ""))
      plpData <- PatientLevelPrediction::loadPlpData(plpDataFile)
    }

    if (!file.exists(resultsFileName)) {
      # only create the model if it doesn't exist
      population <- PatientLevelPrediction::createStudyPopulation(plpData,
                                                                  population = NULL,
                                                                  outcomeId = xSpecCohort,
                                                                  firstExposureOnly = FALSE,
                                                                  washoutPeriod = 0,
                                                                  removeSubjectsWithPriorOutcome = TRUE,
                                                                  priorOutcomeLookback = 1,
                                                                  riskWindowStart = 0,
                                                                  requireTimeAtRisk = FALSE,
                                                                  minTimeAtRisk = 0,
                                                                  addExposureDaysToStart = FALSE,
                                                                  riskWindowEnd = 1,
                                                                  addExposureDaysToEnd = T)

      modelSettings <- PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01, seed = 5)

      lr_results <- PatientLevelPrediction::runPlp(population,
                                                   plpData,
                                                   modelSettings = modelSettings,
                                                   testSplit = "person",
                                                   testFraction = 0.25,
                                                   splitSeed = 5,
                                                   nfold = 3,
                                                   savePlpData = F, savePlpResult = F,
                                                   savePlpPlots = F, saveEvaluation = F, )

      print(resultsFileName)
      saveRDS(lr_results, resultsFileName)

      print(resultsDirName)
      PatientLevelPrediction::savePlpResult(lr_results, resultsDirName)
    } else {
      writeLines(paste("\n...Loading ", resultsFileName, " from existing file", sep = ""))
      lr_results <- readRDS(resultsFileName)
    }

    DatabaseConnector::disconnect(conn)
  }

