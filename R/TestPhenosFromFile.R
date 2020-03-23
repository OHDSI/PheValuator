testPhenosFromFile <- function(connectionDetails,
                               evaluationOutputFileName,
                               phenotypeEvaluationFileName,
                               databaseId,
                               xSpecCohortId,
                               xSensCohortId,
                               prevalenceCohortId,
                               cohortDatabaseSchema,
                               cohortTable,
                               cohortDefinitionsToTest,
                               outFolder,
                               modelType) {

  results <- data.frame()
  phenoFile <- cohortDefinitionsToTest

  if(file.exists(file.path(outFolder,paste0(evaluationOutputFileName,".rds")))) {
    writeLines(paste("\n",Sys.time(), "test:", databaseId))

    for(phenos in 1:nrow(phenoFile)) {
      phenoResult <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                            cutPoints = "EV",
                                            evaluationOutputFileName = file.path(outFolder,paste0(evaluationOutputFileName,".rds")),
                                            phenotypeCohortId = phenoFile$atlasId[[phenos]],
                                            phenotypeText  = phenoFile$atlasName[[phenos]],
                                            order = phenos,
                                            modelText = phenoFile$name[[phenos]],
                                            xSpecCohortId = xSpecCohortId,
                                            xSensCohortId = xSensCohortId,
                                            prevalenceCohortId = prevalenceCohortId,
                                            cohortDatabaseSchema = cohortDatabaseSchema,
                                            cohortTable = cohortTable,
                                            databaseId = databaseId,
                                            washoutPeriod = phenoFile$washoutPeriod[[phenos]],
                                            modelType = modelType)

      if(nrow(results) == 0) {results <- as.data.frame(phenoResult[1], stringsAsFactors = FALSE, check.names = FALSE)}
      else {results <- rbind(results, as.data.frame(phenoResult[1], stringsAsFactors = FALSE, check.names = FALSE))
      }

    }

    write.csv(results[with(results, order(`Phenotype Order`, xtfrm(CDM))),],
              file.path(outFolder, paste0(phenotypeEvaluationFileName, ".csv")), row.names = FALSE)

    writeLines(paste0("\nOutput file saved as: ", file.path(outFolder, paste0(phenotypeEvaluationFileName, ".csv")), sep = ""))

  } else {
    writeLines(paste0("Missing file: ", file.path(outFolder,evaluationOutputFileName,".rds")))
  }

}
