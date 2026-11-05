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

  if (file.exists(file.path(outFolder, paste0(evaluationOutputFileName, ".rds")))) {
    writeLines(paste("\n", Sys.time(), "test:", databaseId))

    for (phenos in 1:nrow(phenoFile)) {
      phenoResult <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                            cutPoints = "EV",
                                            evaluationOutputFileName = file.path(outFolder,
                                                                                 paste0(evaluationOutputFileName, ".rds")),
                                            phenotypeCohortId = phenoFile$atlasId[[phenos]],
                                            phenotypeText = phenoFile$atlasName[[phenos]],
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
      results <- rbind(results, as.data.frame(phenoResult[1],
                                              stringsAsFactors = FALSE,
                                              check.names = FALSE))

    }

    write.csv(results[with(results, order(`Phenotype Order`, xtfrm(CDM))),
                      ],
              file.path(outFolder, paste0(phenotypeEvaluationFileName, ".csv")),
              row.names = FALSE)

    ParallelLogger::logInfo("Output file saved as: ",
                      file.path(outFolder, paste0(phenotypeEvaluationFileName, ".csv")))

  } else {
    ParallelLogger::logInfo("Missing file: ", file.path(outFolder, evaluationOutputFileName), ".rds")
  }
}
