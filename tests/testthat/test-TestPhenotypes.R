library("testthat")
library("PheValuator")

test_that("TestPhenotype - missing connectionDetails message", {
  connectionDetails <- NULL

  test <- testPhenotypeAlgorithm(connectionDetails = connectionDetails,
                                 cutPoints = c("EV"),
                                 outFolder,
                                 evaluationCohortId = "main",
                                 phenotypeCohortId,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 washoutPeriod = 0,
                                 splayPrior = 7,
                                 splayPost = 7)

  expect_message("Must supply a connection string")
})

testParam <- NULL
test <- function(testParam = "a") {
  if (length(testParam) == 0)
    ParallelLogger::logInfo("Must supply a connection string")
}
 a <- test(testParam)
