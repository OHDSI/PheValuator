library("testthat")
library("PheValuator")

test_that("createDefaultAcuteCovariateSettings: 3 acute time periods", {
  excludedCovariateConceptIds <- c(12345)
  includedCovariateIds <- c(1,2,3)
  includedCovariateConceptIds <- c(4,5,6,7)

  CovSettingsAcute <- createDefaultAcuteCovariateSettings(excludedCovariateConceptIds = excludedCovariateConceptIds,
                                                          includedCovariateIds = includedCovariateIds,
                                                          includedCovariateConceptIds = includedCovariateConceptIds,
                                                          addDescendantsToExclude = TRUE,
                                                          startDayWindow1 = 0,
                                                          endDayWindow1 = 10,
                                                          startDayWindow2 = 11,
                                                          endDayWindow2 = 20,
                                                          startDayWindow3 = 21,
                                                          endDayWindow3 = 30)
  expect_length(CovSettingsAcute[[1]], 29)
  expect_length(CovSettingsAcute[[1]], 29)
  expect_length(CovSettingsAcute[[2]], 25)
  expect_length(CovSettingsAcute[[3]], 25)
  expect_equal(CovSettingsAcute[[1]]$shortTermStartDays, 0)
  expect_equal(CovSettingsAcute[[3]]$endDays, 30)
  expect_equal(CovSettingsAcute[[1]]$includedCovariateConceptIds, CovSettingsAcute[[3]]$includedCovariateConceptIds)
  expect_equal(CovSettingsAcute[[1]]$includedCovariateConceptIds, includedCovariateConceptIds)
})
