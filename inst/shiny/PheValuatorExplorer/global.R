if (!exists("shinySettings")) {
  if (file.exists("data")) {
    shinySettings <- list(dataFolder = "data")
  } else {
    shinySettings <- list(dataFolder = "C:/TEMP/PheValuatorExport")
  }
}
dataFolder <- shinySettings$dataFolder

suppressWarnings(rm("cohort", "database", "phevaluatorAnalysis", "phevaluatorResult"))

load(file.path(dataFolder, "PreparedPheValuatorData.RData"))

cohort <- cohort[, c("cohortFullName", "cohortId", "cohortName")]
