% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PheValuator.R
\name{execute}
\alias{execute}
\title{Execute PheValuator}
\usage{
execute(
  connectionDetails,
  cdmDatabaseSchema,
  cdmVersion,
  cohortDatabaseSchema = cdmDatabaseSchema,
  cohortTable = "cohort",
  workDatabaseSchema = cohortDatabaseSchema,
  oracleTempSchema = cohortDatabaseSchema,
  outputFolder,
  databaseId = "Unknown",
  databaseName = "Unknown",
  databaseDescription = "Unknown",
  createCohorts = TRUE,
  runPheValuator = TRUE,
  exportResults = TRUE,
  minCellCount = 5
)
}
\arguments{
\item{connectionDetails}{An object of type \code{connectionDetails} as created using the
\code{\link[DatabaseConnector]{createConnectionDetails}} function in the
DatabaseConnector package.}

\item{cdmDatabaseSchema}{Schema name where your patient-level data in OMOP CDM format resides.
Note that for SQL Server, this should include both the database and
schema name, for example 'cdm_data.dbo'.}

\item{cdmVersion}{Define the OMOP CDM version used: currently supports "5".}

\item{cohortDatabaseSchema}{Schema name where intermediate data can be stored. You will need to have
write priviliges in this schema. Note that for SQL Server, this should
include both the database and schema name, for example 'cdm_data.dbo'.}

\item{cohortTable}{The name of the table that will be created in the work database schema.
This table will hold the cohorts used in this study.}

\item{workDatabaseSchema}{The name of the database schema that is the location where
a table can be created and afterwards removed.
Requires write permissions to this database.}

\item{oracleTempSchema}{Should be used in Oracle to specify a schema where the user has write
priviliges for storing temporary tables.}

\item{outputFolder}{Name of local folder to place results; make sure to use forward slashes
(/). Do not use a folder on a network drive since this greatly impacts
performance.}

\item{databaseId}{A short string for identifying the database (e.g.
'Synpuf').}

\item{databaseName}{The full name of the database (e.g. 'Medicare Claims
Synthetic Public Use Files (SynPUFs)').}

\item{databaseDescription}{A short description (several sentences) of the database.}

\item{createCohorts}{Create the cohortTable table with the exposure and outcome cohorts?}

\item{runPheValuator}{Run PheValuator?}

\item{exportResults}{Export the PheValuator results?}

\item{minCellCount}{The minimum number of subjects contributing to a count before it can be included 
in packaged results.}
}
\description{
Execute PheValuator
}
\details{
This function executes PheValuator.
}
