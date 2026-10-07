## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 notes ✔

## Test environments

* Local Windows 11, R 4.4.1
* win-builder (R-devel)
* R-hub: Ubuntu 22.04, R 4.4.1
* R-hub: macOS, R 4.4.1

## Submission notes

This is the second CRAN submission. Responses to the first review are as follows:

**Comment 1 - References in DESCRIPTION:** Added two DOI references (Swerdedl 2022 and 2019) to the Description field in DESCRIPTION file with proper formatting.

**Comment 2 - Missing \value tags:** Added @return roxygen2 tags to all 6 functions in source files documenting return types and structures.

**Comment 3 - \dontrun{} usage:** Removed \dontrun{} from 4 self-contained, executable examples. Kept \dontrun{} for functions requiring external dependencies (database connection, pre-existing objects) and updated savePheValuatorAnalysisList example to use tempdir().

**Comment 4 - Writing to home filespace:** Removed default getwd() value from outFolder parameter in createPhenoModel, createEvaluationCohort, and CreateEvaluationCohort functions, making outFolder a required parameter.


2 notes are generated. 
These are caused by dplyr nonstandard eval (createEvalCohort.R line 387). 
dplyr::join_by does not allow the .data pronoun that would suppress the note.

## Reverse dependencies

There are no reverse dependencies.
