## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 notes ✔

## Test environments

* Local Windows 11, R 4.4.1
* win-builder (R-devel)
* R-hub: Ubuntu 22.04, R 4.4.1
* R-hub: macOS, R 4.4.1

## Submission notes

This is the initial CRAN submission.

```r
devtools::check(cran=TRUE)
devtools::check_win_devel()
```

2 notes are generated. 
These are caused by dplyr nonstandard eval (createEvalCohort.R line 387). 
dplyr::join_by does not allow the .data pronoun that would suppress the note.

## Reverse dependencies

There are no reverse dependencies.
