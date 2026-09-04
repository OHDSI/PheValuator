## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Test environments

* Local Windows 11, R 4.4.1
* win-builder (R-devel)
* R-hub: Ubuntu 22.04, R 4.4.1
* R-hub: macOS, R 4.4.1

## Submission notes

This is the initial CRAN submission.

```r
rcmdcheck::rcmdcheck(args = "--as-cran", error_on = "warning")
devtools::check_win_devel()
devtools::check_win_release()
```

## Reverse dependencies

There are no reverse dependencies.
