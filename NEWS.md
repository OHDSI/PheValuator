PheValuator 2.2.15
=================

Changes:
1. Update model cohort build for faster performance

PheValuator 2.2.14
=================

Changes:
1. Update evaluation cohort build for faster performance

PheValuator 2.2.13
=================

Changes:
1. Bug fix in PLP parameters

PheValuator 2.2.12
=================

Changes:
1. Update model and evaluation cohort sql files to remove temporary files

PheValuator 2.2.11
=================

Changes:
1. Update evaluation cohort processing to allow multiple matches with comparative phenotype algorithm

PheValuator 2.2.10
=================

Changes:
1. Add parameter to force evaluation visits to be only within the start and end dates of the cohort
2. Bug fix within time-at-risk calculation

PheValuator 2.2.9
=================

Changes:
1. Allow for a designated Case cohort to use for comparisons between cases and non-cases when viewing false postives and negatives
2. Allow for use of evaluation saved PLP data when flag for data save is TRUE
3. Replace duplicate names for shiny output
4. Create the cohort definition set within the tool for shiny use
5. Correct bug when there are NA's in test phenotype
6. Reduce constraints on xSpec cohort allowing for more subjects to be included within the cohort

PheValuator 2.2.8
=================

Changes:
1. Add capability to supply custom visit occurrence table to increase speed of processing for extremely large visit occurrence tables
2. Bug fix in sql files needed in sql server

PheValuator 2.2.7
=================

Changes:
1. Add capability to use exisitng model for a run
2. Bug fix for getPopnPrev.sql needed in sql server

PheValuator 2.2.6
=================

Changes:
1. add in additional parameters for output csv models
2. add default value for xSens cohort as prevalence cohort

PheValuator 2.2.3
=================

Changes:

1. Added capability to supply designated model cohort instead of using xSpec
2. Added parameters to ensure correct offset from start and end of observation period
3. Created output csv files to be used in shiny applications as well as for easier access to results

PheValuator 2.2.2
=================

Changes:

1. Eliminate evaluation population as an input parameter
2. Add capability to input special inclusion rules for evaluation cohort
3. Add capability to input special exclusion rules for evaluation cohort
4. Add parameter to set the number of false positive and negative examples saved for analysis



PheValuator 2.2.0
=================

Changes:

1. Add capability to create simpler xSpec cohorts not based on visit dates
2. Add capability to have PheValuator generate the evaluation cohort automatically
3. Use a faster method to develop predicted probabilities for evaluation cohort
4. Include a set of subjects designated as true and false positives and negatives in output

PheValuator 2.1.13
=================

Changes:

1. Put unit in testing in place for inclusion in HADES.

PheValuator 2.0.0
=================

Changes:

1. Major overhaul of the code base and functions, aimed at improving stability, consistency with other OHSDI packages, and usability.
