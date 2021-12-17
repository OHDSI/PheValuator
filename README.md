PheValuator
===========

[![Build Status](https://github.com/OHDSI/PheValuator/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/PheValuator/actions?query=workflow%3AR-CMD-check)

An R package for evaluating phenotype algorithms.


Introduction
============

The goal of PheValuator is to produce a large cohort of subjects each with a predicted probability for a specified health outcome of interest (HOI).  This is achieved by developing a diagnostic predictive model for the HOI using the PatientLevelPrediction (PLP) R package and applying the model to a large, randomly selected population.  These subjects can be used to test one or more phenotype algorithms.  

Process Steps
=============
The first step in the process, developing the evaluation cohort, is shown below:
![](https://github.com/OHDSI/PheValuator/raw/master/vignettes/Figure1.png)

The model is created using a cohort of subjects with a very high likelihood of having the HOI. These "noisy" positives ("noisy" in that they are very likely positive for the HOI but not a true gold standard) are called the "xSpec" cohort - extremely specific.  This cohort will be the Outcome (O) cohort in the PLP model.  There are several methods to create this cohort but the simplest would be to develop a cohort of subjects who have multiple condition codes for the HOI in their patient record.  A typical number to use might be 5 or more condition codes for acute HOI's, say myocardial infarction, or 10 or more condition codes for chronic HOI's, say diabetes mellitus. We also define a noisy negatives cohort.  This cohort is created by taking a random sample of the subjects in the database who have no evidence of the HOI.  These would be determined by creating a very sensitive cohort, in most cases 1 or more condition codes for the HOI and excluding these subjects for the noisy negative cohort.  The xSpec cohort and the noisy negative cohort are combined to for the Target (T) cohort for the PLP model.  We then create a diagnostic predictive model with LASSO regularized regression using all the data in the subjects record. The data to inform this model is created using the FeatureExtraction package.  The data includes conditions, drug exposures, procedures, and measurements.  The developed model has a set of features with beta coefficients that can be used to discriminate between those with the HOI and those without.

We next create and "evaluation" cohort - a large group of randomly selected subjects to be used to evaluate the phenotype algorithms (PA). The subjects are selected by pulling up to 1,000,000 subjects from the dataset.  We extract the same covariates as we extracted form the T cohort in the model creation phase.  We use the PLP function applyModel to apply the model to this large cohort producing probabilities for the HOI for each subject in the evaluation cohort.  The subjects in this cohort with their associated probability of the HOI are used as a "gold" standard for the HOI. We save this output for use in the next step of the process

The second step in the process, evaluating the PAs, is shown below:
![](https://github.com/OHDSI/PheValuator/raw/master/vignettes/Figure2.png)

The next step in the process tests the PA(s).  Phenotype algorithms are created based upon the needs of the research to be performed.  Every subject in the evaluation cohort should be eligible to be included in the cohort developed from this algorithm.  The figure describes how the predicted probabilities for subjects either included or excluded from the phenotype algorithm cohort are used to evaluate the PA.  To fully evaluate a PA, you need to estimate the sensitivity, specificity, and positive and negative predictive values.  These values are estimated through calculations involving subjects that are True Positives (TP), True Negatives (TN), False Positives (FP), and False Negatives (FN).  These statistics are generated using the predicted probabilities.  Examples of the calculations are shown in the diagram.  The formulas for the final calculations are also displayed.

The results from the evaluation for Opioid Abuse is shown below:
![](https://github.com/OHDSI/PheValuator/raw/master/vignettes/Figure3.png)

The diagram shows the complete performance evaluation for 5 PAs for the Expected Value as described above where the predicted value is used for summing the TP, FP, TN, and FN values.  The full table created by the function also includes the performance characteristics based on the prediction thresholds specified when running the function. 

Technology
==========
PheValuator is an R package.

System Requirements
===================
Requires R (version 3.3.0 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Some of the packages used by PheValuator require Java.

Installation
=============
1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.
2. In R, use the following commands to download and install PheValuator:

    ```r
    install.packages("remotes")
    remotes::install_github("ohdsi/PheValuator")
    ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/PheValuator).

PDF versions of the documentation are also available:
* Vignette: [Performing a Phenotype Algorithm Evaluation using Phevaluator](https://github.com/OHDSI/PheValuator/blob/master/inst/doc/EvaluatingPhenotypeAlgorithms.pdf)
* Package manual: [PheValuator manual](https://raw.githubusercontent.com/OHDSI/PheValuator/master/extras/PheValuator.pdf) 


Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/PheValuator">GitHub issue tracker</a> for all bugs/issues/enhancements
 
Contributing
============

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
PheValuator is licensed under Apache License 2.0

Development
===========
PheValuator is being developed in R Studio.

### Development status

Beta

# Acknowledgements

- The package is maintained by Joel Swerdel and has been developed with major contributions from Jenna Reps, Peter Rijnbeek, Martijn Schuemie, Patrick Ryan, and Marc Suchard.

