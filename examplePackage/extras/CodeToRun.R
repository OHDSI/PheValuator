# Make sure to install all dependencies (not needed if already done):
devtools::install_github("ohdsi/PheValuator")

# Load the package
library(examplePackage)

# Optional: specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "s:/FFtemp")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("PDW_SERVER"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("PDW_PORT"))

# For Oracle: define a schema that can be used to emulate temp tables:
oracleTempSchema <- NULL

# Details specific to the database:
outputFolder <- "s:/examplePackage/mdcd"
cdmDatabaseSchema <- "cdm_ibm_mdcd_v1023.dbo"
cdmVersion <- 5
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_test"
workDatabaseSchema <- "scratch.dbo"
databaseId <- "MDCD"
databaseName <- "IBM MarketScan® Multi-State Medicaid Database"
databaseDescription <- "IBM MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data."

# Use this to PheValuator The results will be stored in the diagnosticsExport subfolder of the outputFolder. This can be shared between sites.
execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cdmVersion = cdmVersion,
        workDatabaseSchema = workDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        oracleTempSchema = oracleTempSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        createCohorts = FALSE,
        runPheValuator = TRUE,
        exportResults = TRUE,
        minCellCount = 5)
