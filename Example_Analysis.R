# Clear environment variable and console
rm(list = ls())
cat("\014")

# Installs useful packages
# Comment out after you have installed these packages (R will re-install them regardless)
# install.packages('tidyverse')
# install.packages('data.table')
# install.packages('lubridate')
# install.packages('ggplot2')

# Loads packages into environment
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)

# Loading data
# 1. Get file locations
file_paths <- list.files(
    './data/',
    pattern = '*.csv',
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
)

# 2. Read files into environment
# fread is a data.table function that performs faster than base R read.csv
# Helpful when reading large data sets
PerfTreatment <- fread(file_paths[1])
WellHeader <- fread(file_paths[3])
WellProduction <- fread(file_paths[4])

# ====================================================================================================================

# Viewing an object's structure
# Analogous to expanding the variable in the environment pane
str(WellHeader)
str(PerfTreatment)
str(WellProduction)

# Alternatively:
# summary provides a basic statistical summary by column
summary(WellHeader)
summary(PerfTreatment)
summary(WellProduction)

# Specific to RStudio
# Launches a spreadsheet style viewer with familiar functionality like filter, etc.
View(WellHeader)
View(PerfTreatment)
View(WellProduction)


# ====================================================================================================================
# Basic data clean up
# csv files load with what appears to be a row number, not needed 
# Dropping a column from a data.table
PerfTreatment[, V1 := NULL]
WellHeader[, V1 := NULL]
WellProduction[, V1 := NULL]

# Very basic plotting to explore some of the data fields
PerfTreatment$ActivityDate <- as.POSIXct(PerfTreatment$ActivityDate)
plot(x = PerfTreatment$ActivityDate, y = PerfTreatment$IntervalBase)


# ====================================================================================================================
# Match for perf treatment
WellHeader[, PerfTreatmentMatch := ifelse(EPAssetsId %in% PerfTreatment$EPAssetsId, TRUE, FALSE)]

# Do we have any wells that do not have production data?
unique(WellHeader$PerfTreatmentMatch)

# If so, how many?
length(which(WellHeader$PerfTreatmentMatch))

# Match for well production
WellHeader[, WellProductionMatch := EPAssetsId %in% WellProduction$EPAssetsId]

# Do we have any wells that do not have production data?
unique(WellHeader$WellProductionMatch)

# If so, how many?
length(which(WellHeader$WellProductionMatch))


# ====================================================================================================================
# Let's dive a little deeper into the production data
# Starting with some data transformations to make the next steps easier...
# Pivot data
WellProduction_Pivot <- dcast(WellProduction, EPAssetsId + ProdPeriod ~ ProdType, value.var = 'Volume')

# Convert all NA values to 0 for simplified math
WellProduction_Pivot[is.na(WellProduction_Pivot)] <- 0

# Create some calculated fields, namely BOE and VOLUME_SUM
WellProduction_Pivot[, BOE := `Condensate Production (Bbls)` + (`Gas Production (MMcf)` * 1000 / 6) + `Oil Production (Bbls)`]
WellProduction_Pivot[, VOLUME_SUM := `Condensate Production (Bbls)` + (`Gas Production (MMcf)` * 1000 / 6) + `Oil Production (Bbls)` + `Water Production (Bbls)`]

# Determine the min/max prod month by well and assign to columns
WellProduction_Pivot[, ProdPeriodMin := min(ProdPeriod, na.rm = T), by = 'EPAssetsId']
WellProduction_Pivot[, ProdPeriodMax := max(ProdPeriod, na.rm = T), by = 'EPAssetsId']

# Creating a more feature for a less strict DUC definition
# The idea here is to capture wells that have likely been tested, but are not producing through permanent facilities
# Logic:
# - lifetime production was produced in it's first month producing
# - do not include if that is the case, but for the last month in data set, possible/likely the well will continue to produce
# 1. Calculate lifetime BOE sum
WellProduction_Pivot[, BOE_SUM := sum(BOE), by = 'EPAssetsId']

# 2. Invoke logic to new feature
WellProduction_Pivot[, FirstMonthOnly := ifelse(ProdPeriod==ProdPeriodMin & BOE == BOE_SUM & ProdPeriodMin != max(ProdPeriod), TRUE, FALSE)]

# Cumulative sum of producing hours
WellProduction_Pivot[, NormalizedProducingTime := cumsum(`Production Hours`), by = 'EPAssetsId']

ggplot(WellProduction_Pivot) +
    aes(x = NormalizedProducingTime, y = BOE, colour = EPAssetsId, group = EPAssetsId) +
    geom_line(size = 0.5) +
    labs(x = "Normalized Producing Hours", y = "Barrels of Oil Equivalent", title = "BOE over Normalized Producing Hours", subtitle = "Well Production Data") +
    theme_minimal()

# Join new feature into well header, similar to above
WellHeader[, FirstMonthOnly := ifelse(EPAssetsId %in% WellProduction_Pivot$EPAssetsId[which(WellProduction_Pivot$FirstMonthOnly)], TRUE, FALSE)]

# ====================================================================================================================
# Create a logic function to classify wells into DUC/noDUC
doesItQuack <- function(perf, prod, month) {
    if (!perf  && !prod) {
        # no completion and no prod, classic definition
        return('traditional duc')
    } else if (perf && !prod) {
        return('completed but not producing')
    } else if (month) {
        return('tested but not producing')
    } else {
        return("doesn't quack")
    }
}

# Apply our new logic function to our data set
WellHeader[, quack := doesItQuack(PerfTreatmentMatch, WellProductionMatch, FirstMonthOnly), by = 'EPAssetsId']

# Summary counts by class
table(WellHeader$quack)

