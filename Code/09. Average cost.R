# HUD Aggregated USPS Administrative Data on Vacancies - 2020 standardized version
# Ben Glasner 01/03/2025

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(openxlsx)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/oz-housing-supply"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]

path_data = file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/HUD USPS Data Standardized")
path_data_tract = file.path(path_project, "data/Tract Characteristics")

path_output = file.path(path_project, "output")

# Additional calculations based on the provided text
avg_effect <- 35.66         # average new addresses per OZ tract
# avg_effect <- 32.68         # average new addresses per OZ tract after excluding bordering tracts
oz_tracts <- 8764           # total number of OZ tracts

# avg_crowd_out <- 7.82
# border_tracts <- 
  
oz_new_estimated <- avg_effect * oz_tracts  # estimated new addresses due to OZs
cost <- 8200000000
cost_character <- "8.2 billion"

# Calculate percentages relative to different groups
pct_of_OZ_total    <- round((oz_new_estimated / OZ_res_change$change) * 100, 2)
pct_of_LIC_total   <- round((oz_new_estimated / LIC_res_change$change) * 100, 2)
pct_of_national    <- round((oz_new_estimated / national_res_change$change) * 100, 2)

# Compose the additional summary statement
additional_statement <- paste0(
  "If the average effect of OZ's was ", avg_effect, 
  " new active/vacant addresses per tract, and we have ", 
  prettyNum(oz_tracts, big.mark = ","), " OZ tracts, that translates to ", 
  prettyNum(round(oz_new_estimated), big.mark = ","), 
  " new addresses caused by OZs, ", pct_of_OZ_total, 
  "% of all new residential active/vacant addresses since 2019 in OZs, ", 
  pct_of_LIC_total, "% among LICs regardless of designation, and ", pct_of_national, 
  "% of all new active/vacant addresses in the country as a whole."
)

cost_statement <- paste0(
  "At a projected cost of $",
  cost_character,
  ", the average cost per unit caused by OZ's is $",
  prettyNum(round(cost/oz_new_estimated), big.mark = ","),
  "."
)

# Print the additional summary
print(additional_statement)
print(cost_statement)
