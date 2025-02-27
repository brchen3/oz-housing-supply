# HUD Aggregated USPS Administrative Data on Vacancies - 2020 standardized version
# final dataset construction step.

# Ben Glasner 01/03/2025

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################

library(dplyr)
library(tidyr)
library(readxl)
library(foreign)
library(janitor)  # Ensure janitor is loaded
library(plm)
library(ggplot2)
library(tigris)
library(stringr)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO"
  )

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]

path_data <- file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/2020 Standardized")
path_data_tract <- file.path(path_project, "data/Tract Characteristics")
path_data_crosswalks <- file.path(path_data, "HUD crosswalks")
path_data_lodes <- file.path(path_project, "data/LODES") # Longitudinal Employer-Household Dynamics https://lehd.ces.census.gov/

path_output <- file.path(path_project, "output")


#################
### Data load ###
#################
# tract - crosswalk

CENSUS_TRACT_CROSSWALK <- read_excel(file.path(path_data_crosswalks, "CENSUS_TRACT_CROSSWALK_2010_to_2020_2010.xlsx"))

# time invariant characteristics
NCWM_Economic_Development2 <- read_excel(file.path(path_data, "NCWM_Economic_Development2.xlsx")) %>%
  mutate(geoid = as.numeric(GEOID))

#####################
### Load HUD data ###
#####################

setwd(path_data_USPS)
data_list <- list()

# List all .xlsx files
excel_files <- list.files(path = path_data_USPS, pattern = "\\.xlsx$", full.names = TRUE)

# Read each Excel file into a list of data frames
data_list <- lapply(excel_files, read_excel)

# Combine all data frames into one using bind_rows
USPS_data <- bind_rows(data_list)

USPS_data <- USPS_data %>%
  mutate(date = as.Date(paste0(YEAR, "/", MONTH, "/01"), format = "%Y/%m/%d"))

# simplify the variable list:
USPS_data <- USPS_data %>%
  select(
    TRACT20:YEAR_MONTH,date,
    TOTAL_RESIDENTIAL_ADDRESSES:NO_STAT_OTHER_ADDRESSES
    )

# Ensure that only tracts in 50 states and DC are included in the analysis - 
USPS_data <- USPS_data %>%
  mutate(state_fips = as.numeric(str_sub(TRACT20,start = 1,end = 2))) %>%
  filter(state_fips <=56)


#################################
# Merge in Tract Characteristics
# There were 84,414 census tracts in the United States in 2022
# There were 74,134 census tracts in the United States and its territories in 2010.

setwd(path_data_tract)

# see 2. OZ eligible tracts build.R
tract_designation <- read.csv("2020_tracts_with_2010_qualifications.csv") %>%
  mutate(geoid = as.numeric(GEOID_2020)) %>% 
  select(-X) %>%
  select(-GEOID_2020)
  
tract_designation_geoid <- sort(unique(tract_designation$geoid))

# National Center for Education Statistics
NCES_Locales_Tract_2020 <- readr::read_csv("NCES Locales Tract CSV.csv") %>%
  rename(geoid = FIPS) %>%
  select(geoid, `Type tract`) 


# For the ACS data, crosswalk using the residential ratio from the HUD data on
# addresses since the variables are at the person level
load("ACS_2012_2023_2020tracts.RData")

ACS_2012_2023 <- ACS_2012_2023 %>% rename(YEAR = year) %>% mutate(YEAR = as.character(YEAR))
openxlsx::write.xlsx(ACS_2012_2023, file = "ACS_2012_2023_2020tracts.xlsx") # save as xlsx.


# tract neighboring info
OZ_Adjc_Trcts <- read.csv("OZ_Adjc_Trcts.csv")
OZ_Adjc_Trcts <- OZ_Adjc_Trcts %>%
  mutate(geoid = as.numeric(Adj_FIPS)) %>%
  select(geoid, OZ_FIPS) %>%
  rename(neighbor_oz = OZ_FIPS)

# Load the LODES data 
# see 3. LODES data build.R

setwd(path_data_lodes)
LODES  <- readr::read_csv("tract_workers_and_residents.csv") %>%
  filter(year >=2012) %>%
  mutate(YEAR = as.character(year)) %>%
  rename(geoid = tract_geocode) %>%
  select(geoid, YEAR, jobs_in_tract, employed_tract_residents)



#########################################
# time invariant tract characteristics

time_inv <- NCWM_Economic_Development2 %>%
  left_join(tract_designation) %>%
  mutate(
    
  Sample = case_when(
    Designation_category %in% c("Ineligible",
                                "LIC selected",
                                "LIC not selected",
                                "Contiguous selected",
                                "Contiguous not selected") 
    ~ "In Clean Sample",
    TRUE ~ "In Mixed Sample"
  ),
    `OZ Designation` = case_when(
      Designation_category %in% c("LIC selected",
                                  "Contiguous selected") 
      ~ 1,
      TRUE ~ 0
    )
  
  ) %>%
  left_join(NCES_Locales_Tract_2020) %>%
  select(geoid,
         QCT,NMTC,`OZ Designation`,OZ_2020,
         `Designation_category`,Sample,
         `Type tract`) %>%
  left_join(OZ_Adjc_Trcts) %>%
  mutate(next_to_oz = case_when(
    is.na(neighbor_oz) ~ 0,
    !is.na(neighbor_oz) ~ 1,
    TRUE ~ NA
  ))

time_inv <- time_inv %>%
  distinct(geoid, .keep_all = TRUE)


setwd(path_data)
save(time_inv, file = "Tract_2020_time_invariant_characteristics.RData")



# Step 1: Summarize and pivot the data
pivot_tbl <- time_inv %>%
  filter(Sample == "In Clean Sample") %>%
  group_by(Designation_category, OZ_2020) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = OZ_2020,
    values_from = count,
    values_fill = list(count = 0)
  )

# Step 2: Add a totals column by summing across each row
pivot_tbl <- pivot_tbl %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Step 3: Create a summary row that sums each numeric column
total_row <- pivot_tbl %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Designation_category = "Total")

# Step 4: Append the summary row to the pivot table
final_table <- bind_rows(pivot_tbl, total_row)

final_table

# Step 1: Summarize and pivot the data
pivot_tbl <- time_inv %>%
  filter(Sample == "In Clean Sample") %>%
  group_by(Designation_category, `OZ Designation`) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = `OZ Designation`,
    values_from = count,
    values_fill = list(count = 0)
  )

# Step 2: Add a totals column by summing across each row
pivot_tbl <- pivot_tbl %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Step 3: Create a summary row that sums each numeric column
total_row <- pivot_tbl %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Designation_category = "Total")

# Step 4: Append the summary row to the pivot table
final_table <- bind_rows(pivot_tbl, total_row)

final_table

# From the Neighborhood Change Web Map:
#   
# Most Qualified Opportunity Zones (84.8 percent) were able to be matched exactly between 
# the 2010 and 2020 census tract boundaries. Some Qualified Opportunity Zone census tracts 
# were split (12.6 percent) or merged (2.6 percent) with other census tracts. 
# The Neighborhood Change Web Map can map one-to-one matches, splits, and most merges. 
# However, a small number of merged census tracts are not shown as Approximate Qualified 
# Opportunity Zones, although a portion of the 2020 census tract contains a 2010 census 
# tract that is a Qualified Opportunity Zone. Merged Approximate Qualified Opportunity Zones 
# shown are 2020 census tracts that had at least half of their residential addresses in the 2010 
# census tract that was a Qualified Opportunity Zone.

# For this pass we are only going to use OZ_2020 defined as "OK"
  

###########
USPS_data <- USPS_data %>%
  mutate(geoid = as.numeric(as.character(TRACT20))) %>%
  distinct(geoid, date, .keep_all = TRUE) %>%
  group_by(geoid) %>%
  mutate(number_of_quarters = n()) %>%
  ungroup()

max_quarters <- max(USPS_data$number_of_quarters)

USPS_data <- USPS_data %>%
  filter(number_of_quarters == max_quarters)

USPS_data <- USPS_data %>% 
  left_join(time_inv) %>%
  left_join(ACS_2012_2023) %>%
  left_join(LODES)

USPS_data <- USPS_data %>%
  mutate(Designation_category_detailed = case_when(
    Designation_category == "Contiguous not selected" & next_to_oz == 1 ~ "Contiguous not selected, border tract",
    Designation_category == "Contiguous not selected" & next_to_oz == 0 ~ "Contiguous not selected, not a border tract",
    
    Designation_category == "Ineligible" & next_to_oz == 1 ~ "Ineligible, border tract",
    Designation_category == "Ineligible" & next_to_oz == 0 ~ "Ineligible, not a border tract",
    
    Designation_category == "LIC not selected" & next_to_oz == 1 ~ "LIC not selected, border tract",
    Designation_category == "LIC not selected" & next_to_oz == 0 ~ "LIC not selected, not a border tract",
    
    Designation_category == "LIC selected" ~ "LIC selected",
    Designation_category == "Contiguous selected" ~ "Contiguous selected",
    
    TRUE ~ NA
  ))

# After all the cleaning counts are:
USPS_data %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR_MONTH == "2020-03") %>% 
  group_by(`Designation_category`, `OZ Designation`) %>%
  summarise(count = n()) %>%
  ungroup() 

USPS_data %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR_MONTH == "2020-03") %>% 
  group_by(`Designation_category_detailed`, `OZ Designation`) %>%
  summarise(count = n()) %>%
  ungroup() 


########
# Export
setwd(path_data)
save(USPS_data, file = "USPS_tract_vacancy_2012_2024_2020_definitions.RData")

