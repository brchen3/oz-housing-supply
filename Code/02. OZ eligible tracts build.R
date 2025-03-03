
# Sarah Eckhardt 01/03/2025

# crosswalk OZ eligibility from 2010 to 2020 census tract definitions;
# identify full or partial matches, and composition of tract eligibility qualifications.

# remove dependencies
rm(list = ls())

# packages
library(readxl)
library(dplyr)
library(tidyr)

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

path_data <- file.path(path_project, "Data")
path_data_USPS <- file.path(path_data, "HUD USPS Data Standardized") # not publicly available.
path_data_tract <- file.path(path_data, "Tract Characteristics")
path_data_crosswalks <- file.path(path_data, "HUD crosswalks")
path_output <- file.path(path_project, "Output")

  
  
#######################################################################
# read in census tract OZ eligibility criteria; for 2010 census tracts
setwd(path_data)

initial_data = read_excel("Master Census Tract File_2013 to 2017.xlsx",
                     sheet = "Data", skip=1) %>%
  select(GEOID:`OZ Designation`)

# What are the categories of eligibility? 

#            Low-Income Community Non-LIC Contiguous Not selected
# Contiguous                   32                168        10064
# Ineligible                   16                  1        31828
# LIC                        7608                  0        23314
# N/A                           1                  0           26


# create a single variable that defines tracts by eligibility and selection
  # selected tract
    # eligible yes-no (by what criteria)
  # non-selected tract
    # eligible yes-no (by what criteria)

# the goal is to subset analysis to only LIC eligible tracts

oz_data <- initial_data %>%
  ungroup() %>%
  mutate(Designation = case_when(
    # Selected
    `OZ Designation Criteria` == "Low-Income Community" ~ "LIC selected",
    `OZ Designation Criteria` == "Non-LIC Contiguous" ~ "Contiguous selected",
    
    # Unselected
    `OZ Designation Criteria` == "Not selected" & `Pre-Deisgnation 2011-2015 Eligibilty` == "LIC" ~ "LIC not selected",
    `OZ Designation Criteria` == "Not selected" & `Pre-Deisgnation 2011-2015 Eligibilty` == "Contiguous" ~ "Contiguous not selected",
    `OZ Designation Criteria` == "Not selected" & `Pre-Deisgnation 2011-2015 Eligibilty` == "Ineligible" ~ "Ineligible",
    TRUE ~ NA
  )) %>%
  select(c(GEOID,
           Designation)) %>%
  na.omit() %>%
    # transform
  mutate(GEOID = as.numeric(trimws(GEOID)))


table(oz_data$Designation)

# # how many ineligible tracts were selected???
# problem_tracts = initial_data %>% ungroup() %>%
#       filter( `Pre-Deisgnation 2011-2015 Eligibilty` == "Ineligible" & `OZ Designation Criteria` != "Not selected")
# 
# count(problem_tracts) # 17
    

###########################################################
# walk 2010 eligibility forward to 2020 tract definitions.
# crosswalk source: https://www.huduser.gov/apps/public/usps/home

setwd(path_data_crosswalks)

xwalk = read_excel("CENSUS_TRACT_CROSSWALK_2010_to_2020_2010.xlsx") %>%
  select(c(contains("GEOID"), TOT_RATIO)) %>%
  mutate(GEOID_2010 = as.numeric(GEOID_2010),
         GEOID_2020 = as.numeric(GEOID_2020))
  

#############################  
# merge and construct dataset

# define a helper function to generate descriptive mixed category names
dynamic_designation <- function(ineligible, lic_selected, lic_not_selected, contiguous_selected, contiguous_not_selected) {
  categories <- c(
    if(ineligible > 0) "Ineligible" else NULL,
    if(lic_selected > 0) "LIC selected" else NULL,
    if(lic_not_selected > 0) "LIC not selected" else NULL,
    if(contiguous_selected > 0) "Contiguous selected" else NULL,
    if(contiguous_not_selected > 0) "Contiguous not selected" else NULL
  )
  
  # Determine the output string based on number of applicable categories
  if(length(categories) == 0) {
    return("Other Mixed")
  } else if(length(categories) == 1) {
    return(categories[1])
  } else if(length(categories) == 2) {
    return(paste("Both", categories[1], "and", categories[2]))
  } else if(length(categories) == 3) {
    return(paste(categories[1], categories[2], "and", categories[3]))
  } else {
    # For more than three categories, list them with commas and an 'and' before the last one
    return(paste(paste(head(categories, -1), collapse = ", "), "and", tail(categories, 1)))
  }
}

matched = oz_data %>%
  left_join(xwalk, by = c("GEOID" = "GEOID_2010")) %>% na.omit() %>%
  
  pivot_wider(names_from = "Designation",
              values_from = "TOT_RATIO") %>%
  
  ungroup() %>% group_by(GEOID_2020) %>%
  
  # determine portions of the 2010 tract in the 2020 tract;
  # note that this should not add to 100%, used for an indicator.
  
  mutate(across(c("Ineligible", "LIC not selected", "Contiguous not selected",
                  "LIC selected", "Contiguous selected"),
                ~ sum(., na.rm = TRUE),
                .names = paste("{col}", "(%)"))) %>%
  
  # select only distinct 2020 tracts
  select(c(GEOID_2020, contains("(%)"))) %>%
  distinct() %>%
  
  
  # create indicators for the criteria met for 2010 tracts within the new 2020 tracts.
  
  mutate(
    Designation_category = case_when(
      
      ##############  Uniform Treatment Type
      
      # The tract is fully Ineligible -
      `LIC selected (%)` == 0 &
        `LIC not selected (%)` == 0 &
        `Contiguous not selected (%)` == 0 &
        `Contiguous selected (%)` == 0 
      ~ "Ineligible",
      
      # The tract is fully LIC and Selected -
      `Ineligible (%)` == 0 &
        `LIC not selected (%)` == 0 &
        `Contiguous not selected (%)` == 0 &
        `Contiguous selected (%)` == 0 
      ~ "LIC selected",
      
      # The tract is fully LIC and not selected - 
      `Ineligible (%)` == 0 &
        `LIC selected (%)` == 0 &
        `Contiguous not selected (%)` == 0 &
        `Contiguous selected (%)` == 0 
      ~ "LIC not selected",
      
      # The tract is fully Contiguous and selected - 
      `Ineligible (%)` == 0 &
        `LIC selected (%)` == 0 &
        `LIC not selected (%)` == 0 &
        `Contiguous not selected (%)` == 0
      ~ "Contiguous selected",
      
      # The tract is fully Contiguous and not selected - 
      `Ineligible (%)` == 0 &
        `LIC selected (%)` == 0 &
        `LIC not selected (%)` == 0 &
        `Contiguous selected (%)` == 0
      ~ "Contiguous not selected",
      
      ############## Mixed Treatment Type
      TRUE ~ dynamic_designation(
        `Ineligible (%)`, 
        `LIC selected (%)`, 
        `LIC not selected (%)`, 
        `Contiguous selected (%)`, 
        `Contiguous not selected (%)`
      )
      )
    )

# save output.
setwd(path_data_tract)
write.csv(matched, "2020_tracts_with_2010_qualifications.csv")
