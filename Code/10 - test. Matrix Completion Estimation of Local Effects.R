# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 1/15/2025
# Testing code

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
# devtools::install_github("xuyiqing/fect")
library(fect) # https://yiqingxu.org/packages/fect/01-start.html

library(openxlsx)
library(tidyr)
library(dplyr)
library(panelView)
library(ggplot2)

library(broom)
library(lmtest)     # For robust standard errors
library(sandwich)   # For clustered standard errors
library(fixest)

library(modelsummary)
library(gt)
library(webshot2)

library(purrr)
library(progress)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/2010 Census Tract Summary Files")
path_data_tract <- file.path(path_project, "data/Tract Characteristics")
path_output <- file.path(path_project, "output")

#################
### Data load ###
#################
setwd(path_data)
load(file = "USPS_tract_vacancy_2012_2024_2020_definitions.RData")

##################
### Data clean ###
##################

### Clean up uncertain tracts from the crosswalk
USPS_data <- USPS_data %>%
  filter(`Designation_category` %in% c("LIC selected","LIC not selected")) %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR >= 2014) %>%
  filter(YEAR < 2024) %>%
  filter(MONTH == "09") %>% 
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date >= "2020-03-01", 1, 0),
    Total_active = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES +
      ACTIVE_BUSINESS_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +
      ACTIVE_OTHER_ADDRESSES + STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES, 
    log_Total_active = log(Total_active),
    `current median income decile` = ntile(median_income, 10),
    `current poverty rate decile` = ntile(poverty_rate, 10),
    `current solo detached decile` = ntile(solo_detached_housing_share, 10)
  ) %>%
  select(id, time, date, state_fips,
         `Type tract`, Designation, `Designation_category`,
         poverty_rate, median_income, unemployment_rate, prime_age_share, solo_detached_housing_share,
         `current median income decile`, `current poverty rate decile`, `current solo detached decile`,
         Total_active, log_Total_active) %>%
  na.omit() %>%
  mutate(type_tract = as.numeric(as.factor("Type tract")))

# Ensure panel is balanced after removing missing data periods
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)
USPS_data <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)

###############################
### Counterfactual Analysis Function ###
###############################

# Setup formula
outcome_var   <- "Total_active"
treatment_var <- "Designation"
# Uncomment the next line if you wish to include controls
# controls      <- c("current median income decile", "current poverty rate decile", "current solo detached decile")
# control_vars  <- paste(controls, collapse = " + ")
current_formula <- as.formula(paste(outcome_var, "~", treatment_var))
# current_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", control_vars))

# Helper function to run fect, save, print, and plot results
run_and_plot <- function(method, selected_data) {
  # Set common fect parameters
  args <- list(
    formula = current_formula,
    data = selected_data,
    na.rm = TRUE,
    index  = c("id", "time"),
    force = "two-way",
    r = c(0, 5),
    nlambda = 5,
    CV = TRUE,
    k = 10,
    cv.prop = 0.05,
    cv.treat = FALSE,
    cv.nobs = 3,
    cv.donut = 0,
    criterion = "mspe",
    method = method,
    se = TRUE,
    quantile.CI = FALSE,
    nboots = 100,
    alpha = 0.05,
    parallel = TRUE,
    cores = 6,
    max.iteration = 1000,
    seed = 42,
    min.T0 = 5,
    max.missing = 0,
    proportion = 0.3,
    f.threshold = 0.5,
    degree = 2,
    sfe = c("type_tract"),
    cfe = list(c("type_tract", "time"), c("state_fips", "time")),
    fill.missing = FALSE,
    placeboTest = FALSE,
    carryoverTest = FALSE,
    loo = FALSE,
    permute = FALSE,
    m = 2,
    normalize = TRUE
  )
  
  out <- do.call(fect, args)
  invisible(out)
}

#####################################
### Create Treated & Control Sets ###
#####################################

# Get list of treated unit IDs
treated_units <- USPS_data %>%
  filter(Designation_category == "LIC selected") %>%
  distinct(id)

# Get full set of control unit IDs (all "LIC not selected")
control_ids <- USPS_data %>%
  filter(Designation_category == "LIC not selected") %>%
  distinct(id) %>%
  pull(id)

# Initialize list to store effect estimates
estimate_list <- list()

# Set up progress bar over treated units
n_treated <- nrow(treated_units)
pb <- progress_bar$new(
  format = "Treated unit :current/:total [:bar] :elapsed | ETA: :eta",
  total = n_treated,
  width = 60
)

###########################################
### Loop Over Each Treated Unit Individually ###
###########################################

for (i in seq_len(n_treated)) {
  current_treated_id <- treated_units$id[i]
  
  # Create temporary dataset: current treated unit + full control pool
  temp_data <- USPS_data %>%
    filter(id == current_treated_id | id %in% control_ids)
  
  # Run matrix completion estimation (using method = "mc")
  out_temp <- run_and_plot(method = "fe", selected_data = temp_data)
  
  # Extract effect estimates for the treated unit
  Effect <- out_temp[["eff"]]
  # Transpose and convert to data frame
  Effect <- t(Effect)
  Effect <- as.data.frame(Effect)
  colnames(Effect) <- paste("Period:", seq_len(ncol(Effect)))
  Effect <- cbind(id = current_treated_id, Effect)
  
  # Reshape into long format and compute confidence bounds
  effect_long <- Effect %>%
    pivot_longer(
      cols = starts_with("Period:"),
      names_to = "Period",
      values_to = "Effect on Total Addresses"
    ) %>%
    mutate(`Average S.E.` = out_temp[["est.avg"]][[2]],
           Upper = `Effect on Total Addresses` + `Average S.E.` * 1.96,
           Lower = `Effect on Total Addresses` - `Average S.E.` * 1.96,
           Significant = case_when(
             (Lower > 0 & Upper > 0) | (Lower < 0 & Upper < 0) ~ 1,
             (pmin(Lower, Upper) <= 0 & pmax(Lower, Upper) >= 0) ~ 0,
             TRUE ~ NA_real_
           ))
  
  estimate_list[[i]] <- effect_long
  
  pb$tick()  # update progress bar
  
  # Remove temporary dataset to free memory
  rm(temp_data)
  gc()
}

# Combine all effect estimates into one data frame
All_estimates <- bind_rows(estimate_list)

###########################################
### (Optional) Further Analysis & Export ###
###########################################

# Save the effect estimates and estimation outputs as needed
setwd(path_output)
save(All_estimates, file = "MC_FECT_Effect_Estimates_and_SE.RData")
# Optionally, you might want to also save the individual model outputs if needed.

# You can now proceed with plotting or additional analysis on All_estimates
# For example, plotting the average effect for a given period:
current_estimate <- All_estimates %>%
  filter(Period == "Period: 10") %>%
  summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  pull(Average_Effect)

print(paste("Average Effect on Total Addresses in Period 10:", round(current_estimate, 4)))
