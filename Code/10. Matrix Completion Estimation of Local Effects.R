# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 1/15/2025

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
library(lmtest) # For robust standard errors
library(sandwich) # For clustered standard errors
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
    "name" = "PATH TO GITHUB REPO"
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
### If a tract in 2020 is a mix of any version of the LIC, contiguous or inelligible, then drop it from the analysis - conservative first pass

USPS_data <- USPS_data %>%
  filter(`Designation_category` %in% c("LIC selected","LIC not selected")) %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR >=2014) %>%
  filter(YEAR <2024) %>%
  filter(MONTH == "09") %>% 
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date>="2020-03-01", 1, 0),
    Total_active = 
      ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES +
      ACTIVE_BUSINESS_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +
      ACTIVE_OTHER_ADDRESSES + STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES, 
    log_Total_active = log(Total_active),
    `current median income decile` = ntile(median_income, 10),
    `current poverty rate decile` = ntile(poverty_rate, 10),
    `current solo detached decile` = ntile(solo_detached_housing_share, 10)
  ) %>%
  select(id,time,date,state_fips,
         `Type tract`, Designation,`Designation_category`,
         poverty_rate, median_income,unemployment_rate, prime_age_share,solo_detached_housing_share,
         `current median income decile`,`current poverty rate decile`,`current solo detached decile`,
         Total_active,log_Total_active) %>%
  na.omit() %>%
  mutate(type_tract = as.numeric(as.factor("Type tract")))

# table(USPS_data$time)

#####
# ensure the panel is balanced after removing missing data periods
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)

USPS_data <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)

########################################
### Counterfactual Analysis Function ###
########################################

# Multifamily share
# Housing elasticity 

# Setup formula
outcome_var   <- "Total_active"
# outcome_var   <- "log_Total_active"
treatment_var <- "Designation"
# controls      <- c("poverty_rate", "median_income","solo_detached_housing_share")
controls      <- c("current median income decile","current poverty rate decile","current solo detached decile")
control_vars  <- paste(controls, collapse = " + ")
current_formula <- as.formula(paste(outcome_var, "~", treatment_var))
# current_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", control_vars))

# Helper function to run fect, save, print, and plot results
run_and_plot <- function(method,selected_data) {
  
  
  # Common fect parameters
  args <- list(
    formula = current_formula,
    data = selected_data,
    # group = NULL,
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
    # criterion = "moment",
    criterion = "mspe",
    method = method,
    # se = FALSE,
    se = TRUE,
    # vartype = "bootstrap",
    # vartype = "parametric",
    # vartype = "jackknife",
    # cl = NULL,
    quantile.CI = FALSE,
    nboots = 25,
    alpha = 0.05,
    parallel = TRUE,
    cores = 6,
    # tol = 1e-5,
    max.iteration = 1000,
    seed = 42,
    min.T0 = 5,
    max.missing = 0,
    proportion = 0.3,
    # pre.periods = NULL,
    f.threshold = 0.5,
    # tost.threshold = NULL,
    # knots = NULL,
    degree = 2,
    sfe = c("type_tract"),
    cfe = list(
      c("type_tract","time")
      , c("state_fips","time")
    ),
    # balance.period = NULL,
    fill.missing = FALSE,
    placeboTest = FALSE,
    # placebo.period = NULL,
    carryoverTest = FALSE,
    # carryover.period = NULL,
    # carryover.rm = NULL,
    loo = FALSE,
    permute = FALSE,
    m = 2,
    normalize = TRUE
  )
  
  # Execute fect call with specified arguments
  out <- do.call(fect, args)
  
  invisible(out)
}

###############################
### Sample Construction     ###
###############################

## Data subsets by random treated subgroups to lower the computational load

# Step 1: Define treated and control geoids
treated_units <- USPS_data %>%
  filter(Designation_category == "LIC selected") %>%
  distinct(id)

control_pool <- USPS_data %>%
  mutate(
    `solo detached quintile` = ntile(solo_detached_housing_share, 5)
  ) %>%
  filter(Designation_category == "LIC not selected") %>%
  distinct(id, `Type tract`,`solo detached quintile`)

# Step 2: Determine the smallest group size across Type tract groups
min_control_group_size <- control_pool %>%
  # group_by(`Type tract`,`solo detached quartile`) %>%
  group_by(`solo detached quintile`) %>%
  summarize(count = n(), .groups = "drop") %>%
  summarize(min_size = min(count)) %>%
  pull(min_size)

# Determine how many control geoids to sample from each group
# samples_per_tract_group <- min(min_control_group_size, 100)
samples_per_tract_group <- min(min_control_group_size, 10000)

# Step 3: Create subsamples of treated units for manageable batch sizes
treated_units_grouped <- treated_units %>%
  ungroup() %>%
  sample_n(n()) %>%    # Randomly shuffle rows
  # mutate(treated_batch = ceiling(row_number() / 25))  # Assign a batch number
mutate(treated_batch = ceiling(row_number() / 10))  # Assign a batch number

treated_batch_names <- sample(unique(treated_units_grouped$treated_batch))

# Initialize list to store data batches
data_batches <- list()
estimate_list <- list()

# Initialize progress bar
pb <- progress_bar$new(
  format = "Batch :current/:total [:bar] :elapsed | Time per batch: :eta",
  total = length(treated_batch_names),
  width = 60
)

# Step 4: Iterate over treated batches and sample a new control group for each batch
for (i in seq_along(treated_batch_names)) {
  # Select treated units for this batch
  treated_ids_for_batch <- treated_units_grouped$id[treated_units_grouped$treated_batch == treated_batch_names[[i]]]
  
  # Sample a new set of control geoids for this batch (randomized every iteration)
  selected_control_geoids <- control_pool %>%
    # group_by(`Type tract`,`solo detached quartile`) %>%
    group_by(`solo detached quintile`) %>%
    sample_n(samples_per_tract_group, replace = FALSE) %>%  # Allow replacement
    ungroup() %>%
    select(id)
  
  # Create dataset for this batch with its unique set of controls
  data_batches[[i]] <- USPS_data %>%
    filter(id %in% treated_ids_for_batch | id %in% selected_control_geoids$id)
  
  pb$tick()  # Update progress bar
  
}

rm(USPS_data)

gc()

# Initialize progress bar
pb <- progress_bar$new(
  format = "Batch :current/:total [:bar] :elapsed | Time per batch: :eta",
  total = length(data_batches),
  width = 60
)

# Place to store the average effect estimates
average_effects <- numeric()

out.output <- list()

for (i in seq_along(data_batches)) {

  start_time <- Sys.time()  # Start timing
  # Run estimation method
  out.output[[i]]  <- run_and_plot(method = "fe", selected_data = data_batches[[i]])
  # out.output[[i]]  <- run_and_plot(method = "mc", selected_data = data_batches[[i]])
  # out.output[[i]]  <- run_and_plot(method = "ife", selected_data = data_batches[[i]])
  
  Effect <- out.output[[i]][["eff"]]
  
  # Transpose the data frame
  Effect <- t(Effect)
  # Convert the transposed matrix back to a data frame
  Effect <- as.data.frame(Effect)
  # Rename the new columns to "Period: X"
  colnames(Effect) <- paste("Period:", seq_len(ncol(Effect)))
  # Define the GEOID column
  Effect <- cbind(id = rownames(Effect), Effect)
  rownames(Effect) <- NULL
  
  estimate_list[[i]] <- Effect %>%
    pivot_longer(
      cols = starts_with("Period:"),
      names_to = "Period",
      values_to = "Effect on Total Addresses"
    ) %>%
    mutate(`Average S.E.` = out.output[[i]][["est.avg"]][[2]],
           Upper = `Effect on Total Addresses` + `Average S.E.`*1.96,
           Lower = `Effect on Total Addresses` - `Average S.E.`*1.96,
           `Significant` = case_when(
             (Lower > 0 & Upper > 0) | (Lower < 0 & Upper < 0) ~ 1,  # CI does NOT contain zero (significant)
             (pmin(Lower, Upper) <= 0 & pmax(Lower, Upper) >= 0) ~ 0, # CI contains zero (not significant)
             TRUE ~ NA_real_  # Handle unexpected cases explicitly
           )
    ) %>%
    filter(id %in% treated_units_grouped$id) 
  
  
  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed_time <- round(difftime(end_time, start_time, units = "secs"), 2)
  print(paste("Batch", treated_batch_names[[i]], "completed in", elapsed_time, "seconds"))
  
  # Compute the average effect estimate for Period 10
  current_estimate <- bind_rows(estimate_list)  %>%
    filter(Period == "Period: 10") %>%
    summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
    pull(Average_Effect)
  
  # Store the estimate
  average_effects <- c(average_effects, current_estimate)
  
  # Create a dataframe for plotting
  plot_data <- data.frame(Iteration = seq_along(average_effects), 
                          Average_Effect = average_effects)
  
  # Generate the plot
  p <- ggplot(plot_data, aes(x = Iteration, y = Average_Effect)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = current_estimate, color = "red") +
    geom_line() +
    geom_point() +
    labs(title = "Average Effect on Total Addresses Over Iterations",
         x = "Iteration (Number of Loops)",
         y = "Average Effect Estimate") +
    theme_minimal()

  grid::grid.newpage()
  dev.off()
  print(p)
  
  print(paste("Current average Effect on Total Addresses in final period:", round(current_estimate, 4)))
  
  pb$tick()  # Update progress bar

  
  gc()
}

rm(data_batches)
gc()

All_estimates <- bind_rows(estimate_list) 

setwd(path_output)
save(out.output, file = "FE FECT Estiamted Models.RData")
save(All_estimates, file = "FE FECT Effect Estimates and SE.RData")
# load(file = "FE FECT Estiamted Models.RData")
# load(file = "FE FECT Effect Estimates and SE.RData")

# save(out.output, file = "IFE FECT Estiamted Models.RData")
# save(All_estimates, file = "IFE FECT FECT Effect Estimates and SE.RData")
# load(file = "IFE FECT Estiamted Models.RData")
# load(file = "IFE FECT Effect Estimates and SE.RData")

# save(out.output, file = "MC FECT Estiamted Models.RData")
# save(All_estimates, file = "MC FECT FECT Effect Estimates and SE.RData")
# load(file = "MC FECT Estiamted Models.RData")
# load(file = "MC FECT Effect Estimates and SE.RData")

setwd(path_data)
load(file = "USPS_tract_vacancy_2012_2024_2020_definitions.RData")

Full_sample <- USPS_data %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR >=2014) %>%
  filter(YEAR <2024) %>%
  filter(MONTH == "09") %>% 
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date>="2020-03-01", 1, 0),
    Total_active = 
      ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES +
      ACTIVE_BUSINESS_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +
      ACTIVE_OTHER_ADDRESSES + STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES, 
    log_Total_active = log(Total_active)
  ) %>%
  select(id,time,date,state_fips,
         `Type tract`, Designation,`Designation_category`,
         poverty_rate, median_income,unemployment_rate, prime_age_share,solo_detached_housing_share,
         Total_active,log_Total_active) %>%
  na.omit() %>%
  mutate(type_tract = as.numeric(as.factor("Type tract")))


Deciles <- Full_sample %>%
  filter(date == "2016-09-01") %>%
  mutate(
    `Poverty Rate Decile, All Tracts` = ntile(poverty_rate, 10)
    , `Median Income Decile, All Tracts` = ntile(median_income, 10)
    , `Unemployment Rate Decile, All Tracts` = ntile(unemployment_rate, 10)
    , `Prime-age Share Decile, All Tracts` = ntile(prime_age_share, 10)
    , `Solo Detached Housing Decile, All Tracts` = ntile(solo_detached_housing_share, 10)
  ) %>%
  select(
    id, 
    `Poverty Rate Decile, All Tracts`,
    `Median Income Decile, All Tracts`,
    `Unemployment Rate Decile, All Tracts`,
    `Prime-age Share Decile, All Tracts`,
    `Solo Detached Housing Decile, All Tracts`
  ) 

# Calculate deciles for median_income
median_income_deciles <- Full_sample %>%
  filter(date == "2016-09-01") %>%
  summarize(across(median_income, list(
    p10 = ~quantile(.x, 0.1),
    p20 = ~quantile(.x, 0.2),
    p30 = ~quantile(.x, 0.3),
    p40 = ~quantile(.x, 0.4),
    p50 = ~quantile(.x, 0.5),
    p60 = ~quantile(.x, 0.6),
    p70 = ~quantile(.x, 0.7),
    p80 = ~quantile(.x, 0.8),
    p90 = ~quantile(.x, 0.9)
  ))) %>%
  pivot_longer(everything(), names_to = "decile", values_to = "value")

# Calculate deciles for median_income
poverty_deciles <- Full_sample %>%
  filter(date == "2016-09-01") %>%
  summarize(across(poverty_rate, list(
    p10 = ~quantile(.x, 0.1),
    p20 = ~quantile(.x, 0.2),
    p30 = ~quantile(.x, 0.3),
    p40 = ~quantile(.x, 0.4),
    p50 = ~quantile(.x, 0.5),
    p60 = ~quantile(.x, 0.6),
    p70 = ~quantile(.x, 0.7),
    p80 = ~quantile(.x, 0.8),
    p90 = ~quantile(.x, 0.9)
  ))) %>%
  pivot_longer(everything(), names_to = "decile", values_to = "value")

poverty_medincome_pretreatment <- Full_sample %>%
  filter(date == "2016-09-01") %>%
  select(id, poverty_rate, median_income) %>%
  rename(poverty_rate_2016 = poverty_rate,
         median_income_2016 = median_income)


rm(Full_sample)

USPS_data <- USPS_data %>%
  filter(`Designation_category` %in% c("LIC selected","LIC not selected")) %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR >=2014) %>%
  filter(YEAR <2024) %>%
  filter(MONTH == "09") %>% 
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date>="2020-03-01", 1, 0),
    Total_active = 
      ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES +
      ACTIVE_BUSINESS_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +
      ACTIVE_OTHER_ADDRESSES + STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES, 
    log_Total_active = log(Total_active)
  ) %>%
  select(id,time,date,state_fips,
         `Type tract`, Designation,`Designation_category`,
         poverty_rate, median_income,unemployment_rate, prime_age_share,solo_detached_housing_share,
         Total_active,log_Total_active) %>%
  na.omit() %>%
  mutate(type_tract = as.numeric(as.factor("Type tract")))

#####
# ensure the panel is balanced after removing missing data periods
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)

USPS_data <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)

Dates_list <- sort(unique(USPS_data$date))

All_estimates %>%
  mutate(`Significance Category` = case_when(
    Significant == 1 & `Effect on Total Addresses`>0 ~ "Significant Increase",
    Significant == 1 & `Effect on Total Addresses`<0 ~ "Significant Decrease",
    Significant == 0 & `Effect on Total Addresses`>0 ~ "Insignificant Increase",
    Significant == 0 & `Effect on Total Addresses`<0 ~ "Insignificant Decrease",
    TRUE ~ NA
  )) %>% 
  group_by(id) %>%
  mutate(time = row_number()) %>%
  group_by(time, `Significance Category`) %>%
  summarise(`Effect on Total Addresses` = sum(`Effect on Total Addresses`)) %>%
  ggplot(aes(x = time,
             y = `Effect on Total Addresses`,
             group = `Significance Category`,
             color = `Significance Category`)
         ) +
  geom_vline(xintercept = 6.5) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1:10),
                     labels = Dates_list) +
  theme_bw()

    
Final_estimates <- All_estimates %>%
  filter(Period == "Period: 10") %>%
  mutate(id = as.double(id)) %>%
  right_join(USPS_data) %>%
  filter(time == max(USPS_data$time)) %>%
  na.omit() %>%
  left_join(Deciles) %>%
  left_join(poverty_medincome_pretreatment)

Final_estimates %>%
  filter(Significant == 1) %>%
  group_by(`Poverty Rate Decile, All Tracts`) %>%
  summarise(`Effect on Total Addresses` = sum(`Effect on Total Addresses`),
            count = n()) %>% 
  ggplot(aes(x = `Poverty Rate Decile, All Tracts` ,
             y = `Effect on Total Addresses`)) + 
  geom_hline(yintercept = 0) +
  geom_line() + 
  geom_point(aes(size = count,
                 alpha = count/max(count))) + 
  theme_bw()

Final_estimates %>%
  filter(Significant == 1) %>%
  group_by(`Median Income Decile, All Tracts`) %>%
  summarise(`Effect on Total Addresses` = sum(`Effect on Total Addresses`),
            count = n()) %>% 
  ggplot(aes(x = `Median Income Decile, All Tracts` ,
             y = `Effect on Total Addresses`)) + 
  geom_hline(yintercept = 0) +
  geom_line() + 
  geom_point(aes(size = count,
                 alpha = count/max(count))) + 
  theme_bw()


Final_estimates %>%
  filter(Significant == 1) %>%
  group_by(`Solo Detached Housing Decile, All Tracts`) %>%
  summarise(`Effect on Total Addresses` = sum(`Effect on Total Addresses`),
            count = n()) %>% 
  ggplot(aes(x = `Solo Detached Housing Decile, All Tracts` ,
             y = `Effect on Total Addresses`)) + 
  geom_hline(yintercept = 0) +
  geom_line() + 
  geom_point(aes(size = count,
                 alpha = count/max(count))) + 
  theme_bw()

rm(USPS_data)

state_names <- tigris::states() %>%
  as.data.frame() %>%
  mutate(state_fips = as.double(STATEFP)) %>% 
  select(state_fips,STUSPS, NAME) 

State <- Final_estimates %>%
  filter(Significant == 1) %>%
  group_by(state_fips) %>%
  summarise(`Number of Tracts` = n(),
            `Total Effect on All Addresses` = sum(`Effect on Total Addresses`),
            `Average Effect on Tracts` = mean(`Effect on Total Addresses`)) %>%
  left_join(state_names) %>%
  select(NAME, STUSPS, `Number of Tracts`, `Total Effect on All Addresses`, `Average Effect on Tracts`) %>%
  arrange(-`Total Effect on All Addresses`)

setwd(path_output)
write.xlsx(State, file = "State Effects.xlsx")


setwd(path_output)

P <- Final_estimates %>%
  mutate(`Significance Category` = case_when(
    Significant == 1 & `Effect on Total Addresses`>0 ~ "Significant Increase",
    Significant == 1 & `Effect on Total Addresses`<0 ~ "Significant Decrease",
    Significant == 0 & `Effect on Total Addresses`>0 ~ "Insignificant Increase",
    Significant == 0 & `Effect on Total Addresses`<0 ~ "Insignificant Decrease",
    TRUE ~ NA
  )) %>% 
  filter(Period == "Period: 10" ) %>%
  # filter(`Effect on Total Addresses` >-60 & `Effect on Total Addresses`<60) %>%
  ggplot(aes(x = `Effect on Total Addresses`,
             group = `Significance Category`,
             # color = Significant_label,
             fill = `Significance Category`)) + 
  # geom_histogram(aes(weight = `Effect on Total Addresses`),
  #                binwidth = 20, alpha = 0.7, position = "stack") +
  geom_histogram(aes(weight = `Effect on Total Addresses`),
                 binwidth = 20, alpha = 0.7, position = "identity") +
  # scale_fill_manual(values = c("gray", "black")) +  # Adjust colors as needed
  # scale_color_manual(values = c("gray", "black")) +  # Adjust colors as needed
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Effect of OZs on the Count of Addresses, Distribution",
    x = "Tract-level Effect on Total Addresses",
    y = "Aggregated Effect",
    fill = "Significance of the Effect Estimate"
  ) +
  theme_minimal()

png(filename = "Aggregating the Effectby tract level effects.png",height = 600, width = 1000)
plot(P)
dev.off()


# Plot with vertical lines at deciles
P <- Final_estimates %>%
  mutate(`Significance Category` = case_when(
    Significant == 1 & `Effect on Total Addresses`>0 ~ "Significant Increase",
    Significant == 1 & `Effect on Total Addresses`<0 ~ "Significant Decrease",
    Significant == 0 & `Effect on Total Addresses`>0 ~ "Insignificant Increase",
    Significant == 0 & `Effect on Total Addresses`<0 ~ "Insignificant Decrease",
    TRUE ~ NA
  )) %>% 
  ggplot(aes(x = median_income_2016,
             group = `Significance Category`,
             color = `Significance Category`,
             fill = `Significance Category`)) +
  geom_histogram(binwidth = 1000, alpha = 0.3, position = "identity", aes(weight = `Effect on Total Addresses`)) + # Adjust binwidth as needed
  # geom_histogram(binwidth = 1000, alpha = 0.6, position = "stack") + # Adjust binwidth as needed
  geom_vline(data = median_income_deciles, aes(xintercept = value), 
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of the Effect on Total Addresses by Median Income with Decile Markers",
       x = "Median Income",
       y = "Effect on Total Addresses")

png(filename = "Distribution of the Effect on Total Addresses by Median Income.png",height = 600, width = 1000)
plot(P)
dev.off()

P <- Final_estimates %>%
  mutate(`Significance Category` = case_when(
    Significant == 1 & `Effect on Total Addresses`>0 ~ "Significant Increase",
    Significant == 1 & `Effect on Total Addresses`<0 ~ "Significant Decrease",
    Significant == 0 & `Effect on Total Addresses`>0 ~ "Insignificant Increase",
    Significant == 0 & `Effect on Total Addresses`<0 ~ "Insignificant Decrease",
    TRUE ~ NA
  )) %>% 
  ggplot(aes(x = poverty_rate_2016,
             group = `Significance Category`,
             color = `Significance Category`,
             fill = `Significance Category`)) +
  geom_histogram(binwidth = 0.5, alpha = 0.3, position = "identity", aes(weight = `Effect on Total Addresses`)) + # Adjust binwidth as needed
  # geom_histogram(binwidth = 1000, alpha = 0.6, position = "stack") + # Adjust binwidth as needed
  geom_vline(data = poverty_deciles, aes(xintercept = value), 
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of the Effect on Total Addresses by Poverty Rate with Decile Markers",
       x = "Poverty Rate (Winzorized at 0.99)",
       y = "Effect on Total Addresses")

png(filename = "Distribution of the Effect on Total Addresses by Poverty Rate.png",height = 600, width = 1000)
plot(P)
dev.off()
