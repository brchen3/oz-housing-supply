# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 10/29/2024

# datacheck Sarah Eckhardt 01/14/2025

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(ggplot2)
library(did)
library(lubridate)
library(openxlsx)

################
# John list - 
# use the participation among those who have access from sipp
# Anyone who increases their contribution, automatically increase to 5% 
# What is the rate of savings among low-income savers?
# If we are just talking about full-time and the eligible population, what is the cost?


#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "bglasner" = "C:/Users/bglasner/EIG Dropbox/Benjamin Glasner/EIG/HUD Agg USPS Administrative Data on Vacancies",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/EIG/HUD Agg USPS Administrative Data on Vacancies",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/EIG/HUD Agg USPS Administrative Data on Vacancies",
  "sarah" = "/Users/sarah/EIG Dropbox/Sarah  Eckhardt/HUD datacheck"
  )

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]

path_data <- file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/2020 Standardized")
path_data_tract = file.path(path_data, "Tract Characteristics")

path_output = file.path(path_project, "output")

#################
### Data load ###
#################
setwd(path_data)
load(file = "USPS_tract_vacancy_2012_2024_2020_definitions.RData")

################
### Clean up uncertain tracts from the crosswalk
### If a tract in 2020 is a mix of any version of the LIC, contiguous or inelligible, then drop it from the analysis - conservative first pass

USPS_data <- USPS_data %>%
  filter(`Designation_category` %in% c("LIC selected","LIC not selected")) %>%
  filter(Sample == "In Clean Sample") %>%
  # filter(`Designation_category_detailed` != "LIC not selected, border tract") %>%
  mutate(NO_STAT_ALL = NO_STAT_OTHER_ADDRESSES + NO_STAT_BUSINESS_ADDRESSES + NO_STAT_RESIDENTIAL_ADDRESSES)


# table(USPS_data$date)

#################
### What are the trends in vacancy (counts and share) across designated and undesignated but eligible tracts? 
#################

USPS_data <- USPS_data %>% 
  mutate( period = dense_rank(date))

period_value <- min(USPS_data$period[USPS_data$date == "2020-03-01"])

##### Time Invariant Values
time_invariant <- USPS_data %>% 
  filter(date == "2016-12-01") %>%
  select(geoid,`poverty_rate`:`prime_age_share`)

# Continue with other data transformations as needed
USPS_data <- USPS_data %>%
  select(-c(`poverty_rate`:`prime_age_share`)) %>%
  filter(!is.na(ACTIVE_RESIDENTIAL_ADDRESSES)) %>%
  filter(!is.na(ACTIVE_BUSINESS_ADDRESSES)) %>%
  filter(!is.na(ACTIVE_OTHER_ADDRESSES)) %>%
  mutate(
    G = if_else(`OZ Designation` == 1, period_value, 0),
    geoid_num = as.numeric(geoid),
    
    Total_active_vacant_exclude_nostat = 
      ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES +
      ACTIVE_BUSINESS_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +
      ACTIVE_OTHER_ADDRESSES + STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES, 
    
    Total_active_vacant_exclude_nostat_RESIDENTIAL = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
    Total_active_vacant_exclude_nostat_BUSINESS = ACTIVE_BUSINESS_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES,
    Total_active_vacant_exclude_nostat_OTHER = ACTIVE_OTHER_ADDRESSES + STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES,
    
    VACANCY_RATE_ALL = 100*((STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES)/(Total_active_vacant_exclude_nostat)),
    VACANCY_RATE_RESIDENTIAL = 100*((STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES)/(Total_active_vacant_exclude_nostat_RESIDENTIAL)),
    VACANCY_RATE_BUSINESS = 100*((STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES)/(Total_active_vacant_exclude_nostat_BUSINESS)),
    VACANCY_RATE_OTHER = 100*((STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES)/(Total_active_vacant_exclude_nostat_OTHER))
  ) %>%
  left_join(time_invariant) %>%
  mutate(
    log_total_active_vacant_exclude_nostat = log(Total_active_vacant_exclude_nostat),
    log_res_address = log(Total_active_vacant_exclude_nostat_RESIDENTIAL),
    log_bus_address = log(Total_active_vacant_exclude_nostat_BUSINESS),
    log_other_address = log(Total_active_vacant_exclude_nostat_OTHER)
  ) %>%
  mutate(
    log_total_active_vacant_exclude_nostat = if_else(log_total_active_vacant_exclude_nostat<0,NA,log_total_active_vacant_exclude_nostat),
    log_res_address = if_else(log_res_address<0,NA,log_res_address),
    log_bus_address = if_else(log_bus_address<0,NA,log_bus_address),
    log_other_address = if_else(log_other_address<0,NA,log_other_address)
    )

USPS_data <- USPS_data %>%
  distinct(geoid, date, .keep_all = TRUE) %>%
  group_by(geoid) %>%
  mutate(number_of_quarters = n()) %>%
  ungroup()

max_quarters <- max(USPS_data$number_of_quarters)

USPS_data <- USPS_data %>%
  filter(number_of_quarters == max_quarters) %>%
  mutate( NO_STAT_ALL = case_when(
    date == "2017-09-01" ~ NA,
    TRUE ~ NO_STAT_ALL
  ))
  
#########################
### keep only the last period of treatment and pre-treatment periods
USPS_data <- USPS_data %>%
  filter( period == max(period) | period < period_value) %>%
  mutate(period = if_else(period > period_value, period_value, period))


data_list <- list()
dynamic_results_list <- list()
csdid_treated<- list()
dynamic <- list()
outcome_vars <- list()

outcome_vars[[1]] <- c(
  "Total_active_vacant_exclude_nostat_RESIDENTIAL"
  , "log_res_address"
)

titles <- c(
  "Effect on  Active and Vacant Residential"
  , "Effect on Log(Active Residential)"
)

table_titles <- c(
  "Active and Vacant Residential"
  , "Log(Active and Vacant Residential)"
)

titles_df <- as.data.frame(cbind(table_titles, outcome_vars[[1]]))
names(titles_df) <- c("titles", "outcome_var")

Metro_groupings <- sort(unique(USPS_data$`Type tract`))
# Metro_groupings <- c("Large urban")

# Metro_groupings <- sort(unique(USPS_data$res_quintile))

geo_groupings <- c("All",Metro_groupings)

plot_data <- list()
plot_data_list <- list()
dates <- sort(unique(USPS_data$date))[2:length(sort(unique(USPS_data$date)))]
plots <- list()
plot_list <- list()



data_list[[1]] <- USPS_data
for(i in seq_along(Metro_groupings)){
  j <- i + 1
  
  data_list[[j]] <- USPS_data %>% filter(`Type tract` == Metro_groupings[[i]])
  # data_list[[j]] <- USPS_data %>% filter(`res_quintile` == Metro_groupings[[i]])
}

dates_df <- USPS_data %>%
  select(period, date) %>%
  distinct() %>%
  arrange(period) %>% 
  mutate(period = period - period_value)

for (j in seq_along(geo_groupings)){
  print(geo_groupings[[j]])
  
  for(i in seq_along(outcome_vars[[1]])){
    print(outcome_vars[[1]][[i]])
    
    if(outcome_vars[[1]][[i]] %in% c("jobs_in_tract","employed_tract_residents")){
      analysis_data <- data_list[[j]] %>%
        filter(MONTH =="12")
    }else{
      analysis_data <- data_list[[j]]
    }
    
    
    
    csdid_treated[[i]] <- att_gt(
      yname = outcome_vars[[1]][[i]],
      tname = "period",
      idname = "geoid_num",
      gname = "G",
      xformla = ~poverty_rate + median_income + unemployment_rate + prime_age_share,
      biters = 1000,
      # clustervars = "State",
      # anticipation = 4,
      pl = TRUE,
      # cores = 6,
      # print_details = TRUE,
      # allow_unbalanced_panel = TRUE,
      data = analysis_data
    )
    
    dynamic[[i]] <- aggte(csdid_treated[[i]], 
                          alp = 0.1,
                          # alp = 0.05,
                          # alp = 0.01,
                          type = "dynamic",
                          # clustervars = "State",
                          na.rm = TRUE) 
    
    using <- data.frame(cbind(dynamic[[i]][["egt"]],
                              dynamic[[i]][["att.egt"]],
                              dynamic[[i]][["se.egt"]])) 
    names(using) <- c("Period", "ATT", "SE")
    
    using_dates <- dates_df %>% filter(period %in% c(using$Period))
    
    plot_data[[i]] <- using %>% 
      mutate(
        Row = row_number(),
        date = using_dates$date,
        Lower = ATT - dynamic[[i]]$crit.val.egt*SE,
        Upper = ATT + dynamic[[i]]$crit.val.egt*SE,
        `Pre/Post` = case_when(
          Period<0 ~ "Pre-Treatment",
          Period>=0 ~ "Post-Treatment",
          TRUE ~ NA
        )
      )
    # Create a data frame for the vline information
    vline_data <- data.frame(
      date = as.Date(c("2017-12-20", "2018-06-14", "2019-12-19")),
      label = c("OZs Enacted", "OZ Map Certified", "Regulations Finalized"),  # Labels for the legend
      linetype = c("Dashed Line 1", "Dashed Line 2", "Solid Line"),
      color = c("#254f85", "#79c5fd", "#008080") # Colors for the lines
    )
    
    plots[[i]] <- plot_data[[i]] %>%
      ggplot(aes(x = date,
                 color = `Pre/Post`)) +   
      ###################################################
    # Add geom_vline with legend information
    geom_vline(data = vline_data, aes(xintercept = date, color = label, linetype = linetype),
               size = 1, alpha = 0.3) +
      ###################################################
    geom_hline(yintercept = 0, color = "grey20") +
      geom_point(aes(y = ATT)) + 
      geom_errorbar(aes(ymin = Lower, ymax = Upper)) + 
      ggtitle(label = titles[[i]],
              subtitle = geo_groupings[[j]]) + 
      theme_minimal() +
      scale_color_manual(
        values = c("#254f85", "#79c5fd", "grey20","grey80","#008080"),  # Custom colors
        guide = guide_legend(title = "Policy Events") # Add legend title
      ) +
      scale_linetype_manual(
        values = c("Dashed Line 1" = "dashed", "Dashed Line 2" = "dashed", "Solid Line" = "solid"),
        # guide = guide_legend(title = "Line Types")   # Add legend title for linetype
        guide = guide_legend(element_blank())
      ) +
      guides(linetype = "none") + 
      theme(legend.position = "right")
    
  }
  
  dynamic_results_list[[j]] <- dynamic
  plot_data_list[[j]] <- plot_data
  plot_list[[j]] <- plots
  
}

# Initialize the results_export_list with all combinations
results_export_list <- expand.grid(
  geo_grouping = geo_groupings,
  outcome_var = outcome_vars[[1]],
  stringsAsFactors = FALSE
) %>% 
  arrange(geo_grouping, outcome_var)

# Initialize vectors to store formatted coefficients and SEs
formatted_coef <- character(nrow(results_export_list))
se_values <- numeric(nrow(results_export_list))

# Loop through each row to populate the coefficients and SEs
for (k in seq_len(nrow(results_export_list))) {
  # Extract current geo_grouping and outcome_var
  current_geo <- results_export_list$geo_grouping[k]
  current_outcome <- results_export_list$outcome_var[k]
  
  # Find the indices for geo_grouping and outcome_var
  geo_index <- which(geo_groupings == current_geo)
  outcome_index <- which(outcome_vars[[1]] == current_outcome)
  
  # Number of periods estimated 
  # time_period_count <- length(dynamic_results_list[[geo_index]][[outcome_index]]$att.egt)
  
  # Extract ATT, SE, and critical value
  att <- dynamic_results_list[[geo_index]][[outcome_index]]$overall.att
  crit_val <- dynamic_results_list[[geo_index]][[outcome_index]]$crit.val.egt[1]
  
  # Format the coefficient with an asterisk if significant
  if(!current_outcome %in% c( "log_total_active_vacant_exclude_nostat","log_bus_address","log_other_address","log_res_address")){
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$overall.se,2)
    
    formatted_coef[k] <- paste0(
      round(att, 2), " ",
      if_else((att / se) > crit_val, "*", ""))
  }else{
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$overall.se,4)
    
    
    formatted_coef[k] <- paste0(
      round(att, 4), " ",
      if_else((att / se) > crit_val, "*", ""))
  }
  
  
  # Store the standard error
  se_values[k] <- se
}

# Add the formatted coefficients and SEs to the results_export_list
results_export_list <- results_export_list %>%
  mutate(
    Coef = formatted_coef,
    SE = se_values
  )

# Pivot coefficients to wide format
coef_table <- results_export_list %>%
  select(geo_grouping, outcome_var, Coef) %>%
  pivot_wider(names_from = geo_grouping, values_from = Coef, names_prefix = "Coef_")

# Pivot SEs to wide format
se_table <- results_export_list %>%
  select(geo_grouping, outcome_var, SE) %>%
  pivot_wider(names_from = geo_grouping, values_from = SE, names_prefix = "SE_")

# Combine the coefficients and SE tables
final_table <- coef_table %>%
  left_join(se_table, by = "outcome_var") %>%
  arrange(outcome_var) %>%
  mutate(
    `All` = paste0(`Coef_All`," (",`SE_All`,")"),
    `Large urban` = paste0(`Coef_Large urban`," (",`SE_Large urban`,")"),
    `Mid-sized urban` = paste0(`Coef_Mid-sized urban`," (",`SE_Mid-sized urban`,")"),
    `Small urban` = paste0(`Coef_Small urban`," (",`SE_Small urban`,")"),
    `Suburban` = paste0(`Coef_Suburban`," (",`SE_Suburban`,")"),
    `Small town` = paste0(`Coef_Small town`," (",`SE_Small town`,")"),
    `Rural` = paste0(`Coef_Rural`," (",`SE_Rural`,")"),
         ) %>%
  left_join(titles_df) %>%
  select(
    titles,
    `All`,`Large urban`,`Mid-sized urban`,`Small urban`,`Suburban`,`Small town`,`Rural`
  )


reshaped_table <- final_table %>%
  pivot_longer(
    cols = c(`All`, `Large urban`, `Mid-sized urban`, `Small urban`, `Suburban`, `Small town`, `Rural`),
    names_to = "geo_grouping",
    values_to = "Coef_SE"
  ) %>%
  pivot_wider(
    names_from = titles,
    values_from = Coef_SE
  ) %>%
  arrange(match(geo_grouping, c("All", "Large urban", "Mid-sized urban", "Small urban", "Suburban", "Small town", "Rural"))) %>%
  select(geo_grouping,all_of(table_titles)) %>%
  rename(`Tract Geography` = geo_grouping)
  


reshaped_table

setwd(path_output)
write.xlsx(reshaped_table, file = "CSDID Effect Estimate.xlsx", overwrite = TRUE)

