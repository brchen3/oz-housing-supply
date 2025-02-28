# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 10/29/2024

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
# use the participation among those who have access from sipp
# Anyone who increases their contribution, automatically increase to 5% 
# What is the rate of savings among low-income savers?
# If we are just talking about full-time and the eligible population, what is the cost?


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
path_data_tract <- file.path(path_data, "Tract Characteristics")

path_output <- file.path(path_project, "output")

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

# Count the number of quarters covered by the data set for each tract
USPS_data <- USPS_data %>%
  distinct(geoid, date, .keep_all = TRUE) %>%
  group_by(geoid) %>%
  mutate(number_of_quarters = n()) %>%
  ungroup()

max_quarters <- max(USPS_data$number_of_quarters)

# Filter out tracts with missing quarters
USPS_data <- USPS_data %>%
  filter(number_of_quarters == max_quarters) %>%
  mutate( NO_STAT_ALL = case_when(
    date == "2017-09-01" ~ NA,
    TRUE ~ NO_STAT_ALL
  ))

#######################
### Build DID Model ###
#######################
# Variable initialization
data_list <- list()
dynamic_results_list <- list()
csdid_treated<- list()
dynamic <- list()
outcome_vars <- list()
plot_data <- list()
plot_data_list <- list()
dates <- sort(unique(USPS_data$date))[2:length(sort(unique(USPS_data$date)))]
plots <- list()
plot_list <- list()

# List the column names of the dependent variables
outcome_vars[[1]] <- c(
  "Total_active_vacant_exclude_nostat"
  , "log_total_active_vacant_exclude_nostat"
  
  , "Total_active_vacant_exclude_nostat_RESIDENTIAL"
  , "log_res_address"
  
  , "Total_active_vacant_exclude_nostat_BUSINESS"
  , "log_bus_address"
  
  , "Total_active_vacant_exclude_nostat_OTHER"
  , "log_other_address"
  
  , "ACTIVE_RESIDENTIAL_ADDRESSES"
)

# Generate dependent variable table titles
titles <- c(
  "Effect on All Active and Vacant"
  , "Effect on Log(All Active and Vacant)"
  
  , "Effect on  Active and Vacant Residential"
  , "Effect on Log(Active Residential)"

  , "Effect on  Active and Vacant Business"
  , "Effect on Log( Active and Vacant Business)"

  , "Effect on  Active and Vacant Other"
  , "Effect on Log( Active and Vacant Other)"
  
  , "ACTIVE_RESIDENTIAL_ADDRESSES"
)

table_titles <- c(
  "All Active and Vacant"
  , "Log(All Active and Vacant)"
  
  , "Active and Vacant Residential"
  , "Log(Active and Vacant Residential)"

  , "Active and Vacant Business"
  , "Log(Active and Vacant Business)"

  , "Active and Vacant Other"
  , "Log(Active and Vacant Other)"
  
  , "ACTIVE_RESIDENTIAL_ADDRESSES"
)

titles_df <- as.data.frame(cbind(table_titles, outcome_vars[[1]]))
names(titles_df) <- c("titles", "outcome_var")

# Get sorted list of geoids for the census tracts of interest
Metro_groupings <- sort(unique(USPS_data$`Type tract`))
geo_groupings <- c("All",Metro_groupings)

# Split data by census tract
data_list[[1]] <- USPS_data
for(i in seq_along(Metro_groupings)){
  j <- i + 1
  data_list[[j]] <- USPS_data %>% filter(`Type tract` == Metro_groupings[[i]])
}

# Transform time variable
dates_df <- USPS_data %>%
  select(period, date) %>%
  distinct() %>%
  arrange(period) %>% 
  mutate(period = period - period_value)

# Create the DID model and plot the pre-post treatment comparison
for (j in seq_along(geo_groupings)){
  print(geo_groupings[[j]])
  
  for(i in seq_along(outcome_vars[[1]])){
    print(outcome_vars[[1]][[i]])
    
    # filter job and employment data by year
    if(outcome_vars[[1]][[i]] %in% c("jobs_in_tract","employed_tract_residents")){
      analysis_data <- data_list[[j]] %>%
        filter(MONTH =="12")
    }else{
      analysis_data <- data_list[[j]]
    }
    
    # create linear model for the treated group
    csdid_treated[[i]] <- att_gt(
      yname = outcome_vars[[1]][[i]],
      tname = "period",
      idname = "geoid_num",
      gname = "G",
      xformla = ~poverty_rate + median_income + unemployment_rate + prime_age_share,
      biters = 1000,
      pl = TRUE,
      data = analysis_data
    )
    
    # compute the overall effect by averaging the effect of the treatment across
    # all positive lengths of exposure
    dynamic[[i]] <- aggte(csdid_treated[[i]], 
                          alp = 0.01,
                          type = "dynamic",
                          na.rm = TRUE) 
    
    using <- data.frame(cbind(dynamic[[i]][["egt"]],
                              dynamic[[i]][["att.egt"]],
                              dynamic[[i]][["se.egt"]])) 
    names(using) <- c("Period", "ATT", "SE")
    
    using_dates <- dates_df %>% filter(period %in% c(using$Period))
    
    # Export data for treatment and experiment lines to graph
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
        ),
        `Pre/Post` = factor(`Pre/Post`, levels = c("Pre-Treatment", "Post-Treatment"))
      )
    
    
    # Create a data frame for the vline information
    vline_data <- data.frame(
      date = as.Date(c("2017-12-20", "2018-06-14", "2019-12-19")),
      label_date = as.Date(c("2017-09-20", "2018-09-14", "2020-03-19")),
      label = c("OZs Enacted", "OZ Map Certified", "Regulations Finalized"),  # Labels for the legend
      linetype = c("Dashed Line 1", "Dashed Line 2", "Solid Line"),
      # color = c("#254f85", "#79c5fd", "#008080") # Colors for the lines
      color = c("black", "black", "black") # Colors for the lines
    )
    
    plots[[i]] <- plot_data[[i]] %>%
      ggplot(aes(x = date,
                 color = `Pre/Post`)) +   
      ###################################################
      # Add geom_vline with legend information
      # geom_vline(data = vline_data, aes(xintercept = date, color = label, linetype = linetype),
      #            size = 1, alpha = 0.3) +
      ###################################################
      
      # Add text labels and arrows
      geom_segment(data = vline_data, 
                   aes(x = date, xend = date, 
                       y = min(plot_data[[i]]$Lower, na.rm = TRUE), 
                       yend = max(plot_data[[i]]$Upper, na.rm = TRUE)),
                   # arrow = arrow(length = unit(0.02, "npc")), 
                   color = vline_data$color) +
      geom_text(data = vline_data, 
                aes(x = label_date, 
                    y = max(plot_data[[i]]$Upper, na.rm = TRUE) * 0.65, 
                    label = label),
                angle = 90, hjust = -0.1, vjust = 0.5, 
                color = vline_data$color, size = 3) +
      
      # Horizontal line at zero
      geom_hline(yintercept = 0, color = "grey20") +
      geom_vline(xintercept = as.Date("2012-01-01"), color = "grey20") +
      
      # Data points and error bars
      geom_point(aes(y = ATT)) + 
      geom_errorbar(aes(ymin = Lower, ymax = Upper)) + 
      
      # Theme and axis labels
      theme_minimal() +
      ylab("Average Treatment on the Treated") + 
      xlab("Date") + 
      
      # Custom colors for Pre/Post Treatment
      scale_color_manual(
        values = c("Pre-Treatment" = "grey60", "Post-Treatment" = "grey10"), 
        guide = guide_legend(title = "Treatment Period: ")
      ) +
      
      # Remove linetype legend
      guides(linetype = "none") +
      
      # Adjust legend position
      theme(legend.position = "bottom",
            panel.grid = element_blank())
    
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
  time_period_count <- length(dynamic_results_list[[geo_index]][[outcome_index]]$att.egt)
  # Extract ATT, SE, and critical value
  att <- dynamic_results_list[[geo_index]][[outcome_index]]$att.egt[time_period_count]
  crit_val <- dynamic_results_list[[geo_index]][[outcome_index]]$crit.val.egt[1]
  
  # Format the coefficient with an asterisk if significant
  if(!current_outcome %in% c( "log_total_active_vacant_exclude_nostat","log_bus_address","log_other_address","log_res_address")){
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count],2)
    
    formatted_coef[k] <- paste0(
      round(att, 2), " ",
      if_else((att / se) > crit_val, "*", ""))
  }else{
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count],4)
    
    
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

# Pivot table to horizontal for datawrapper display
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

png(file = "CSDID Event Study All Active and Vacant.png",width = 800, height = 533)
plot_list[[1]][[1]]
dev.off()

png(file = "CSDID Event Study Logged All Active and Vacant.png",width = 800, height = 533)
plot_list[[1]][[2]]
dev.off()

pdf(file = "CSDID Event Studies.pdf",width = 12, height = 8)
plot_list
dev.off()

# Selected list of significant impacts
export_plot_list <- list()
export_plot_list[[1]] <- plot_data_list[[1]][[1]] # All tracts, Active and Vacant 
export_plot_list[[2]] <- plot_data_list[[1]][[2]] # All tracts, log(Active and Vacant) 
export_plot_list[[3]] <- plot_data_list[[1]][[3]] # All tracts, Active and Vacant Residential
export_plot_list[[4]] <- plot_data_list[[1]][[4]] # All tracts, log(Active and Vacant Residential) 

export_plot_list[[5]] <- plot_data_list[[2]][[1]] # Large Urban tracts, Active and Vacant 
export_plot_list[[6]] <- plot_data_list[[2]][[2]] # Large Urban tracts, log(Active and Vacant) 
export_plot_list[[7]] <- plot_data_list[[2]][[3]] # Large Urban tracts, Active and Vacant  Residential
export_plot_list[[8]] <- plot_data_list[[2]][[4]] # Large Urban tracts, log(Active and Vacant Residential) 

export_plot_list[[9]] <- plot_data_list[[3]][[3]] # Mid-size Urban tracts, log(Active and Vacant Residential) 
export_plot_list[[10]] <- plot_data_list[[4]][[3]] # rural tracts, log(Active and Vacant Residential) 
export_plot_list[[11]] <- plot_data_list[[5]][[3]] # small town tracts, log(Active and Vacant Residential) 
export_plot_list[[12]] <- plot_data_list[[6]][[3]] # small Urban tracts, log(Active and Vacant Residential) 
export_plot_list[[13]] <- plot_data_list[[7]][[3]] # suburban tracts, log(Active and Vacant Residential) 



sheet_names <- c(
  "All, Act and Vac"
  , "All, Log(Act and Vac)"
  , "All, Act and Vac Res"
  , "All, Log(Act and Vac Res)"
  
  , "Large, Act and Vac"
  , "Large, Log(Act and Vac)"
  , "Large, Act and Vac Res"
  , "Large, Log(Act and Vac Res)"
  
  , "Mid-size, Act and Vac Res"
  , "rural, Act and Vac Res"
  , "Small town, Act and Vac Res"
  , "Small Urban, Act and Vac Res"
  , "Suburban, Act and Vac Res"
)

# Create a new workbook
wb <- createWorkbook()

# Loop through each model result
for(i in seq_along(export_plot_list)){

  # Define sheet name using geo_groupings
  sheet_name <- sheet_names[[i]]

  # Add worksheet to the workbook
  addWorksheet(wb, sheet_name)

  # Write data to the worksheet
  writeData(wb, sheet = sheet_name, x = export_plot_list[[i]])
}

# Define the file path where you want to save the workbook
file_path <- "CSDID_model_results.xlsx"

# Save the workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)


