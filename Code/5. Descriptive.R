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
library(ggplot2)
library(lubridate)
library(panelView)
library(gsynth)
library(plm)
library(openxlsx)

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

path_data = file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/HUD USPS Data Standardized")
path_data_tract = file.path(path_project, "data/Tract Characteristics")

path_output = file.path(path_project, "output")

#################
### Data load ###
#################

setwd(path_data)
load(file = "USPS_tract_vacancy_2012_2024_2020_definitions.RData") # master data set. see 4. final dataset build.R


################
# Clean up uncertain tracts from the crosswalk
# If a tract in 2020 is a mix of any version of:
  # LIC, contiguous or inelligible, 
# then drop it from the analysis - conservative first pass

USPS_data <- USPS_data %>%
  mutate(`Active address count` = ACTIVE_RESIDENTIAL_ADDRESSES + ACTIVE_BUSINESS_ADDRESSES + ACTIVE_OTHER_ADDRESSES) %>%
  filter(Sample == "In Clean Sample") %>%
  mutate(NO_STAT_ALL = NO_STAT_OTHER_ADDRESSES + NO_STAT_BUSINESS_ADDRESSES + NO_STAT_RESIDENTIAL_ADDRESSES)

#################
### What are the trends in vacancy (counts and share) across designated and undesignated but eligible tracts? 
#################

USPS_data <- USPS_data %>%
  mutate(
    Group_var = case_when(
      `Designation_category` == "LIC selected" & `OZ Designation` == 1 ~ "Designated LIC",
      `Designation_category` == "LIC not selected" & `OZ Designation` == 0 ~ "Undesignated LIC",
      `Designation_category` == "Contiguous not selected" & `OZ Designation` == 0 ~ "Undesignated Contiguous",
      `Designation_category` == "Contiguous selected" & `OZ Designation` == 1 ~ "Designated Contiguous",
      `Designation_category` == "Ineligible" ~ "Ineligible",
      TRUE ~ NA
    )
  ) 
  
# is the panel balanced?
table(USPS_data$date, USPS_data$Group_var)

####################################
### Descriptive Statistics Table ###
####################################

table_df <- USPS_data %>%
  filter(date == "2017-03-01") %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES)
  

address_counts <- table_df %>%
  filter(!is.na(Group_var)) %>%
  group_by(Group_var) %>%
  summarise(
    `Median Active and Vacant Residential Address Count` = median(`Active and Vacant, Residential`, na.rm = TRUE),
    `Median Active Residential Address Count` = median(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
    `Median Active Business Address Count` = median(ACTIVE_BUSINESS_ADDRESSES, na.rm = TRUE),
    `Median No Status Address Count` = median(NO_STAT_ALL, na.rm = TRUE),
    `Median Active Other Address Count` = median(ACTIVE_OTHER_ADDRESSES, na.rm = TRUE),
    
    `Median Poverty Rate` = round(median(poverty_rate, na.rm = TRUE),2),  
    `Median MFI` = round(median(median_income, na.rm = TRUE),2),  
    `Median Unemployment Rate` = round(median(unemployment_rate, na.rm = TRUE),2),  
    `Median Prime-Age Share` = round(median(prime_age_share, na.rm = TRUE),2),
    
    `Median Number of Tracts` = n()
  )

# Step 3: Reshape Address Counts to Long Format
address_counts_long <- address_counts %>%
  pivot_longer(
    cols = starts_with("Median"),
    names_to = "Outcome",
    values_to = "Value"
  )

# Step 4: Summarize Share Table
share_table <- table_df %>%
  filter(!is.na(Group_var)) %>%
  filter(!is.na(`Type tract`)) %>%
  group_by(`Type tract`, Group_var) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Group_var) %>%
  mutate(share = count / sum(count)) %>%
  arrange(Group_var, desc(share)) %>%
  mutate(share_percent = round(share * 100, 2)) %>%
  ungroup()

# Step 5: Reshape Share Table to Long Format
share_table_long <- share_table %>%
  mutate(Outcome = paste0("Share ", `Type tract`)) %>%  # Create unique outcome names for each Type tract
  select(Outcome, Group_var, share_percent) %>%
  rename(Value = share_percent)


# Step 6: Combine Address Counts and Share Table
combined_long <- bind_rows(
  address_counts_long,
  share_table_long
)

# Step 7: Pivot Combined Data to Wide Format
final_table <- combined_long %>%
  pivot_wider(
    names_from = Group_var,
    values_from = Value
  )

# Step 8: Arrange the Final Table for Readability (Optional)

desired_order <- c(
  "Median Active and Vacant Residential Address Count",
  "Median Active Residential Address Count",
  "Median Active Business Address Count",
  "Median Active Other Address Count",
  "Median No Status Address Count",
  "Median Tract Workers",
  "Median Non-resident Workers",
  "Median Resident Workers",
  "Median MFI",
  "Median Poverty Rate",
  "Median Prime-Age Share",
  "Median Unemployment Rate",
  "Share Large urban",
  "Share Mid-sized urban",
  "Share Small urban",
  "Share Suburban",
  "Share Small town",
  "Share Rural",
  "Number of Tracts"
)

final_table <- final_table %>%
  mutate(Outcome = if_else(Outcome == "Median Number of Tracts", "Number of Tracts", Outcome)) %>%
  mutate(Outcome = factor(Outcome, levels = desired_order)) %>%
  arrange(Outcome) %>%
  select("Outcome",
         "Designated LIC", "Undesignated LIC",
         "Designated Contiguous", "Undesignated Contiguous",
         "Ineligible")

setwd(path_output)
write.csv(final_table, file = "Descriptive Table.csv", row.names = FALSE)

####################################
###         Top line stat        ###
####################################
# Define the time period of interest
target_months <- c("2014-09","2019-09", "2024-09")

# Helper function to calculate residential address change for a given filter
calculate_res_change <- function(data) {
  data %>%
    # Sum up all types of residential addresses into one column
    mutate(Residential = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES) %>%
    # Keep only rows for the target months
    filter(YEAR_MONTH %in% target_months) %>%
    # Group by month and calculate total residential addresses
    group_by(YEAR_MONTH) %>%
    summarise(TotalResidential = sum(Residential, na.rm = TRUE), .groups = 'drop') %>%
    # Spread months into separate columns for easier comparison
    pivot_wider(names_from = YEAR_MONTH, values_from = TotalResidential) %>%
    # Calculate the change from 2019-09 to 2024-09
    mutate(change = `2024-09` - `2019-09`,
           pre_change = `2019-09` - `2014-09`)
}

# Calculate national residential change without any category filter
national_res_change <- calculate_res_change(USPS_data)

# Calculate change for LIC-eligible tracts
LIC_res_change <- calculate_res_change(
  subset(USPS_data,Designation_category %in% c("LIC not selected", "LIC selected"))
)

# Calculate change for designated OZs (assumed under "LIC selected")
OZ_res_change <- calculate_res_change(  
  subset(USPS_data,Designation_category %in% c("LIC selected"))
)

# Compose a summary statement comparing OZ growth relative to LIC and national changes
summary_statement <- paste0(
  "OZ's added a total of ", prettyNum(OZ_res_change$change, big.mark = ","), 
  " new active and vacant residential addresses from Q3 2019 to Q3 2024. ",
  "This accounted for ", round(OZ_res_change$change / LIC_res_change$change, 3) * 100,
  "% of all new active and vacant addresses among all LIC eligible tracts regardless of OZ designation, which added ",
  prettyNum(LIC_res_change$change, big.mark = ","), 
  ". OZs also accounted for ",
  round(OZ_res_change$change / national_res_change$change, 3) * 100,
  "% of the ", prettyNum(national_res_change$change, big.mark = ","), 
  " new active and vacant residential addresses across the full analysis sample.",
  " From Q3 of 2014 to Q3 of 2019, OZ's accounted for ", 
  round(OZ_res_change$pre_change / national_res_change$pre_change, 3) * 100,
  "% of the ", prettyNum(national_res_change$pre_change, big.mark = ","), 
  " new active and vacant residential addresses across the full analysis sample."
  
)


# Additional calculations based on the provided text
# avg_effect <- 35.66         # average new addresses per OZ tract
avg_effect <- 32.68         # average new addresses per OZ tract after excluding bordering tracts
oz_tracts <- 8764           # total number of OZ tracts

avg_crowd_out <- 7.82
border_tracts <- 
  
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

# Print the first summary
print(summary_statement)
# Print the additional summary
print(additional_statement)
print(cost_statement)

# If you compare designated OZs to overall tracts, did OZs have a stronger pre-trend of address growth?
# Compare address growth rates
Count_add  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-09","2015-09","2016-09","2017-09","2018-09","2019-09","2020-09","2021-09","2022-09","2023-09","2024-09"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation_category %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation_category %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )
         ) %>%
  arrange(YEAR_MONTH) %>%
  group_by(Designation_category,YEAR_MONTH) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Designation_category`, values_from = `Active and Vacant, Residential`) %>%
  rename(`Time Period` = YEAR_MONTH)

growth  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-09","2015-09","2016-09","2017-09","2018-09","2019-09","2020-09","2021-09","2022-09","2023-09","2024-09"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation_category %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation_category %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )) %>%
  arrange(YEAR_MONTH) %>%
  group_by(Designation_category,YEAR) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(Designation_category) %>%
  mutate(`Treatment Period` = case_when(
    YEAR <= 2019 ~ "Pre-OZs Avg. Annual Growth",
    YEAR >=2020 ~ "Post-OZs Avg. Annual Growth",
    TRUE ~ NA
  ),
  growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`))/dplyr::lag(`Active and Vacant, Residential`))*100) %>%
  na.omit() %>%
  group_by(Designation_category, `Treatment Period`) %>%
  summarize(growth_rate = mean(growth_rate, na.omit = TRUE)) %>%
  pivot_wider(names_from = `Designation_category`, values_from = growth_rate) %>%
  rename(`Time Period` = `Treatment Period`) %>%
  arrange(`Contiguous not selected`)

growth <- bind_rows(Count_add, growth)

write.xlsx(growth, file = "Total Address Growth Rate.xlsx")

########################
# Simplified version
Count_add  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-09","2015-09","2016-09","2017-09","2018-09","2019-09","2020-09","2021-09","2022-09","2023-09","2024-09"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation_category %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation_category %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )
  ) %>%
  arrange(YEAR_MONTH) %>%
  group_by(`OZ designation`,YEAR_MONTH) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `OZ designation`, values_from = `Active and Vacant, Residential`) %>%
  rename(`Time Period` = YEAR_MONTH)

growth  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-09","2015-09","2016-09","2017-09","2018-09","2019-09","2020-09","2021-09","2022-09","2023-09","2024-09"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation_category %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation_category %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )) %>%
  arrange(YEAR_MONTH) %>%
  group_by(`OZ designation`,YEAR) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(`OZ designation`) %>%
  mutate(`Treatment Period` = case_when(
    YEAR <= 2019 ~ "Pre-OZs Avg. Annual Growth",
    YEAR >=2020 ~ "Post-OZs Avg. Annual Growth",
    TRUE ~ NA
  ),
  growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`))/dplyr::lag(`Active and Vacant, Residential`))*100) %>%
  na.omit() %>%
  group_by(`OZ designation`, `Treatment Period`) %>%
  summarize(growth_rate = mean(growth_rate, na.omit = TRUE)) %>%
  pivot_wider(names_from = `OZ designation`, values_from = growth_rate) %>%
  rename(`Time Period` = `Treatment Period`) %>%
  arrange(-as.numeric(as.factor(`Time Period`)))

growth <- bind_rows(Count_add, growth)

write.xlsx(growth, file = "Total Address Growth Rate, simplified.xlsx")

########################
# Simplified version - growth rate

growth  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-09","2015-09","2016-09","2017-09","2018-09","2019-09","2020-09","2021-09","2022-09","2023-09","2024-09"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation_category %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation_category %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )) %>%
  arrange(YEAR_MONTH) %>%
  group_by(`OZ designation`,YEAR) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(`OZ designation`) %>%
  mutate(
    growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`))/dplyr::lag(`Active and Vacant, Residential`))*100
    ) %>%
  ungroup() %>%
  na.omit() %>%
  select(YEAR,`OZ designation`,growth_rate) %>%
  pivot_wider(names_from = `OZ designation`, values_from = growth_rate) 


write.xlsx(growth, file = "Total Address Growth Rate, simplified line plot.xlsx")


####################################
###           Time Series        ###
####################################
annual <- USPS_data %>%
  group_by(date) %>%
  summarise(`Total residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active and Vacant residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE) - sum(NO_STAT_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active residential address count` = sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE))
write.csv(annual,file = file.path(path_output,"time series.csv"), row.names = FALSE)

####################################
###         Time Series  - LIC   ###
####################################

annual <- USPS_data %>%
  filter(`Designation_category` %in% c("LIC not selected","LIC selected")) %>%
  group_by(date) %>%
  summarise(`Total residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active and Vacant residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE) - sum(NO_STAT_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active residential address count` = sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE))

write.csv(annual,file = file.path(path_output,"time series LIC.csv"), row.names = FALSE)

########################################
###   Share of addresses over time   ###
########################################

share_designation <- USPS_data %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES) %>%
  group_by(`Designation_category`,date) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE), 
            ACTIVE_RESIDENTIAL_ADDRESSES = sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE), .groups = 'drop') %>%
  group_by(date) %>%
  # mutate(share = ACTIVE_RESIDENTIAL_ADDRESSES / sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE)) %>%
  mutate(share = `Active and Vacant, Residential` / sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  arrange(date, desc(share)) %>%
  mutate(share_percent = round(share * 100, 2)) %>%
  ungroup() %>%
  group_by(Designation_category) %>%
  mutate(share_normalized = round((`Active and Vacant, Residential` / first(`Active and Vacant, Residential`)) * 100, 2)) %>%
  ungroup()

share_designation %>%
  filter(`Designation_category` %in% c("LIC not selected","LIC selected","Ineligible")) %>%
  ggplot(aes(x = date, 
             y = share_normalized, 
             group = `Designation_category`,
             color = `Designation_category`)) + 
  geom_line() + 
  geom_point()

share_designation_wide <- share_designation %>%
  select(Designation_category,date,share_percent) %>%
  pivot_wider(
    names_from = Designation_category,
    values_from = share_percent
  )

share_designation %>%
  ggplot(aes(x = date, 
             y =share_percent,
             group = Designation_category)) +
  geom_line() + 
  facet_wrap(Designation_category ~ .,scales = "free_y" )

write.xlsx(share_designation_wide, "Share of Addresses.xlsx")

####################################################################################################

### Split by tract type

summarized <- USPS_data  %>%
  filter(`Designation_category` %in% c("LIC not selected","LIC selected")) %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES) %>% 
  select(
    geoid,
    date,`Type tract`,
    `Active address count`, `Active and Vacant, Residential`,
    `OZ Designation`
  ) 


summarized <- summarized %>%
  group_by(`OZ Designation`,`Type tract`, date) %>%
  summarise(
    `Typical Active and Vacant, Residential` = median(`Active and Vacant, Residential`, na.rm = TRUE)
  )


summarized %>%
  filter(!is.na(`Type tract`)) %>% 
  ggplot(aes(x = date,
             group = `Type tract`,
             color = as.factor(`Type tract`))) + 
  geom_vline(xintercept = as.Date("2020/03/01", format = "%Y/%m/%d")) +
  geom_line(aes(y = `Typical Active and Vacant, Residential`)) + 
  geom_point(aes(y = `Typical Active and Vacant, Residential`)) + 
  facet_wrap(`OZ Designation` ~ .)

reshaped_data <- summarized %>%
  filter(!is.na(`Type tract`)) %>% 
  pivot_longer(cols = starts_with("Typical") | starts_with("Growth Rate") | starts_with("Share"),
               names_to = "Outcome_Variable", 
               values_to = "Value") %>%
  # Group by the tier, date, and outcome variable
  group_by(`Type tract`, date, Outcome_Variable) %>%
  # Calculate the difference between OZ Designation 1 and 0
  summarize(Difference = Value[`OZ Designation` == 1] - Value[`OZ Designation` == 0], .groups = "drop")

# Reshape the data back to wide format (optional, if needed)
wide_data <- reshaped_data %>%
  pivot_wider(names_from = Outcome_Variable, values_from = Difference)

# View the resulting data
head(wide_data)

wide_data %>%
  filter(date > "2015-12-01") %>% 
  filter(!is.na(`Type tract`)) %>% 
  group_by(`Type tract`) %>%
  mutate(`Typical Active and Vacant, Residential - 2019-12 to zero` = `Typical Active and Vacant, Residential` - 
           `Typical Active and Vacant, Residential`[date == "2019-12-01"]) %>%
  ggplot(aes(x = date,
             group = `Type tract`,
             color = as.factor(`Type tract`))) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2020/03/01", format = "%Y/%m/%d")) +
  geom_line(aes(y = `Typical Active and Vacant, Residential - 2019-12 to zero`)) +
  geom_point(aes(y = `Typical Active and Vacant, Residential - 2019-12 to zero`))


wide_data_res  <- wide_data %>%
  filter(date > "2015-12-01") %>% 
  filter(!is.na(`Type tract`)) %>% 
  group_by(`Type tract`) %>%
  mutate(`Typical Active and Vacant, Residential - 2019-12 to zero` = `Typical Active and Vacant, Residential` - 
           `Typical Active and Vacant, Residential`[date == "2019-12-01"]) %>%
  ungroup() %>%
  select(`Type tract`, date, `Typical Active and Vacant, Residential - 2019-12 to zero`) %>%
  pivot_wider(names_from = `Type tract`, values_from = `Typical Active and Vacant, Residential - 2019-12 to zero`)

setwd(path_output)
write.csv(wide_data_res, "Gap in residential addresses by OZ designation and tract type.csv", row.names = FALSE)

