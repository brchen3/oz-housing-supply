

# ACS occupied housing units for 2012 - 2023 relative to HUD address 
# counts for the same period.

# Sarah Eckhardt 02/11/2025

rm(list = ls())

# load relevant packages
library(tidyr)
library(dplyr)
library(tidycensus)
library(readxl)
library(ggplot2)
library(foreign)

project_directories <- list(
  "bglasner" = "C:/Users/bglasner/EIG Dropbox/Benjamin Glasner/EIG/HUD Agg USPS Administrative Data on Vacancies",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/EIG/HUD Agg USPS Administrative Data on Vacancies",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/EIG/HUD Agg USPS Administrative Data on Vacancies",
  "sarah" = "/Users/sarah/EIG Dropbox/Sarah  Eckhardt/HUD Agg USPS Administrative Data on Vacancies"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

# set user and file paths
path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")

#########################################################################
# get tract- level data Census decennial files on occuped housing units

# load in tidycensus variables. variable lists are different for 2010 & 2020

vars2010 <- load_variables(year = 2010, dataset = "sf1", cache = TRUE) # sf1 dropped for 2020.
vars2010 %>% filter(concept == "OCCUPANCY STATUS") # want occupied units -- H003002
    occ_var_2010 = vars2010 %>% filter(name == "H003002")
    occ_var_2010 = occ_var_2010$name[1]
    
vars2020 <- load_variables(year = 2020, dataset = "dhc")
vars2020 %>% filter(concept == "OCCUPANCY STATUS") # want occupied units -- H3_002N
    occ_var_2020 = vars2020 %>% filter(name == "H3_002N")
    occ_var_2020 = occ_var_2020$name[1]
  
    
# set up census api key
census_api_key("f0a4d766bde27f14627892a18bdc610ab945336d", install = TRUE, overwrite = TRUE)

# Reload environment to use the key
readRenviron("~/.Renviron")



# retrieve list of states
states_df <- tigris::states(cb = TRUE) %>%
  filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI")) %>%  # Exclude territories
  select(NAME, STATEFP, STUSPS) %>%
  mutate(state_name = NAME,
         state_fips = STATEFP,
         state_abbr = STUSPS) %>%
  as.data.frame() %>%
  select(state_name, state_fips, state_abbr) %>% 
  arrange(state_fips)


# set year list
years = c(2010, 2020)


# create lists to store tidy_census calls
tract_list = list()


# read in each state year; combine
for(j in seq_along(states_df$state_fips)) {
  
  decennial_2010 = get_decennial(
    state = states_df$state_fips[[j]],
    geography = "tract",
    variables = occ_var_2010,    # total occupied housing units; proxy for addresses
    year = 2010,
    sumfile = "sf1",
    cache_table = TRUE,
    geometry = FALSE,
    output = "wide")  %>% 
    rename(census_occ_housing_units = H003002) %>%
    mutate(YEAR = 2010) %>% 
    select(-c(NAME))

  decennial_2020 = get_decennial(
    state = states_df$state_fips[[j]],
    geography = "tract",
    variables = occ_var_2020,    # total occupied housing units; proxy for addresses
    year = 2020,
    sumfile = "dhc",
    cache_table = TRUE,
    geometry = FALSE,
    output = "wide")  %>% 
    rename(census_occ_housing_units = H3_002N) %>%
    mutate(YEAR = 2020) %>% 
    select(-c(NAME))
  
  
  tract_list[[j]] = bind_rows(decennial_2010, decennial_2020)
  
}


# compile data   
decennial_2010_2020 <- bind_rows(tract_list) %>%
  rename_at(c("GEOID", "YEAR"),
            .funs = tolower)


      # clean the skew of the data by winzorizing at the 99th level
      winsorize_upper <- function(x, p = 0.99) {
        threshold <- quantile(x, p, na.rm = TRUE)
        pmin(x, threshold)
      }
      
      decennial_2010_2020 <- decennial_2010_2020 %>%
        group_by(year) %>%
        mutate(census_occ_housing_units = winsorize_upper(census_occ_housing_units)) %>% ungroup()


    # ensure all tract - years compiled
    decennial_2010_2020 %>%
        count(year) %>% rename(tracts = n)
          
    
    
# crosswalk to 2020 tract designations.
decennial_2010 <- decennial_2010_2020 %>% filter(year == 2010) # with 2010 tract definitions
decennial_2020 <- decennial_2010_2020 %>% filter(year == 2020) %>% # with 2020 tract definitions
  mutate(geoid = as.numeric(geoid)) %>%
  select(geoid, year, census_occ_housing_units)


# load crosswalk 2010 -> 2020 tract definitions from IPUMS: https://www.nhgis.org/geographic-crosswalks
setwd(path_data)
hud_tract_crosswalk = read_excel("CENSUS_TRACT_CROSSWALK_2010_to_2020_2010.xlsx")


    # merge 2010 tract info to 2020 tract info in IPUMS crosswalk
    decennial_2010 <- decennial_2010 %>%
      rename(GEOID_2010 = geoid) %>%
      mutate(geoid = as.numeric(GEOID_2010)) %>%
      
      # merge & generate new housing estimate
      right_join(hud_tract_crosswalk, relationship = "many-to-many") %>%
      mutate(census_occ_housing_units = census_occ_housing_units*TOT_RATIO)  %>%
      
      ungroup() %>% group_by(GEOID_2020, year) %>%
      summarise(census_occ_housing_units = sum(census_occ_housing_units, na.rm = TRUE)) %>%
      
      ungroup() %>%  mutate(geoid = as.numeric(GEOID_2020)) %>%
      select(geoid, year, census_occ_housing_units)
      

    
# combine all data years expressed in 2020 tract definitions.
decennial_2010_2020_tracts_2020 <- bind_rows(decennial_2010, decennial_2020) %>%
  mutate(census_occ_housing_units = as.numeric(census_occ_housing_units)) %>% ungroup() %>%
  filter(!is.na(year))

    # compare post - crosswalk tracts.
decennial_2010_2020_tracts_2020 %>%
      count(year) %>% rename(tracts = n)

  length(unique(hud_tract_crosswalk$GEOID_2020)) # can only walk forward 83875 tracts

################################# 
# load in HUD counts of addresses
  
load("USPS_tract_vacancy_2012_2024_2020_definitions_with_earlier_years.RData")
  
# keep subset. census is collected in Q2.
USPS_data = USPS_data %>%
  filter(MONTH == "06") %>% filter(YEAR == 2010 | YEAR == 2020) %>% 
  filter(Sample == "In Clean Sample") %>%
  select(TRACT20,
         YEAR,
         ACTIVE_RESIDENTIAL_ADDRESSES)

#####################################################################
# check correlation between decennial occupied housing units by tract
# and HUD address counts.

# generate decade changes
  decennial_2010_2020_tracts_2020 <- decennial_2010_2020_tracts_2020 %>% ungroup() %>%
    pivot_wider(names_from = year, values_from = census_occ_housing_units) %>%
    
    mutate(census_occ_housing_units_chng = round(`2020` - `2010`,0)) %>%
    select(-c(contains("20")))
  
  USPS_data <- USPS_data %>% ungroup() %>%
    arrange(YEAR) %>%
    ungroup() %>% group_by(TRACT20) %>%
    pivot_wider(names_from = YEAR, values_from = ACTIVE_RESIDENTIAL_ADDRESSES) %>%
    mutate(usps_residential_addreses_chng = `2020` - `2010`) %>%
    mutate(geoid = as.numeric(TRACT20)) %>%
    select(-c(`2020`, `2010`)) 
 

# combine for comparison
USPS_decennial = decennial_2010_2020_tracts_2020 %>%
  right_join(USPS_data) %>% # right join for proper subset
  na.omit()


# graph
USPS_decennial %>%
  filter(usps_residential_addreses_chng > -5000) %>%
  filter(usps_residential_addreses_chng < 5000) %>%
  ggplot(aes(y = usps_residential_addreses_chng, x = census_occ_housing_units_chng)) +
  geom_point(alpha = 0.7, color = 'grey') +
  stat_summary_bin(fun = "mean", bins = 50, color = 'blue',geom = "point") +
  labs(x = "Census occupied housing units",
         y = "USPS residential units",
         title = "Change in residential units 2010 -> 2020 \n binned scatter") +
  theme_minimal()

# save
setwd(project_path)
ggsave("census_usps_2010_2020_bin_overlay.png", bg = "white")  # Save in 'figures/' directory

  
cor(USPS_decennial$usps_residential_addreses_chng, USPS_decennial$census_occ_housing_units_chng)
