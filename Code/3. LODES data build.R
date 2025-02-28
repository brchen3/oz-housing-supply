
# Sarah Eckhardt 01/10/2025

# https://lehd.ces.census.gov/data/#lodes

# EHD Origin-Destination Employment Statistics (LODES) 
# from 2012 to 2022 (last year available) 

# workers coming into a tract
# workers living in a tract

# to see the number of workers coming into a tract 
# and workers living in a tract. 

# Crosswalk to have everything in 2020 census tract definitions.
# Should be compliant with the HUD data

rm(list = ls())

library(dplyr)
library(tidyr)
library(openxlsx)
library(R.utils)

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
path_data <- file.path(path_project, "Data")
path_data_crosswalks <- file.path(path_data, "MCDC crosswalks") # https://mcdc.missouri.edu/applications/geocorr2022.html
path_data_lodes <- file.path(path_data, "LODES") # Longitudinal Employer-Household Dynamics https://lehd.ces.census.gov/

################################################################################
# read in Census Block - Census tract crosswalk from MCDC and construct.
# block-based crosswalks are only downloadable in 13 state increments per MCDC's policy.
setwd(path_data_crosswalks)
mcdc_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
xwalk_raw <- bind_rows(lapply(mcdc_files, read.csv)) %>% filter(tract !="Tract")

# reformat block and tract geocode and ensure there are no duplicates.
options(scipen = 999)
xwalk = xwalk_raw %>% mutate(block_geocode = paste0(county,tract,block),
                             block_geocode = as.numeric(gsub("\\.","",block_geocode)),
                             tract_geocode = paste0(county, tract),
                             tract_geocode = as.numeric(gsub("\\.","",tract_geocode))) %>% 
  select(tract_geocode, block_geocode) %>% distinct

# clean up.
rm(xwalk_raw)

#######################
# read in LODES data    

# Total people who live in tract who have jobs anywhere,
# vs total jobs in the tract.

setwd(path_project)
results_list = list()

files_od = list.files(paste(path_project , "od", sep="/"), pattern = "*.gz")
setwd(paste(path_project , "od", sep="/"))

for (gz_file in files_od) {

      #gunzip("gz_file", remove = FALSE) # un-comment this line if files are zipped.
      
      csv_file = gsub(".gz" , "", gz_file)
      
      year_val = substr(csv_file, 16, 19) # extract data year.
      print(csv_file)
      print(year_val)
      
      file = read.csv(paste(project_path, "od", csv_file, sep="/")) %>%
        
        mutate(year = year_val) %>%
        
        select(w_geocode, h_geocode, S000, year)
      
      results_list[[gz_file]] = file
      
}
      
results = bind_rows(results_list)


# 1. collapse total people who live in the tract with jobs anywhere
tract_resident_workers = results %>%
    left_join(xwalk, by = c("h_geocode" = "block_geocode")) %>%
    rename(h_tract_geocode = tract_geocode) %>%
    ungroup() %>%
    group_by(h_tract_geocode, year) %>%
    summarise(employed_tract_residents = sum(S000))


# 2. collapse total jobs in the tract
  tract_jobs = results %>%
        left_join(xwalk, by = c("w_geocode" = "block_geocode")) %>%
        rename(w_tract_geocode = tract_geocode) %>%
        ungroup() %>%
        group_by(w_tract_geocode, year) %>%
        summarise(jobs_in_tract = sum(S000))

  
  rm(results, file, xwalk)
  

# construct final dataset.
lodes_residentworkers_jobs = full_join(tract_resident_workers, tract_jobs, 
                          by = c("h_tract_geocode" = "w_tract_geocode",
                                 "year")) %>%
  
  filter(!is.na(h_tract_geocode)) %>%
  
  mutate(employed_tract_residents = 
           ifelse(is.na(employed_tract_residents), 0 , employed_tract_residents),
         
         jobs_in_tract = 
           ifelse(is.na(jobs_in_tract), 0 , jobs_in_tract)) %>%
  
  
  rename(tract_geocode = h_tract_geocode)


setwd(path_data_lodes)
write.csv(lodes_residentworkers_jobs, "tract_workers_and_residents.csv")
