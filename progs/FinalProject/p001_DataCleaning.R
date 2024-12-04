#########################################################################
#
# Program: p001_DataCleaning
# Purpose: The purpose of this program is to format the data in the 
#   appropriate way for the analysis 
# Author:  Sarah Bird
#
#########################################################################


# Include all packages needed
library(tidyverse)
library(survival)
library(here)
library(haven)
library(labelled)

#####################################################
#
# First, read in the data 
#
#####################################################


# Read in the data from SAS
root <- "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject"
setwd(root) # Set the working directory

# Read in the epoch dataset directly
epoch <- read_sas(here::here(root, "data", "processedData", "epoch2_20241111.sas7bdat")) %>%
  select(!c("sexcat"))

# Read in the demographics dataset
demographics <- read_sas(here::here(root, "data", "processedData", "demographics_20241111.sas7bdat"))



################################################################
#
#   Create variables and format the dataset as I want it
#
################################################################


# Step 1) Subset the data, create the obesity and overweight outcomes 
epoch2 <- epoch %>%
  select(PID, source, sex, ageyearsnew, gdm, BMI, height, 
         weight, bmi_zscore, bmi_percentile, FH28, FH33) %>%
  mutate(obesity = case_when( 
    bmi_percentile < 95 ~ 0,
    bmi_percentile >= 95 ~ 1,
    .default = 0
  ), overweight = case_when(
    bmi_percentile < 85 ~ 0,
    ((bmi_percentile >= 85) & (bmi_percentile < 95)) ~ 1,
    .default = 0
  )) 


# Step 2) Gather the covariates of our specific interest
covariates <- demographics %>%
  select(pid, child_sex, ageyrs, agemos, child_race,
         education_mother_birth:householdincome_epoch1, highestEdu_Completed, 
         householdIncome_Epoch2) %>%
  mutate(child_race = generate_Race(child_race), 
         across(c(education_mother_birth, education_mother_epoch1, highestEdu_Completed),
         generate_Edu),
  across(c(householdincome_birth, householdincome_epoch1, householdIncome_Epoch2),
         generate_Income)) %>%
  rename("PID" = "pid")

# Save demographics for a table 1
saveRDS(covariates,
        here::here(root, "data", "processedData", "table1Data_20241124.rds"))



# Step 3) Merge the longitudinal dataset to the demographics dataset that we have. This will
#           also create a lagged age variable that will motivate the counting process notation
#           that we'll need to implement for the time varying covariate piece
epoch3 <- merge(x = epoch2, 
                y = covariates,
                by = "PID",
                all.x = TRUE) %>%
  arrange(PID, ageyearsnew) %>% # Arrange by PID and increasing age 
  group_by(PID) %>% # Perform calculations by PID
  mutate(last_age = lag(ageyearsnew)) %>% # Create a lagged age variable 
  ungroup() %>% # Ungroup as to remove the PID grouping 
  filter(!is.na(last_age))  # Remove first observation




#######################################################################
#
#   Create the recurrent events dataset 
#
#######################################################################


# Step 4) This creates the dataset for the recurrent data analysis. This will
#           create the last age variable and a variable that identifies the last
#           observation for a single participant
epoch4 <- merge(x = epoch2, 
                y = covariates,
                by = "PID",
                all.x = TRUE) %>%
  group_by(PID) %>% # Perform calculations by PID
  mutate(last_age = lag(ageyearsnew),
         is_last = if_else(row_number() == n(), 1, 0)) %>% 
  ungroup() %>%
  filter(!is.na(last_age)) %>%
  relocate(PID, source, sex, last_age, ageyearsnew, gdm, bmi_percentile, obesity, overweight, is_last) 

# Step 4) This is the reccurrent dataset!!! Save this for analysis
saveRDS(epoch4,
        "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject/data/processedData/analysis_datasets/recurrentEPOCH_20241113.rds")



############################################################
#
# Create the dataset for time-varying covariates
#
############################################################




# Step 3) Create the time-varying education covariate
data_long <- epoch4 %>%
  arrange(PID, ageyearsnew) %>% # arrange in order of PID and increasing age
  mutate(temp_age = round(ageyearsnew, digits = 2),
         temp_age2 = round(ageyrs, digits = 2)) %>% 
  mutate( eduation_mother = case_when(
    temp_age < temp_age2 ~ education_mother_birth,
    (temp_age >= temp_age2) & (source != "EPOCH Visit 2") ~ education_mother_epoch1,
    (source == "EPOCH Visit 2") ~ highestEdu_Completed,
    .default = NA
  )) %>%
  mutate(household_income = case_when(
    temp_age < temp_age2 ~ householdincome_birth,
    (temp_age >= temp_age2) & (source != "EPOCH Visit 2") ~ householdincome_epoch1,
    (temp_age >= temp_age2) & ((source == "EPOCH Visit 2") & is.na(householdIncome_Epoch2)) ~  householdincome_epoch1,
    (source == "EPOCH Visit 2") ~ householdIncome_Epoch2,
    .default = NA
  )) %>%
  select(PID:bmi_zscore, child_sex, child_race, eduation_mother, household_income) %>%
  group_by(PID) %>%
  mutate(
    # Flag first occurrence of obesity
    first_obesity = ifelse(obesity == 1 & cumsum(obesity) == 1, 1, 0),
    # Flag first occurrence of overweight
    first_overweight = ifelse(overweight == 1 & cumsum(overweight) == 1, 1, 0),
  ) %>%
  arrange(PID, ageyearsnew) %>%
  ungroup() %>%
  relocate(PID, source, sex, last_age, ageyearsnew, bmi_percentile, 
           obesity, first_obesity,
           overweight, first_overweight)

# Check to see if the education variables make sense and are what is expected
test <- data_long %>%
  select(PID, temp_age, temp_age2, source, education_mother_birth, education_mother_epoch1,
         highestEdu_Completed, eduation_mother)

# Check to see if the income variables make sense and are what is expected
test <- data_long %>%
  select(PID, temp_age, temp_age2, source, householdincome_birth, householdincome_epoch1,
         householdIncome_Epoch2, household_income)





# Step 4) Finalize the obesity outcome dataset. This chunk of code will remove
#           observations after the first instance of obesity and retain all obs
#           from participants who have no instances of obesity
obesity_dataset <- data_long %>%
  group_by(PID) %>%
  mutate(first_obesity_index = ifelse(any(first_obesity == 1), min(which(first_obesity == 1)), NA)) %>%
  filter(is.na(first_obesity_index) | row_number() <= first_obesity_index) %>%
  ungroup() %>%
  select(!c("overweight", "first_overweight"))


# Step 4) Finalize the obesity outcome dataset. This chunk of code will remove
#           observations after the first instance of obesity and retain all obs
#           from participants who have no instances of obesity
overweight_dataset <- data_long %>%
  group_by(PID) %>%
  mutate(first_overweight_index = ifelse(any(first_overweight == 1), min(which(first_overweight == 1)), NA)) %>%
  filter(is.na(first_overweight_index) | row_number() <= first_overweight_index) %>%
  ungroup() %>%
  select(!c("obesity", "first_obesity"))




# Save these two datasets
saveRDS(obesity_dataset, 
        here::here(root, "data", "processedData", "analysis_datasets",
                   "obesityDataset_20241116.rds"))

saveRDS(overweight_dataset, 
        here::here(root, "data", "processedData", "analysis_datasets",
                   "overweightDataset_20241116.rds"))


