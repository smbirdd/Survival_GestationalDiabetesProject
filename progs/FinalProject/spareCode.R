##########################################################################
#
# Program: spareCode
# Purpose: The purpose of this program is to generate exploratory 
#   plots that will show the data and the cohort of kids that are 
#   represented 
#
#########################################################################


# Step 2) Sort the data by PID and age in years
epoch4 <- merge(x = epoch2, 
                y = covariates,
                by = "PID",
                all.x = TRUE) %>%
  mutate(ageyearsnew = round(ageyearsnew, digits = 2),
         ageyrs = round(ageyrs, digits = 2)) %>%
  filter( !( (source == "MRA") & (ageyearsnew == ageyrs) ) ) 

epoch_analysis <- epoch2 %>%
  arrange(PID, ageyearsnew) %>%
  select(!source) 


# Step 3) Identify the first obesity event for each participant
data_long <- epoch4 %>%
  group_by(PID) %>%
  mutate(
    # Flag first occurrence of obesity
    first_obesity = ifelse(obesity == 1 & cumsum(obesity) == 1, 1, 0),
    # Time when the first obesity event occurs (if applicable)
    t_event_obesity = ifelse(first_obesity == 1, ageyearsnew, NA),
    # Flag first occurrence of overweight
    first_overweight = ifelse(overweight == 1 & cumsum(overweight) == 1, 1, 0),
    # Time when the first overweight event occurs (if applicable)
    t_event_overweight = ifelse(first_overweight == 1, ageyearsnew, NA)
  ) %>%
  arrange(PID, ageyearsnew) %>%
  ungroup() %>%
  mutate( eduation_mother = case_when(
    ageyearsnew < ageyrs ~ education_mother_birth,
    (ageyearsnew >= ageyrs) & (source != "EPOCH Visit 2") ~ education_mother_epoch1,
    (source == "EPOCH Visit 2") ~ highestEdu_Completed,
    .default = NA
  ))

test <- data_long %>%
  filter(PID == 20034)

########################################################
#
# Build the survival obesity dataset first
#
########################################################

# Extract single rows where the participants are first obese
obese_obs <- data_long %>%
  filter(first_obesity == 1) %>%
  rename("t_event" = "t_event_obesity",
         "status" = "obesity") %>%
  select(PID, sex, ageyearsnew, gdm, FH28, FH33, t_event, status)

# Extract the PIDs of participants who are obese
obese_ids <- obese_obs %>%
  select(PID) %>%
  unique() %>%
  pull() 

# Extract single rows where the participants are not obese
notObese_obs <- data_long %>%
  filter( !(PID %in% obese_ids)) %>%
  group_by(PID) %>%
  slice_tail() %>%
  rename("t_event" = "t_event_obesity",
         "status" = "obesity") %>%
  mutate(t_event = ageyearsnew) %>%
  select(PID, sex, ageyearsnew, gdm, FH28, FH33, t_event, status)


# Set the two data sets on top of one another
analysis_dataset <- rbind(obese_obs, notObese_obs)



########################################################
#
# Build the survival overweight dataset first
#
########################################################

# Extract single rows where the participants are first obese
overweight_obs <- data_long %>%
  filter(first_overweight == 1) %>%
  rename("t_event" = "t_event_overweight",
         "status" = "overweight") %>%
  select(PID, sex, ageyearsnew, gdm, FH28, FH33, t_event, status)

# Extract the PIDs of participants who are obese
overweight_ids <- overweight_obs %>%
  select(PID) %>%
  unique() %>%
  pull() 

# Extract single rows where the participants are not obese
notOverweight_obs <- data_long %>%
  filter( !(PID %in% overweight_ids)) %>%
  group_by(PID) %>%
  slice_tail() %>%
  rename("t_event" = "t_event_overweight",
         "status" = "overweight") %>%
  mutate(t_event = ageyearsnew) %>%
  select(PID, sex, ageyearsnew, gdm, FH28, FH33, t_event, status)


# Set the two data sets on top of one another
analysis_dataset_overweight <- rbind(overweight_obs, notOverweight_obs)


# Save these two datasets
saveRDS(analysis_dataset, 
        "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject/data/processedData/analysis_datasets/obese_dataset_20241106.rds")

saveRDS(analysis_dataset_overweight, 
        "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject/data/processedData/analysis_datasets/overweight_dataset_20241106.rds")

