#########################################################################
#
# Program: p003_ModelFitting
# Purpose: The purpose of this program is to fit the models associated
#             with each aim
# Author:  Sarah Bird
#
#########################################################################

# Load packages
library(survival)
library(tidyverse)
library(survminer)
library(here)
library(broom)
library(kableExtra)

###################################################
#
#   Read in the data
#
###################################################


# Read in data needed 
root <- "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject"
source(here::here(root, "progs", "FinalProject", "p10_FunctionsFile.R"))
obesity <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                              "obesityDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))

overweight <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                                 "overweightDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))

recurrent <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                                "recurrentEPOCH_20241113.rds"))


###################################################
#
#   Create categories for education AND income
#
###################################################

obesity2 <- obesity %>%
  rename("education_mother" = "eduation_mother") %>%
  mutate(education_mother2 = case_when(
    education_mother %in% c("Less than High School Diploma", "High School Diploma", 
                            "Some College") ~ "HighSchoolorLT",
    education_mother == "Bachelor's Degree" ~ "Bachelors",
    (education_mother %in% c("Associate Degree", "Business/Technical School",
                             "Professional or Doctorate Degree", "Masters Degree")) ~ "MoreThanBach",
    education_mother == "Don't Know" ~ "Unknown",
    .default = "99"
  )) %>%
  mutate(education_mother3 = case_when(
    education_mother %in% c("Less than High School Diploma",
                            "Some College") ~ "LessThanHS",
    education_mother %in% c("Don't Know") ~ "Don't Know",
    .default = "MoreThanHS"
  )) %>%
  mutate(education_mother2 = factor(education_mother2, 
                                    levels = c("HighSchoolorLT", "Bachelors", "MoreThanBach",
                                               "Unknown")),
         education_mother3 = factor(education_mother3,
                                    levels = c("LessThanHS", "MoreThanHS", "Don't Know")))

table(obesity2$education_mother, obesity2$education_mother2)
table(obesity2$education_mother, obesity2$education_mother3)


obesity3 <- obesity2 %>%
  mutate(household_income2 = case_when(
    (household_income == "$0-$24,999") | (household_income == "$25,000-$49,999") ~ "LT50,000",
    .default = household_income
  )) %>%
  mutate(household_income3 = case_when(
    (household_income == "$25,000-$49,999") | 
      (household_income == "$50,000-$74,999") ~ "$25,000-$74,999",
    .default = household_income
  ), household_income4 = case_when(
    household_income %in% c("$0-$24,999", "$25,000-$49,999") ~ "LT50,000",
    household_income %in% c("$50,000-$74,999", "$75,000+") ~ "GT50,000",
    .default = household_income
  ),
  household_income5 = case_when(
    household_income %in% c("$0-$24,999") ~ "0to24,999",
    household_income %in% c("$25,000-$49,999") ~ "25to49,999",
    household_income %in% c("$50,000-$74,999", "$75,000+") ~ "GT50,000",
    .default = household_income
  ))   %>%
  mutate(household_income2 = factor(household_income2, 
                                    levels = c("LT50,000", "$50,000-$74,999",
                                               "$75,000+", "Don't know or Refused")),
         household_income3 = factor(household_income3, 
                                    levels = c("$0-$24,999", "$25,000-$74,999",
                                               "$75,000+", "Don't know or Refused")),
         household_income4 = factor(household_income4,
                                    levels = c("LT50,000", "GT50,000", "Don't know or Refused")),
         household_income5 = factor(household_income5,
                                    levels = c("0to24,999", "25to49,999", 
                                               "GT50,000", "Don't know or Refused")))

table(obesity3$household_income, obesity3$household_income2)
table(obesity3$household_income, obesity3$household_income3)
table(obesity3$household_income, obesity3$household_income4)
table(obesity3$household_income, obesity3$household_income5)


overweight2 <- overweight %>%
  rename("education_mother" = "eduation_mother") %>%
  mutate(education_mother2 = case_when(
    education_mother %in% c("Less than High School Diploma", "High School Diploma", 
                            "Some College") ~ "HighSchoolorLT",
    education_mother == "Bachelor's Degree" ~ "Bachelors",
    (education_mother %in% c("Associate Degree", "Business/Technical School",
                             "Professional or Doctorate Degree", "Masters Degree")) ~ "MoreThanBach",
    education_mother == "Don't Know" ~ "Unknown",
    .default = "99"
  )) %>%
  mutate(education_mother3 = case_when(
    education_mother %in% c("Less than High School Diploma",
                            "Some College") ~ "LessThanHS",
    education_mother %in% c("Don't Know") ~ "Don't Know",
    .default = "MoreThanHS"
  )) %>%
  mutate(education_mother2 = factor(education_mother2, 
                                    levels = c("HighSchoolorLT", "Bachelors", "MoreThanBach",
                                               "Unknown")),
         education_mother3 = factor(education_mother3,
                                    levels = c("LessThanHS", "MoreThanHS", "Don't Know")))

table(overweight2$education_mother, overweight2$education_mother2)
table(overweight2$education_mother, overweight2$education_mother3)

overweight3 <- overweight2 %>%
  mutate(household_income2 = case_when(
    (household_income == "$0-$24,999") | (household_income == "$25,000-$49,999") ~ "LT50,000",
    .default = household_income
  )) %>%
  mutate(household_income3 = case_when(
    (household_income == "$25,000-$49,999") | 
      (household_income == "$50,000-$74,999") ~ "$25,000-$74,999",
    .default = household_income
  ), household_income4 = case_when(
    household_income %in% c("$0-$24,999", "$25,000-$49,999") ~ "LT50,000",
    household_income %in% c("$50,000-$74,999", "$75,000+") ~ "GT50,000",
    .default = household_income
  ),
  household_income5 = case_when(
    household_income %in% c("$0-$24,999") ~ "0to24,999",
    household_income %in% c("$25,000-$49,999") ~ "25to49,999",
    household_income %in% c("$50,000-$74,999", "$75,000+") ~ "GT50,000",
    .default = household_income
  ))   %>%
  mutate(household_income2 = factor(household_income2, 
                                    levels = c("LT50,000", "$50,000-$74,999",
                                               "$75,000+", "Don't know or Refused")),
         household_income3 = factor(household_income3, 
                                    levels = c("$0-$24,999", "$25,000-$74,999",
                                               "$75,000+", "Don't know or Refused")),
         household_income4 = factor(household_income4,
                                    levels = c("LT50,000", "GT50,000", "Don't know or Refused")),
         household_income5 = factor(household_income5,
                                    levels = c("0to24,999", "25to49,999", 
                                               "GT50,000", "Don't know or Refused")))

table(overweight3$household_income, overweight3$household_income2)
table(overweight3$household_income, overweight3$household_income3)
table(overweight3$household_income, overweight3$household_income4)
table(overweight3$household_income, overweight3$household_income5)

###################################################
#
#   Fit original cox proportional hazards models
#
###################################################


# Pre-processing Step: Create a tibble object that will store all final models,
#   coefficients, and information on which model was selected and why
models <- tibble(model_type = c("Aim1_Obesity", "Aim2_Overweight"))
# Initalize columns
models$covariate_model       <- vector("list", nrow(models))
models$stratified_model      <- vector("list", nrow(models))



##################################
#
#   Aim 1: Obesity Outcome, time to first instance of becoming obese
#
##################################

# Model 1) Fit a model with a fixed effect for exposure to 
#           gestational diabetes
fit_obesity <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm,
                        data = obesity)
summary(fit_obesity)

# Model 2) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for mother's education. Categories
#           for this are 1) less than or equal to High School, 2) Bachelors degree
#           3) More than bachelors degree, and 4) Unknown
fit_obesity_education <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm + 
                                    education_mother2,
                                    data = obesity2)
summary(fit_obesity_education)

# Model 3) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for mother's education. Categories
#           for this are 1) High School or Less, 2) More than High School
#           3) Don't Know
fit_obesity_education2 <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm + education_mother3,
                                  data = obesity2)
summary(fit_obesity_education2)


# Model 4) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for household income. The levels
#           in this analysis are 1) $0 - $24,999, 2) $25,000-$49,999, 3) $50,000-$74,999,
#           4) $75,000+, and 5) Don't know or refused
fit_obesity_income <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income,
                              data = obesity2)
summary(fit_obesity_income)
proportionalityTest <- cox.zph(fit_obesity_income) # Not proportional
proportionalityTest2 <- cox.zph(fit_obesity_income, transform = "log") # log transformation satisfies

# Examine the AICs of both models 
AIC(fit_obesity_education2)   # 2236.764
AIC(fit_obesity_income)       # 2229.68
## Through examining these two models, we can see that the cox model including
##    household income has a smaller AIC (approximately by 7 units), so we can
##    proceed with that model as our final model (fit_obesity_income)


# Model 5) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for household income. The levels
#           in this analysis are 1) $0 -$49,999, 3) $50,000-$75,000+, 
#           and 5) Don't know or refused
fit_obesity_income <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income4,
                            data = obesity3)
summary(fit_obesity_income)
cox.zph(fit_obesity_income) # Not proportional
cox.zph(fit_obesity_income, transform = "log") # shown to be proportional?


# Model 6) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for household income. The levels
#           in this analysis are 1) $0 - $24,999, 2) $25,000-$49,999, 3) $50,000+, and 
#           4) Don't know or refused
fit_obesity_income2 <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income5,
                            data = obesity3)
summary(fit_obesity_income2) # The interaction terms are significant, proportionality is not upheld
cox.zph(fit_obesity_income2) # Not proportional



# Model 7) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with household_income being stratified
fit_obesity_income_strata <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm + strata(household_income5),
                                    data = obesity3)
summary(fit_obesity_income_strata)
AIC(fit_obesity_income_strata) # The AIC is 1955.794
cox.zph(fit_obesity_income_strata) # Proportional




##################
#
# Save models as final ones
#
#################

# Save the income model with three categories and the stratified model
models$covariate_model[[1]]  <- fit_obesity_income2
models$stratified_model[[1]] <- fit_obesity_income_strata

# Perform likelihood ratio test to determine if we need household income
anova(fit_obesity_income2, fit_obesity)



##################################
#
#   Aim 2: Overweight Outcome, time to first instance of becoming overweight
#
##################################


# Aim 2, Outcome = time to first instance of being overweight
fit_overweight <- coxph(Surv(last_age, ageyearsnew, overweight) ~ gdm,
                          data = overweight)
summary(fit_overweight)

# Model 2) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for mother's education. Categories
#           for this are 1) less than or equal to High School, 2) Bachelors degree
#           3) More than bachelors degree, and 4) Unknown
fit_overweight_education <- coxph(Surv(last_age, ageyearsnew, overweight) ~ gdm + 
                                    education_mother2,
                                    data = overweight2)
summary(fit_overweight_education)

# Model 3) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for mother's education. Categories
#           for this are 1) High School or Less, 2) More than High School
#           3) Don't Know
fit_overweight_education2 <- coxph(Surv(last_age, ageyearsnew, overweight) ~ gdm + 
                                     education_mother3,
                                     data = overweight2)
summary(fit_overweight_education2)

# Model 4) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for household income. The levels
#           in this analysis are 1) $0 - $24,999, 2) $25,000-$49,999, 3) $50,000-$74,999,
#           4) $75,000+, and 5) Don't know or refused
fit_overweight_income <- coxph(Surv(last_age, ageyearsnew, overweight) ~ gdm + 
                                 household_income,
                                 data = overweight3)
summary(fit_overweight_income)
cox.zph(fit_overweight_income) # Proportional


AIC(fit_overweight_education2)  # 3095.191
AIC(fit_overweight_income)      # 3093.31
## These models are within 2 units of the AIC from each other. I would deign
##    to say that these models perform very similarly, thus, I would go with 
##    the model containing income to be consistent with the previous result


# Model 5) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for household income. The levels
#           in this analysis are 1) $0-$49,999, 3) $50,000-$75,000+, and 
#           5) Don't know or refused
fit_overweight_income2 <- coxph(Surv(last_age, ageyearsnew, overweight) ~ gdm + 
                                 household_income4,
                               data = overweight3)
summary(fit_overweight_income2)
cox.zph(fit_overweight_income2) # Not proportional

# Model 6) Fit a model with a fixed effect for exposure to gestational diabetes
#           along with stratification for household income
fit_overweight_income_strata <- coxph(Surv(last_age, ageyearsnew, overweight) ~ gdm + 
                                  strata(household_income),
                                data = overweight3)
summary(fit_overweight_income_strata)
cox.zph(fit_overweight_income_strata) # Not proportional


# COMPARISON OF MODELS FIT

AIC(fit_overweight_education2)        # 3095.191
AIC(fit_overweight_income)            # 3093.31
AIC(fit_overweight_income2)           # 3093.178
AIC(fit_overweight_income_strata)     # 2416.162
## These models are within 2 units of the AIC from each other. I would deign
##    to say that these models perform very similarly, thus, I would go with 
##    the model containing income to be consistent with the previous result





##################
#
# Save models as final ones
#
#################

# Save the income model with three categories and the stratified model
models$covariate_model[[2]]  <- fit_overweight_income
models$stratified_model[[2]] <- fit_overweight_income_strata


saveRDS(models,
        here::here(root, "data", "processedData", "analysis_results", "models_20241123.rds"))




#############################################
#
#   Final Models: Table
#
#############################################

# Read in the model
models <- readRDS(here::here(root, "data", "processedData", "analysis_results", "models_20241123.rds"))

#############################
#
# Create a table to present the obesity results
#
#############################

# Extract the obesity fit
fit_obesity <- models$covariate_model[[1]]

# Create the table

obesity_table <- tidy(fit_obesity, exp = TRUE, conf.int = TRUE) %>%
  select(!c(statistic, std.error)) %>%
  mutate(term = case_when(
    term == "gdm" ~ "Gestational Diabetes",
    term == "household_income525to49,999" ~ "Household Income: $25,000-$49,999",
    term == "household_income5GT50,000" ~ "Household Income: Greater than $50,000",
    term == "household_income5Don't know or Refused" ~ "Household Income: Don't Know or Refused",
  )) %>%
  knitr::kable(col.names = c("Term", "Estimate", "P-value", "95% Lower Bound",
                             "95% Upper Bound"),
               digits = 3,
               booktabs = TRUE) %>%
  kableExtra::kable_styling(c("striped", "scale_down"))

writeTex(obesity_table, 
         destination = here::here(root, "output", "Tables"),
         texname = "Aim1_Table.tex")  # Write LaTeX code to file



#############################
#
# Create a table to present the overweight results
#
#############################


# Extract the obesity fit
fit_overweight <- models$covariate_model[[2]]

# Create the table

overweight_table <- tidy(fit_overweight, exp = TRUE, conf.int = TRUE) %>%
  select(!c(statistic, std.error)) %>%
  mutate(term = case_when(
    term == "gdm" ~ "Gestational Diabetes",
    term == "household_income$25,000-$49,999" ~ "Household Income: $25,000-$49,999",
    term == "household_income$50,000-$74,999" ~ "Household Income: $50,000-$74,999",
    term == "household_income$75,000+ " ~ "Household Income: $75,000+",
    term == "household_incomeDon't know or Refused" ~ "Household Income: Don't Know or Refused",
  )) %>%
  knitr::kable(col.names = c("Term", "Estimate", "P-value", "95% Lower Bound",
                             "95% Upper Bound"),
               digits = 3,
               booktabs = TRUE) %>%
  kableExtra::kable_styling(c("striped", "scale_down"))

writeTex(overweight_table, 
         destination = here::here(root, "output", "Tables"),
         texname = "Aim2_Table.tex")  # Write LaTeX code to file





