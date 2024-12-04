#########################################################################
#
# Program: p004_ModelDiagnostics
# Purpose: The purpose of this program is to compute model diagnostic
#   plots for the survival data analysis final project
# Author:  Sarah Bird
#
#########################################################################

# Load in packages needed
library(tidyverse)
library(survival)
library(survminer)
library(RColorBrewer)
library(viridis)

##############################
#
# Load working space
#
##############################

# Read in data needed 
root <- "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject"
obesity <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                              "obesityDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))
obesity3 <- obesity %>%
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


overweight <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                                 "overweightDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))


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

overweight3 <- overweight %>%
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
  ))   %>%
  mutate(household_income2 = factor(household_income2, 
                                    levels = c("LT50,000", "$50,000-$74,999",
                                               "$75,000+", "Don't know or Refused")),
         household_income3 = factor(household_income3, 
                                    levels = c("$0-$24,999", "$25,000-$74,999",
                                               "$75,000+", "Don't know or Refused")),
         household_income4 = factor(household_income4,
                                    levels = c("LT50,000", "GT50,000", "Don't know or Refused"))) 


# Read in the models 
models <- readRDS(here::here(root, "data", "processedData", "analysis_results", "models_20241123.rds"))

# Pre-processing Step: Create a tibble object that will store all plots
diagnosticPlots <- tibble(model_type = c("Aim1_Obesity", "Aim2_Overweight"))
# Initalize columns
diagnosticPlots$coxsnell                <- vector("list", nrow(diagnosticPlots))
diagnosticPlots$deviance_residuals      <- vector("list", nrow(diagnosticPlots))
diagnosticPlots$dfbetas                 <- vector("list", nrow(diagnosticPlots))

###########################################################################
#
#
#   Aim 1 Models: Time to Obesity
#
#
###########################################################################


#######################
#
#   Model 1) Covariate model
#
#######################

# Pull in the model 
obesity_covariate <- models$covariate_model[[1]]


#######################
#
# Examine cox snell residuals
#
#######################


# Fit null model to get Nelson-Aalen estimator
obesity$csresidCovariate <- abs(obesity3$obesity - obesity_covariate$residuals)
fit1 <- coxph(Surv(csresidCovariate, obesity) ~ 1, data = obesity, method = c("efron"))
km <- survfit(fit1, type = "aalen")

# Create cox-snell residuals to examine overall fit
plot_cs <- tibble(time = summary(km)$time,
                  surv = summary(km)$surv)
obesity_cs <- plot_cs %>%
                ggplot(aes(x = time, y = -log(surv))) +
                geom_point() +
                geom_abline(intercept = 0, slope = 1) +
                labs(x = "Time",
                     y = "Cox Snell Residual")

diagnosticPlots$coxsnell[[1]] <- obesity_cs


#######################
#
# Examine deviance residuals
#
#######################

plot_devResid <- tibble(deviance_residuals = resid(obesity_covariate, type = "deviance"),
                        gdm = as.character(obesity3$gdm),
                        household_income = obesity3$household_income4) %>%
  pivot_longer(cols = gdm:household_income,
               names_to = "variable",
               values_to = "type")

obesity_devResid <- plot_devResid %>%
                      mutate(variable = ifelse(variable == "gdm", "Gestational Diabetes", "Household Income")) %>%
                      ggplot(aes(x = type, y = deviance_residuals)) +
                      geom_point() +
                      facet_wrap(~ variable, scales = "free") + 
                      labs(x = NULL,
                           y = "Deviance Residuals")

diagnosticPlots$deviance_residuals[[1]] <- obesity_devResid


#######################
#
# Examine dfbetas
#
#######################

obesity_dfbetas <- resid(obesity_covariate, type = "dfbetas")
sequence <- seq(1:nrow(obesity3))
plot(sequence, obesity_dfbetas[,1], type = "h")
plot(sequence, obesity_dfbetas[,2], type = "h")
plot(sequence, obesity_dfbetas[,3], type = "h")
plot(sequence, obesity_dfbetas[,4], type = "h") # CONCERNING OBSERVATIONS HERE 

obesity_dfbetas <- data.frame(seq = sequence, 
                       var_gdm = obesity_dfbetas[,1],
                       var_household25to49 = obesity_dfbetas[,2],
                       var_householdGT50  = obesity_dfbetas[,3],
                       var_householdDontKnow = obesity_dfbetas[,4]
                       ) %>%
  pivot_longer(cols = var_gdm:var_householdDontKnow,
               values_to = "deviance_resids",
               names_to = "variable")

obesity_dfbetas_plot <- obesity_dfbetas %>%
  mutate(flag = factor(ifelse(deviance_resids > 0.4, 1, 0)),
         variable = case_when(
           variable == "var_gdm" ~ "Gestational Diabetes",
           variable == "var_householdDontKnow" ~ "House Income: Don't know",
           variable == "var_householdGT50" ~ "House Income: Greater than $50,000",
           variable == "var_household25to49" ~ "House Income: $25,000 to $49,999"
         )) %>%
  ggplot(aes(x = seq, y = deviance_resids, group = flag, color = flag)) +
  geom_point() + 
  geom_segment( aes(x=seq, xend=seq, y=0, yend=deviance_resids)) +
  facet_wrap(~variable) +
  theme(legend.position="none") +
  labs(x = "Observation",
       y = "Difference in Betas") +
  scale_color_brewer(palette = "Set2")


diagnosticPlots$dfbetas[[1]] <- obesity_dfbetas_plot

  

# Pull out WHAT the influential observation was
influential_obesity <- obesity_dfbetas %>%
  filter(deviance_resids > 0.4) %>%
  select(seq) %>%
  pull()

# Identify the observations
influential_obs <- obesity %>% 
                    mutate(seq = 1:6265) %>%
                    filter(seq %in% c(5048, 6132))

# Remove influential observations 
obesity4 <- obesity3 %>%
  mutate(seq = 1:6265) %>%
  filter( !((seq == 5048) | (seq == 6132)) )

# Refit the model to see how things change
#  Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for household income. The levels
#           in this analysis are 1) $0 -$49,999, 3) $50,000-$75,000+, 
#           and 5) Don't know or refused
fit_obesity_income <- coxph(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income4,
                            data = obesity4)
summary(fit_obesity_income)
cox.zph(fit_obesity_income) # Not proportional





#######################
#
#   Outcome 2) Overweight
#
#######################


# Pull in the model 
overweight_covariate <- models$covariate_model[[2]]

#######################
#
# Examine cox snell residuals
#
#######################

# Fit null model to get Nelson-Aalen estimator
overweight3$csresid <- abs(overweight3$overweight - overweight_covariate$residuals)
fit1 <- coxph(Surv(csresid, overweight) ~ 1, data = overweight3, method = c("efron"))
km <- survfit(fit1, type = "aalen")

# Create cox-snell residuals to examine overall fit
plot_cs <- tibble(time = summary(km)$time,
                  surv = summary(km)$surv)
overweight_csResid <- plot_cs %>%
                        ggplot(aes(x = time, y = -log(surv))) +
                        geom_point() +
                        geom_abline(intercept = 0, slope = 1) +
                        labs(x = "Time",
                             y = "Cox Snell Residual") +
                        theme_bw()

diagnosticPlots$coxsnell[[2]] <- overweight_csResid

#######################
#
# Examine deviance residuals
#
#######################

plot_devResid <- tibble(deviance_residuals = resid(overweight_covariate, type = "deviance"),
                        gdm = as.character(overweight3$gdm),
                        household_income = overweight3$household_income) %>%
  pivot_longer(cols = gdm:household_income,
               names_to = "variable",
               values_to = "type")

overweight_devResidPlot <- plot_devResid %>%
  mutate(variable = case_when(
    variable == "gdm" ~ "Gestational Diabetes",
    variable == "household_income" ~ "Household Income",
    .default = variable
  )) %>%
  ggplot(aes(x = type, y = deviance_residuals)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Variable",
       y = "Deviance Residuals")

diagnosticPlots$deviance_residuals[[2]] <- overweight_devResidPlot


#######################
#
# Examine dfbetas
#
#######################

overweight_dfbetas <- resid(overweight_covariate, type = "dfbetas")
sequence <- seq(1:nrow(overweight3))

plotting_overweight <- data.frame(seq = sequence, 
                                   var_gdm = overweight_dfbetas[,1],
                                   var_household25_49999 = overweight_dfbetas[,2],
                                   var_household50_74999 = overweight_dfbetas[,3],
                                   var_household75Plus = overweight_dfbetas[,4],
                                   var_householdDontKnow = overweight_dfbetas[,5]) %>%
              pivot_longer(cols = var_gdm:var_householdDontKnow,
                           values_to = "deviance_resids",
                           names_to = "variable")

overweight_dfbetas <- plotting_overweight %>%
  mutate(flag = factor(ifelse(deviance_resids > 0.4, 1, 0)),
         variable = case_when(
           variable == "var_gdm" ~ "Gestational Diabetes",
           variable == "var_householdDontKnow" ~ "House Income: Don't know",
           variable == "var_household25_49999" ~ "House Income: $25,000 to $49,999",
           variable == "var_household75Plus" ~ "House Income: $75,000+",
           variable == "var_household50_74999" ~ "House Income: $50,000 to $74,999",
         )) %>%
  ggplot(aes(x = seq, y = deviance_resids, group = flag, color = flag)) +
  geom_point() + 
  geom_segment( aes(x=seq, xend=seq, y=0, yend=deviance_resids)) +
  facet_wrap(~variable, scales = "free") +
  theme(legend.position="none") +
  labs(x = "Observation",
       y = "Difference in Betas")

diagnosticPlots$dfbetas[[2]] <- overweight_dfbetas

# Pull out WHAT the influential observation was
influential_overweight <- plotting_overweight %>%
  filter(deviance_resids > 0.4) %>%
  select(seq) %>%
  pull()

# Identify the observations
influential_obs <- overweight3 %>% 
  mutate(seq = 1:5627) %>%
  filter(seq %in% c(424))

# Remove influential observations 
overweight4 <- overweight3 %>%
  mutate(seq = 1:5627) %>%
  filter( !(seq == 424)  )

# Refit the model to see how things change
#  Fit a model with a fixed effect for exposure to gestational diabetes
#           along with a time varying covariate for household income. The levels
#           in this analysis are 1) $0 -$49,999, 3) $50,000-$75,000+, 
#           and 5) Don't know or refused
fit_overweight_income2 <- coxph(Surv(last_age, ageyearsnew, overweight) ~ gdm + household_income,
                                  data = overweight4)
summary(fit_overweight_income2)
cox.zph(fit_overweight_income2) # Not proportional




###############################################
#
# Save the diagnostic plots
#
###############################################

# Save these plots as an RDS file
saveRDS(diagnosticPlots,
        here::here(root, "data/processedData/analysis_results/diagnosticPlots_20241124.rds"))



