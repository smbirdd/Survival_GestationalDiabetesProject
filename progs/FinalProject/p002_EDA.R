#########################################################################
#
# Program: p002_EDA
# Purpose: The purpose of this program is to generate exploratory 
#   plots that will show the data and the cohort of kids that are 
#   represented 
# Author:  Sarah Bird
#
#########################################################################



# Include all packages needed
library(tidyverse)
library(survival)
library(here)
library(haven)
library(labelled)
library(reReg)


# Read in data needed 
root <- "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject"
obesity <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                              "obesityDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))

overweight <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                              "overweightDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))

recurrent <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                                "recurrentEPOCH_20241113.rds"))


####################################################################
#
#
#   Create visualizations of the data
#
#
####################################################################

epoch2 %>%
  mutate(obesity = factor(obesity)) %>%
  filter( (source == "EPOCH Visit 2") |  (source == "EPOCH Visit 1")) %>%
  ggplot() +
  geom_histogram(aes(x = bmi_zscore, group = obesity, fill = obesity)) +
  facet_wrap(~source)

epoch2 %>%
  mutate(overweight = factor(overweight)) %>%
  filter( (source == "EPOCH Visit 2") |  (source == "EPOCH Visit 1")) %>%
  ggplot() +
  geom_histogram(aes(x = bmi_zscore, group = overweight, fill = overweight)) +
  facet_wrap(~source)




#### Recurrent Analysis
recurrent2 <- recurrent %>%
  mutate(obesity = ifelse( (obesity == 1) & (is_last == 1), 0, 1))
reObj <- with(recurrent2, Recur(ageyearsnew, PID, obesity))
plot(reObj)


####################################################################
#
#
#   Obesity Kaplan-Meier Curve
#
#
####################################################################


# OBESITY -- Just the curve itself
obese_outcome <- survfit(Surv(last_age, ageyearsnew, obesity) ~ 1, data= obesity)
ggsurvplot(obese_outcome)


obese_outcomeGDM <- survfit(Surv(last_age, ageyearsnew, obesity) ~ gdm, data= obesity)
ggsurvplot(obese_outcomeGDM)
ggsurvplot(obese_outcomeGDM, fun = "cloglog")

obese_outcomeSex <- survfit(Surv(last_age, ageyearsnew, obesity) ~ sex, data= obesity)
ggsurvplot(obese_outcomeSex)
# These two curves are pretty close, let's do a log rank test
survdiff(Surv(ageyearsnew, obesity) ~ sex, data= obesity) # These don't appear to differ that badly

# Let's investigate the hazards by differing household income levels
plot_income <- c()
for (i in 1:length(table(obesity$household_income))) {
  level <- noquote(names(table(obesity$household_income))[i])
  temp <- survfit(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income, 
                            data= obesity, 
                            subset = (household_income == level))
  plot_income[[i]] <- ggsurvplot(temp) + 
    labs(title = paste0("Kaplan-Meier Curve: Income Level = ", level))
}

plot_grid(plot_income[[1]]$plot, plot_income[[2]]$plot, 
          plot_income[[3]]$plot, plot_income[[4]]$plot,
          plot_income[[5]]$plot,
          nrow = 3, ncol = 2)

# Let's investigate the hazards by differing household income levels
plot_income <- c()
for (i in 1:length(table(obesity$household_income))) {
  level <- noquote(names(table(obesity$household_income))[i])
  temp <- survfit(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income4, 
                  data= obesity3, 
                  subset = (household_income == level))
  plot_income[[i]] <- ggsurvplot(temp) + 
    labs(title = paste0("Kaplan-Meier Curve: Income Level = ", level))
}

plot_grid(plot_income[[1]]$plot, plot_income[[2]]$plot, 
          plot_income[[3]]$plot, plot_income[[4]]$plot,
          plot_income[[5]]$plot,
          nrow = 3, ncol = 2)




obese_outcomeIncome <- survfit(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income, 
                         data= obesity)
ggsurvplot(obese_outcomeIncome)
ggsurvplot(obese_outcomeIncome, fun = "cloglog")


####################################################################
#
#
#   Overweight Kaplan-Meier Curve
#
#
####################################################################

# OVERWEIGHT -- Just the curve itself
overweight_outcome <- survfit(Surv(last_age, ageyearsnew, overweight) ~ 1, data= overweight)
ggsurvplot(overweight_outcome)

overweight_outcomeGDM <- survfit(Surv(last_age, ageyearsnew, overweight) ~ gdm, data= overweight)
ggsurvplot(overweight_outcomeGDM)
ggsurvplot(overweight_outcomeGDM, fun = "cloglog")
# This one is legit perfect

overweight_outcomeSex <- survfit(Surv(last_age, ageyearsnew, overweight) ~ sex, data= overweight)
ggsurvplot(overweight_outcomeSex)


overweight_outcomeSexGDM <- survfit(Surv(last_age, ageyearsnew, overweight) ~ gdm + household_income, data= overweight)
ggsurvplot(overweight_outcomeSexGDM)
ggsurvplot(overweight_outcomeSexGDM, fun = "cloglog")


plot_income <- c()
for (i in 1:length(table(overweight$household_income))) {
  level <- noquote(names(table(overweight$household_income))[i])
  temp <- survfit(Surv(last_age, ageyearsnew, overweight) ~ gdm + household_income, 
                  data= overweight, 
                  subset = (household_income == level))
  plot_income[[i]] <- ggsurvplot(temp) + 
    labs(title = paste0("Kaplan-Meier Curve: Income Level = ", level))
}

plot_grid(plot_income[[1]]$plot, plot_income[[2]]$plot, 
          plot_income[[3]]$plot, plot_income[[4]]$plot,
          plot_income[[5]]$plot,
          nrow = 3, ncol = 2)





####################################################################
#
#
#   Explore data
#
#
####################################################################

# Extract all PIDs who were in the sample
ids <- unique(obesity$PID)

# See the average age of both study visits
recurrent %>%
  filter(source == "EPOCH Visit 1") %>%
  summarize(age = mean(ageyearsnew, na.rm = TRUE))

recurrent %>%
  filter(source == "EPOCH Visit 2") %>%
  summarize(age = mean(ageyearsnew, na.rm = TRUE))





