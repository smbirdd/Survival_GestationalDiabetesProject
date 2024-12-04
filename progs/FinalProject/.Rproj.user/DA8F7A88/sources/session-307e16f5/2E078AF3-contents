#########################################################################
#
# Program: p005_FigureTableGeneration
# Purpose: The purpose of this program is to generate the figures and
#   tables for both the paper and for the presentation
# Author:  Sarah Bird
#
#########################################################################

# Load in packages needed
library(tidyverse)
library(survival)
library(survminer)
library(RColorBrewer)
library(viridis)
library(cowplot)

##############################
#
# Load working space
#
##############################

# Read in data needed 
root <- "/Users/sarahbird/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/SurvivalProject"
source(here::here(root, "progs", "FinalProject", "p10_FunctionsFile.R"))

# Read in the models and diagnostic plots
models <- readRDS(here::here(root, "data", "processedData", "analysis_results", "models_20241123.rds"))
plots  <- readRDS(here::here(root, "data/processedData/analysis_results/diagnosticPlots_20241124.rds"))


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
               booktabs = TRUE, "latex",
               caption = "Time to first development of obesity") %>%
  kableExtra::kable_styling(latex_options = c("striped", "scale_down")) %>%
  footnote("This table presents the hazard ratios for each covariate in the model. 95% Wald confidence intervals are presented.")

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
               booktabs = TRUE,
               "latex",
               caption = "Time to first instance of becoming overweight") %>%
  kableExtra::kable_styling(latex_options = c("striped", "scale_down")) %>%
  footnote("This table presents the hazard ratios for each covariate in the model. 95% Wald confidence intervals are presented.")


writeTex(overweight_table, 
         destination = here::here(root, "output", "Tables"),
         texname = "Aim2_Table.tex")  # Write LaTeX code to file



###########################################################
#
# Create Kaplan Meier Curves for the obesity aim 
#
###########################################################

obesity <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                              "obesityDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))

# Let's investigate the hazards by differing household income levels
obesity_plots <- c()
obesity_loglog <- c()
for (i in 1:length(table(obesity$household_income))) {
  level <- noquote(names(table(obesity$household_income))[i])
  temp <- survfit(Surv(last_age, ageyearsnew, obesity) ~ gdm + household_income, 
                  data= obesity, 
                  subset = (household_income == level))
  obesity_plots[[i]] <- ggsurvplot(temp) + 
    labs(title = paste0("Kaplan-Meier Curve: Income Level = ", level))
  obesity_loglog[[i]] <- ggsurvplot(temp, fun = "cloglog") + 
    labs(title = paste0("Kaplan-Meier Curve: Income Level = ", level))
}

obesityKM <- plot_grid(obesity_plots[[1]]$plot, obesity_plots[[2]]$plot, 
                        obesity_plots[[3]]$plot, obesity_plots[[4]]$plot,
                        obesity_plots[[5]]$plot,
                        nrow = 3, ncol = 2)

obesityLoglogKKM <- plot_grid(obesity_loglog[[1]]$plot, obesity_loglog[[2]]$plot, 
                              obesity_loglog[[3]]$plot, obesity_loglog[[4]]$plot,
                              obesity_loglog[[5]]$plot,
                                 nrow = 3, ncol = 2)


save_plot(plot = obesityKM,
          filename = here::here(root, "output", "Images", "ObesityKaplanMeier_20241124.png"),
          ncol = 2, nrow = 3)

save_plot(plot = obesityLoglogKKM,
          filename = here::here(root, "output", "Images", "ObesityLogLogKaplanMeier_20241124.png"),
          ncol = 2, nrow = 3)




###########################################################
#
# Create Kaplan Meier Curves for the overweight aim 
#
###########################################################

overweight <- readRDS(here::here(root, "data", "processedData", "analysis_datasets",
                                 "overweightDataset_20241116.rds")) %>%
  filter( (PID != 20178) & (PID != 20053))


overweight_income <- c()
overweight_loglog <- c()
for (i in 1:length(table(overweight$household_income))) {
  level <- noquote(names(table(overweight$household_income))[i])
  temp <- survfit(Surv(last_age, ageyearsnew, overweight) ~ gdm + household_income, 
                  data= overweight, 
                  subset = (household_income == level))
  overweight_income[[i]] <- ggsurvplot(temp) + 
    labs(title = paste0("Kaplan-Meier Curve: Income Level = ", level))
  overweight_loglog[[i]] <- ggsurvplot(temp, fun = "cloglog") + 
    labs(title = paste0("Kaplan-Meier Curve: Income Level = ", level))
}

overweightKKM <- plot_grid(overweight_income[[1]]$plot, overweight_income[[2]]$plot, 
                            overweight_income[[3]]$plot, overweight_income[[4]]$plot,
                            overweight_income[[5]]$plot,
                            nrow = 3, ncol = 2)
overweightLoglogKKM <- plot_grid(overweight_loglog[[1]]$plot, overweight_loglog[[2]]$plot, 
                                   overweight_loglog[[3]]$plot, overweight_loglog[[4]]$plot,
                                   overweight_loglog[[5]]$plot,
                                   nrow = 3, ncol = 2)

save_plot(plot = overweightKKM,
          filename = here::here(root, "output", "Images", "OverweightKaplanMeier_20241124.png"),
          ncol = 2, nrow = 3)

save_plot(plot = overweightLoglogKKM,
          filename = here::here(root, "output", "Images", "OverweightLogLogKaplanMeier_20241124.png"),
          ncol = 2, nrow = 3)


