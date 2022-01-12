setwd("/exports/eddie3_homes_local/s1932788/Dataset")

library(DT)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(brms)
library(cmdstanr)
library(rstan)
library(bayestestR)
library(sjPlot)
library(psych)
library(BayesFactor)
library(bayesplot)
library(kableExtra)
library(tidyverse)
options(mc.corrs = parallel::detectCores(), backend = "cmdstanr") # can run chains in parallel with each other
rstan_options(auto_write = TRUE)
experiment_1_Dataset <- read.csv("DoPL_DOSPERT.csv", stringsAsFactors = FALSE)
experiment_dataset_analysis <- read.csv("experiment_dataset_analysis.csv")
correlation_table <- read.csv("table_attempt.csv")
locfunc <- function(data, to){
  which(colnames({{data}})=={{to}})
}
locfunc(experiment_1_Dataset, "DoPL_1")
lfunc <- function(dataset, to, from) {
  loc1 <- which(colnames({{dataset}}) == {{to}})
  loc2 <- which(colnames({{dataset}}) == {{from}})
  length({{dataset}}[loc1:loc2])
}

# Benefit, perception and risk taking across subdomains for DOPL motives
m4 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum + prestigeSum + leadershipSum + Age + Gender, data= experiment_dataset_analysis,
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
              prior(normal(-3, 1),  coef = "Age", resp = "riskSum"),
              prior(normal(-3, 1),  coef = "Gender1", resp = "riskSum"),
              prior(normal(3, 1),  coef = "dominanceSum", resp = "riskSum"),
              prior(normal(-2, 1),  coef = "leadershipSum", resp = "riskSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "riskSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
              prior(normal(3, 1),  coef = "Age", resp = "riskPerceptionSum"),
              prior(normal(3, 1),  coef = "Gender1", resp = "riskPerceptionSum"),
              prior(normal(-3, 1),  coef = "dominanceSum", resp = "riskPerceptionSum"),
              prior(normal(2, 1),  coef = "leadershipSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "riskPerceptionSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
              prior(normal(-3, 1),  coef = "Age", resp = "riskBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender1", resp = "riskBenefitSum"),
              prior(normal(3, 1),  coef = "dominanceSum", resp = "riskBenefitSum"),
              prior(normal(-2, 1),  coef = "leadershipSum", resp = "riskBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "riskBenefitSum")), save_all_pars = T, iter = 6500, cores = 6, warmup = 500)
              
summary(m4) 

saveRDS(m4, "m4.rds")

