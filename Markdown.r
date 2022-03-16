
library(DT)
library(ggplot2)
library(ggthemes)
library(brms)
library(cmdstanr)
library(rstan)
library(bayestestR)
library(sjPlot)
library(psych)
library(BayesFactor)
library(bayesplot)
library(kableExtra)
library(tidyselect)
options(mc.corrs = parallel::detectCores(), brms.backend = "cmdstanr") # can run chains in parallel with each other
rstan_options(auto_write = TRUE)
experiment_1_Dataset <- read.csv("DoPL_DOSPERT.csv", stringsAsFactors = FALSE)
experiment_dataset_analysis <- read.csv("experiment_dataset_analysis1.csv")
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
co2 <- readRDS("co2.rds")
m1 <- readRDS("m1.rds")
m2 <- readRDS("m2.rds")
m3 <- readRDS("m3.rds")
m5 <- readRDS("m5.rds")
m6 <- readRDS("m6.rds")
m7 <- readRDS("m7.rds")


m1_hdi <- readRDS("m1_hdi.rds")
m2_hdi <- readRDS("m2_hdi.rds")
m3_hdi <- readRDS("m3_hdi.rds")
m4_hdi <- readRDS("m4_hdi.rds")
m5_hdi <- readRDS("m5_hdi.rds")
m6_hdi <- readRDS("m6_hdi.rds")
m7_hdi <- readRDS("m7_hdi.rds")
m1_hdi <- readRDS("m1_hdi.rds")
m1_int <- readRDS("m1_int.rds")
m1_int_d <- readRDS("m1_int_d.rds")
m1_int_l <- readRDS("m1_int_l.rds")
m1_int_p <- readRDS("m1_int_p.rds")
m1_int_d_hdi <- readRDS("m1_int_d_hdi.rds")
m1_int_l_hdi <- readRDS("m1_int_l_hdi.rds")
m1_int_p_hdi <- readRDS("m1_int_p_hdi.rds")
demo_m1 <- readRDS("demo_m1.rds")
m7_DoPL_DOSPERT <- readRDS("m7_DoPL_DOSPERT.rds")
m4_perceivedRisk_Age <- readRDS("m4_perceivedRisk_Age.rds")
m5_generalRiskPreference <- readRDS("m5_generalRiskPreference.rds")
m5_benefitRisk_Age <- readRDS("m5_benefitRisk_Age.rds")
m4_perceivedRisk_Gender <- readRDS("m4_perceivedRisk_Gender.rds")
m5_benefitRisk_Gender <- readRDS("m5_benefitRisk_Gender.rds")
demo_m1_hdi <- readRDS("demo_m1_hdi.rds")
m7_DoPL_DOSPERT_hdi <- readRDS("m7_DoPL_DOSPERT_hdi.rds")
m4_perceivedRisk_Age_hdi <- readRDS("m4_perceivedRisk_Age_hdi.rds")
m5_generalRiskPreference_hdi <- readRDS("m5_generalRiskPreference_hdi.rds")
m5_benefitRisk_Age_hdi <- readRDS("m5_benefitRisk_Age_hdi.rds")
m4_perceivedRisk_Gender_hdi <- readRDS("m4_perceivedRisk_Gender_hdi.rds")
m5_benefitRisk_Gender_hdi <- readRDS("m5_benefitRisk_Gender_hdi.rds")
# bayesian correlation


library(brms)
library(rstan)
 # can remove the unnecessary iterations
corrs <- brm(mvbind(ethicalPreference, financialPreference, socialPreference, healthAndSafetyPreference, recreationalPreference) ~ 1, data = experiment_dataset_analysis, family = student, prior = c(prior(gamma(2, .1), class = "nu"), prior(normal(0, 1), class = "Intercept"), prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"), prior(normal(0, 1), class = "sigma", resp = "financialPreference"), prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"), prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"), prior(normal(0, 1), class = "sigma", resp = "socialPreference")), iter = 2000, warmup = 500)
summary(corrs)
plot(corrs)



kable(correlation_table, format = "html", booktabs = T, escape = F, longtabe = F, digits = 2, col.names = c("Parameters",	'Estimate',	'Est.Error',	'l.95..CI',	'u.95..CI',	'Rhat',	'Bulk_ESS',	'Tail_ESS')) %>%
  kable_styling(full_width = F) %>%
  remove_column(6:8)


# DoPL and General Risk preference 

experiment_dataset_analysis <- subset(experiment_dataset_analysis, experiment_dataset_analysis$Gender != "2")
m1 <- brm(generalRiskPreference ~ dominanceSum + prestigeSum + leadershipSum + Gender + Age, data = experiment_dataset_analysis, 
           prior = c(prior(normal(0, 1), class = "Intercept"),
                     prior(normal(3, 1), class = "b", coef = "dominanceSum"),
                     prior(normal(0, 1), class = "b", coef = "prestigeSum"), 
                     prior(normal(-2, 1), class = "b", coef = "leadershipSum"),
                     prior(normal(-3, 1), class = "b", coef = "Gender"), 
                     prior(normal(-3, 1), class = "b", coef = "Age")), save_all_pars = T, iter = 4000)
summary(m1)



## HDI 


library(bayestestR)
m1_hdi <- bayestestR::hdi(m1, effects = "fixed", component = "conditional", ci = .95)
kable(m1_hdi[sign(m1_hdi$CI_low) == sign(m1_hdi$CI_high),
                c('Parameter', 'CI','CI_low', 'CI_high')], format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Interaction between gender and each of the DoPL motives 


# interaction between gender and each of the DoPL motives
m1_int <- brm(generalRiskPreference ~ dominanceSum*Gender+ prestigeSum*Gender + leadershipSum*Gender + Age, data=experiment_dataset_analysis,
              prior=c(prior(normal(0,1), class="Intercept"),
                      prior(normal(3,1), class="b", coef="dominanceSum"),
                      prior(normal(0,1), class="b", coef="prestigeSum"),
                      prior(normal(-2,1), class="b", coef="leadershipSum"),
                      prior(normal(-3,1), class="b", coef="Gender"),
                      prior(normal(-3,1), class="b", coef="Age"),
                      prior(normal(0,1), class="b", coef="dominanceSum:Gender"),
                      prior(normal(0,1), class="b", coef="Gender:leadershipSum"),
                      prior(normal(0,1), class="b", coef="Gender:prestigeSum")), 
              save_all_pars = T, iter = 40000, warmup = 500)
summary(m1_int)
tab_model(m1_int)

## HDI


# hdi intervals interaction model
m1_int_hdi <- bayestestR::hdi(m1_int, effects = "fixed", component = "conditional", ci = .95)
kable(m1_int_hdi[sign(m1_int_hdi$CI_low) == sign(m1_int_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Specific interaction between gender and dominance orientation. 


# Interaction between gender and dominance
m1_int_d <- brm(generalRiskPreference ~ dominanceSum*Gender+ prestigeSum + leadershipSum + Age, data =experiment_dataset_analysis,
              prior=c(prior(normal(0,1), class="Intercept"),
                      prior(normal(3,1), class="b", coef="dominanceSum"),
                      prior(normal(0,1), class="b", coef="prestigeSum"),
                      prior(normal(-2,1), class="b", coef="leadershipSum"),
                      prior(normal(-3,1), class="b", coef="Gender"),
                      prior(normal(-3,1), class="b", coef="Age"),
                      prior(normal(0,1), class="b", coef="dominanceSum:Gender")), 
              save_all_pars = T, iter = 4000, warmup = 500)
summary(m1_int_d)


## HDI 
# HDI
m1_int_d_hdi <- bayestestR::hdi(m1_int_d, effects = "fixed", component = "conditional", ci = .95)
kable(m1_int_d_hdi[sign(m1_int_d_hdi$CI_low) == sign(m1_int_d_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


### Specific interaction between gender and leadership

# Interaction between gender and leadership
m1_int_l <- brm(generalRiskPreference ~ dominanceSum + prestigeSum + leadershipSum*Gender + Age, data=experiment_dataset_analysis,
              prior=c(prior(normal(0,1), class="Intercept"),
                      prior(normal(3,1), class="b", coef="dominanceSum"),
                      prior(normal(0,1), class="b", coef="prestigeSum"),
                      prior(normal(-2,1), class="b", coef="leadershipSum"),
                      prior(normal(-3,1), class="b", coef="Gender"),
                      prior(normal(-3,1), class="b", coef="Age"),
                      prior(normal(0,1), class="b", coef="leadershipSum:Gender")), 
              save_all_pars = T, iter = 4000, warmup = 500)
summary(m1_int_l)


## HDI 


# HDI 
m1_int_l_hdi <- bayestestR::hdi(m1_int_l, effects = "fixed", component = "conditional", ci = .95)
kable(m1_int_l_hdi[sign(m1_int_l_hdi$CI_low) == sign(m1_int_l_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Interaction specifically between gender and prestige

            
            
# Interaction between gender and prestige
m1_int_p <- brm(generalRiskPreference ~ dominanceSum+ prestigeSum*Gender + leadershipSum + Age, data=experiment_dataset_analysis,
              prior=c(prior(normal(0,1), class="Intercept"),
                      prior(normal(3,1), class="b", coef="dominanceSum"),
                      prior(normal(0,1), class="b", coef="prestigeSum"),
                      prior(normal(-2,1), class="b", coef="leadershipSum"),
                      prior(normal(-3,1), class="b", coef="Gender"),
                      prior(normal(-3,1), class="b", coef="Age"),
                      prior(normal(0,1), class="b", coef="prestigeSum:Gender")), 
              save_all_pars = T, iter = 4000)
summary(m1_int_p)


## HDI


# HDI
m1_int_p_hdi <- bayestestR::hdi(m1_int_p, effects = "fixed", component = "conditional", ci = .95)
kable(m1_int_p_hdi[sign(m1_int_p_hdi$CI_low) == sign(m1_int_p_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')], format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Models with domain specific risk-taking as dependent variable

## Domain specific model with dopl as predictors

# Domain specific model
m2 <- brm(mvbind(ethicalPreference, financialPreference, socialPreference, healthAndSafetyPreference, recreationalPreference) ~ dominanceSum + prestigeSum + leadershipSum + Gender + Age, data = experiment_dataset_analysis, cores = 6,
prior = c(prior(normal(0, 1),  coef = "Age", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "ethicalPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "financialPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
              prior(normal(0, 1), class = "sigma", resp = "financialPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "healthAndSafetyPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"),
              #----
              prior(normal(0, 1),  coef = "Age", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "recreationalPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "socialPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "socialPreference")), 
              save_all_pars = T, iter = 6000, warmup = 500) 
summary(m2)


## HDI


m2_hdi <- bayestestR::hdi(m2, effects = "fixed", component = "conditional", ci = .95)
kable(m2_hdi[sign(m2_hdi$CI_low) == sign(m2_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Domain specific model with gender*dopl interactions

            
            
 # Domain specific model with Gender-DOPL interactions
m3 <- brm(mvbind(ethicalPreference, financialPreference, socialPreference, healthAndSafetyPreference, recreationalPreference) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data = experiment_dataset_analysis, cores = 6,
              prior = c(prior(normal(0, 1),  coef = "Age", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "ethicalPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "ethicalPreference"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "financialPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "financialPreference"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
              prior(normal(0, 1), class = "sigma", resp = "financialPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "healthAndSafetyPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "recreationalPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "recreationalPreference"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"),
              #----
              prior(normal(0, 1),  coef = "Age", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "Gender", resp = "socialPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "socialPreference"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "socialPreference")), 
save_all_pars = T, iter = 10000, warmup = 500) 
summary(m3)


## HDI including model comparisons


# HDI
m3_hdi <- bayestestR::hdi(m3, effects = "fixed", component = "conditional", ci = .95)
kable(m3_hdi[sign(m3_hdi$CI_low) == sign(m3_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1) %>% 
            save_kable('/exports/chss/eddie/ppls/groups/Moorehdmlab/Andrew/Documents/R-analysis/R-output/papaja/testKable.pdf')
            
# Model Comparison: M2 and M3
#co <- loo(m2,m3, moment_match = T)
#co$loos$m2$looic
#co$loos$m2$elpd_loo
#co$loos$m3$looic
#co$loos$m3$elpd_loo
# Model Comparison (m2 and m3)
co <- loo(m2,m3)
looic_ <- c(co$loos$m2$looic, co$loos$m3$looic)
looic_se <- c(co$loos$m2$se_looic, co$loos$m3$se_looic)
looic_elpd <- c(co$loos$m2$elpd_loo, co$loos$m3$elpd_loo)
looic_elpd_se <- c(co$loos$m2$se_elpd_loo, co$loos$m3$se_elpd_loo)
loo_table <- data.frame(looic_, looic_se, looic_elpd,looic_elpd_se, row.names = c("m2", "m3"))
kable(loo_table, format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>%
  kable_styling(full_width = F)
apa_table(bayes_R2(m2))
apa_table(bayes_R2(m3))


# Domain specific model with dopl*age interactions


m6 <- brm(mvbind(ethicalPreference, financialPreference, socialPreference, healthAndSafetyPreference, recreationalPreference) ~ dominanceSum*Age + prestigeSum*Age + leadershipSum*Age + Age, data = experiment_dataset_analysis, cores = 6,
              prior = c(prior(normal(0, 1),  coef = "Age", resp = "ethicalPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "ethicalPreference"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "financialPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "financialPreference"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
              prior(normal(0, 1), class = "sigma", resp = "financialPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "healthAndSafetyPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"), 
              #----
              prior(normal(0, 1),  coef = "Age", resp = "recreationalPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "recreationalPreference"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"),
              #----
              prior(normal(0, 1),  coef = "Age", resp = "socialPreference"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "socialPreference"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "socialPreference")), 
save_all_pars = T, iter = 6000, warmup = 500) 


## HDI


# summary(m6)
m6_hdi <- bayestestR::hdi(m6, effects = "fixed", component = "conditional", ci = .95)
kable(m6_hdi[sign(m6_hdi$CI_low) == sign(m6_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# DOSPERT sub-domains and dopl


# Benefit, perception and risk taking across subdomains for DOPL motives
m4 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum + prestigeSum + leadershipSum + Age + Gender, data= experiment_dataset_analysis,
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
              prior(normal(-3, 1),  coef = "Age", resp = "riskSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "riskSum"),
              prior(normal(3, 1),  coef = "dominanceSum", resp = "riskSum"),
              prior(normal(-2, 1),  coef = "leadershipSum", resp = "riskSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "riskSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
              prior(normal(3, 1),  coef = "Age", resp = "riskPerceptionSum"),
              prior(normal(3, 1),  coef = "Gender", resp = "riskPerceptionSum"),
              prior(normal(-3, 1),  coef = "dominanceSum", resp = "riskPerceptionSum"),
              prior(normal(2, 1),  coef = "leadershipSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "riskPerceptionSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
              prior(normal(-3, 1),  coef = "Age", resp = "riskBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "riskBenefitSum"),
              prior(normal(3, 1),  coef = "dominanceSum", resp = "riskBenefitSum"),
              prior(normal(-2, 1),  coef = "leadershipSum", resp = "riskBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "riskBenefitSum")), save_all_pars = T, iter = 6500, cores = 6, warmup = 500)
              
summary(m4) 


## HDI


# HDI
m4_hdi <- bayestestR::hdi(m4, effects = "fixed", component = "conditional", ci = .95)
kable(m4_hdi[sign(m4_hdi$CI_low) == sign(m4_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)
   


# DOSPERT sub-domain and dopl*gender interactions

     
# Benefit, perception and risk taking across subdomains for DOPL motives and interactions
m5 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data= experiment_dataset_analysis, iter = 6000, cores = 6, warmup = 500, 
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
              prior(normal(-3, 1), coef = "Age", resp = "riskSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "riskSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "riskSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "riskSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
              prior(normal(3, 1), coef = "Age", resp = "riskPerceptionSum"),
              prior(normal(3, 1), coef = "Gender", resp = "riskPerceptionSum"),
              prior(normal(-3, 1), coef = "dominanceSum", resp = "riskPerceptionSum"),
              prior(normal(2, 1), coef = "leadershipSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "riskPerceptionSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "Age", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "riskBenefitSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskBenefitSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "riskBenefitSum")), save_all_pars = T)
summary(m5) 


## HDI and model comparisons


# HDI
m5_hdi <- bayestestR::hdi(m5, effects = "fixed", component = "conditional", ci = .95)
kable(m5_hdi[sign(m5_hdi$CI_low) == sign(m5_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)
m5_hdi <- hdi(m5, effects = "fixed", component = "conditional", ci = .95)
m5_hdi[sign(m5_hdi$CI_low) == sign(m5_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')]
# checking interaction slopes
# plot(conditional_effects(m5), points=T)
# Model comparison
# co2 <- loo(m4,m5)
# co2
# comparisonm4 <- loo(m4)
# comparisonm5 <- loo(m5)
# loocomparison4_5 <- loo_compare(comparisonm4, comparisonm5)
# loocomparison4_5
# 
# co2$loos$m4$looic
# co2$loos$m4$se_looic
# co2$loos$m5$looic
# co2$loos$m5$se_looic
# 
# co2$loos$m4$elpd_loo
# co2$loos$m4$se_elpd_loo
# co2$loos$m5$elpd_loo
# co2$loos$m5$se_elpd_loo
looic_2 <- c(co2$loos$m4$looic, co2$loos$m5$looic)
looic_2_se <- c(co2$loos$m4$se_looic, co2$loos$m5$se_looic)
looic_elpd_2 <- c(co2$loos$m4$elpd_loo, co2$loos$m5$elpd_loo)
looic_elpd_2_se <- c(co2$loos$m4$se_elpd_loo, co2$loos$m5$se_elpd_loo)
loo_table2 <- data.frame(looic_2, looic_2_se, looic_elpd_2,looic_elpd_2_se, row.names = c("m4", "m5"))
kable(loo_table2, format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>%
  kable_styling(full_width = F)
# bayes_R2(m4)
# bayes_R2(m5)
# 
# pp_check(m5, resp="riskSum", nsamples = 1000)
# pp_check(m5, resp="riskPerceptionSum", nsamples = 1000)
# pp_check(m5, resp="riskBenefitSum", nsamples = 1000)


# DOSPERT sub-domains and dopl*age interactions


m7 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum*Age + prestigeSum*Age + leadershipSum*Age + Gender, data= experiment_dataset_analysis, iter = 4000, cores = 6, 
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
              prior(normal(-3, 1), coef = "Age", resp = "riskSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "riskSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "riskSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "riskSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
              prior(normal(3, 1), coef = "Age", resp = "riskPerceptionSum"),
              prior(normal(3, 1), coef = "Gender", resp = "riskPerceptionSum"),
              prior(normal(-3, 1), coef = "dominanceSum", resp = "riskPerceptionSum"),
              prior(normal(2, 1), coef = "leadershipSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "riskPerceptionSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "Age", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "riskBenefitSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskBenefitSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "riskBenefitSum")), save_all_pars = T)
summary(m7) 


## HDI


# HDI
m7_hdi <- bayestestR::hdi(m7, effects = "fixed", component = "conditional", ci = .95)
kable(m7_hdi[sign(m7_hdi$CI_low) == sign(m7_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')], format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)



# library(brms)
# demo_m1 <- brm(mvbind(dominanceSum, prestigeSum, leadershipSum) ~ Gender + Age + Ethnicity + Education, 
#               data = experiment_dataset_analysis, iter = 4000, cores = 6, warmup = 500, 
#               prior = c(prior(normal(0,1), class="Intercept"),
#                       prior(normal(3,1), coef="Gender", resp = "dominanceSum"),
#                       prior(normal(0,1), coef="Gender", resp = "leadershipSum"),
#                       prior(normal(-2,1), coef="Gender", resp = "prestigeSum")))
# summary(demo_m1)



# demo_m1_hdi <- bayestestR::hdi(demo_m1, effects = "fixed", component = "conditional", ci = .95)
# demo_m1_hdi[sign(demo_m1_hdi$CI_low) == sign(demo_m1_hdi$CI_high),
#             c('Parameter', 'CI','CI_low', 'CI_high')]


experiment_dataset_analysis %>%
  data_grid(generalRiskPreference = seq_range(generalRiskPreference, n = 111)) %>%
  add_predicted_draws(m2) %>%
  ggplot(aes(x = generalRiskPreference, y = dominanceSum)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = experiment_1_Dataset, size = 2) +
  scale_fill_brewer()


priorTest <- c(prior(normal(0, 1), class = "b",resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "ethicalPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "ethicalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"), 
              prior(normal(0, 1), class = "b",resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "financialPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "financialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
              prior(normal(0, 1), class = "sigma", resp = "financialPreference"), 
              prior(normal(0, 1), class = "b",resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "healthAndSafetyPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"), 
              prior(normal(0, 1), class = "b",resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "recreationalPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "recreationalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"), 
              prior(normal(0, 1), class = "b",resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "socialPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "socialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "socialPreference"))


# Risk preferences predicting dopl motivation

m7_DoPL_DOSPERT <- brm(mvbind(dominanceSum, prestigeSum, leadershipSum) ~ ethicalPreference + financialPreference + socialPreference + healthAndSafetyPreference + recreationalPreference, data = experiment_dataset_analysis,
                       iter = 6500, warmup = 500,
                     prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1), class = "sigma", resp = "dominanceSum"),
                               prior(normal(0, 1), class = "sigma", resp = "prestigeSum"),
                               prior(normal(0, 1), class = "sigma", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "ethicalPreference", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "financialPreference", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "healthAndSafetyPreference", resp = "dominanceSum"), 
                               prior(normal(0, 1), coef = "recreationalPreference", resp = "dominanceSum"), 
                               prior(normal(0, 1), coef = "socialPreference", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "ethicalPreference", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "financialPreference", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "healthAndSafetyPreference", resp = "prestigeSum"), 
                               prior(normal(0, 1), coef = "recreationalPreference", resp = "prestigeSum"), 
                               prior(normal(0, 1), coef = "socialPreference", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "ethicalPreference", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "financialPreference", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "healthAndSafetyPreference", resp = "leadershipSum"), 
                               prior(normal(0, 1), coef = "recreationalPreference", resp = "leadershipSum"), 
                               prior(normal(0, 1), coef = "socialPreference", resp = "leadershipSum")),
                               save_all_pars = T)
summary(m7_DoPL_DOSPERT)


## HDI


m7_DoPL_DOSPERT_hdi <- bayestestR::hdi(m7_DoPL_DOSPERT, effects = "fixed", component = "conditional", ci = .95)
kable(m7_DoPL_DOSPERT_hdi[sign(m7_DoPL_DOSPERT_hdi$CI_low) == sign(m7_DoPL_DOSPERT_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)



bayes_R2(m1)


# Risk perception and DoPL*Age interaction

m4_perceivedRisk_Age <- brm(mvbind(ethicalQuestionsPerceptionSum, financialQuestionsPerceptionSum, socialQuestionsPerceptionSum, recreationalQuestionsPerceptionSum, healthAndSafetyQuestionsPerceptionSum) ~ dominanceSum*Age + prestigeSum*Age + leadershipSum*Age + Gender + Age, iter = 4000, warmup = 500,
                           data = experiment_dataset_analysis, 
                           prior =  
                          c(prior(normal(0, 1), class = "Intercept",resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-3, 1), coef = "dominanceSum:Age", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "Age:leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "Age:prestigeSum", resp = "ethicalQuestionsPerceptionSum"), 
              #---- Perception of financial risk taking
              prior(normal(0, 1), coef = "Age", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "financialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Age", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "financialQuestionsPerceptionSum"),
              #---- Perception of risk in health and safety
              prior(normal(0, 1), coef = "Age", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Age", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsPerceptionSum"),
              #---- Perception of risky recreational settings
              prior(normal(0, 1), coef = "Age", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Age", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsPerceptionSum"),
              #---- Perception of social risk
              prior(normal(0, 1), coef = "Age", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "socialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Age", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsPerceptionSum")), 
              save_all_pars = T)
              summary(m4_perceivedRisk_Age)


## HDI


m4_perceivedRisk_Age_hdi <- bayestestR::hdi(m4_perceivedRisk_Age, effects = "fixed", component = "conditional", ci = .95)
kable(m4_perceivedRisk_Age_hdi[sign(m4_perceivedRisk_Age_hdi$CI_low) == sign(m4_perceivedRisk_Age_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Risk benefit and DoPL*Age interaction


m5_generalRiskPreference <- brm(generalRiskPreference ~ ethicalPreference + socialPreference + financialPreference + healthAndSafetyPreference + recreationalPreference, data = experiment_dataset_analysis,iter = 4000, warmup = 500,
                                prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1), coef = "ethicalPreference"),
                               prior(normal(0, 1), coef = "financialPreference"),
                               prior(normal(0, 1), coef = "healthAndSafetyPreference"), 
                               prior(normal(0, 1), coef = "recreationalPreference"), 
                               prior(normal(0, 1), coef = "socialPreference")),
                                          save_all_pars = T)
summary(m5_generalRiskPreference)
m5_benefitRisk_Age <- brm(mvbind(ethicalQuestionsBenefitSum, financialQuestionsBenefitSum, socialQuestionsBenefitSum, recreationalQuestionsBenefitSum, healthAndSafetyQuestionsBenefitSum) ~ dominanceSum*Age + prestigeSum*Age + leadershipSum*Age + Gender + Age, 
                           data = experiment_dataset_analysis, iter = 5000, warmup = 500,
                           prior =  c(prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "Age", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "Gender", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Age", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:leadershipSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:prestigeSum", resp = "ethicalQuestionsBenefitSum"),
              #---- Believed benefit of financial risk 
              prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "Age", resp = "financialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "financialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Age", resp = "financialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:leadershipSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:prestigeSum", resp = "financialQuestionsBenefitSum"),
              #---- Believed benefit of health and safety risk
              prior(normal(0, 1),  coef = "Age", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(3, 1),  coef = "dominanceSum:Age", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsBenefitSum"),
              #---- Believed benefit of recreational risk
              prior(normal(0, 1),  coef = "Age", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Age", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:leadershipSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:prestigeSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsBenefitSum"),
              #---- Believed benefit of social risk
              prior(normal(0, 1),  coef = "Age", resp = "socialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "socialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Age", resp = "socialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:leadershipSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Age:prestigeSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsBenefitSum")))
summary(m5_benefitRisk_Age)


## HDI


m5_generalRiskPreference_hdi <- bayestestR::hdi(m5_generalRiskPreference, effects = "fixed", component = "conditional", ci = .95)
m5_benefitRisk_Age_hdi <- bayestestR::hdi(m5_benefitRisk_Age, effects = "fixed", component = "conditional", ci = .95)
kable(m5_benefitRisk_Age_hdi[sign(m5_benefitRisk_Age_hdi$CI_low) == sign(m5_benefitRisk_Age_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Risk perception and DoPL*Gender interaction


m4_perceivedRisk_Gender <- brm(mvbind(ethicalQuestionsPerceptionSum, financialQuestionsPerceptionSum, socialQuestionsPerceptionSum, recreationalQuestionsPerceptionSum, healthAndSafetyQuestionsPerceptionSum) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Gender + Age, iter = 6000, warmup = 500,
                           data = experiment_dataset_analysis, 
                           prior =  
                          c(prior(normal(0, 1), class = "Intercept",resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Age", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-3, 1), coef = "dominanceSum:Gender", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "Gender:leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "Gender:prestigeSum", resp = "ethicalQuestionsPerceptionSum"), 
              #----
              prior(normal(0, 1), coef = "Age", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "financialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Gender", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "financialQuestionsPerceptionSum"),
              #----
              prior(normal(0, 1), coef = "Age", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Gender", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsPerceptionSum"),
              #----
              prior(normal(0, 1), coef = "Age", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Gender", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsPerceptionSum"),
              #----
              prior(normal(0, 1), coef = "Age", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender", resp = "socialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "dominanceSum:Gender", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsPerceptionSum")), 
              save_all_pars = T)summary(m4_perceivedRisk_Gender)


## HDI


m4_perceivedRisk_Gender_hdi <- bayestestR::hdi(m4_perceivedRisk_Gender, effects = "fixed", component = "conditional", ci = .95)
kable(m4_perceivedRisk_Gender_hdi[sign(m4_perceivedRisk_Gender_hdi$CI_low) == sign(m4_perceivedRisk_Gender_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Risk benefit and DoPL*Gender interaction

m5_generalRiskPreference <- brm(generalRiskPreference ~ ethicalPreference + socialPreference + financialPreference + healthAndSafetyPreference + recreationalPreference, data = experiment_dataset_analysis,iter = 4000, warmup = 500,
                                prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1) ,class = "b", coef = "ethicalPreference"),
                               prior(normal(0, 1) ,class = "b", coef = "financialPreference"),
                               prior(normal(0, 1) ,class = "b", coef = "healthAndSafetyPreference"), 
                               prior(normal(0, 1) ,class = "b", coef = "recreationalPreference"), 
                               prior(normal(0, 1) ,class = "b", coef = "socialPreference")),
                                          save_all_pars = T)
summary(m5_generalRiskPreference)
m5_benefitRisk_Gender <- brm(mvbind(ethicalQuestionsBenefitSum, financialQuestionsBenefitSum, socialQuestionsBenefitSum, recreationalQuestionsBenefitSum, healthAndSafetyQuestionsBenefitSum) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Gender + Age, 
                           data = experiment_dataset_analysis, iter = 5000, warmup = 500, cores = 6, 
                           prior =  c(prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "Age", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "Gender", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Gender", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:leadershipSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:prestigeSum", resp = "ethicalQuestionsBenefitSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "Age", resp = "financialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "financialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Gender", resp = "financialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:leadershipSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:prestigeSum", resp = "financialQuestionsBenefitSum"),
              #----
              prior(normal(0, 1),  coef = "Age", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(3, 1),  coef = "dominanceSum:Gender", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsBenefitSum"),
              #----
              prior(normal(0, 1),  coef = "Age", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Gender", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:leadershipSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:prestigeSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsBenefitSum"),
              #----
              prior(normal(0, 1),  coef = "Age", resp = "socialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "socialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "leadershipSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1),  coef = "prestigeSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(2, 1),  coef = "dominanceSum:Gender", resp = "socialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:leadershipSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(-3, 1),  coef = "Gender:prestigeSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsBenefitSum")), 
              save_all_pars = T)summary(m5_benefitRisk_Gender)


## HDI


m5_benefitRisk_Gender_hdi <- bayestestR::hdi(m5_benefitRisk_Gender, effects = "fixed", component = "conditional", ci = .95)
kable(m5_benefitRisk_Gender_hdi[sign(m5_benefitRisk_Gender_hdi$CI_low) == sign(m5_benefitRisk_Gender_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


# Full exploratory modelhttps://rpubs.com/aithurburn/777501


m8 <- brm(mvbind(ethicalQuestionsRiskSum, ethicalQuestionsPerceptionSum, ethicalQuestionsBenefitSum, 
                 financialQuestionsRiskSum, financialQuestionsPerceptionSum, financialQuestionsBenefitSum,
                 healthAndSafetyQuestionsRiskSum, healthAndSafetyQuestionsPerceptionSum, healthAndSafetyQuestionsBenefitSum,
                 recreationalQuestionsRiskSum, recreationalQuestionsPerceptionSum, recreationalQuestionsBenefitSum,
                 socialQuestionsRiskSum, socialQuestionsPerceptionSum, socialQuestionsBenefitSum) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data= experiment_dataset_analysis, iter = 8000, warmup = 500, save_all_pars = T, cores = 6,
          #----
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsRiskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalQuestionsRiskSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "ethicalQuestionsRiskSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "ethicalQuestionsRiskSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "ethicalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "ethicalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "ethicalQuestionsRiskSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsPerceptionSum"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalQuestionsPerceptionSum"), 
              prior(normal(2, 1), coef = "Age", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "Gender", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "dominanceSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "ethicalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsBenefitSum"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalQuestionsBenefitSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "ethicalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "ethicalQuestionsBenefitSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsRiskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "financialQuestionsRiskSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "financialQuestionsRiskSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "financialQuestionsRiskSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "financialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "financialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "financialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "financialQuestionsRiskSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsPerceptionSum"), 
              prior(normal(0, 1), class = "sigma", resp = "financialQuestionsPerceptionSum"), 
              prior(normal(2, 1), coef = "Age", resp = "financialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "Gender", resp = "financialQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "dominanceSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "financialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "financialQuestionsPerceptionSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsBenefitSum"), 
              prior(normal(0, 1), class = "sigma", resp = "financialQuestionsBenefitSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "financialQuestionsBenefitSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "financialQuestionsBenefitSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "financialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "financialQuestionsBenefitSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsRiskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsRiskSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "healthAndSafetyQuestionsRiskSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "healthAndSafetyQuestionsRiskSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsRiskSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsRiskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsRiskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "healthAndSafetyQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "healthAndSafetyQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "healthAndSafetyQuestionsRiskSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsPerceptionSum"), 
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsPerceptionSum"), 
              prior(normal(2, 1), coef = "Age", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "Gender", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsBenefitSum"), 
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsBenefitSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsRiskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsRiskSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "recreationalQuestionsRiskSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "recreationalQuestionsRiskSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "recreationalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "recreationalQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "recreationalQuestionsRiskSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsPerceptionSum"), 
              prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsPerceptionSum"), 
              prior(normal(2, 1), coef = "Age", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "Gender", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "dominanceSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsBenefitSum"), 
              prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsBenefitSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "recreationalQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "recreationalQuestionsBenefitSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsRiskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "socialQuestionsRiskSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "socialQuestionsRiskSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "socialQuestionsRiskSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "socialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "socialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "socialQuestionsRiskSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "socialQuestionsRiskSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsPerceptionSum"), 
              prior(normal(0, 1), class = "sigma", resp = "socialQuestionsPerceptionSum"), 
              prior(normal(2, 1), coef = "Age", resp = "socialQuestionsPerceptionSum"),
              prior(normal(2, 1), coef = "Gender", resp = "socialQuestionsPerceptionSum"),
              prior(normal(-2, 1), coef = "dominanceSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "socialQuestionsPerceptionSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "socialQuestionsPerceptionSum"),
              #
              prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsBenefitSum"), 
              prior(normal(0, 1), class = "sigma", resp = "socialQuestionsBenefitSum"), 
              prior(normal(-2, 1), coef = "Age", resp = "socialQuestionsBenefitSum"),
              prior(normal(-2, 1), coef = "Gender", resp = "socialQuestionsBenefitSum"),
              prior(normal(2, 1), coef = "dominanceSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "socialQuestionsBenefitSum"),
              prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "socialQuestionsBenefitSum")))
summary(m8)
m8_hdi <- hdi(m8, effects = "fixed", component = "conditional", ci = .95)
m8_hdi[sign(m8_hdi$CI_low) == sign(m8_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')]



mcmc_areas_m1 <- mcmc_areas(m1, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
mcmc_dens_m1 <- mcmc_dens_overlay(m1, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
m1_plot <- mcmc_plot(m1, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
mcmc_areas_m2 <- mcmc_areas(m2, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
mcmc_dens_m2 <- mcmc_dens_overlay(m2, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
m2_plot <- mcmc_plot(m2, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
mcmc_areas_m3 <- mcmc_areas(m3, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
mcmc_dens_m3 <- mcmc_dens_overlay(m3, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
m3_plot <- mcmc_plot(m3, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
mcmc_areas_m4 <- mcmc_areas(m4, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
mcmc_dens_m4 <- mcmc_dens_overlay(m4, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))
m4_plot <- mcmc_plot(m4, pars = c("b_dominanceSum", "b_prestigeSum", "b_leadershipSum", "b_Gender"))


q <- ggplot(data = experiment_1_Dataset_analysis, 
       aes(x   = dominanceSum,
           y   = riskSum,
           col = as.factor(Gender),
           group = Gender))+
  geom_point(size     = .7,
             alpha    = .8,
             position = "jitter") +
  geom_smooth(method = "lm",
              se     = FALSE, 
              size   = 2,
              alpha  = .8)+
  theme_classic()+
  labs(title    = "Linear Relationship for Risk Domains ", 
       col      = "Risk Domains")
jz <- q + scale_color_manual (labels = c("Men", "Women"), values = c("#f8766d", "#00bfc4"), name = "Participant \n Gender")
ggsave("jz.png")
q1 <- ggplot(data = experiment_1_Dataset_analysis, 
       aes(x   = dominanceSum,
           y   = riskBenefitSum,
           col = as.factor(Gender),
           group = Gender))+
  geom_point(size     = .7,
             alpha    = .8,
             position = "jitter") +
  geom_smooth(method = "lm",
              se     = FALSE, 
              size   = 2,
              alpha  = .8)+
  theme_classic()+
  labs(title    = "Linear Relationship for Risk Domains ", 
       col      = "Risk Domains")
jz1 <- q1 + scale_color_manual (labels = c("Men", "Women"), values = c("#f8766d", "#00bfc4"), name = "Participant \n Gender")
ggsave("jz1.png")
q2 <- ggplot(data = experiment_1_Dataset_analysis, 
       aes(x   = dominanceSum,
           y   = riskPerceptionSum,
           col = as.factor(Gender),
           group = Gender))+
  geom_point(size     = .7,
             alpha    = .8,
             position = "jitter") +
  geom_smooth(method = "lm",
              se     = FALSE, 
              size   = 2,
              alpha  = .8)+
  theme_classic()+
  labs(title    = "Linear Relationship for Risk Domains ", 
       col      = "Risk Domains")
jz2 <- q2 + scale_color_manual (labels = c("Men", "Women"), values = c("#f8766d", "#00bfc4"), name = "Participant \n Gender")
ggsave("jz2.png")
experiment_1_long <- experiment_1_Dataset_analysis %>% 
  select("dominanceSum", "Gender", "ethicalQuestionsRiskSum", "financialQuestionsRiskSum", "healthAndSafetyQuestionsRiskSum", "recreationalQuestionsRiskSum", "socialQuestionsRiskSum") %>% 
  pivot_longer(-c("dominanceSum", "Gender"), names_to = "variable", values_to = "value")


ggplot(experiment_1_long, aes(x = dominanceSum, y = value, color = variable, group = as.factor(Gender), fill = as.factor(Gender))) +
  geom_point(size     = .7,
             alpha    = .8,
             position = "jitter") +
  geom_smooth(method = "lm",
              se     = FALSE, 
              size   = 2,
              alpha  = .8)+
  scale_color_manual(values = c("darkred", "steelblue", "green", "orange", "black")) + 
  facet_wrap(~ variable)


savePlot <- function(myPlot) {
        png("ggplot.png")
        print(myPlot)
        dev.off()
}


df <- names(experiment_1_Dataset_analysis[16:32])
## riskSum ggplots
if(i in df){
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = {{i}},
      color = as.factor(Gender), group = Gender)) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave(paste0({{ i }}, ".png"))
}
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = financialQuestionsRiskSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("financialQuestionsRiskSum.png")
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = recreationalQuestionsRiskSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("recreationalQuestionsRiskSum.png")

ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = healthAndSafetyQuestionsRiskSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")

ggsave("healthAndSafetyQuestionsRiskSum.png")
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = socialQuestionsRiskSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("socialQuestionsRiskSum.png")

## riskPerceptionSum ggplots
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = ethicalQuestionsPerceptionSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("ethicalQuestionsPerceptionSum.png")
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = financialQuestionsPerceptionSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("financialQuestionsPerceptionSum.png")
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = recreationalQuestionsPerceptionSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("recreationalQuestionsPerceptionSum.png")

ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = healthAndSafetyQuestionsPerceptionSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("healthAndSafetyQuestionsPerceptionSum.png")

ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = socialQuestionsPerceptionSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("socialQuestionsPerceptionSum.png")

## riskBenefitSum ggplots
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = ethicalQuestionsBenefitSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("ethicalQuestionsBenefitSum.png")
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = financialQuestionsBenefitSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("financialQuestionsBenefitSum.png")
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = recreationalQuestionsBenefitSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")
ggsave("recreationalQuestionsBenefitSum.png")

ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = healthAndSafetyQuestionsBenefitSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains ",
    col = "Risk Domains")

ggsave("healthAndSafetyQuestionsBenefitSum.png")
ggplot(data = experiment_1_Dataset_analysis,
    aes(x = dominanceSum,
      y = socialQuestionsBenefitSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risks Domainsfd ",
    col = "Risk Domains")
ggsave("./R-output/ggplot/socialQuestionsBenefitSum.png")


ggplot_func <- function(dataset, xcol, ycol){
    titleName = {{ycol}}
	ggplot(data = {{dataset}}, aes(x = {{xcol}}, y = {{ycol}}, color = as.factor(Gender))) + 
	geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) + 
    geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains: ", subtitle = titleName,
    col = "Risk Domains")
ggsave(paste0("./R-output/ggplot/", {{i}}, ".png"))
}

experiment_dataset_analysis <- experiment_1_Dataset_analysis[!(experiment_1_Dataset_analysis$Gender == "NA"),]

experiment_dataset_analysis <- read.csv("experiment_dataset_analysis1.csv")
for(i in df){
  testPlot <- ggplot(data = experiment_dataset_analysis,
    aes_string(x = "dominanceSum",
      y = {{i}}, color = "Gender", group = "Gender")) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains: ", subtitle = paste0({{i}}),
    col = "Participant \n Gender")
ggsave(paste0("./R-output/ggplot/", {{i}}, ".png"))
}
for(i in df){ggplot_func(experiment_1_Dataset_analysis, dominanceSum, i)}

ggplot(data = experiment_1_Dataset_analysis,
    aes_string(x = "dominanceSum",
      y = "socialQuestionsBenefitSum", color = "Gender", group = "Gender")) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains: ", subtitle = paste0({{i}}),
    col = "Participant \n Gender")
ggsave(paste0("./R-output/ggplot/testing.png"))

write.csv(experiment_dataset_analysis, "experiment_dataset_analysis1.csv")

for(i in df){
  testPlot <- ggplot(data = experiment_dataset_analysis,
    aes_string(x = "Age",
      y = {{i}})) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains: ", subtitle = paste0({{i}}),
    col = "Participant \n Gender")
ggsave(paste0("./R-output/ggplot/Age_", {{i}}, ".png"))
}

for(i in df){
  testPlot <- ggplot(data = experiment_dataset_analysis,
    aes_string(x = "prestigeSum",
      y = {{i}}, color = "Gender", group = "Gender")) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains: ", subtitle = paste0({{i}}),
    col = "Participant \n Gender")
ggsave(paste0("./R-output/ggplot/Prestige_", {{i}}, ".png"))
}
for(i in df){
  testPlot <- ggplot(data = experiment_dataset_analysis,
    aes_string(x = "leadershipSum",
      y = {{i}}, color = "Gender", group = "Gender")) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risk Domains: ", subtitle = paste0({{i}}),
    col = "Participant \n Gender")
ggsave(paste0("./R-output/ggplot/Leadership_", {{i}}, ".png"))
}
head(experiment_dataset_analysis$Education)


experiment_dataset_analysis <- experiment_dataset_analysis %>%
  mutate_at(vars(locfunc(experiment_dataset_analysis, "Gender")), ~as.character(recode(., "Male" = 1, "Female" = 0))) %>%
  mutate_at(vars(locfunc(experiment_dataset_analysis, 'Ethnicity')), ~as.character(recode(., "0" = "Prefer not to respond", "1" = "Scottish","2" = "English", "3" = "European","4" = "Latin American","5" = "Asian", "6" = "Arab", "7" = "African","8" = "Other")))

experiment_dataset_analysis$Education <- factor(experiment_dataset_analysis$Education, levels = c("Prefer not to respond","Primary School", "GCSEs or equivalent", "A-levels or equivalent", "University Undergraduate Program", "University Postgraduate Program", "Doctoral Degree"))
ggplot(experiment_dataset_analysis, aes(x = Education)) + 
  geom_histogram(stat = 'count', fill = c('#fef0d9',	'#fdd49e',	'#fdbb84',	'#fc8d59',	'#e34a33',	'#b30000', '#000000')) +
  scale_x_discrete(labels = c("Prefer not \nto respond", "Primary \nSchool", "GCSes or \nEquivalent", "A-Levels or \nEquivalent", "University \nUndergraduate \nProgram", "University \nPost-Graduate \nProgram", "Doctoral \nDegree")) + 
  theme_classic()

ggsave("./R-output/ggplot/educationGGplot1.png")

ggplot(experiment_dataset_analysis, aes(x = Ethnicity)) + 
  geom_histogram(stat = 'count', fill = c('#fef0d9',	'#fdd49e',	'#fdbb84',	'#fc8d59',	'#e34a33',	'#b30000', '#000000')) +
  scale_x_discrete(labels = c("Prefer not \nto respond", "Primary \nSchool", "GCSes or \nEquivalent", "A-Levels or \nEquivalent", "University \nUndergraduate \nProgram", "University \nPost-Graduate \nProgram", "Doctoral \nDegree")) + 
  theme_classic()

ggsave("./R-output/ggplot/ethinicityGGplot1.png")

ggplot(experiment_dataset_analysis, aes(x = Age)) + 
  geom_histogram(color = "black", fill = "#fc8d59") + 
  theme_light() + 
  labs(x = "Age of participants")

ggsave("./R-output/ggplot/ageGGPlot.png")

ggplot(data = experiment_dataset_analysis,
    aes(x = dominanceSum,
      y = socialQuestionsBenefitSum,
      color = as.factor(Gender),
      group = as.factor(Gender))) +
  geom_point(size = .7,
    alpha = .8,
    position = "jitter") +
  geom_smooth(method = "lm",
    se = FALSE,
    size = 2,
    alpha = .8) +
  theme_classic() +
  labs(title = "Linear Relationship for Risks Domainsfd ",
    col = "Risk Domains")
ggsave("./R-output/ggplot/socialQuestionsBenefitSum.png")

m3 <- brm(mvbind(ethicalPreference, financialPreference, socialPreference, healthAndSafetyPreference, recreationalPreference) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data = experiment_dataset_analysis, save_all_pars = T, iter = 10000,
          
          prior(
            prior(normal(0, 1), coef = "Age", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "Gender", resp = "ethicalPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "ethicalPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"), 
            prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "financialPreference"),
            prior(normal(0, 1), coef = "Gender", resp = "financialPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "financialPreference"),
            prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "financialPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
            prior(normal(0, 1), class = "sigma", resp = "financialPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "Gender", resp = "healthAndSafetyPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "Gender", resp = "recreationalPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "recreationalPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
            prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "socialPreference"),
            prior(normal(0, 1), coef = "Gender", resp = "socialPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender", resp = "socialPreference"),
            prior(normal(0, 1), coef = "Gender:prestigeSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "Gender:leadershipSum", resp = "socialPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
            prior(normal(0, 1), class = "sigma", resp = "socialPreference")))


summary(m3)


########################################################

m4 <-  brm(mvbind(dominanceSum, prestigeSum, leadershipSum) ~ riskSum + riskPerceptionSum + riskBenefitSum + Age + Gender, data= experiment_dataset_analysis,
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "dominanceSum"), 
              prior(normal(0, 1), class = "sigma", resp = "dominanceSum"), 
              prior(normal(-3, 1),  coef = "Age", resp = "dominanceSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "dominanceSum"),
              prior(normal(3, 1),  coef = "riskSum", resp = "dominanceSum"),
              prior(normal(-2, 1),  coef = "riskPerceptionSum", resp = "dominanceSum"),
              prior(normal(0, 1),  coef = "riskBenefitSum", resp = "dominanceSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "leadershipSum"),
              prior(normal(0, 1), class = "sigma", resp = "leadershipSum"), 
              prior(normal(3, 1),  coef = "Age", resp = "leadershipSum"),
              prior(normal(3, 1),  coef = "Gender", resp = "leadershipSum"),
              prior(normal(-3, 1),  coef = "riskSum", resp = "leadershipSum"),
              prior(normal(2, 1),  coef = "riskPerceptionSum", resp = "leadershipSum"),
              prior(normal(0, 1),  coef = "riskBenefitSum", resp = "leadershipSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "prestigeSum"),
              prior(normal(0, 1), class = "sigma", resp = "prestigeSum"),
              prior(normal(-3, 1),  coef = "Age", resp = "prestigeSum"),
              prior(normal(-3, 1),  coef = "Gender", resp = "prestigeSum"),
              prior(normal(3, 1),  coef = "riskSum", resp = "prestigeSum"),
              prior(normal(-2, 1),  coef = "riskPerceptionSum", resp = "prestigeSum"),
              prior(normal(0, 1),  coef = "riskBenefitSum", resp = "prestigeSum")), save_all_pars = T, iter = 6500, cores = 6, warmup = 500)
              
summary(m4) 


## HDI


# HDI
m4_hdi <- bayestestR::hdi(m4, effects = "fixed", component = "conditional", ci = .95)
kable(m4_hdi[sign(m4_hdi$CI_low) == sign(m4_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)
   


# DOSPERT sub-domain and dopl*gender interactions


     
# Benefit, perception and risk taking across subdomains for DOPL motives and interactions
m5 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data= experiment_dataset_analysis, iter = 6000, cores = 6, warmup = 500, 
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
              prior(normal(-3, 1), coef = "Age", resp = "riskSum"),
              prior(normal(-3, 1), coef = "GenderMale", resp = "riskSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "dominanceSum:GenderMale", resp = "riskSum"),
              prior(normal(0, 1), coef = "GenderMale:prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "GenderMale:leadershipSum", resp = "riskSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
              prior(normal(3, 1), coef = "Age", resp = "riskPerceptionSum"),
              prior(normal(3, 1), coef = "GenderMale", resp = "riskPerceptionSum"),
              prior(normal(-3, 1), coef = "dominanceSum", resp = "riskPerceptionSum"),
              prior(normal(2, 1), coef = "leadershipSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:GenderMale", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "GenderMale:prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "GenderMale:leadershipSum", resp = "riskPerceptionSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "Age", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "GenderMale", resp = "riskBenefitSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskBenefitSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:GenderMale", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "GenderMale:prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "GenderMale:leadershipSum", resp = "riskBenefitSum")), save_all_pars = T)

summary(m5) 

kable(summary(m5))

## HDI and model comparisons

# HDI

m5_hdi <- bayestestR::hdi(m5, effects = "fixed", component = "conditional", ci = .95)
kable(m5_hdi[sign(m5_hdi$CI_low) == sign(m5_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)

m5_hdi <- hdi(m5, effects = "fixed", component = "conditional", ci = .95)
m5_hdi[sign(m5_hdi$CI_low) == sign(m5_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')]




looic_2 <- c(co2$loos$m4$looic, co2$loos$m5$looic)
looic_2_se <- c(co2$loos$m4$se_looic, co2$loos$m5$se_looic)
looic_elpd_2 <- c(co2$loos$m4$elpd_loo, co2$loos$m5$elpd_loo)
looic_elpd_2_se <- c(co2$loos$m4$se_elpd_loo, co2$loos$m5$se_elpd_loo)

loo_table2 <- data.frame(looic_2, looic_2_se, looic_elpd_2,looic_elpd_2_se, row.names = c("m4", "m5"))
kable(loo_table2, format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>%
  kable_styling(full_width = F)




# DOSPERT sub-domains and dopl*age interactions

m7 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum*Age + prestigeSum*Age + leadershipSum*Age + Gender, data= experiment_dataset_analysis, iter = 4000, cores = 6, 
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
              prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
              prior(normal(-3, 1), coef = "Age", resp = "riskSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "riskSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "riskSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "riskSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "riskSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
              prior(normal(3, 1), coef = "Age", resp = "riskPerceptionSum"),
              prior(normal(3, 1), coef = "Gender", resp = "riskPerceptionSum"),
              prior(normal(-3, 1), coef = "dominanceSum", resp = "riskPerceptionSum"),
              prior(normal(2, 1), coef = "leadershipSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "riskPerceptionSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "riskPerceptionSum"),
              #----
              prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
              prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "Age", resp = "riskBenefitSum"),
              prior(normal(-3, 1), coef = "Gender", resp = "riskBenefitSum"),
              prior(normal(3, 1), coef = "dominanceSum", resp = "riskBenefitSum"),
              prior(normal(-2, 1), coef = "leadershipSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "dominanceSum:Age", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "Age:prestigeSum", resp = "riskBenefitSum"),
              prior(normal(0, 1), coef = "Age:leadershipSum", resp = "riskBenefitSum")), save_all_pars = T)

summary(m7) 


## HDI


# HDI
m7_hdi <- bayestestR::hdi(m7, effects = "fixed", component = "conditional", ci = .95)
kable(m7_hdi[sign(m7_hdi$CI_low) == sign(m7_hdi$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')], format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T) %>% remove_column(1)


experiment_dataset_analysis %>%
  data_grid(generalRiskPreference = seq_range(generalRiskPreference, n = 111)) %>%
  add_predicted_draws(m2) %>%
  ggplot(aes(x = generalRiskPreference, y = dominanceSum)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = experiment_1_Dataset, size = 2) +
  scale_fill_brewer()

priorTest <- c(prior(normal(0, 1), class = "b",resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "ethicalPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "ethicalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "ethicalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"), 
              prior(normal(0, 1), class = "b",resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "financialPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "financialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "financialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "financialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
              prior(normal(0, 1), class = "sigma", resp = "financialPreference"), 
              prior(normal(0, 1), class = "b",resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "healthAndSafetyPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
              prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"), 
              prior(normal(0, 1), class = "b",resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "recreationalPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "recreationalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
              prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"), 
              prior(normal(0, 1), class = "b",resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "Age", resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "Gender", resp = "socialPreference"),
              prior(normal(2, 1), class = "b", coef = "dominanceSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "b", coef = "prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), class= "b", coef = "dominanceSum:Gender", resp = "socialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:prestigeSum", resp = "socialPreference"),
              prior(normal(0, 1), class= "b", coef = "Gender:leadershipSum", resp = "socialPreference"),
              prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
              prior(normal(0, 1), class = "sigma", resp = "socialPreference"))



# Risk preferences predicting dopl motivation


m7_DoPL_DOSPERT_Gender <- brm(mvbind(dominanceSum, prestigeSum, leadershipSum) ~ ethicalPreference*Gender + financialPreference*Gender + socialPreference*Gender + healthAndSafetyPreference*Gender + recreationalPreference*Gender, data = experiment_dataset_analysis,
                       iter = 6500, warmup = 500,
                     prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(0, 1), class = "sigma", resp = "dominanceSum"),
                               prior(normal(0, 1), class = "sigma", resp = "prestigeSum"),
                               prior(normal(0, 1), class = "sigma", resp = "leadershipSum"),
                               ### Dominance
                               prior(normal(0, 1), coef = "ethicalPreference", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "financialPreference", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "healthAndSafetyPreference", resp = "dominanceSum"), 
                               prior(normal(0, 1), coef = "recreationalPreference", resp = "dominanceSum"), 
                               prior(normal(0, 1), coef = "socialPreference", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "ethicalPreference:Gender", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "Gender:financialPreference", resp = "dominanceSum"),
                               prior(normal(0, 1), coef = "Gender:healthAndSafetyPreference", resp = "dominanceSum"), 
                               prior(normal(0, 1), coef = "Gender:recreationalPreference", resp = "dominanceSum"), 
                               prior(normal(0, 1), coef = "Gender:socialPreference", resp = "dominanceSum"),
                               ### Prestige
                               prior(normal(0, 1), coef = "ethicalPreference", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "financialPreference", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "healthAndSafetyPreference", resp = "prestigeSum"), 
                               prior(normal(0, 1), coef = "recreationalPreference", resp = "prestigeSum"), 
                               prior(normal(0, 1), coef = "socialPreference", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "ethicalPreference:Gender", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "Gender:financialPreference", resp = "prestigeSum"),
                               prior(normal(0, 1), coef = "Gender:healthAndSafetyPreference", resp = "prestigeSum"), 
                               prior(normal(0, 1), coef = "Gender:recreationalPreference", resp = "prestigeSum"), 
                               prior(normal(0, 1), coef = "Gender:socialPreference", resp = "prestigeSum"),
                               ### Leadership
                               prior(normal(0, 1), coef = "ethicalPreference", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "financialPreference", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "healthAndSafetyPreference", resp = "leadershipSum"), 
                               prior(normal(0, 1), coef = "recreationalPreference", resp = "leadershipSum"), 
                               prior(normal(0, 1), coef = "socialPreference", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "ethicalPreference:Gender", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "Gender:financialPreference", resp = "leadershipSum"),
                               prior(normal(0, 1), coef = "Gender:healthAndSafetyPreference", resp = "leadershipSum"), 
                               prior(normal(0, 1), coef = "Gender:recreationalPreference", resp = "leadershipSum"), 
                               prior(normal(0, 1), coef = "Gender:socialPreference", resp = "leadershipSum")),
                               save_all_pars = T)

summary(m7_DoPL_DOSPERT_Gender)



m7_DoPL_DOSPERT_HDI <- hdi(m7_DoPL_DOSPERT, effects = "fixed", component = "conditional", ci = .95)
m7_DoPL_DOSPERT_HDI[sign(m7_DoPL_DOSPERT_HDI$CI_low) == sign(m7_DoPL_DOSPERT_HDI$CI_high),
            c('Parameter', 'CI','CI_low', 'CI_high')]