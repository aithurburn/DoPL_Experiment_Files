library(papaja)
library(ggplot2)
library(sjPlot)
library(readr)
library(RColorBrewer)
library(tidyr)
library(grid)  
library(gtable)  
library(DiagrammeR)
library(lubridate)
library(DT)
library(kableExtra)
library(svgPanZoom)
library(knitr)
library(ggmcmc)
library(ggridges)
library(rstan)
library(insight)
library(shiny)
library(gridExtra)
library(ggpubr)
library(gghighlight)
library(ggtext)
library(readr)
library(ggcorrplot)
library(ggprism)
library(brms)
library(parameters)
library(rmarkdown)
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
options(mc.corrs = parallel::detectCores(), brms.backend = "cmdstanr") # can run chains in parallel with each other
rstan_options(auto_write = TRUE)
m1 <- readRDS("./R-analysis/m1.rds")
m2 <- readRDS("./R-analysis/m2.rds")
m3 <- readRDS("./R-analysis/m3.rds")
m5 <- readRDS("./R-analysis/m5.rds")
m6 <- readRDS("./R-analysis/m6.rds")
m7 <- readRDS("./R-analysis/m7.rds")


m1_hdi <- readRDS("./R-analysis/m1_hdi.rds")
m2_hdi <- readRDS("./R-analysis/m2_hdi.rds")
m3_hdi <- readRDS("./R-analysis/m3_hdi.rds")
m4_hdi <- readRDS("./R-analysis/m4_hdi.rds")
m5_hdi <- readRDS("./R-analysis/m5_hdi.rds")
m6_hdi <- readRDS("./R-analysis/m6_hdi.rds")
m7_hdi <- readRDS("./R-analysis/m7_hdi.rds")
m1_hdi <- readRDS("./R-analysis/m1_hdi.rds")
m1_int <- readRDS("./R-analysis/m1_int.rds")
m1_int_d <- readRDS("./R-analysis/m1_int_d.rds")
m1_int_l <- readRDS("./R-analysis/m1_int_l.rds")
m1_int_p <- readRDS("./R-analysis/m1_int_p.rds")
m1_int_d_hdi <- readRDS("./R-analysis/m1_int_d_hdi.rds")
m1_int_l_hdi <- readRDS("./R-analysis/m1_int_l_hdi.rds")
m1_int_p_hdi <- readRDS("./R-analysis/m1_int_p_hdi.rds")
m7_DoPL_DOSPERT <- readRDS("./R-analysis/m7_DoPL_DOSPERT.rds")
m4_perceivedRisk_Age <- readRDS("./R-analysis/m4_perceivedRisk_Age.rds")
m5_generalRiskPreference <- readRDS("./R-analysis/m5_generalRiskPreference.rds")
m5_benefitRisk_Age <- readRDS("./R-analysis/m5_benefitRisk_Age.rds")
m4_perceivedRisk_Gender <- readRDS("./R-analysis/m4_perceivedRisk_Gender.rds")
m5_benefitRisk_Gender <- readRDS("./R-analysis/m5_benefitRisk_Gender.rds")
m7_DoPL_DOSPERT_hdi <- readRDS("./R-analysis/m7_DoPL_DOSPERT_hdi.rds")
m4_perceivedRisk_Age_hdi <- readRDS("./R-analysis/m4_perceivedRisk_Age_hdi.rds")
m5_generalRiskPreference_hdi <- readRDS("./R-analysis/m5_generalRiskPreference_hdi.rds")
m5_benefitRisk_Age_hdi <- readRDS("./R-analysis/m5_benefitRisk_Age_hdi.rds")
m4_perceivedRisk_Gender_hdi <- readRDS("./R-analysis/m4_perceivedRisk_Gender_hdi.rds")
m5_benefitRisk_Gender_hdi <- readRDS("./R-analysis/m5_benefitRisk_Gender_hdi.rds")
#load(".RData")
experiment_1_Dataset <- read.csv("experiment_1_data.csv")
experiment_dataset_analysis <- read.csv("experiment_dataset_analysis1.csv")

DoPLQuestions <- c('DoPL_1', 'DoPL_6', 'DoPL_11', 'DoPL_13', 'DoPL_14', 'DoPL_16', 'DoPL_5', 'DoPL_7', 'DoPL_8', 'DoPL_12', 'DoPL_17', 'DoPL_18', 'DoPL_2', 'DoPL_3', 'DoPL_4', 'DoPL_9', 'DoPL_10', 'DoPL_15')

dominanceQuestions <- c('DoPL_2','DoPL_3','DoPL_4','DoPL_9','DoPL_10','DoPL_15')
prestigeQuestions <- c('DoPL_5','DoPL_7','DoPL_8','DoPL_12','DoPL_17','DoPL_18')
leadershipQuestions <- c('DoPL_1','DoPL_6','DoPL_11','DoPL_13','DoPL_14','DoPL_16')


# UMS
UMSQuestions <- c('UMS_1', 'UMS_2', 'UMS_3', 'UMS_4','UMS_5','UMS_6','UMS_7','UMS_8','UMS_9','UMS_11', 'UMS_12', 'UMS_13')
UMSIntimacyQuestions <- c('UMS_11', 'UMS_12', 'UMS_13')
UMSAffiliationQuestions <- c('UMS_1', 'UMS_2', 'UMS_3', 'UMS_4','UMS_5','UMS_6','UMS_7','UMS_8','UMS_9')


# DOSPERT
riskQuestions <- c('risk_1', 'risk_2', 'risk_3', 'risk_4', 'risk_5', 'risk_6', 'risk_7', 'risk_8', 'risk_9', 'risk_10', 'risk_11', 'risk_12', 'risk_13', 'risk_14', 'risk_15', 'risk_16', 'risk_17', 'risk_18', 'risk_19', 'risk_20', 'risk_21', 'risk_22', 'risk_23', 'risk_24', 'risk_25', 'risk_26', 'risk_27', 'risk_28', 'risk_29', 'risk_30', 'risk_31', 'risk_32', 'risk_33', 'risk_34', 'risk_35', 'risk_36', 'risk_37', 'risk_38', 'risk_39', 'risk_40')

ethicalQuestionsRisk <- c('risk_7',    
'risk_10',	
'risk_11',	
'risk_21',	
'risk_28',	
'risk_29',	
'risk_39',	
'risk_40'	)
financialQuestionsRisk <- c('risk_3',	
'risk_5',	
'risk_9',	
'risk_15',	
'risk_18',	
'risk_23',	
'risk_26',	
'risk_19'	)
healthAndSafetyQuestionsRisk <- c('risk_6',	
'risk_20',	
'risk_22',	
'risk_25',	
'risk_31',	
'risk_34',	
'risk_35',	
'risk_36'	)
recreationalQuestionsRisk <- c('risk_2',	
'risk_4',	
'risk_12',	
'risk_14',	
'risk_17',	
'risk_24',	
'risk_32',	
'risk_33'	)
socialQuestionsRisk <- c('risk_1',	
'risk_8',	
'risk_13',	
'risk_16',	
'risk_27',	
'risk_30',	
'risk_37',	
'risk_38')

riskPerceptionQuestions <- c('riskPerception_1', 'riskPerception_2', 'riskPerception_3', 'riskPerception_4', 'riskPerception_5', 'riskPerception_6', 'riskPerception_7', 'riskPerception_8', 'riskPerception_9', 'riskPerception_10', 'riskPerception_11', 'riskPerception_12', 'riskPerception_13', 'riskPerception_14', 'riskPerception_15', 'riskPerception_16', 'riskPerception_17', 'riskPerception_18', 'riskPerception_19', 'riskPerception_20', 'riskPerception_21', 'riskPerception_22', 'riskPerception_23', 'riskPerception_24', 'riskPerception_25', 'riskPerception_26', 'riskPerception_27', 'riskPerception_28', 'riskPerception_29', 'riskPerception_30', 'riskPerception_31', 'riskPerception_32', 'riskPerception_33', 'riskPerception_34', 'riskPerception_35', 'riskPerception_36', 'riskPerception_37', 'riskPerception_38', 'riskPerception_39', 'riskPerception_40')

ethicalQuestionsPerception <- c('riskPerception_7', 
'riskPerception_10',
'riskPerception_11',
'riskPerception_21',
'riskPerception_28',
'riskPerception_29',
'riskPerception_39',
'riskPerception_40')
financialQuestionsPerception <- c('riskPerception_3',	
'riskPerception_5',	
'riskPerception_9',	
'riskPerception_15',
'riskPerception_18',
'riskPerception_23',
'riskPerception_26',
'riskPerception_19'	)
healthAndSafetyQuestionsPerception <- c('riskPerception_6',	
'riskPerception_20',
'riskPerception_22',
'riskPerception_25',
'riskPerception_31',
'riskPerception_34',
'riskPerception_35',
'riskPerception_36'	)
recreationalQuestionsPerception <- c('riskPerception_2',	
'riskPerception_4',	
'riskPerception_12',
'riskPerception_14',
'riskPerception_17',
'riskPerception_24',
'riskPerception_32',
'riskPerception_33'	)
socialQuestionsPerception <- c('riskPerception_1',	
'riskPerception_8',	
'riskPerception_13',
'riskPerception_16',
'riskPerception_27',
'riskPerception_30',
'riskPerception_37',
'riskPerception_38')

riskBenefitQuestions <- c('riskBenefit_1', 'riskBenefit_2', 'riskBenefit_3', 'riskBenefit_4', 'riskBenefit_5', 'riskBenefit_6', 'riskBenefit_7', 'riskBenefit_8', 'riskBenefit_9', 'riskBenefit_10', 'riskBenefit_11', 'riskBenefit_12', 'riskBenefit_13', 'riskBenefit_14', 'riskBenefit_15', 'riskBenefit_16', 'riskBenefit_17', 'riskBenefit_18', 'riskBenefit_19', 'riskBenefit_20', 'riskBenefit_21', 'riskBenefit_22', 'riskBenefit_23', 'riskBenefit_24', 'riskBenefit_25', 'riskBenefit_26', 'riskBenefit_27', 'riskBenefit_28', 'riskBenefit_29', 'riskBenefit_30', 'riskBenefit_31', 'riskBenefit_32', 'riskBenefit_33', 'riskBenefit_34', 'riskBenefit_35', 'riskBenefit_36', 'riskBenefit_37', 'riskBenefit_38', 'riskBenefit_39', 'riskBenefit_40')
ethicalQuestionsBenefit <- c('riskBenefit_7', 
'riskBenefit_10',
'riskBenefit_11',
'riskBenefit_21',
'riskBenefit_28',
'riskBenefit_29',
'riskBenefit_39',
'riskBenefit_40')
financialQuestionsBenefit <- c('riskBenefit_3',	
'riskBenefit_5',	
'riskBenefit_9',	
'riskBenefit_15',
'riskBenefit_18',
'riskBenefit_23',
'riskBenefit_26',
'riskBenefit_19'	)
healthAndSafetyQuestionsBenefit <- c('riskBenefit_6',	
'riskBenefit_20',
'riskBenefit_22',
'riskBenefit_25',
'riskBenefit_31',
'riskBenefit_34',
'riskBenefit_35',
'riskBenefit_36'	)
recreationalQuestionsBenefit <- c('riskBenefit_2',	
'riskBenefit_4',	
'riskBenefit_12',
'riskBenefit_14',
'riskBenefit_17',
'riskBenefit_24',
'riskBenefit_32',
'riskBenefit_33'	)
socialQuestionsBenefit <- c('riskBenefit_1',	
'riskBenefit_8',	
'riskBenefit_13',
'riskBenefit_16',
'riskBenefit_27',
'riskBenefit_30',
'riskBenefit_37',
'riskBenefit_38')
demo_table <- read.csv("demo_table.csv")
theme_set(theme_apa(base_size = 12))

time_df <- read.csv("DoPL_DOSPERT.csv")
time_df <- time_df[-1:-2,]
library(lubridate) 
average_completion <- mean(as.numeric(time_df$Duration..in.seconds.))
stdv_DF <- sd(as.numeric(time_df$Duration..in.seconds.))
