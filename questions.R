dominanceQuestions <- c('DoPL_2','DoPL_3','DoPL_4','DoPL_9','DoPL_10','DoPL_15')
prestigeQuestions <- c('DoPL_5','DoPL_7','DoPL_8','DoPL_12','DoPL_17','DoPL_18')
leadershipQuestions <- c('DoPL_1','DoPL_6','DoPL_11','DoPL_13','DoPL_14','DoPL_16')

DoPL_df <- data.frame(dominanceQuestions, prestigeQuestions, leadershipQuestions)

experiment_1_Dataset$dominanceSum <-
  rowSums(experiment_1_Dataset[, dominanceQuestions])
experiment_1_Dataset$prestigeSum <-
  rowSums(experiment_1_Dataset[, prestigeQuestions])
experiment_1_Dataset$leadershipSum <-
  rowSums(experiment_1_Dataset[, leadershipQuestions])


riskQuestions <- c('risk_1', 'risk_2', 'risk_3', 'risk_4', 'risk_5', 'risk_6', 'risk_7', 'risk_8', 'risk_9', 'risk_10', 'risk_11', 'risk_12', 'risk_13', 'risk_14', 'risk_15', 'risk_16', 'risk_17', 'risk_18', 'risk_19', 'risk_20', 'risk_21', 'risk_22', 'risk_23', 'risk_24', 'risk_25', 'risk_26', 'risk_27', 'risk_28', 'risk_29', 'risk_30', 'risk_31', 'risk_32', 'risk_33', 'risk_34', 'risk_35', 'risk_36', 'risk_37', 'risk_38', 'risk_39', 'risk_40')
riskPerceptionQuestions <- c('riskPerception_1', 'riskPerception_2', 'riskPerception_3', 'riskPerception_4', 'riskPerception_5', 'riskPerception_6', 'riskPerception_7', 'riskPerception_8', 'riskPerception_9', 'riskPerception_10', 'riskPerception_11', 'riskPerception_12', 'riskPerception_13', 'riskPerception_14', 'riskPerception_15', 'riskPerception_16', 'riskPerception_17', 'riskPerception_18', 'riskPerception_19', 'riskPerception_20', 'riskPerception_21', 'riskPerception_22', 'riskPerception_23', 'riskPerception_24', 'riskPerception_25', 'riskPerception_26', 'riskPerception_27', 'riskPerception_28', 'riskPerception_29', 'riskPerception_30', 'riskPerception_31', 'riskPerception_32', 'riskPerception_33', 'riskPerception_34', 'riskPerception_35', 'riskPerception_36', 'riskPerception_37', 'riskPerception_38', 'riskPerception_39', 'riskPerception_40')
riskBenefitQuestions <- c('riskBenefit_1', 'riskBenefit_2', 'riskBenefit_3', 'riskBenefit_4', 'riskBenefit_5', 'riskBenefit_6', 'riskBenefit_7', 'riskBenefit_8', 'riskBenefit_9', 'riskBenefit_10', 'riskBenefit_11', 'riskBenefit_12', 'riskBenefit_13', 'riskBenefit_14', 'riskBenefit_15', 'riskBenefit_16', 'riskBenefit_17', 'riskBenefit_18', 'riskBenefit_19', 'riskBenefit_20', 'riskBenefit_21', 'riskBenefit_22', 'riskBenefit_23', 'riskBenefit_24', 'riskBenefit_25', 'riskBenefit_26', 'riskBenefit_27', 'riskBenefit_28', 'riskBenefit_29', 'riskBenefit_30', 'riskBenefit_31', 'riskBenefit_32', 'riskBenefit_33', 'riskBenefit_34', 'riskBenefit_35', 'riskBenefit_36', 'riskBenefit_37', 'riskBenefit_38', 'riskBenefit_39', 'riskBenefit_40')

DoPLQuestions <- c('DoPL_1', 'DoPL_6', 'DoPL_11', 'DoPL_13', 'DoPL_14', 'DoPL_16', 'DoPL_5', 'DoPL_7', 'DoPL_8', 'DoPL_12', 'DoPL_17', 'DoPL_18', 'DoPL_2', 'DoPL_3', 'DoPL_4', 'DoPL_9', 'DoPL_10', 'DoPL_15')

ethicalQuestionsRisk <- c("risk_5", "risk_9", "risk_12", "risk_13", "risk_14", "risk_20", "risk_25", "risk_28")
financialQuestionsRisk <- c("risk_3", "risk_7", "risk_11", "risk_18", "risk_22", "risk_24", "risk_30", "risk_33")
healthAndSafetyQuestionsRisk <- c("risk_3", "risk_7", "risk_27", "risk_29", "risk_32", "risk_36", "risk_39", "risk_40")
recreationalQuestionsRisk <- c("risk_2", "risk_5", "risk_15", "risk_17", "risk_21", "risk_31", "risk_37", "risk_38")
socialQuestionsRisk <- c("risk_1", "risk_10", "risk_16", "risk_18", "risk_23", "risk_26", "risk_34", "risk_35")
ethicalQuestionsBenefit <- c("riskBenefit_5", "riskBenefit_9", "riskBenefit_12", "riskBenefit_13", "riskBenefit_14", "riskBenefit_20", "riskBenefit_25", "riskBenefit_28")
financialQuestionsBenefit <- c("riskBenefit_3", "riskBenefit_7", "riskBenefit_11", "riskBenefit_18", "riskBenefit_22", "riskBenefit_24", "riskBenefit_30", "riskBenefit_33")
healthAndSafetyQuestionsBenefit <- c("riskBenefit_3", "riskBenefit_7", "riskBenefit_27", "riskBenefit_29", "riskBenefit_32", "riskBenefit_36", "riskBenefit_39", "riskBenefit_40")
recreationalQuestionsBenefit <- c("riskBenefit_2", "riskBenefit_5", "riskBenefit_15", "riskBenefit_17", "riskBenefit_21", "riskBenefit_31", "riskBenefit_37", "riskBenefit_38")
socialQuestionsBenefit <- c("riskBenefit_1", "riskBenefit_10", "riskBenefit_16", "riskBenefit_18", "riskBenefit_23", "riskBenefit_26", "riskBenefit_34", "riskBenefit_35")
ethicalQuestionsPerception <- c("riskPerception_5", "riskPerception_9", "riskPerception_12", "riskPerception_13", "riskPerception_14", "riskPerception_20", "riskPerception_25", "riskPerception_28")
financialQuestionsPerception <- c("riskPerception_3", "riskPerception_7", "riskPerception_11", "riskPerception_18", "riskPerception_22", "riskPerception_24", "riskPerception_30", "riskPerception_33")
healthAndSafetyQuestionsPerception <- c("riskPerception_3", "riskPerception_7", "riskPerception_27", "riskPerception_29", "riskPerception_32", "riskPerception_36", "riskPerception_39", "riskPerception_40")
recreationalQuestionsPerception <- c("riskPerception_2", "riskPerception_5", "riskPerception_15", "riskPerception_17", "riskPerception_21", "riskPerception_31", "riskPerception_37", "riskPerception_38")
socialQuestionsPerception <- c("riskPerception_1", "riskPerception_10", "riskPerception_16", "riskPerception_18", "riskPerception_23", "riskPerception_26", "riskPerception_34", "riskPerception_35")

experiment_1_Dataset$riskSum <- rowSums(experiment_1_Dataset[, riskQuestions])
```

```{r It would be good to get the sums of the some of the data}

experiment_1_Dataset$riskSum <- rowSums(experiment_1_Dataset[, riskQuestions])
experiment_1_Dataset$riskPerceptionSum <- rowSums(experiment_1_Dataset[, riskPerceptionQuestions])
experiment_1_Dataset$riskBenefitSum <- rowSums(experiment_1_Dataset[, riskBenefitQuestions])
experiment_1_Dataset$ethicalQuestionsRiskSum <- rowSums(experiment_1_Dataset[, ethicalQuestionsRisk])
experiment_1_Dataset$financialQuestionsRiskSum <- rowSums(experiment_1_Dataset[, financialQuestionsRisk])
experiment_1_Dataset$healthAndSafetyQuestionsRiskSum <- rowSums(experiment_1_Dataset[, healthAndSafetyQuestionsRisk])
experiment_1_Dataset$recreationalQuestionsRiskSum  <- rowSums(experiment_1_Dataset[, recreationalQuestionsRisk])
experiment_1_Dataset$socialQuestionsRiskSum  <- rowSums(experiment_1_Dataset[, socialQuestionsBenefit])
experiment_1_Dataset$ethicalQuestionsBenefitSum  <- rowSums(experiment_1_Dataset[, ethicalQuestionsBenefit])
experiment_1_Dataset$financialQuestionsBenefitSum  <- rowSums(experiment_1_Dataset[, financialQuestionsBenefit])
experiment_1_Dataset$healthAndSafetyQuestionsBenefitSum  <- rowSums(experiment_1_Dataset[, healthAndSafetyQuestionsBenefit])
experiment_1_Dataset$recreationalQuestionsBenefitSum  <- rowSums(experiment_1_Dataset[, recreationalQuestionsBenefit])
experiment_1_Dataset$socialQuestionsBenefitSum  <- rowSums(experiment_1_Dataset[, socialQuestionsBenefit])
experiment_1_Dataset$ethicalQuestionsPerceptionSum  <- rowSums(experiment_1_Dataset[, ethicalQuestionsPerception])
experiment_1_Dataset$financialQuestionsPerceptionSum  <- rowSums(experiment_1_Dataset[, financialQuestionsPerception])
experiment_1_Dataset$healthAndSafetyQuestionsPerceptionSum  <- rowSums(experiment_1_Dataset[, healthAndSafetyQuestionsPerception])
experiment_1_Dataset$recreationalQuestionsPerceptionSum  <- rowSums(experiment_1_Dataset[, recreationalQuestionsPerception])
experiment_1_Dataset$socialQuestionsPerceptionSum  <- rowSums(experiment_1_Dataset[, socialQuestionsPerception])
experiment_1_Dataset$DoPLSum  <- rowSums(experiment_1_Dataset[, DoPLQuestions])
