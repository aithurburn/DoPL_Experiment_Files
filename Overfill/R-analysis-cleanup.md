---
title: "R Data tutorial"
author: "Andrew Ithurburn"
date: "22/03/2021"
output:
  word_document: default
  toc: yes
  html_document: null
---

```{r loading the necessary files}
library(DT)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(papaja)
experiment_1_Dataset <- read.csv("DoPL_DOSPERT.csv", stringsAsFactors = FALSE)

locfunc <- function(data, to){
  which(colnames({{data}})=={{to}})
}
locfunc(experiment_1_Dataset, "DoPL_1")
lfunc <- function(dataset, to, from) {
  loc1 <- which(colnames({{dataset}}) == {{to}})
  loc2 <- which(colnames({{dataset}}) == {{from}})
  length({{dataset}}[loc1:loc2])
}
theme_set(theme_apa(base_size = 12))

[@]

```

```{r Look at the data}

datatable(experiment_1_Dataset)

```


```{r}

experiment_1_Dataset <- experiment_1_Dataset[-1:-2,]
```

```{r Look again at the data}

datatable(experiment_1_Dataset)

```


```{r rename the demographic columns, include=FALSE}
colnames(experiment_1_Dataset)[colnames(experiment_1_Dataset) == "Q1"] <- "Gender"
colnames(experiment_1_Dataset)[colnames(experiment_1_Dataset) == "Q78"] <- "Age"
colnames(experiment_1_Dataset)[colnames(experiment_1_Dataset) == "Q35"] <- "Occupation"
colnames(experiment_1_Dataset)[colnames(experiment_1_Dataset) == "Q49"] <- "Education"
colnames(experiment_1_Dataset)[colnames(experiment_1_Dataset) == "Q50"] <- "Ethnicity"


```

```{r locate the the necessary columns and rename to the corresponding datatype}

varname <- "risk"
varname2 <- "riskPerception"
varname3 <- "riskBenefit"



n <- lfunc(experiment_1_Dataset,"Q55_1", "Yes.Q57_10")
n2 <- lfunc(experiment_1_Dataset, "Yes.Q65_1", "Q62_10")
n3 <- lfunc(experiment_1_Dataset, "Q67_1", "Q70_10")


names(experiment_1_Dataset)[locfunc(experiment_1_Dataset, "Q55_1"):locfunc(experiment_1_Dataset, "Yes.Q57_10")] <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep="_"), varname, n))
names(experiment_1_Dataset)[locfunc(experiment_1_Dataset, "Yes.Q65_1"):locfunc(experiment_1_Dataset, "Q62_10")] <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep="_"), varname2, n2))
names(experiment_1_Dataset)[locfunc(experiment_1_Dataset, "Q67_1"):locfunc(experiment_1_Dataset, "Q70_10")] <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep="_"), varname3, n3))

```

```{r begin the recoding process}

experiment_1_Dataset <- experiment_1_Dataset %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "Gender")), ~as.numeric(recode(.,"Male" = 0, "Female" = 1, "Gender Non-Binary" = 2))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "Education")), ~as.numeric(recode(., "Prefer not to say" = 0, "Primary School" = 1, "GCSEs or equivalent" = 2, "A-levels or equivalent" = 3, "University Undergraduate Program" = 4, "University Postgraduate Program" = 5, "Doctoral Degree" = 6))) %>%
   mutate_at(vars(locfunc(experiment_1_Dataset, 'Ethnicity')), ~as.numeric(recode(., "Prefer not to respond" = 0, "Scottish" = 1, "English" = 2, "European" = 3, "Latin American" = 4, "Asian" = 5, "Arab" = 6, "African" = 7, "Other" = 8))) %>%
   mutate_at(vars(locfunc(experiment_1_Dataset, "risk_1"):locfunc(experiment_1_Dataset, "risk_40")), ~as.numeric(recode(., "Very unlikely" = 1, "Unlikely" = 2, "Not sure" = 3, "Likely" = 4, "Very likely" = 5))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "riskPerception_1"):locfunc(experiment_1_Dataset, "riskPerception_40")), ~as.numeric(recode(., "Not at all risky" = 1, "Somewhat risky" = 2, "Moderately risky" = 3, "Very risky" = 4, "Extremely risky" = 5))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "riskBenefit_1"):locfunc(experiment_1_Dataset, "riskBenefit_40")), ~as.numeric(recode(., "No benefits at all" = 1, "Few benefits" = 2, "Moderate benefits" = 3, "Many benefits" = 4, "Great benefits" = 5))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "DoPL_1"):locfunc(experiment_1_Dataset, "DoPL_5")), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Somewhat agree" = 4, "Agree" = 5, "Strongly agree" = 6)))  %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "DoPL_6"), locfunc(experiment_1_Dataset, "DoPL_14")), ~as.numeric(recode(., "Strongly disagree" = 6, "Disagree" = 5, "Somewhat disagree" = 4, "Somewhat agree" = 3, "Agree" = 2, "Strongly agree" = 1))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "DoPL_7"):locfunc(experiment_1_Dataset, "DoPL_13")), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Somewhat agree" = 4, "Agree" = 5, "Strongly agree" = 6))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "DoPL_15"):locfunc(experiment_1_Dataset, "UMS_10")), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Somewhat agree" = 4, "Agree" = 5, "Strongly agree" = 6))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "DoPL_17"):locfunc(experiment_1_Dataset, "DoPL_18")), ~as.numeric(recode(., "Not Important To Me" = 1, "Of Little Importance To me" = 2, "Of Some Importance To Me" = 3, "Important To Me" = 4, "Very Important To me" = 5, "Extremely Important To Me" = 6))) %>%
  mutate_at(vars(locfunc(experiment_1_Dataset, "UMS_11"):locfunc(experiment_1_Dataset, "UMS_13")), ~as.numeric(recode(., "Not Important To Me" = 1, "Of Little Importance To me" = 2, "Of Some Importance To Me" = 3, "Important To Me" = 4, "Very Important To me" = 5, "Extremely Important To Me" = 6)))


```

```{r Once recoded remove the unnecessary columns}
experiment_1_Dataset <- experiment_1_Dataset[,-locfunc(experiment_1_Dataset, "StartDate"):-locfunc(experiment_1_Dataset, "Progress")]
experiment_1_Dataset <- experiment_1_Dataset[,-locfunc(experiment_1_Dataset, "Finished"):-locfunc(experiment_1_Dataset, "Q52")]

```

```{r For ease of analysis store the like columns to their corresponding question type}
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

plot_dopl <- function(dopldf, doplxcol, doplycol, dopltitle, doplgeom, doplColor){
  ggplot2::ggplot(data = dopldf, aes(x ={{doplxcol }}, y = {{doplycol}}), fill = {{doplColor}}) + 
    {{doplgeom}} + 
    ggtitle(dopltitle) +
    theme_pubclean() +
    theme(plot.title=element_text(size=18))
}

var.labels = c("Male", "Female", "Gender Non-Binary")

testerDF <- experiment_1_Dataset

myfunction <- function(enterkey, entervalue, rangeofdata){
  testerDF <- experiment_1_Dataset %>%
  gather(key = {{enterkey}}, value = {{entervalue}}, {{rangeofdata}})
}

```

```{r}
sapply(experiment_1_Dataset, class)

experiment_1_Dataset$Age <- as.numeric(as.character(experiment_1_Dataset$Age))
experiment_1_Dataset$Duration..in.seconds. <- as.numeric(as.character(experiment_1_Dataset$Duration..in.seconds.))
```

```{r}

datatable(experiment_1_Dataset)

```

