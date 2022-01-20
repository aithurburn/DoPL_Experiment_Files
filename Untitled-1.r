demographicQuestions <- c("Age", "Gender", "Ethnicity", "Education")

demo_table_1 <- experiment_1_Dataset[, demographicQuestions]

demo_table_1 <- demo_table_1 %>% mutate(Gender = case_when(
demo_table_1$Gender == 0 ~ "Male",
demo_table_1$Gender == 1 ~  "Female",
demo_table_1$Gender == 2 ~ "Gender Non-Binary"
)) %>% mutate(Ethnicity = case_when(
demo_table_1$Ethnicity == 0	~	"Prefer not to respond",
demo_table_1$Ethnicity == 1	~	"Scottish",
demo_table_1$Ethnicity == 2	~	"English",
demo_table_1$Ethnicity == 3	~	"European",
demo_table_1$Ethnicity == 4	~	"Latin American",
demo_table_1$Ethnicity == 5	~	"Asian",
demo_table_1$Ethnicity == 6	~	"Arab",
demo_table_1$Ethnicity == 7	~	"African",
demo_table_1$Ethnicity == 8	~	"Other"
)) %>% mutate(Education = case_when(
demo_table_1$Education == 0 ~ "Prefer not to answer",
demo_table_1$Education == 1 ~ "Primary School",
demo_table_1$Education == 2 ~ "GCSes or Equivalent",
demo_table_1$Education == 3 ~ "A-Levels or Equivalent",
demo_table_1$Education == 4 ~ "University Undergraduate Program",
demo_table_1$Education == 5 ~ "University Post-Graduate Program",
demo_table_1$Education == 6 ~ "Doctoral Degree"
))


options(digits = 2)
demo_names <- matrix(1:24, nrow = 24)
  for (i in names(demo_table_1)){
  if(is.numeric(demo_table_1[[i]])){
    options(digits = 2)
    demo_table_1_sd <- sd(demo_table_1$Age, na.rm = T)
    demo_table_1_mean <- mean(demo_table_1$Age, na.rm = T)
    demo_table_1_median <- median(demo_table_1$Age, na.rm = T)
    demo_table_1_min <- min(demo_table_1$Age, na.rm = T)
    demo_table_1_max <- max(demo_table_1$Age, na.rm = T)
    age_demo_table_1 <- data.frame("Mean (SD)", demo_table_1_mean, demo_table_1_sd)
    age_demo_table_2 <- data.frame("Median [Min, Max]", demo_table_1_median, demo_table_1_min, demo_table_1_max)
    age_demo_table_2$Freq <- paste0(age_demo_table_2$demo_table_1_median, " [", age_demo_table_2$demo_table_1_min, ",", age_demo_table_2$demo_table_1_max, "]")
    age_demo_table_2 <- age_demo_table_2[, -c(2:4)]
    age_demo_table_1$Freq <- paste0(round(age_demo_table_1$demo_table_1_mean, digits = 2), " (", round(age_demo_table_1$demo_table_1_sd, digits = 2), ")")
    age_demo_table_1 <- age_demo_table_1[, -c(2:3)]
    colnames(age_demo_table_1) <- c("Var1", "new")
colnames(age_demo_table_2) <- c("Var1", "new")
    }
  for (i in colnames(demo_table_1)){
 
 if(!is.numeric(demo_table_1[[i]])){
   options(digits = 2)
     Education_Table <- data.frame(table(demo_table_1$Education))
     Education_Table_percentage <- data.frame(sprintf("(%.1f%%)",round(prop.table(table(demo_table_1$Education))*100, 3)))
     colnames(Education_Table_percentage) <- "Freq"
     Ethnicity_Table <- data.frame(table(demo_table_1$Ethnicity))
     Ethnicity_Table_percentage <- data.frame(sprintf("(%.1f%%)",round(prop.table(table(demo_table_1$Ethnicity))*100, 3)))
     colnames(Ethnicity_Table_percentage) <- "Freq"
     Gender_Table <- data.frame(table(demo_table_1$Gender))
     Gender_Table_percentage <- data.frame(sprintf("(%.1f%%)",round(prop.table(table(demo_table_1$Gender))*100, 3)))
      colnames(Gender_Table_percentage) <- "Freq"
 }
  }
  }

options(digits = 2)
demographic_table <- rbind(Gender_Table, Education_Table, Ethnicity_Table)
demographic_table_percentage <- rbind(Gender_Table_percentage, Education_Table_percentage, Ethnicity_Table_percentage)
demographic_table["Percentage"] <- demographic_table_percentage["Freq"]
demographic_table$new <- paste(demographic_table$Freq, demographic_table$Percentage)

demographic_table <- demographic_table[, -c(2:3)]
demographic_table <- rbind(age_demo_table_1, age_demo_table_2, demographic_table)

write.csv(demographic_table, "demo_table.csv")

kable(demographic_table, format = "html", bootabs = T, escape = F, longtable = T,
      col.names = c("", "Overall *n* = 92"),
      align = c("l", "c"),
      caption = "*Participant Demographic Information*") %>%
  kable_styling(full_width = F) %>%
      row_spec(row = 0, align = "c") %>%
      column_spec(column = 1, width = "1.5in") %>%
      column_spec(column = 2, width = "1in") %>%
      pack_rows("Age", 1, 2) %>%
      pack_rows("Gender", 3, 4) %>%
      pack_rows("Ethnicity", 11, 17) %>%
      pack_rows("Ethnic Origin", 18, 22) %>%
      pack_rows("Educational Attainment", 5, 10)
  








0	~	Prefer not to respond
1	~	Scottish
2	~	English
3	~	European
4	~	Latin American
5	~	Asian
6	~	Arab
7	~	African
8	~	Other

saveRDS(corr, "corr.rds")
