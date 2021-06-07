#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(papaja)
library(brms)
library(cmdstanr)
library(rstan)
library(bayestestR)
library(sjPlot)
library(psych)
library(rstanarm)
library(BayesFactor)
library(bayesplot)
library(kableExtra)
library(tidyverse)
library(shinyjs)
options(mc.corrs = parallel::detectCores()) # can run chains in parallel with each other
rstan_options(auto_write = TRUE)
bayesplot::bayesplot_theme_set(theme_apa())
 experiment_1_Dataset <- read.csv("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/DoPL_DOSPERT.csv", stringsAsFactors = FALSE)
experiment_dataset_analysis <- read.csv("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/experiment_dataset_analysis.csv")
correlation_table <- read.csv("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/table_attempt.csv")
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
co2 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/co2.rds")
m1 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1.rds")
m2 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m2.rds")
m3 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m3.rds")
m4 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m4.rds")
m5 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5.rds")
m6 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m6.rds")
m7 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m7.rds")
m1_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_hdi.rds")
m2_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m2_hdi.rds")
m3_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m3_hdi.rds")
m4_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m4_hdi.rds")
m5_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5_hdi.rds")
m6_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m6_hdi.rds")
m7_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m7_hdi.rds")
m1_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_hdi.rds")
m1_int <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_int.rds")
m1_int_d <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_int_d.rds")
m1_int_l <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_int_l.rds")
m1_int_p <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_int_p.rds")
m1_int_d_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_int_d_hdi.rds")
m1_int_l_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_int_l_hdi.rds")
m1_int_p_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m1_int_p_hdi.rds")
demo_m1 <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/demo_m1.rds")
m7_DoPL_DOSPERT <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m7_DoPL_DOSPERT.rds")
m4_perceivedRisk_Age <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m4_perceivedRisk_Age.rds")
m5_generalRiskPreference <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5_generalRiskPreference.rds")
m5_benefitRisk_Age <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5_benefitRisk_Age.rds")
m4_perceivedRisk_Gender <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m4_perceivedRisk_Gender.rds")
m5_benefitRisk_Gender <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5_benefitRisk_Gender.rds")
demo_m1_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/demo_m1_hdi.rds")
m7_DoPL_DOSPERT_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m7_DoPL_DOSPERT_hdi.rds")
m4_perceivedRisk_Age_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m4_perceivedRisk_Age_hdi.rds")
m5_generalRiskPreference_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5_generalRiskPreference_hdi.rds")
m5_benefitRisk_Age_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5_benefitRisk_Age_hdi.rds")
m4_perceivedRisk_Gender_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m4_perceivedRisk_Gender_hdi.rds")
m5_benefitRisk_Gender_hdi <- readRDS("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/m5_benefitRisk_Gender_hdi.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Results from DoPL and DOSPERT Experiment"),
    shinyjs::useShinyjs(),
    # Sidebar with a slider input for number of bins
    navbarPage(
        "Choose either shiny app or rmarkdown file",
        tabPanel("Shiny App",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "bayesianTest1",
                             "Select Main Category:",
                             c(
                                 "DoPL" = "dopl_var",
                                 "Gender" = "gen_var",
                                 "Education" = "edu_var",
                                 "Age" = "age_var",
                                 "DOSPERT" = "dospert_var",
                                 "DOSPERT:Benefit" = "dospert_ben_var",
                                 "DOSPERT:Perception" = "dospert_per_var",
                                 "General Risk" = "dospert_gen_var"
                             )
                         ),
                         hr(),
                         selectInput(
                             "bayesianTest2",
                             "Select Second Group:",
                             c(
                                 "DoPL" = "dopl_var",
                                 "Gender" = "gen_var",
                                 "Education" = "edu_var",
                                 "Age" = "age_var",
                                 "DOSPERT" = "dospert_var",
                                 "DOSPERT:Benefit" = "dospert_ben_var",
                                 "DOSPERT:Perception" = "dospert_per_var",
                                 "General Risk" = "dospert_gen_var"
                             )
                         ),
                         hr(),
                         submitButton(text = "Submit")
                     ),
                     mainPanel(tableOutput("bayesTable"))
                 )
        ),

        # Show a plot of the generated distribution
        tabPanel("RMarkdown File", id = "test",
                 h3("Rmarkdown File"),

        mainPanel(

                div(id = "advanced", htmlOutput("inc"))
        )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {

    shinyjs::onclick("toggleAdvanced",
                     shinyjs::toggle(id = "advanced", anim = TRUE))

   getPage <- function() {
       return(includeHTML("C:/Users/s1932788/OneDrive/Documents/1_UoE/Research/PhD/Experiments/DoPL_DOSPERT/R-analysis/Bayesian/R-analysis_clean.html"))
   }

   output$inc <- renderUI({getPage()})

    output$bayesTable <- function() {

        if(input$bayesianTest1 == "dopl_var" & input$bayesianTest2 == "dopl_var"){
testKable
        }
        else if(input$bayesianTest1 == "dopl_var" & input$bayesianTest2 == "dospert_var"){
            kable(m7_DoPL_DOSPERT_hdi[sign(m7_DoPL_DOSPERT_hdi$CI_low) == sign(m7_DoPL_DOSPERT_hdi$CI_high),
                                      c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_var" & input$bayesianTest2 == "dopl_var"){
            kable(m1_int_hdi[sign(m1_int_hdi$CI_low) == sign(m1_int_hdi$CI_high),
                             c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_ben_var" & input$bayesianTest2 == "gen_var"){
            kable(m5_benefitRisk_Gender_hdi[sign(m5_benefitRisk_Gender_hdi$CI_low) == sign(m5_benefitRisk_Gender_hdi$CI_high),
                                            c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_ben_var" & input$bayesianTest2 == "age_var"){
            kable(m5_benefitRisk_Age_hdi[sign(m5_benefitRisk_Age_hdi$CI_low) == sign(m5_benefitRisk_Age_hdi$CI_high),
                                         c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_per_var" & input$bayesianTest2 == "gen_var"){
            kable(m4_perceivedRisk_Gender_hdi[sign(m4_perceivedRisk_Gender_hdi$CI_low) == sign(m4_perceivedRisk_Gender_hdi$CI_high),
                                              c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_per_var" & input$bayesianTest2 == "age_var"){
            kable(m4_perceivedRisk_Age_hdi[sign(m4_perceivedRisk_Age_hdi$CI_low) == sign(m4_perceivedRisk_Age_hdi$CI_high),
                                           c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_gen_var" & input$bayesianTest2 == "dopl_var"){
            kable(m4_hdi[sign(m4_hdi$CI_low) == sign(m4_hdi$CI_high),
                         c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_gen_var" & input$bayesianTest2 == "gen_var"){
            kable(m5_hdi[sign(m5_hdi$CI_low) == sign(m5_hdi$CI_high),
                         c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
        else if(input$bayesianTest1 == "dospert_gen_var" & input$bayesianTest2 == "age_var"){
            kable(m6_hdi[sign(m6_hdi$CI_low) == sign(m6_hdi$CI_high),
                         c('Parameter', 'CI','CI_low', 'CI_high')] , format = "html", booktabs = T, escape = F, longtable = F, digits = 2) %>% kable_styling(full_width = T)
        }
    }

}

# Run the application
shinyApp(ui = ui, server = server)
