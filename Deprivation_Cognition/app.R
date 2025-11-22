install.packages("shiny")
install.packages("DT")
install.packages("ggplot2")
install.packages("dplyr")
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

sleep_df <- read.csv(
  "sleep_deprivation_dataset_detailed 2.csv",
  stringsAsFactors = FALSE
)

sleep_df$Gender <- factor(sleep_df$Gender)

# Create UI:

ui <- fluidPage(
  titlePanel("Sleep Deprivation & Cognitive Performance Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Participants"),
      sliderInput(
        "sleepRange",
        "Sleep Hours (per night):",
        min = min(sleep_df$Sleep_Hours, na.rm = TRUE),
        max = max(sleep_df$Sleep_Hours, na.rm = TRUE),
        value = c(min(sleep_df$Sleep_Hours, na.rm = TRUE),
                  max(sleep_df$Sleep_Hours, na.rm = TRUE)),
        step = 0.1
      ),
      checkboxGroupInput(
        "genderFilter",
        "Gender:",
        choices = levels(sleep_df$Gender),
        selected = levels(sleep_df$Gender)
      ),
      tags$hr(),
      h4("Plot Controls"),
      selectInput(
        "histVar",
        "Histogram variable:",
        choices = c(
          "Sleep_Hours",
          "Sleep_Quality_Score",
          "Daytime_Sleepiness",
          "Stroop_Task_Reaction_Time",
          "N_Back_Accuracy",
          "PVT_Reaction_Time",
          "Emotion_Regulation_Score",
          "Caffeine_Intake",
          "Physical_Activity_Level",
          "Stress_Level",
          "BMI"
        ),
        selected = "Sleep_Hours"
      ),
      selectInput(
        "xVar",
        "X-axis (usually Sleep):",
        choices = c("Sleep_Hours","Sleep_Quality_Score","Daytime_Sleepiness"),
        selected = "Sleep_Hours"
      ),
      selectInput(
        "yVar",
        "Y-axis (cognitive outcome):",
        choices = c(
          "Stroop_Task_Reaction_Time",
          "N_Back_Accuracy",
          "PVT_Reaction_Time",
          "Emotion_Regulation_Score"
        ),
        selected = "PVT_Reaction_Time"
      ),
      
      tags$hr(),
      h4("Model Controls"),
      
      selectInput(
        "outcomeVar",
        "Model outcome (Y):",
        choices = c(
          "Stroop_Task_Reaction_Time",
          "N_Back_Accuracy",
          "PVT_Reaction_Time",
          "Emotion_Regulation_Score"
        ),
        selected = "PVT_Reaction_Time"
      ),
      
      checkboxGroupInput(
        "covariates",
        "Add covariates:",
        choices = c("Age","Gender","BMI","Caffeine_Intake",
                    "Physical_Activity_Level","Stress_Level"),
        selected = c("Age","Gender")
      )
    ),
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Data",
          DTOutput("dataTable")
        ),
        
        tabPanel(
          "Summary",
          verbatimTextOutput("summaryOutput")
        ),
        
        tabPanel(
          "Sleep → Cognition",
          h4("Histogram"),
          plotOutput("histPlot"),
          tags$hr(),
          h4("Sleep vs Cognitive Outcome"),
          plotOutput("scatterPlot")
        ),
        
        tabPanel(
          "Lifestyle",
          h4("Caffeine vs Sleep"),
          plotOutput("caffeinePlot"),
          tags$hr(),
          h4("Stress vs Sleep"),
          plotOutput("stressPlot")
        ),
        
        tabPanel(
          "Model",
          verbatimTextOutput("modelOutput")
        )
      )
    )
  )
)