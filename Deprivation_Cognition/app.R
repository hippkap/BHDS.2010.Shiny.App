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
      sliderInput(
        "ageRange",
        "Age:",
        min = min(sleep_df$Age, na.rm = TRUE),
        max = max(sleep_df$Age, na.rm = TRUE),
        value = c(min(sleep_df$Age, na.rm = TRUE),
                  max(sleep_df$Age, na.rm = TRUE)),
        step = 1
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
        "X-axis (usually sleep):",
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
        tabPanel("Data", DTOutput("dataTable")),
        tabPanel("Summary", verbatimTextOutput("summaryOutput")),
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
        tabPanel("Model", verbatimTextOutput("modelOutput"))
      )
    )
  )
)

# Create Server:

server <- function(input, output, session) {
  
filtered_data <- reactive({
    sleep_df %>%
      filter(
        Sleep_Hours >= input$sleepRange[1],
        Sleep_Hours <= input$sleepRange[2],
        Age >= input$ageRange[1],
        Age <= input$ageRange[2],
        Gender %in% input$genderFilter
      )
  })
output$dataTable <- renderDT({
  filtered_data()
}, options = list(pageLength = 8, scrollX = TRUE))
output$summaryOutput <- renderPrint({
  summary(filtered_data())
})
output$histPlot <- renderPlot({
  df <- filtered_data()
  req(nrow(df) > 0)
ggplot(df, aes_string(x = input$histVar)) +
    geom_histogram(bins = 25, color = "black") +
    labs(
      title = paste("Distribution of", input$histVar),
      x = input$histVar,
      y = "Count"
    ) +
    theme_minimal()
})
output$scatterPlot <- renderPlot({
  df <- filtered_data()
  req(nrow(df) > 0)
ggplot(df, aes_string(x = input$xVar, y = input$yVar, color = "Gender")) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste(input$yVar, "vs", input$xVar),
      x = input$xVar,
      y = input$yVar
    ) +
    theme_minimal()
})
output$caffeinePlot <- renderPlot({
  df <- filtered_data()
  req(nrow(df) > 0)
ggplot(df, aes(x = Caffeine_Intake, y = Sleep_Hours)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Caffeine Intake vs Sleep Hours",
      x = "Caffeine Intake",
      y = "Sleep Hours"
    ) +
    theme_minimal()})
output$stressPlot <- renderPlot({
  df <- filtered_data()
  req(nrow(df) > 0)
ggplot(df, aes(x = Stress_Level, y = Sleep_Hours)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Stress Level vs Sleep Hours",
      x = "Stress Level",
      y = "Sleep Hours"
    ) +
    theme_minimal()})
output$modelOutput <- renderPrint({
  df <- filtered_data()
  req(nrow(df) >= 10)
rhs <- c("Sleep_Hours", input$covariates)
  formula_str <- paste(input$outcomeVar, "~", paste(rhs, collapse = " + "))
  model <- lm(as.formula(formula_str), data = df)
  cat("Linear Regression Model:\n")
  print(formula(model))
  cat("\n\nModel Summary:\n")
  summary(model)
})
}

# Run the application
shinyApp(ui = ui, server = server)