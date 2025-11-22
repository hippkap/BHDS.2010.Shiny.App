install.packages("shiny")
install.packages("DT")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bslib")
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(bslib)

sleep_df <- read.csv(
  "sleep_deprivation_dataset_detailed 2.csv",
  stringsAsFactors = FALSE)
sleep_df$Gender <- factor(sleep_df$Gender)

# Generating a custom theme 
my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Inter"),
  bg = "#f7f9fc",
  fg = "#1b1f24",
  primary = "#2C7FB8"
)

# Create UI:

ui <- fluidPage(theme = my_theme,
  tags$head(
  tags$style(HTML("
      .sidebar {
        background: white;
        border-radius: 16px;
        padding: 16px;
        box-shadow: 0 4px 18px rgba(0,0,0,0.06);}
      .section-title {
        font-weight: 700;
        font-size: 18px;
        margin-top: 6px;
        margin-bottom: 10px;
        color: #2C7FB8;}
      .help-note {
        font-size: 12.5px;
        color: #6c757d;
        margin-top: -6px;
        margin-bottom: 12px;}"))
        ),
  titlePanel("Sleep Deprivation & Cognitive Performance Explorer"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      div(class="section-title", "Filter Participants"),
      sliderInput(
        "sleepRange",
        "Sleep Hours (per night):",
        min = min(sleep_df$Sleep_Hours, na.rm = TRUE),
        max = max(sleep_df$Sleep_Hours, na.rm = TRUE),
        value = c(min(sleep_df$Sleep_Hours, na.rm = TRUE),
                  max(sleep_df$Sleep_Hours, na.rm = TRUE)),
        step = 0.1
      ),
      div(class="help-note", "Drag to focus on short vs long sleepers."),
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
        selected = levels(sleep_df$Gender),
        inline = TRUE
      ),
      actionButton("resetBtn", "Reset filters", icon = icon("rotate-left")),
      tags$hr(),
      div(class="section-title", "Plot Controls"),
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
        "X-axis (sleep metric):",
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
      div(class="section-title", "Model Controls"),
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
      ),
      downloadButton("downloadData", "Download filtered data")
    ),
    mainPanel(
      navset_card_tab(
        nav_panel("Data", icon = icon("table"),
                  card(
                    card_header(textOutput("nText")),
                    DTOutput("dataTable"))
        ),
        nav_panel("Summary", icon = icon("chart-bar"),
                  card(
                    card_header("Summary statistics"),
                    verbatimTextOutput("summaryOutput"))
        ),
        nav_panel("Sleep → Cognition", icon = icon("brain"),
                  layout_column_wrap(width = 1/2,
                                     card(
                                       card_header("Histogram"),
                                       plotOutput("histPlot", height = 320)),
                  card(card_header("Sleep vs Cognitive Outcome"),
                  plotOutput("scatterPlot", height = 320)))
        ),
        nav_panel("Lifestyle", icon = icon("mug-hot"),
                  layout_column_wrap(width = 1/2,
                  card(card_header("Caffeine vs Sleep"),
                  plotOutput("caffeinePlot", height = 320)),
                  card(card_header("Stress vs Sleep"),
                  plotOutput("stressPlot", height = 320)))
        ),
        nav_panel("Model", icon = icon("calculator"),
                  card(card_header("Linear regression"),
                    verbatimTextOutput("modelOutput"))
        )
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
    geom_histogram(
      bins = 25,
      aes(fill = ..count..),
      color = "white",
      alpha = 0.95
    ) +
    scale_fill_gradient(
      low = "#D9F3EF",
      high = "#3BAFA3"
    ) +
    labs(
      title = paste("Distribution of", input$histVar),
      x = input$histVar,
      y = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "none"
    )})
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
      y = "Sleep_Hours"
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
