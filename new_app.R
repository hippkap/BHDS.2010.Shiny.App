#install.packages("shiny")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("bslib")
#install.packages(tidyr)
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)

sleep_df <- read.csv(
  "sleep_deprivation_dataset.csv",
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
                                  DTOutput("summaryTable"))
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
                                card(
                                  card_header("Linear regression"),
                                  p(class = "help-note",
                                    "This model uses the filtered sample and the covariates 
                      you selected below."),
                                  textOutput("modelSummaryText"),
                                  tags$hr(),
                                  verbatimTextOutput("modelOutput"))
                      )
                    )
                  )
                )
)

# Create Server:

server <- function(input, output, session) {
  
  observeEvent(input$resetBtn, {
    updateSliderInput(session, "sleepRange",
                      value = c(min(sleep_df$Sleep_Hours, na.rm=TRUE),
                                max(sleep_df$Sleep_Hours, na.rm=TRUE)))
    updateSliderInput(session, "ageRange",
                      value = c(min(sleep_df$Age, na.rm=TRUE),
                                max(sleep_df$Age, na.rm=TRUE)))
    updateCheckboxGroupInput(session, "genderFilter",
                             selected = levels(sleep_df$Gender))
  })
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
  output$nText <- renderText({
    paste0("Filtered sample size: n = ", nrow(filtered_data()))
  })
  output$dataTable <- renderDT({
    filtered_data()
  }, options = list(pageLength = 8, scrollX = TRUE))
  output$summaryTable <- DT::renderDT({
    df <- filtered_data()
    req(nrow(df) > 0)
    num_df <- df %>% dplyr::select(where(is.numeric))
    req(ncol(num_df) > 0)
    summary_tbl <- num_df %>%
      summarise(across(
        everything(),
        list(
          Min    = ~min(.x, na.rm = TRUE),
          Q1     = ~quantile(.x, 0.25, na.rm = TRUE),
          Median = ~median(.x, na.rm = TRUE),
          Mean   = ~mean(.x, na.rm = TRUE),
          Q3     = ~quantile(.x, 0.75, na.rm = TRUE),
          Max    = ~max(.x, na.rm = TRUE)
        )
      )) %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = c("Variable", ".value"),
        names_sep = "_"
      ) %>%
      arrange(Variable)
    DT::datatable(
      summary_tbl,
      options = list(
        pageLength = 8,
        scrollX = TRUE
      ),
      rownames = FALSE)
  })
  gender_cols <- c("Female" = "#E07A9B", "Male" = "#4C9FCD")
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
    df <- filtered_data(); req(nrow(df) > 0)
    ggplot(df, aes_string(x=input$xVar, y=input$yVar, color="Gender")) +
      geom_point(size=2.2, alpha=0.85) +
      geom_smooth(method="lm", se=FALSE) +
      scale_color_manual(values = gender_cols) +
      labs(x=input$xVar, y=input$yVar) +
      theme_minimal(base_size = 12) +
      theme(legend.position="right")
  })
  output$caffeinePlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    ggplot(df, aes(x=Caffeine_Intake, y=Sleep_Hours)) +
      geom_point(alpha=0.7) +
      geom_smooth(method="lm", se=FALSE, color="#2C7FB8") +
      theme_minimal(base_size = 12)
  })
  output$stressPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    ggplot(df, aes(x=Stress_Level, y=Sleep_Hours)) +
      geom_point(alpha=0.7) +
      geom_smooth(method="lm", se=FALSE, color="#2C7FB8") +
      theme_minimal(base_size = 12)
  })
  current_model <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 10)
    rhs <- c("Sleep_Hours", input$covariates)
    formula_str <- paste(input$outcomeVar, "~", paste(rhs, collapse = " + "))
    lm(as.formula(formula_str), data = df)})
  output$modelOutput <- renderPrint({
    model <- current_model()
    cat("Linear Regression Model:\n")
    print(formula(model))
    cat("\n\nModel Summary:\n")
    summary(model)
  })
  output$modelSummaryText <- renderText({
    model <- current_model()
    sm <- summary(model)
    coef_sleep <- coef(model)["Sleep_Hours"]
    outcome <- input$outcomeVar
    r2 <- sm$r.squared
    if (is.na(coef_sleep)) {
      return("Interpretation: The coefficient for Sleep_Hours could not be 
           estimated with the current filters and covariates.")}
    direction <- ifelse(coef_sleep > 0, "increase", "decrease")
    paste0(
      "Interpretation: For each additional hour of sleep, the model predicts a "
      , round(abs(coef_sleep), 2), " unit ", direction, " in ", outcome,
      " on average, holding the selected covariates constant. ",
      "In this filtered sample, the model explains about ",
      round(100 * r2, 1), "% of the variability in ", outcome, "."
    )
  })
  output$downloadData <- downloadHandler(
    filename = function() "filtered_sleep_data.csv",
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)})
}

# Run the application
shinyApp(ui = ui, server = server)
# Uploading to Shiny.io
# install.packages("rsconnect", type = "binary")
# library(rsconnect)