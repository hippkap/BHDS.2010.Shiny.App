#install.packages("shiny")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("bslib")
#install.packages("tidyr")
#install.packages("plotly")
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)
library(plotly)

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

ui <- fluidPage(
  theme = my_theme,
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
        margin-bottom: 12px;}
    "))
  ),
  titlePanel("Sleep Deprivation & Cognitive Performance Explorer"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      div(class = "section-title", "Filter Participants"),
      sliderInput(
        "sleepRange",
        "Sleep Hours (per night):",
        min   = min(sleep_df$Sleep_Hours, na.rm = TRUE),
        max   = max(sleep_df$Sleep_Hours, na.rm = TRUE),
        value = c(min(sleep_df$Sleep_Hours, na.rm = TRUE),
                  max(sleep_df$Sleep_Hours, na.rm = TRUE)),
        step  = 0.1
      ),
      div(class = "help-note", "Drag to focus on short vs long sleepers."),
      sliderInput(
        "ageRange",
        "Age:",
        min   = min(sleep_df$Age, na.rm = TRUE),
        max   = max(sleep_df$Age, na.rm = TRUE),
        value = c(min(sleep_df$Age, na.rm = TRUE),
                  max(sleep_df$Age, na.rm = TRUE)),
        step  = 1
      ),
      checkboxGroupInput(
        "genderFilter",
        "Gender:",
        choices  = levels(sleep_df$Gender),
        selected = levels(sleep_df$Gender),
        inline   = TRUE
      ),
      actionButton("resetBtn", "Reset filters", icon = icon("rotate-left")),
      tags$hr(), div(class = "section-title", "Plot Controls"),
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
        choices  = c("Sleep_Hours","Sleep_Quality_Score","Daytime_Sleepiness"),
        selected = "Sleep_Hours"
      ),
    selectInput(
      "yVar",
      "Y-axis (cognitive outcome):",
      choices  = c(
        "Stroop_Task_Reaction_Time",
        "N_Back_Accuracy",
        "PVT_Reaction_Time",
        "Emotion_Regulation_Score"
      ),
      selected = "PVT_Reaction_Time"
    ),
    tags$hr(), div(class = "section-title", "Summary Controls"),
    checkboxGroupInput(
      "summaryStats",
      "Summary statistics to display:",
      choices  = c("Min","Q1","Median","Mean","SD","Variance","Q3","Max"),
      selected = c("Min","Q1","Median","Mean","SD","Variance","Q3","Max")
    ),
    tags$hr(),
    div(class = "section-title", "Model Controls"),
    selectInput(
      "outcomeVar",
      "Model outcome (Y):",
      choices  = c(
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
      choices  = c("Age","Gender","BMI","Caffeine_Intake",
                   "Physical_Activity_Level","Stress_Level"),
      selected = c("Age","Gender")
    ),
    downloadButton("downloadData", "Download filtered data")
    ),
    mainPanel(
      navset_card_tab(
  nav_panel("Data", icon = icon("table"),
      card(card_header(textOutput("nText")),
      p(class = "help-note",
"Each row is one participant from a 2024 study of 60 adults in the Middle East. ",
 "The data include sleep duration, sleep quality, daytime sleepiness, cognitive ",
  "tests (reaction time, working memory, emotion regulation), and lifestyle factors ",
  "like caffeine, physical activity, and stress."),
  DTOutput("dataTable"))
        ),
  nav_panel("Summary", icon = icon("chart-bar"),
          layout_column_wrap(
            width = 1/2,
            card(
              card_header("Summary statistics"),
              DTOutput("summaryTable")
            ),
            card(card_header(div("Key distributions", tags$span(
          icon("circle-info"),
          style = "margin-left: 6px; color: #6c757d; cursor: help;",
          title = "These boxplots use standardized values (z-scores), so all three variables are shown on the same scale. A z-score tells you how many standard deviations a value is above or below the sample mean."
                  )
                )
              ),
          p(class = "help-note",
        "These boxplots show standardized values (z-scores), so Sleep Hours, Sleep Quality Score, and PVT Reaction Time can be compared on the same scale despite having different units."
              ),
      plotOutput("summaryBoxPlot", height = 320)))),
  nav_panel("Sleep → Cognition", icon = icon("brain"),
          p(class = "help-note",
            "How does sleep duration, quality, or daytime sleepiness relate to each cognitive outcome?"
          ),
      layout_column_wrap(width = 1/2,
            card(card_header("Histogram"),
              plotOutput("histPlot", height = 320)),
            card(card_header("Sleep vs Cognitive Outcome"),
              plotlyOutput("scatterPlot", height = 320)
            )
          )
        ),
   nav_panel("Lifestyle", icon = icon("mug-hot"),
        p(class = "help-note",
        "Do caffeine and stress appear to be associated with sleep hours?"
        ),
        layout_column_wrap(width = 1/2,
          card(card_header("Caffeine vs Sleep"),
            p(class = "help-note",
      "Each point is a participant; the line shows the fitted regression of sleep hours on caffeine intake."
            ),
            plotlyOutput("caffeinePlot", height = 320),tags$br(),
            textOutput("caffeineSummaryText")),
          card(card_header("Stress vs Sleep"),
            p(class = "help-note",
              "Each point is a participant; the line shows the fitted regression of sleep hours on stress level."
            ),
      plotlyOutput("stressPlot", height = 320), tags$br(),
      textOutput("stressSummaryText")))
      ),
    nav_panel("Model", icon = icon("calculator"),
          layout_column_wrap(
            width = 1,
            card(card_header("Linear regression"),
              p(class = "help-note",
                "This model uses the filtered sample and the covariates you selected below."
              ),
              textOutput("modelSummaryText"),
              tags$hr(),
              verbatimTextOutput("modelOutput")
            ),
            card(
              card_header("Diagnostics: Residuals vs Fitted"),
              plotOutput("residPlot", height = 280)
            )
          )
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
    df <- filtered_data()
    if (nrow(df) == 0) {empty_msg <- data.frame(
        Message = "No participants match the current filters. 
Try widening the sleep or age range, or re-select both genders."
      )
  return(datatable(empty_msg, options = list(dom = "t"), rownames = FALSE))}
    datatable(df, options = list(pageLength = 8, scrollX = TRUE))
  })
  output$summaryTable <- DT::renderDT({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
  "No participants match the current filters. Try widening the ranges above."))
    num_df <- df %>% dplyr::select(where(is.numeric))
    validate(need(ncol(num_df) > 0, 
  "No numeric variables available in the current filter."))
    stat_names <- c("Min","Q1","Median","Mean","SD","Variance","Q3","Max")
    summary_tbl <- num_df %>%
      summarise(across(
        everything(),
        list(
          Min      = ~min(.x, na.rm = TRUE),
          Q1       = ~quantile(.x, 0.25, na.rm = TRUE),
          Median   = ~median(.x, na.rm = TRUE),
          Mean     = ~mean(.x, na.rm = TRUE),
          SD       = ~sd(.x, na.rm = TRUE),
          Variance = ~var(.x, na.rm = TRUE),
          Q3       = ~quantile(.x, 0.75, na.rm = TRUE),
          Max      = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )) %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to   = c("Variable", "Statistic"),
        # everything before the last "_" is the variable name
        names_pattern = "^(.*)_(Min|Q1|Median|Mean|SD|Variance|Q3|Max)$"
      ) %>%
      tidyr::pivot_wider(
        names_from  = Statistic,
        values_from = value)
    summary_tbl$Variable <- dplyr::recode(
      summary_tbl$Variable,
      Sleep_Hours               = "Sleep Hours (per night)",
      Sleep_Quality_Score       = "Sleep Quality Score",
      Daytime_Sleepiness        = "Daytime Sleepiness",
      Stroop_Task_Reaction_Time = "Stroop Task Reaction Time",
      N_Back_Accuracy           = "N-Back Accuracy",
      PVT_Reaction_Time         = "PVT Reaction Time",
      Emotion_Regulation_Score  = "Emotion Regulation Score",
      Caffeine_Intake           = "Caffeine Intake",
      Physical_Activity_Level   = "Physical Activity Level",
      Stress_Level              = "Stress Level",
      BMI                       = "BMI")
    summary_tbl <- summary_tbl %>%
      arrange(Variable)
    summary_tbl <- summary_tbl %>%
      dplyr::mutate(across(where(is.numeric), ~round(.x, 2)))
    selected_stats <- input$summaryStats
    if (is.null(selected_stats) || length(selected_stats) == 0) {
      summary_tbl <- summary_tbl %>% dplyr::select(Variable)
    } else {
      keep_stats <- intersect(stat_names, selected_stats)
      summary_tbl <- summary_tbl %>%
        dplyr::select(Variable, dplyr::all_of(keep_stats))}
    DT::datatable(
      summary_tbl,
      options = list(
        pageLength = 8,
        scrollX = TRUE),rownames = FALSE)
  })
  output$summaryBoxPlot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
  "No participants match the current filters. Try widening the ranges above."))
  df_long <- df %>%
      dplyr::select(
        `PVT Reaction Time`   = PVT_Reaction_Time,
        `Sleep Hours`         = Sleep_Hours,
        `Sleep Quality Score` = Sleep_Quality_Score) %>%
      mutate(
        `PVT Reaction Time`   = scale(`PVT Reaction Time`)[, 1],
        `Sleep Hours`         = scale(`Sleep Hours`)[, 1],
        `Sleep Quality Score` = scale(`Sleep Quality Score`)[, 1]) %>%
      tidyr::pivot_longer(
        cols      = everything(),
        names_to  = "Measure",
        values_to = "Z_value")
 df_long$Measure <- factor(df_long$Measure,
      levels = c("PVT Reaction Time", "Sleep Hours", "Sleep Quality Score"))
 ggplot(df_long, aes(x = Measure, y = Z_value, fill = Measure)) +
      geom_boxplot(alpha = 0.7, width = 0.55, outlier.alpha = 0.5) +
      coord_flip() +
      labs(x = NULL, y = "Standardized value (z-score)") +
      scale_fill_manual(values = c(
        "PVT Reaction Time"   = "#F28E8C",
        "Sleep Hours"         = "#8BC9A6",
        "Sleep Quality Score" = "#7FB3E6")) + theme_minimal(base_size = 13) +
      theme(
        legend.position     = "none",
        panel.grid.minor    = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.text.y         = element_text(face = "bold"))
  })
  gender_cols <- c("Female" = "#E07A9B", "Male" = "#4C9FCD")
  output$histPlot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
  "No participants match the current filters. Try widening the ranges above."))
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
  output$scatterPlot <- plotly::renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
  "No participants match the current filters. Try widening the ranges above."))
    if ("Participant_ID" %in% names(df)) {
      df <- df %>%
        mutate(
          Tooltip = paste0(
            "ID: ", Participant_ID,
            "<br>", input$xVar, ": ", round(.data[[input$xVar]], 2),
            "<br>", input$yVar, ": ", round(.data[[input$yVar]], 2),
            "<br>Gender: ", Gender))
    } else {
      df <- df %>%
        mutate(
          Tooltip = paste0(
            input$xVar, ": ", round(.data[[input$xVar]], 2),
            "<br>", input$yVar, ": ", round(.data[[input$yVar]], 2),
            "<br>Gender: ", Gender))
    }
    p <- ggplot(df,aes(
        x = .data[[input$xVar]],
        y = .data[[input$yVar]],
        color = Gender,
        text  = Tooltip)) +
      geom_point(size = 2.2, alpha = 0.85) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = gender_cols) +
      labs(x = input$xVar,y = input$yVar) + theme_minimal(base_size = 12) +
      theme(legend.position = "right")
    ggplotly(p, tooltip = "text")
  })
  output$caffeinePlot <- plotly::renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
  "No participants match the current filters. Try widening the ranges above."))
    sleep_range <- range(df$Sleep_Hours, na.rm = TRUE)
    if ("Participant_ID" %in% names(df)) {
      df <- df %>% mutate(
        Tooltip = paste0(
          "ID: ", Participant_ID,
          "<br>Caffeine intake: ", Caffeine_Intake,
          "<br>Sleep hours: ", round(Sleep_Hours, 2),
          "<br>Gender: ", Gender))
    } else {
      df <- df %>% mutate(
        Tooltip = paste0(
          "Caffeine intake: ", Caffeine_Intake,
          "<br>Sleep hours: ", round(Sleep_Hours, 2),
          "<br>Gender: ", Gender))
    }
    p <- ggplot(df, aes(
      x     = Caffeine_Intake,
      y     = Sleep_Hours,
      color = Gender,
      text  = Tooltip)) +
      geom_point(alpha = 0.8, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE, size = 0.7) +
      scale_color_manual(values = gender_cols) +
      coord_cartesian(ylim = sleep_range) +   # now uses the local object
      labs(
        x = "Caffeine intake (0–5 units per day)",
        y = "Sleep hours per night",
        color = "Gender") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
    ggplotly(p, tooltip = "text")
  })
  output$caffeineSummaryText <- renderText({
    df <- filtered_data()
    req(nrow(df) > 5)
    if (length(unique(df$Caffeine_Intake)) < 2) {
      return("Summary: Not enough variation in caffeine intake in the current filters to summarize a trend.")
    }
    fit <- lm(Sleep_Hours ~ Caffeine_Intake, data = df)
    slope <- coef(fit)["Caffeine_Intake"]
    r <- suppressWarnings(cor(df$Caffeine_Intake, df$Sleep_Hours, 
                              use = "complete.obs"))
    direction <- ifelse(slope < 0, "lower", "higher")
    paste0(
      "Summary: In this filtered sample, each 1-unit increase in caffeine intake is associated with about ",
      round(abs(slope), 2), " fewer hours of sleep per night (r = ",
      round(r, 2), "). That is, higher caffeine tends to be linked with ",
      direction, " sleep.")
  })
  output$stressPlot <- plotly::renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
  "No participants match the current filters. Try widening the ranges above."))
    sleep_range <- range(df$Sleep_Hours, na.rm = TRUE)
    if ("Participant_ID" %in% names(df)) {
      df <- df %>% mutate(
        Tooltip = paste0(
          "ID: ", Participant_ID,
          "<br>Stress level: ", Stress_Level,
          "<br>Sleep hours: ", round(Sleep_Hours, 2),
          "<br>Gender: ", Gender))
    } else {df <- df %>% mutate(Tooltip = paste0(
          "Stress level: ", Stress_Level,
          "<br>Sleep hours: ", round(Sleep_Hours, 2),
          "<br>Gender: ", Gender
        )
      )
    }
    p <- ggplot(df, aes(
      x     = Stress_Level,
      y     = Sleep_Hours,
      color = Gender,
      text  = Tooltip)) +
      geom_point(alpha = 0.8, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE, size = 0.7) +
      scale_color_manual(values = gender_cols) +
      coord_cartesian(ylim = sleep_range) +
      labs(
        x = "Stress level (0–40 scale)",
        y = "Sleep hours per night",
  color = "Gender") + theme_minimal(base_size = 12) +
  theme(legend.position = "top")
  ggplotly(p, tooltip = "text")
  })
  output$stressSummaryText <- renderText({df <- filtered_data()
    req(nrow(df) > 5)
    if (length(unique(df$Stress_Level)) < 2) {
      return("Summary: Not enough variation in stress level in the current filters to summarize a trend.")
    }
    fit <- lm(Sleep_Hours ~ Stress_Level, data = df)
    slope <- coef(fit)["Stress_Level"]
    r <- suppressWarnings(cor(df$Stress_Level, df$Sleep_Hours,
                                  use = "complete.obs"))
    direction <- ifelse(slope < 0, "lower", "higher")
    paste0(
      "Summary: In this filtered sample, each 1-unit increase in stress is associated with about ",
      round(abs(slope), 2), " hours of ",
      direction, " sleep per night (r = ",
      round(r, 2), ").")})
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
  output$residPlot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
  "Not enough participants to fit a model. Try widening filters."))
  model <- current_model()
  df_resid <- data.frame(
    Fitted    = fitted(model),
    Residuals = resid(model))
  ggplot(df_resid, aes(x = Fitted, y = Residuals)) + geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") + labs(x = "Fitted values",
      y = "Residuals") + theme_minimal(base_size = 12)
})
  output$modelSummaryText <- renderText({
    model <- current_model()
    sm <- summary(model)
    coef_sleep <- coef(model)["Sleep_Hours"]
    outcome <- input$outcomeVar
    r2 <- sm$r.squared
    if (is.na(coef_sleep)) {
return("Interpretation: The coefficient for Sleep_Hours could not be estimated with the current filters and covariates.")
    }
    direction <- ifelse(coef_sleep > 0, "increase", "decrease")
    paste0(
    "Interpretation: For each additional hour of sleep, the model predicts a ",
      round(abs(coef_sleep), 2), " unit ", direction, " in ", outcome,
      " on average, holding the selected covariates constant. ",
      "In this filtered sample, the model explains about ",
      round(100 * r2, 1), "% of the variability in ", outcome, ".")
  })
  output$downloadData <- downloadHandler(
    filename = function() "filtered_sleep_data.csv",
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)})
}


# Now to run the application:
shinyApp(ui = ui, server = server)
# Uploading to Shiny.io
# install.packages("rsconnect", type = "binary")
# library(rsconnect)