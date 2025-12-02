###############################################################################
# Assignment 4: Creation of a Shiny App
# Objective: Design, implement and deploy an interactive Shiny web application
# that allows for real-time exploration of a sleep-deprivation data-set, 
# founded within Kaggle; demonstrate proficiency in reactive programming
# principles and biostatistical reasoning. 

# The working directory at hand is the BHDS.2010.Shiny.App repository, 
# within GitHub. From Kaggle, the data-set was downloaded as a .csv file, 
# and uploaded into the repository, with name "sleep_deprivation_dataset.csv".
# We will begin by installing and loading the required packages. The libraries
# below provide the core functionality for the Shiny app: (shiny) is the web
# app framework, (DT) allows for the building of interactive tables, (ggplot2) 
# facilitates plotting, (dplyr) and (tidyr) are critical for data wrangling,
# (bslib) permits boostrap theming for a polished user interface/UI, and
# (plotly) allows for the creation of interactive versions of ggplots. 

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

# We want to read in the data-set and set up the key variables. We will read the
# CSV containing the 2024 sleep deprivation study data, into object "sleep_df". 
# "stringsAsFactors = FALSE prevents automatic conversion of strings to 
# factors, giving us more control over which variables become factors. We
# also want to convert gender to a factor so that we can use it as a grouping
# variable in plots and display gender-level summaries and legends with
# nice labels. 

sleep_df <- read.csv(
  "sleep_deprivation_dataset.csv",
  stringsAsFactors = FALSE)
sleep_df$Gender <- factor(sleep_df$Gender)

# To generate a theme, we can define a custom Bootstrap 5 theme using bslib, 
# wherein we can control the overall aesthetic: fonts, colors, and background.
# We will opt for Bootstrap 5 with a Base Minty theme, a modern sans-serif font,
# a light background for the app body, and a dark text color for emphatic 
# contrast. The "primary" function allows us to set the main accent color, used
# in the tabs, buttons, etc. We will load these details into the object 
# "my_theme" for use when building the UI. 

my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Inter"),
  bg = "#f7f9fc",
  fg = "#1b1f24",
  primary = "#2C7FB8"
)

# We now want to build the User Interface (UI) skeleton. fluidPage() lays out
# the overall structure of the app. We attach the custom bootstrap theme and
# define some extra CSS for the sidebar and helper text. 

ui <- fluidPage(
  theme = my_theme, #Applies the custom bslib theme to the entire app.
  # Custom CSS is incorporated into the <head> of the HTML page. This styles
  # the left sidebar as a white card with rounded corners and styles section 
  # titles plus the help text. 
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
  ), # Now we include the title bar at the top of the app window.
  titlePanel("Sleep Deprivation & Cognitive Performance Explorer"), 
  # Now we split the page into a left sidebar (filters and controls) and a main
  # panel (tabs with data/plots/models). In the sidebar panel, we will have
  # participant filters and analysis controls, and everything here changes
  # what appears in the main tabs. The class = "sidebar" function uses our 
  # custom .sidebar CSS (which gives a card-like look). We will also 
  # incorporate a slider for nightly sleep hours. This controls which
  # participants are included based on Sleep_Hours. 
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
    # We can include a small explanatory blurb under the slider to help
    # non-technical users. We will also include a slider for Age, which 
    # works exactly like the sleep slider. Checkboxes will be included, for 
    # the Gender filter, wherein users can look at males only, females only, 
    # or both. 
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
      ), # To facilitate rapid re-assessment of variables, the inclusion of a 
    # Reset button is critical for returning all filters (sleep, age, gender)
    # back to their full-range defaults.
      actionButton("resetBtn", "Reset filters", icon = icon("rotate-left")),
      tags$hr(),
    # We can now focus on which variables will appear in our histogram within
    # the Sleep -> Cognition tab, using the standard selectInput function, 
    # with the concatenated choices of known variables as per the data-set. 
    div(class = "section-title", "Plot Controls"),
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
    # Our X-axis variable (sleep metric) will be assigned for the Sleep ->
    # Cognition scatter plot. The Y-axis variable (cognitive outcome) will
    # also be inputed for the Sleep -> Cognition scatter. 
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
    tags$hr(), 
    # We want the users to be able to decide which statistics appear in the 
    # Summary tab (e.g., min, median, mean, SD, etc.)
    div(class = "section-title", "Summary Controls"),
    checkboxGroupInput(
      "summaryStats",
      "Summary statistics to display:",
      choices  = c("Min","Q1","Median","Mean","SD","Variance","Q3","Max"),
      selected = c("Min","Q1","Median","Mean","SD","Variance","Q3","Max")
    ),
    tags$hr(),
    # We now want to consider the outcome (Y) for the regression model in the 
    # Model tab. We will also involve covariates that the user can include
    # in the regression model. Sleep_Hours is always in the model, but the 
    # co-variates will be added on top.
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
    ), # The downloadButton command will allow for a Download button to appear,
    # which facilitates the export of a currently filtered dataset, as CSV. 
    downloadButton("downloadData", "Download filtered data")
    ),
    # Ultimately, with respect to the sidear: together, the inputs above allow
    # the user to define a subpopulation (by sleep duration, age and gender) and
    # then choose which variables to visualize or model. Every tab in the main
    # panel reacts to these choices, so the sidebar acts as the central "control
    # panel" for the whole app. We can now move on to the Main panel tabset
    # with Data, Summary, Sleep -> Cognition, Lifestyle and Model tabs. The
    # function navset_card_tab() gives each tab a card-like appearance. We
    # want dynamic text summarizing the filtered sample size (n =...). which
    # can be achieved using card_header(textOutput("nText")). We will also 
    # include a short narrative description so users understand what each row 
    # is, using the function p(class = "help-note",). Finally, an interactive
    # data table (DT) will be included, that displays the filtered data-set, 
    # wherein users can sort, search and paginate with ease.  
    mainPanel(navset_card_tab(nav_panel("Data", icon = icon("table"),
      card(card_header(textOutput("nText")),
      p(class = "help-note",
"Each row is one participant from a 2024 study of 60 adults in the Middle East. ",
 "The data include sleep duration, sleep quality, daytime sleepiness, cognitive ",
  "tests (reaction time, working memory, emotion regulation), and lifestyle factors ",
  "like caffeine, physical activity, and stress."),
  DTOutput("dataTable"))
        ),
# For our Summary tab, we will include two cards side-by-side, using width = 1/2
# and the left card will feature the numeric summary statistics (Min, Q1, 
# Median, etc.) whereas the right card will contain the z-score boxplots for
# key distributions. A little info-icon will also be featured, with a hover 
# tooltip, explaining z-scores to any users that are unfamiliar. To further
# bolster clarity/transparency, an extra explanatory text will be situated
# under the card header.
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
# The Summary tab provides a quick descriptive overview of the filtered data. 
# The table quantifies the distribution of each numeric variable, while the 
# standardized boxplots visually compare the central tendency and spread of 
# three core measures on a common scale. This is especially helpful for 
# spotting outliers and relative variability across metrics. Moving on to the 
# Sleep -> Cognition tab, we will include a guiding question so users know
# what they are exploring. The left card will contain a histogram of whichever
# variable the user selected in "histVar" and the right card contains the 
# sleep metric (x-axis) vs the cognitive outcome (y-axis), colored by Gender,
# and turned into an interactive Plotly scatter. 
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
# This tab is the core exploration space for "does sleep matter for cognition?"
# The histogram helps users understand how the chosen variable is distributed
# in the filtered sample (e.g., whether sleep hours are skewed). The scatterplot
# with its regression line and gender coloring visually encodes potential
# linear trends and sex differences. Moving on to the Lifestyle and Model tabs,
# a card will be featured for Caffeine vs Sleep, with a regression line and
# textual summary. There will also be a card for Stress vs Sleep, with a
# regression line and textual summary. Within the Model tab, the left card
# will present a regression model summary in plain language and printed lm()
# output, whereas the right card will comtain a diagnostics plot (Residuals
# vs Fitted) for model checking. 
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
# The Lifestyle tab connects behavioral exposures (caffeine, stress) with sleep
# duration and provides a concise, automatically-generated interpretation of 
# the regression slope and correlation. The Model tab elevates the app from
# visualization to formal modeling, allowing users to specify multivariable
# linear regression models and insepct both textual summaries and diagnostic
# plots. 


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
    validate(need(
      nrow(df) > 0,
  "No participants match the current filters. Try widening the ranges above."
    ))
    if ("Participant_ID" %in% names(df)) {
      df <- df %>% mutate(
          Tooltip = paste0(
            "ID: ", Participant_ID,
            "<br>", input$xVar, ": ", round(.data[[input$xVar]], 2),
            "<br>", input$yVar, ": ", round(.data[[input$yVar]], 2),
            "<br>Gender: ", Gender)
        )
    } else {df <- df %>%
        mutate(Tooltip = paste0(
            input$xVar, ": ", round(.data[[input$xVar]], 2),
            "<br>", input$yVar, ": ", round(.data[[input$yVar]], 2),
            "<br>Gender: ", Gender))
    }
    p <- ggplot(df,aes(
        x     = .data[[input$xVar]],
        y     = .data[[input$yVar]],
        color = Gender,
        text  = Tooltip)
    ) +
      geom_point(size = 2.2, alpha = 0.85) +
      geom_smooth(
        aes(group = 1),
        method = "lm",
        se     = FALSE,
        color  = "#2C7FB8",
        size   = 1.0
      ) +
      scale_color_manual(values = gender_cols) +
      labs(
        x = input$xVar,
        y = input$yVar,
        color = "Gender"
      ) +
      theme_minimal(base_size = 12) + theme(legend.position = "right")
    ggplotly(p, tooltip = "text")
  })
  output$caffeinePlot <- plotly::renderPlotly({
    df <- filtered_data()
    validate(need(
      nrow(df) > 0,
 "No participants match the current filters. Try widening the ranges above."
    ))
    sleep_range <- range(df$Sleep_Hours, na.rm = TRUE)
    if ("Participant_ID" %in% names(df)) {
      df <- df %>% mutate(Tooltip = paste0(
            "ID: ", Participant_ID,
            "<br>Caffeine intake: ", Caffeine_Intake,
            "<br>Sleep hours: ", round(Sleep_Hours, 2),
            "<br>Gender: ", Gender))
    } else {df <- df %>% mutate(Tooltip = paste0(
            "Caffeine intake: ", Caffeine_Intake,
            "<br>Sleep hours: ", round(Sleep_Hours, 2),
            "<br>Gender: ", Gender))
    }
    p <- ggplot(df,aes(
        x     = Caffeine_Intake,
        y     = Sleep_Hours,
        color = Gender,
        text  = Tooltip)
    ) + geom_point(alpha = 0.8, size = 2.2) +
      geom_smooth(
        aes(group = 1),
        method = "lm",
        se     = TRUE,
        color  = "#2C7FB8",
        fill   = "#2C7FB8",
        alpha  = 0.15,
        size   = 1
      ) +
      scale_color_manual(values = gender_cols) +
      coord_cartesian(ylim = sleep_range) +
      labs(
        x     = "Caffeine intake (0–5 units per day)",
        y     = "Sleep hours per night",
        color = "Gender"
      ) + theme_minimal(base_size = 12) + theme(legend.position = "top")
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
output$stressPlot <- plotly::renderPlotly({df <- filtered_data()
    validate(need(nrow(df) > 0,
"No participants match the current filters. Try widening the ranges above."))
    sleep_range <- range(df$Sleep_Hours, na.rm = TRUE)
    if ("Participant_ID" %in% names(df)) {
      df <- df %>%
        mutate(Tooltip = paste0(
            "ID: ", Participant_ID,
            "<br>Stress level: ", Stress_Level,
            "<br>Sleep hours: ", round(Sleep_Hours, 2),
            "<br>Gender: ", Gender))
    } else {df <- df %>% mutate(Tooltip = paste0(
            "Stress level: ", Stress_Level,
            "<br>Sleep hours: ", round(Sleep_Hours, 2),
            "<br>Gender: ", Gender))
    }
    p <- ggplot(df,aes(
        x     = Stress_Level,
        y     = Sleep_Hours,
        color = Gender,
        text  = Tooltip)
    ) +
      geom_point(alpha = 0.8, size = 2.2) + geom_smooth(
        aes(group = 1),
        method = "lm",
        se     = TRUE,
        color  = "#2C7FB8",
        fill   = "#2C7FB8",
        alpha  = 0.15,
        size   = 1
      ) +
      scale_color_manual(values = gender_cols) +
      coord_cartesian(ylim = sleep_range) +
      labs(
        x     = "Stress level (0–40 scale)",
        y     = "Sleep hours per night",
        color = "Gender"
      ) + theme_minimal(base_size = 12) + theme(legend.position = "top")
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
    sm    <- summary(model)
    coef_sleep <- coef(model)["Sleep_Hours"]
    outcome    <- input$outcomeVar
    covariates <- c("Sleep_Hours", input$covariates)
    model_spec <- paste(outcome, "~", paste(covariates, collapse = " + "))
    r2 <- sm$r.squared
    if (is.na(coef_sleep)) {
      return(paste0(
          "Current model: ", model_spec, ". ",
    "Interpretation: The coefficient for Sleep_Hours could not be estimated ",
    "with the current filters and covariates."))}
    direction <- ifelse(coef_sleep > 0, "increase", "decrease")
    paste0("Current model: ", model_spec, ". ",
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