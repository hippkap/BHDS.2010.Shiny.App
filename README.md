# BHDS.2010.Shiny.App
*Final Submission - December 2nd, 2025*

**Project Purpose**:

The purpose of this assignment is to design, implement, and deploy an 
interactive Shiny web application that enables real-time exploration of a 
sleep-deprivation dataset. The app integrates dynamic filtering, data 
visualization, statistical summarization, and regression modeling into a unified
analytic interface. Its overarching aim is to demonstrate proficiency in 
reactive programming principles, data-driven UI/UX design, and biostatistical 
reasoning through an applied digital tool.

Questions? For replication inquiries, methodological clarifications, or 
troubleshooting assistance, collaborators may open an Issue in this repository 
or contact the project contributors via the team’s Slack channel.

**Direct Objective of Data**:

The dataset contains observational data from a sample of adults, capturing 
nightly sleep duration, sleep quality metrics, daytime sleepiness, cognitive 
performance outcomes (reaction time, working memory, emotion regulation), and 
lifestyle factors (caffeine intake, physical activity, and stress).

The direct analytical objective of the data is to:
•	quantify relationships between sleep patterns and cognitive functioning,
•	inspect lifestyle correlates of sleep behaviors,
•	and enable flexible subgroup analyses using an interactive, user-specified 
filtering framework.

**Raw Data Description**:

Each row of the dataset represents one participant. Variables include:
•	Sleep metrics: Sleep_Hours, Sleep_Quality_Score, Daytime_Sleepiness
•	Cognitive outcomes: Stroop_Task_Reaction_Time, PVT_Reaction_Time, 
N_Back_Accuracy, Emotion_Regulation_Score
•	Lifestyle covariates: Caffeine_Intake, Stress_Level, Physical_Activity_Level, 
BMI
•	Demographics: Age, Gender

All variables are numeric except Gender (factor) and Participant_ID.

**Instructions to Run the Code**:

1. Clone or download this GitHub repository onto your local machine.
2. Open the project folder in RStudio.
   -> Clone or download this GitHub repository onto your local computer using:
   File -> New Project -> Version Control -> Git, then paste the repository URL:
   git clone https://github.com/hippkap/BHDS.2010.Shiny.App.git
3. Ensure that the file sleep_deprivation_dataset.csv is located in the main 
project directory.
4. Begin by installing the required packages if not already installed via:
install.packages(c(“shiny”, “DT”, "tidyr", "ggplot2", "dplyr", “bslib”, 
“plotly”))
5. Load the libraries and their dependencies via:
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(bslib)
library(plotly)
6. Open the Shiny app script (app.R or whichever file contains ui <- … and 
server <- …) and Run App in RStudio. A browser window will open, displaying 
the Sleep Deprivation & Cognitive Performance Explorer. 
7. Deploy to shinyapps.io (if rsconnect is installed and configured, the app 
can be deployed using install.packages(“rsconnect”) followed by 
library(rsconnect). This will generate a public URL that others can use to 
interact with the app, without installing R. 

**Version Control Workflow**: 

1. A repository was created on GitHub under Anna's account. A link was sent over
to Jona to join the repository. Anna populated the BHDS.2010.Shiny.App folder
with the base of the app.R file. 
2. Jona joined as a collaborator, cloned the repository into RStudio, and 
uploaded the core project assets: the sleep_deprivation_dataset.csv and the 
initial scaffold of the README.md file.
3. Separate from the main branch, within the “UI branch,” Jona provided the 
preliminary code for the UI: installed the appropriate packages, started 
building the UI by defining the fluidPage() layut, sidebar filters, and a basic 
tabset, which included the first version of the sliders, gender checkboxes, and 
placeholder outputs for the Data, Summary and Sleep -> Cognition tabs. Changes 
within this branch were committed, pushed into the hub, and a merge pull request
was successfully managed, with commit 6557567 merged into the main branch, and 
the UI branch deleted. 
4. Anna pulled the updated main branch into her local RStudio. Building on 
Jona’s UI, she implemented the base server logic: the filtered_data reactive 
expression, the output$dataTable and output$summaryTable render functions, and 
the first versions of the histogram and scatterplot outputs. All of this was 
completed within the “Server” branch. She then staged, committed and pushed 
these updates into the hub, and merge pull request was successfully managed, 
with commit aa9c189 merged into the main branch, and the Server branch deleted. 
The shared app now had a minimal but functional end-to-end pipeline (inputs -> 
reactive filter -> outputs). 
5.  Jona re-typed some of the code to adjust syntactical structure, because 
Shiny Error: [object Object] message was showing, likely because of output ID 
mismatch or DT output mismatch. 
6. Within the branch “Aesthetic_Fixes,” Jona focused on aesthetics and 
usability, introducing the bslib-based custom theme (my_theme), added Google 
fonts, and wrote custom CSS for the sidebar and help-note elements. She 
refactored the layout to use navset_card_tab() and layout_column_wrap() so that 
each tab displayed polished card-style panels. App now featured a minty base, 
with histograms reflecting different shades of minty-green to match the layout. 
The lettering and titles were also tweaked, and an action button for resetting 
filters was incorporated, along with better navigation panels. Changes within 
this branch were committed, pushed into the hub, and a merge pull request was 
successfully managed, with commit b383f6b merged into the main branch, and the 
Aesthetic_Fixes branch deleted. 
7. Anna pulled these changes into the main branch, and under the “Reset_Tweaks” 
branch, she completed the Reset Button code on the server side, provided the 
histograms with coloring, and added the option to download the filtered data 
using downloadHandler(). Changes within this branch were committed, pushed 
into the hub, and a merge pull request was successfully managed, with commit 
20b30ca merged into the main branch, and the Reset_Tweaks branch deleted. 
8.  Jona restructured some of the brackets/syntax, and tidied up all of the plot
layouts within the “Server_Aesthetics” branch. The Sleep -> Cognition tab was 
enhanced, wiring the histVar, xVar, and yVar controls into the server, refining 
the histogram styling. These changes were committed and a merge pull request was
created, wherein commit e768c6d was merged into the main branch and the 
Server_Aesthetics branch was deleted. 
9. Within the main branch, Jona included the Shiny.io install code, re-uploaded 
the CSV file within the correct folder for the Shiny software to connect the 
data-set to the Shiny app code, and successfully deployed the app draft (after 
a few tweaks to the folder positioning and commenting out of packages/libraries 
to overcome the errors in the app populating the website). Feedback and 
suggestions were obtained from the professor and peers, which the team sought 
to implement. 
10. Within the main tab, Jona re-organized the summary tab into a table, for 
enhanced readability/cleaner output, and included code for a verbal-summary to 
populate the screen each time a regression result shows. Due to a random file 
corruption, the app.R could not be saved and the changes could not be committed 
and pushed out into the hub, so Jona had to re-copy the entirety of the code 
into a new app.R file and re-upload within the appropriate folders. The Summary
tab display was then further enhanced/reorganized with better titles and better 
organization of values across rows instead of repeated vertical entries. Jona 
then provided code to round all values within the tab to 2 decimal places for 
easier readability, and organized the variables alphabetically. She also 
incorporated the option for users to check/uncheck which stats they want to see,
and provided additional code for “standard deviation” and “variance” to present 
itself within the Summary tab.    
11. Within the main tab and in adherence to the suggestions offered by 
classmates, Jona incorporated plotly to enhance the app’s interactivity (with 
plotly hovers for key plots). A diagnostic plot was also added, research 
question blurbs were included for each tab/summary texts were provided, and the
Sleep -> Cognition tab displayed an interactive hover (the same was true for 
both lifestyle plots). Jona updated the Model tab with text summary, and a 
residual vs fitted plot. To augment the visual impact of the Summary tab 
(instead of just maintaining a string of characters/statistics), a boxplot was 
added alongside the generated table. 
12. Jona went through several iterations of updating the aesthetics/viewability 
of the Summary tab’s boxplot, as it initially appeared off-kilter/off-scale and 
unimpressive. She attempted first to facet each variable into their own 
mini-panels with their own y-axes and better facet labels, but the bottom 
portion of graph appeared markedly cluttered. Jona then set up a horizontal trio
of boxplots so that the labels fit better, but the scales of the boxplot were 
still wildly maladjusted to one another. She then attempted to plot standardized
values (z-scores) so that all three variables live on the same scale, whilst 
keeping the horizontal layout. 
13. Jona added an info icon next to key distributions, so that when users hover 
over the icon, they will see a tooltip and message appear. Following debugging 
of the code, the Lifestyle tab was dressed up further, with the UI updated for 
the tab, and the points colored by gender, in addition to cleaner axis labels. 
Jona ensured there was the same y-axis range for both lifestyle plots and 
included an interpretation text with a helper sentence in each card. For further
transparency, an “About This Data” blurb was included into the mainframe of the 
app, and messages were included for when filters might return zero rows, to 
facilitate navigation within the app instead of allowing the user to believe 
that the app is “broken.” The Model tab was tidied up further.
14. As a final design step, Jona observed that regression lines were missing 
in the scatterplots, so code was included to account for regression lines and 
95% confidence interval bands. 
15. Commentary for the entirety of the UI section was included, along with a 
proper header/objectives/About-the-Data frame at the beginning of the document. 
Jona then moved on to providing commentary for the server, in addition to 
conclusive commentary on the findings. 
16. After all branches were merged, both collaborators pulled the latest main 
branch to ensure all local copies were synchronized with the repository.
17. With every section complete and all warning messages being suppressed,
the app script was knit into a finalized, reproducible pdf report (submitted 
in tandem with the app-theory-and-creation report). 
NOTE: Other cosmetic and functional polishing was committed several times over 
the course of the app creation (for things like tweaking helper texts and 
refining color choices and hover tooltips), and these steps may not be fully 
laid out here. However, altogether, they served to finalize the presentation of 
a clean, deployable version of the Sleep Deprivation & Cognitive Performance 
Explorer. 

**Commit and Pull Request Protocol**

- Each collaborator committed changes frequently, using clear and descriptive
messages (e.g. “fixed the code because regression line was missing in the 
scatterplot, re-typed code for debugging, added info icon next to key 
distribution, boxplot still appeared cluttered so set up horizontal trio of 
boxplots so the labels fit nicely” etc.). 
- All commits were pushed to the remote repository before initiating pull 
requests. 
- Each pull request included a summary of the changes and was reviewed by 
the collaborator prior to merging. 
- Merge conflicts did not occur in these instances, but all changes were
checked and confirmed locally before final approval. 

**Shiny App Structure and Key Features (UI + Server Logic)**:

Architecture:
The app follows the canonical Shiny pattern of a ui object and a server 
function, wrapped by shinyApp(ui, server).
•	The UI (user interface) is defined with fluidPage(), a custom bslib theme, 
and a sidebarLayout(). The sidebar holds all user controls (filters, variable 
selectors, model options), while the main panel presents a tabbed set of 
card-style views created with navset_card_tab().
•	The server contains the reactive backbone of the app: a single reactive subset
of the data, filtered_data(), which is updated whenever sleep hours, age, or 
gender inputs change. Every output (tables, plots, and models) is defined in 
terms of this filtered dataset, ensuring all views remain synchronized.

Tabs and user interaction:
1.	Data tab
  o	Shows the current analytic sample in an interactive DT::datatable.
  o	The header text (output$nText) displays the filtered sample size (e.g., n 
  = 47).
  o	A short explanatory blurb clarifies that each row is one participant from 
  the 2024 study and lists the main domains (sleep, cognition, lifestyle, 
  demographics).
  o	If the filters produce no matching participants, the table falls back to 
  a “no participants match the filters” message instead of throwing an error.
2.	Summary tab 
  o	Left card: a numeric summary table (summaryTable) that computes Min, Q1, 
  Median, Mean, SD, Variance, Q3, and Max for all numeric variables using 
  dplyr::across().
  o	Users can choose which summary statistics to display via a 
  checkboxGroupInput,allowing them to customize the level of detail.
  o	Right card: standardized boxplots (summaryBoxPlot) for three key variables 
  (sleep hours, sleep quality score, and PVT reaction time) plotted on the 
  z-score scale so they are directly comparable despite different units.
  o	An information icon and help text explain z-scores in plain language, making 
  the plot interpretable for non-statisticians.
3.	Sleep -> Cognition tab
  o	Provides the core “does sleep matter for cognition?” exploration.
  o	Left panel: a histogram of a user-selected variable (histVar), revealing its
    distribution in the filtered sample.
  o	Right panel: an interactive plotly scatterplot of a sleep metric (xVar) 
    against a cognitive outcome (yVar), colored by gender.
  o	A linear regression line is overlaid (geom_smooth(method = "lm") with a 
    fixed group) to visualize the overall trend.
  o	Hover tooltips display participant ID (when available), the selected x- 
    and y-axis values, and gender, supporting rich, case-level exploration.
4.	Lifestyle tab
  o	Contains two matched regression views:
    	Caffeine vs Sleep: sleep hours regressed on caffeine intake.
    	Stress vs Sleep: sleep hours regressed on stress level.
  o	Both plots:
    	Use ggplot to draw gender-colored points and an overlaid linear regression
      line with 95% confidence band.
    	Anchor the y-axis to a shared sleep_range so the scales are comparable.
    	Are converted to interactive plotly objects with tooltips showing ID, 
      exposure, sleep hours, and gender.
  o	Under each plot, a narrative summary (caffeineSummaryText, 
    stressSummaryText) is dynamically computed, interpreting the regression 
    slope and Pearson correlation in plain language (e.g., “each 1-unit increase
    in stress is associated with about 0.3 hours less sleep per night”).
5.	Model tab
  o	Implements a flexible linear regression engine via the reactive 
    current_model().
  o	Users choose an outcome (outcomeVar) from four cognitive and emotional 
    scores, and specify covariates (Age, Gender, BMI, caffeine intake, physical 
    activity, stress level). Sleep_Hours is always included as the primary 
    predictor.
  o	The left card displays both:
    	the exact model formula and full summary(lm(...)) output; and
    	a prose interpretation of the sleep-duration coefficient and R-squared, 
      updated in real time as filters and covariates change.
  o	The right card shows a Residuals vs Fitted plot (residPlot), letting users 
    visually assess linearity and homoscedasticity.
  o	Defensive programming (req() and validate(need())) guards all modeling 
    outputs, providing informative messages when sample size is too small or 
    when the model cannot be estimated under the current filters.
    
Server-side safeguards:
Throughout the server logic, validate(need(...)) and req() are used 
extensively to handle edge cases (e.g., zero participants, no numeric columns 
under a given filter, insufficient variability in caffeine or stress). This 
design ensures that the user sees a clear, friendly message rather than a 
Shiny error.

**Summary of Results**:

The Shiny app reveals:
•	Substantial individual variability in sleep and cognitive measures.
•	Modest associations between caffeine or stress and sleep duration, with 
  regression summaries provided for each subset.
•	Distributional differences across cognitive outcomes when standardized as 
  z-scores.
•	Limited explanatory power in the multivariable regression model, underscoring 
  the complexity of sleep–cognition relationships.

These results are interactively discoverable, under any user-defined filtering 
conditions.

**Data Source and Course Relevance**:

The app uses a single CSV file, sleep_deprivation_dataset.csv, downloaded from 
Kaggle. The dataset contains observations on N = 60 adult participants, 
each row corresponding to a unique individual. For each participant, the dataset
includes:

Sleep metrics: nightly sleep duration (Sleep_Hours), sleep quality score 
(Sleep_Quality_Score), and daytime sleepiness (Daytime_Sleepiness).

Cognitive outcomes: Stroop task reaction time (Stroop_Task_Reaction_Time), 
N-back working memory accuracy (N_Back_Accuracy), psychomotor vigilance test 
reaction time (PVT_Reaction_Time), and an emotion regulation score 
(Emotion_Regulation_Score).

Lifestyle factors: caffeine intake (Caffeine_Intake), physical activity level 
(Physical_Activity_Level), and perceived stress (Stress_Level).

Demographics / anthropometrics: age (Age), gender (Gender), and body mass 
index (BMI).

This dataset is aligned with the learning goals of BHDS 2010. It illustrates 
core concepts from the course, including:
•	Exploratory data analysis of continuous and categorical variables.
•	Use of histograms, boxplots, and summary statistics to characterize 
  distributions.
•	Simple and multiple linear regression to quantify associations between 
  exposures (e.g., sleep or lifestyle factors) and outcomes (e.g., reaction 
  time).
•	Interpretation of model coefficients, R², confidence intervals, and 
  limitations such as small sample size and potential confounding.
  
Because the data are relatively small and well-structured, they are ideal for 
an instructional Shiny app that foregrounds reactive programming, data wrangling
with dplyr/tidyr, and graphical storytelling with ggplot2 and plotly.

**Final Remarks**:

This Shiny application consolidates the full analytic workflow (from raw data 
structuring to visualization, modeling, and interpretive synthesis) into a 
cohesive interactive tool.

It is designed to support exploratory data analysis, pedagogical demonstration, 
and reproducible biostatistical communication.

The final product demonstrates proficiency in:
•	advanced R programming,
•	interactive UI engineering,
•	statistical modeling and diagnostics,
•	and modern data visualization practices.













