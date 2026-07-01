# Instalación necesaria: install.packages("ggVennDiagram")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(broom)
library(ggVennDiagram)
library(tidyr)

# --- UI: PROFESSIONAL DESIGN ---
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "BIOSTATISTICAL ANALYSIS SYSTEM", titleWidth = 250),
  
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem("Biostatistics I (Home)", tabName = "intro", icon = icon("university")),
                     menuItem("I. Descriptive Statistics", tabName = "descriptiva", icon = icon("chart-line")),
                     menuItem("II. Probability", tabName = "probabilidad", icon = icon("vial")),
                     menuItem("III. Discrete Variables", tabName = "discretas", icon = icon("layer-group")),
                     menuItem("IV. Continuous Variables", tabName = "continuas", icon = icon("ruler-vertical")),
                     menuItem("V. Joint Probability", tabName = "conjunta", icon = icon("project-diagram")),
                     menuItem("VI. Confidence Estimation", tabName = "estimacion", icon = icon("balance-scale")),
                     
                     hr(), # Línea divisoria en el menú
                     # SECCIÓN BIOESTADÍSTICA II
                     menuItem("Biostatistics II: (Home)", tabName = "intro2", icon = icon("university")),
                     menuItem("I. Statistic Inference", tabName = "inference_bio2", icon = icon("chart-line")),
                     menuItem("II. Simple Linear Regression", tabName = "regression_simple", icon = icon("calculator")),
                     menuItem("III. Multiple Regression", tabName = "regression_multiple", icon = icon("project-diagram")),
                     menuItem("VII. Analysis of Variance (ANOVA)", tabName = "anova", icon = icon("table"))
                   )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f8f9fa; }
      .box-title { font-weight: bold; font-family: 'Segoe UI', sans-serif; }
      .explanation-text { font-size: 15px; color: #444; line-height: 1.6; }
      .welcome-box { text-align: center; padding: 30px; }
      .ceu-logo { margin-bottom: 20px; max-width: 100%; height: auto; }
      .result-box { background: #ffffff; padding: 15px; border-radius: 8px; border: 1px solid #ddd; margin-top: 10px;}
      .user-guide-box { border-left: 5px solid #00c0ef; padding-left: 15px; background-color: #f0faff; border-radius: 4px; }
      .formula-box { background-color: #fcfcfc; padding: 10px; border-radius: 6px; border: 1px solid #eaeaea; margin-top: 10px; }
    "))),
    
    tabItems(
      
      # === HOME PAGE: BIOSTATISTICS I ===
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    div(class = "welcome-box",
                        img(src = "logo.jpg", class = "ceu-logo", width = "400px", 
                            onerror = "this.src='https://www.ceu.es/recursos/img/logos/logo-ceu-universidad-san-pablo.png';"),
                        h1("Biostatistical Analysis System"),
                        h4("Biomedical Engineering Degree - Universidad San Pablo CEU"),
                        hr(),
                        p("An interactive platform designed for modeling and analyzing biological variability.", class = "explanation-text")
                    )
                )
              ),
              fluidRow(
                box(title = "Course Program & Structure: Biostatistics I", width = 12, status = "info",
                    div(style = "background-color: #f4f6f7; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 5px solid #3c8dbc;",
                        h3(strong("BIOSTATISTICS I - MODULES"), style = "margin-top: 0; color: #2c3e50; font-family: 'Segoe UI', sans-serif;")
                    ),
                    fluidRow(
                      column(4, h4(strong("I. Descriptive Statistics")), p("Focus on identifying important data characteristics to provide insights for engineering problem-solving.")),
                      column(4, h4(strong("II. Probability")), p("Study of random experiments and assigning probabilities to events to handle uncertainty.")),
                      column(4, h4(strong("III. Discrete Variables")), p("Analysis of random variables with countable outcomes, modeling biological phenomena via classic mass functions."))
                    ), br(),
                    fluidRow(
                      column(4, h4(strong("IV. Continuous Variables")), p("Modeling intervals of real numbers where probability is defined as the area under a density function.")),
                      column(4, h4(strong("V. Joint Probability")), p("Exploration of multidimensional random variables, evaluating simultaneous events and dependencies in systems.")),
                      column(4, h4(strong("VI. Confidence Estimation")), p("Statistical inference techniques to construct interval estimates and quantify uncertainty in sample statistics."))
                    )
                )
              )),
      tabItem(tabName = "anova",
              fluidRow(
                box(title = "ANOVA Configuration", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("anovaType", "Study Design:", 
                                choices = c("Independent Groups" = "indep",
                                            "Repeated Measures (Paired)" = "paired")),
                    conditionalPanel(
                      condition = "input.anovaType == 'indep'",
                      textAreaInput("manualA", "Group A (e.g. 10,12,11):", "10, 12, 11, 13, 10"),
                      textAreaInput("manualB", "Group B (e.g. 15,14,16):", "15, 14, 16, 15, 17"),
                      textAreaInput("manualC", "Group C (e.g. 20,19,21):", "20, 19, 21, 20, 22")
                    ),
                    conditionalPanel(
                      condition = "input.anovaType == 'paired'",
                      p(em("Rows: Subjects | Cols: T1, T2, T3 (Sep by ;)")),
                      textAreaInput("pairedData", "Matrix Data:", "10,15,20; 12,14,19; 11,16,21")
                    ),
                    hr(),
                    actionButton("runAnova", "Run Analysis", icon = icon("play"), class = "btn-success")
                ),
                column(width = 8,
                       tabBox(title = "ANOVA Results", width = NULL,
                              tabPanel("Visualization", plotOutput("anovaPlot")),
                              tabPanel("Statistical Table", verbatimTextOutput("anovaResult")),
                              tabPanel("Interpretation", uiOutput("anovaExplanation"))
                       )
                )
              )
      ),
      
      # === HOME PAGE: BIOSTATISTICS II ===
      tabItem(tabName = "intro2",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    div(class = "welcome-box",
                        img(src = "logo.jpg", class = "ceu-logo", width = "400px", 
                            onerror = "this.src='https://www.ceu.es/recursos/img/logos/logo-ceu-universidad-san-pablo.png';"),
                        h1("Biostatistical Analysis System"),
                        h4("Biomedical Engineering Degree - Universidad San Pablo CEU"),
                        hr(),
                        p("An interactive platform designed for modeling and analyzing biological variability.", class = "explanation-text")
                    )
                )
              ),
              fluidRow(
                box(title = "Course Program & Structure: Biostatistics II", width = 12, status = "info",
                    div(style = "background-color: #f4f6f7; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 5px solid #3c8dbc;",
                        h3(strong("BIOSTATISTICS II - MODULES"), style = "margin-top: 0; color: #2c3e50; font-family: 'Segoe UI', sans-serif;")
                    ),
                    fluidRow(
                      column(4, h4(strong("I. Statistic Inference for One & Two Samples")), p("Explore foundational methods for estimating parameters and testing hypotheses.")),
                      column(4, h4(strong("II. Simple Linear Regression and Correlation")), p("Modeling linear relationships between a predictor and a numeric response.")),
                      column(4, h4(strong("III. Multiple Linear Regression")), p("Analysis of response models using multiple simultaneous regressor variables.")),
                      column(4, h4(strong("IV. ANOVA")), p("Statistical technique to compare means across multiple groups or repeated measures, identifying significant differences in experimental designs.")),
                    ), br()
                )
              )),
      
      # === PESTAÑA: BIOSTATISTICS II INFERENCE ===
      tabItem(tabName = "inference_bio2",
              fluidRow(
                box(title = "Biostatistics II: Advanced Statistical Inference", width = 12, status = "primary", solidHeader = TRUE,
                    p("Integrated module for inference and parametric/non-parametric hypothesis testing for one and two independent samples, based on the official syllabus.")
                )
              ),
              fluidRow(
                column(width = 12,
                       tabBox(title = "Statistical Tools", width = NULL, id = "inference_tabs",
                              tabPanel("Statistical Inference for a Single Sample",
                                       fluidRow(
                                         column(4,
                                                box(title = "Test Configuration", width = NULL, status = "warning", solidHeader = TRUE,
                                                    selectInput("one_param", "Parameter under study:",
                                                                choices = c("Population Mean (μ)" = "mean",
                                                                            "Population Proportion (p)" = "prop",
                                                                            "Population Variance (σ²)" = "var")),
                                                    hr(),
                                                    numericInput("one_null", "Null Hypothesis Value (H0):", value = 50),
                                                    selectInput("one_alt", "Alternative Hypothesis (H1):",
                                                                choices = c("Two-sided (≠)" = "two.sided", "Greater (>)" = "greater", "Less (<)" = "less")),
                                                    sliderInput("one_alpha", "Significance level (α):", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                                                    hr(),
                                                    h5(strong("Sample Data Input:")),
                                                    numericInput("one_n", "Sample Size (n):", value = 25, min = 2),
                                                    numericInput("one_est", "Sample Mean / Proportion / Var:", value = 52),
                                                    conditionalPanel(
                                                      condition = "input.one_param == 'mean'",
                                                      checkboxInput("one_known_var", "Known Population Variance (σ²)?", value = FALSE),
                                                      numericInput("one_sd", "Population σ/Sample s:", value = 5, min = 0.1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.one_param == 'var'",
                                                      numericInput("one_sample_var", "Sample Variance (s²):", value = 45)
                                                    )
                                                )
                                         ),
                                         column(8,
                                                box(title = "Hypothesis Test & Inference Results", width = NULL, status = "success",
                                                    div(class = "result-box",
                                                        uiOutput("one_results_text")
                                                    ),
                                                    hr(),
                                                    div(class = "formula-box",
                                                        h5(strong("Theoretical Framework & Test Statistic:")),
                                                        uiOutput("one_formula")
                                                    )
                                                )
                                         )
                                       )
                              ),
                              tabPanel("Statistical Inference for Two Samples",
                                       fluidRow(
                                         column(4,
                                                box(title = "Design Configuration", width = NULL, status = "warning", solidHeader = TRUE,
                                                    selectInput("two_type", "Inference Scenario:",
                                                                choices = c("Independent Means (μ1 - μ2)" = "means_ind",
                                                                            "Paired Means (Paired t-test)" = "means_paired",
                                                                            "Two Proportions (p1 - p2)" = "prop_two",
                                                                            "Two Variances (F-test)" = "var_two",
                                                                            "Wilcoxon-Mann Whitney (Non-parametric)" = "wilcox")),
                                                    hr(),
                                                    sliderInput("two_alpha", "Significance level (α):", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                                                    hr(),
                                                    h4("Sample 1 Metrics:"),
                                                    numericInput("n1", "Size n1:", value = 15, min = 2),
                                                    numericInput("mean1", "Mean / Prop 1:", value = 24.5),
                                                    conditionalPanel(
                                                      condition = "input.two_type == 'means_ind' || input.two_type == 'var_two'",
                                                      numericInput("sd1", "SD 1 (s1):", value = 3.2)
                                                    ),
                                                    hr(),
                                                    h4("Sample 2 Metrics:"),
                                                    numericInput("n2", "Size n2:", value = 15, min = 2),
                                                    numericInput("mean2", "Mean / Prop 2:", value = 21.2),
                                                    conditionalPanel(
                                                      condition = "input.two_type == 'means_ind' || input.two_type == 'var_two'",
                                                      numericInput("sd2", "SD 2 (s2):", value = 2.9)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.two_type == 'means_ind'",
                                                      checkboxInput("equal_vars", "Assume Equal Variances? (Homocedasticity)", value = TRUE)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.two_type == 'wilcox'",
                                                      p(em("Note: Ranks are calculated by standard ascending configuration order based on size."))
                                                    )
                                                )
                                         ),
                                         column(8,
                                                box(title = "Two-Sample Analysis & Comparison", width = NULL, status = "success",
                                                    div(class = "result-box",
                                                        uiOutput("two_results_text")
                                                    ),
                                                    hr(),
                                                    div(class = "formula-box",
                                                        h5(strong("Mathematical Framework:")),
                                                        uiOutput("two_formula")
                                                    )
                                                )
                                         )
                                       )
                              )
                       )
                )
              )
      ),
      
      # === MODULE I: DESCRIPTIVE STATISTICS ===
      tabItem(tabName = "descriptiva",
              fluidRow(
                column(width = 4,
                       box(title = "1. Data Configuration", width = NULL, status = "primary", solidHeader = TRUE,
                           selectInput("dataSource", "Data Source:",
                                       choices = c("Dataset: Cholesterol (Default)" = "default", 
                                                   "Manual Entry" = "manual", 
                                                   "Upload CSV file" = "upload")),
                           conditionalPanel("input.dataSource == 'manual'",
                                            textAreaInput("manualData", "Values (sep. by commas):", "105, 221, 183, 186, 121, 181")),
                           conditionalPanel("input.dataSource == 'upload'",
                                            fileInput("fileCSV", "Choose CSV file")),
                           hr(),
                           selectInput("plotType", "Visualization Technique:", 
                                       choices = c("Histogram & Density", "Advanced Boxplot", "Time Series", "X-Y Scatter Plot"))
                       ),
                       box(title = "User Guide", width = NULL, status = "info",
                           uiOutput("didacticGuide"))
                ),
                column(width = 8,
                       tabBox(title = "Descriptive Analysis", width = NULL,
                              tabPanel("Plot", plotOutput("mainPlot", height = "500px")),
                              tabPanel("Statistics", DTOutput("statsTable")),
                              tabPanel("Stem and Leaf", verbatimTextOutput("stemLeaf"))
                       )
                )
              )),
      
      # === MODULE II: PROBABILITY ===
      tabItem(tabName = "probabilidad",
              fluidRow(
                box(title = "Theoretical Foundation", width = 12, status = "info",
                    p("Conditional probability updates the estimation of an event when additional information is available."),
                    withMathJax("$$P(B|A) = \\frac{P(B \\cap A)}{P(A)}$$")
                ),
                column(width = 4,
                       box(title = "Combinatorics Calculator", width = NULL, status = "warning", solidHeader = TRUE,
                           numericInput("m", "Total items (m):", 10),
                           numericInput("n", "Group size (n):", 3),
                           selectInput("method", "Technique:", 
                                       choices = c("Combination", "Variation", "Permutation")),
                           hr(),
                           verbatimTextOutput("combResult")
                       ),
                       box(title = "Bayes' Theorem", width = NULL, status = "danger", solidHeader = TRUE,
                           numericInput("incidencia", "Incidence P(D):", 0.0001),
                           numericInput("sensibilidad", "Sensitivity P(S|D):", 0.99),
                           numericInput("especificidad", "Specificity P(Sc|Dc):", 0.95),
                           hr(),
                           div(style="background:#f9f9f9; padding:20px; border-radius:10px; border: 1px solid #ddd;",
                               h3(uiOutput("probBayes"), align = "center"))
                       )
                ),
                column(width = 8,
                       box(title = "Venn Diagram Visualizer", width = NULL, status = "primary", solidHeader = TRUE,
                           fluidRow(
                             column(6, sliderInput("venn_A", "Elements in Event A:", 10, 50, 30)),
                             column(6, sliderInput("venn_B", "Elements in Event B:", 10, 50, 25))
                           ),
                           sliderInput("venn_Inter", "Intersection (A ∩ B):", 0, 25, 10),
                           hr(),
                           plotOutput("vennPlot", height = "400px"),
                           p(class = "explanation-text", em("Note: Adjust sliders to visualize set relations and overlap."))
                       )
                )
              )),
      
      # === MODULE III: DISCRETE RANDOM VARIABLES ===
      tabItem(tabName = "discretas",
              fluidRow(
                column(width = 4,
                       box(title = "Distribution Settings", width = NULL, status = "primary", solidHeader = TRUE,
                           selectInput("distType", "Select Distribution:",
                                       choices = c("Binomial" = "binom", 
                                                   "Geometric" = "geom", 
                                                   "Negative Binomial" = "nbinom", 
                                                   "Poisson" = "pois")),
                           hr(),
                           uiOutput("distInputs"),
                           conditionalPanel(
                             condition = "input.distType != 'binom'",
                             sliderInput("xlim_user", "Max X axis visible:", min = 10, max = 200, value = 30)
                           )
                       ),
                       box(title = "Expected Values", width = NULL, status = "info",
                           uiOutput("distMoments"))
                ),
                column(width = 8,
                       tabBox(title = "Probability Visualization", width = NULL,
                              tabPanel("Mass Function (PMF)", plotOutput("distPlot")),
                              tabPanel("Cumulative (CDF)", plotOutput("distCumPlot")),
                              tabPanel("Theoretical Formulas", uiOutput("distFormula"))
                       )
                )
              )),
      
      # === MODULE IV: CONTINUOUS RANDOM VARIABLES ===
      tabItem(tabName = "continuas",
              fluidRow(
                column(width = 4,
                       box(title = "Continuous Settings", width = NULL, status = "primary", solidHeader = TRUE,
                           selectInput("contDistType", "Select Distribution:",
                                       choices = c("Normal" = "norm", 
                                                   "Exponential" = "exp")),
                           hr(),
                           uiOutput("contDistInputs")
                       ),
                       box(title = "Cumulative Probability", width = NULL, status = "warning", solidHeader = TRUE,
                           p("In continuous variables, we calculate P(X ≤ x)."),
                           uiOutput("contProbResult"))
                ),
                column(width = 8,
                       tabBox(title = "Density & Distribution", width = NULL,
                              tabPanel("Density Function (PDF)", plotOutput("contPDFPlot")),
                              tabPanel("Cumulative (CDF)", plotOutput("contCDFPlot")),
                              tabPanel("Formulas", uiOutput("contFormula"))
                       )
                )
              )),
      
      # === MODULE V: JOINT PROBABILITY ===
      tabItem(tabName = "conjunta",
              fluidRow(
                column(width = 4,
                       box(title = "Interactive Case Study", width = NULL, status = "primary", solidHeader = TRUE,
                           selectInput("jointCase", "Select Scenario:",
                                       choices = c("Digital Transmission (Discrete)" = "digital", 
                                                   "3D Printer Life (Continuous)" = "3dprint")),
                           hr(),
                           uiOutput("jointInputs") 
                       ),
                       box(title = "Live Calculation", width = NULL, status = "warning",
                           uiOutput("jointResults"))
                ),
                column(width = 8,
                       tabBox(title = "Joint Analysis Visualizer", width = NULL,
                              tabPanel("Interactive Plot", plotOutput("jointPlot", height = "500px")),
                              tabPanel("Theory & Formulas", uiOutput("jointTheory"))
                       )
                )
              )
      ),
     
      
      
      # === MODULE VI: CONFIDENCE ESTIMATION ===
      tabItem(tabName = "estimacion",
              fluidRow(
                box(title = "Chapter VI: Confidence Estimation", width = 12, status = "primary", solidHeader = TRUE,
                    p("This module allows you to calculate confidence intervals for the mean of a normal population from a vector of sample data.")
                )
              ),
              fluidRow(
                column(width = 4,
                       box(title = "Parameter Configuration", width = NULL, status = "primary", solidHeader = TRUE,
                           selectInput("estDataSource", "Data Source:",
                                       choices = c("Example Vector" = "example", 
                                                   "Manual Sample Entry" = "manual")),
                           conditionalPanel(
                             condition = "input.estDataSource == 'example'",
                             p(em("Using sample data: 41.92, 42.15, 41.68, 42.01, 41.88... (n=10)"))
                           ),
                           conditionalPanel(
                             condition = "input.estDataSource == 'manual'",
                             textAreaInput("estManualData", "Sample Values (sep. by commas):", 
                                           value = "10, 11, 12, 12, 12, 12, 1")
                           ),
                           hr(),
                           sliderInput("est_conf", "Confidence Level (1-α):", 
                                       min = 0.80, max = 0.99, value = 0.95, step = 0.01)
                       ),
                       box(title = "Theory & Formulas", width = NULL, status = "info",
                           p("Confidence intervals express the degree of uncertainty associated with a sample estimation using the Standard Normal distribution (Z):"),
                           withMathJax("$$\\bar{X} \\pm z_{\\alpha/2} \\cdot \\frac{s}{\\sqrt{n}}$$")
                       )
                ),
                column(width = 8,
                       box(title = "Confidence Interval Visualizer", width = NULL, status = "primary",
                           plotOutput("confPlot", height = "350px")
                       ),
                       box(title = "Calculated Bounds", width = NULL, status = "success",
                           fluidRow(
                             column(width = 6, 
                                    h4(strong("Lower Bound (L):")),
                                    div(style = "background: #f4f4f4; padding: 10px; border-radius: 5px; font-size: 18px;",
                                        textOutput("est_lower"))
                             ),
                             column(width = 6, 
                                    h4(strong("Upper Bound (U):")),
                                    div(style = "background: #f4f4f4; padding: 10px; border-radius: 5px; font-size: 18px;",
                                        textOutput("est_upper"))
                             )
                           )
                       )
                )
              )
      ),
      # --- PESTAÑA: SIMPLE LINEAR REGRESSION ---
      tabItem(tabName = "regression_simple",
              fluidRow(
                box(title = "Simple Linear Regression Configuration", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("regS_x", "Predictor (X):", choices = NULL),
                    selectInput("regS_y", "Response (Y):", choices = NULL),
                    actionButton("runRegS", "Adjust Model", icon = icon("calculator"))
                ),
                tabBox(title = "Simple Regression Analysis", width = 8,
                       tabPanel("Visualization", plotOutput("plotRegSimple")), # [cite: 8]
                       tabPanel("Input Data", DTOutput("tablaSimple")),
                       # En tu pestaña de Regresión Simple
                       wellPanel(
                         h4("Didactic Disclaimer", style = "color: #337ab7;"),
                         p("This application is a didactic tool created for educational purposes to demonstrate the 
    fundamentals of Linear Regression. The data provided is for illustrative use only to 
    help users understand statistical modeling concepts.")
                       )
                )
              )
      ),
      
      # --- PESTAÑA: MULTIPLE LINEAR REGRESSION ---
      tabItem(tabName = "regression_multiple",
              fluidRow(
                box(title = "Multiple Regression Configuration", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("regM_y", "Response (Y):", choices = NULL),
                    checkboxGroupInput("regM_x", "Predictors (X1, X2...):", choices = NULL),
                    actionButton("runRegM", "Adjust Model", icon = icon("project-diagram"))
                ),
                column(width = 8,
                       tabBox(width = NULL,
                              tabPanel("Coefficients", plotOutput("plotRegMultCoef")), # [cite: 22]
                              tabPanel("Observed vs Predicted", plotOutput("plotRegMultObsPred")), # [cite: 29]
                              tabPanel("Input Data", DTOutput("tablaMultiple")),
                              # En tu pestaña de Regresión Simple
                              wellPanel(
                                h4("Didactic Disclaimer", style = "color: #337ab7;"),
                                p("This application is a didactic tool created for educational purposes to demonstrate the 
    fundamentals of Linear Regression. The data provided is for illustrative use only to 
    help users understand statistical modeling concepts.")
                              )
                       )
                )
              )
      )
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  # ==========================================
  # LÓGICA DE DETECCIÓN E INFERENCIA (BIO II)
  # ==========================================
  output$one_results_text <- renderUI({
    req(input$one_param, input$one_n, input$one_est)
    n <- input$one_n
    h0_val <- input$one_null
    est <- input$one_est
    alpha <- input$one_alpha
    
    if(input$one_param == "mean") {
      sd_val <- input$one_sd
      if(input$one_known_var) {
        z_stat <- (est - h0_val) / (sd_val / sqrt(n))
        p_val <- if(input$one_alt == "two.sided") 2 * (1 - pnorm(abs(z_stat))) else if(input$one_alt == "greater") 1 - pnorm(z_stat) else pnorm(z_stat)
        decision <- if(p_val < alpha) "REJECT H0" else "FAIL TO REJECT H0"
        tagList(
          h4(strong("Z-Test on Mean (Variance Known)")),
          p(strong("Test Statistic (Z0): "), round(z_stat, 4)),
          p(strong("p-value: "), round(p_val, 5)),
          h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
        )
      } else {
        t_stat <- (est - h0_val) / (sd_val / sqrt(n))
        p_val <- if(input$one_alt == "two.sided") 2 * (1 - pt(abs(t_stat), df=n-1)) else if(input$one_alt == "greater") 1 - pt(t_stat, df=n-1) else pt(t_stat, df=n-1)
        decision <- if(p_val < alpha) "REJECT H0" else "FAIL TO REJECT H0"
        tagList(
          h4(strong("t-Test on Mean (Variance Unknown)")),
          p(strong("Test Statistic (t0): "), round(t_stat, 4)),
          p(strong("Degrees of Freedom: "), n - 1),
          p(strong("p-value: "), round(p_val, 5)),
          h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
        )
      }
    } else if(input$one_param == "prop") {
      p_null <- h0_val / 100
      if(p_null <= 0 || p_null >= 1) p_null <- 0.5
      z_stat <- (est - p_null) / sqrt((p_null * (1 - p_null)) / n)
      p_val <- if(input$one_alt == "two.sided") 2 * (1 - pnorm(abs(z_stat))) else if(input$one_alt == "greater") 1 - pnorm(z_stat) else pnorm(z_stat)
      decision <- if(p_val < alpha) "REJECT H0" else "FAIL TO REJECT H0"
      tagList(
        h4(strong("Approximate Test on Proportion")),
        p(strong("Test Statistic (Z0): "), round(z_stat, 4)),
        p(strong("p-value: "), round(p_val, 5)),
        h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
      )
    } else {
      s_var <- input$one_sample_var
      chi_stat <- (n - 1) * s_var / h0_val
      p_val <- if(input$one_alt == "two.sided") 2 * min(pchisq(chi_stat, df=n-1), 1 - pchisq(chi_stat, df=n-1)) else if(input$one_alt == "greater") 1 - pchisq(chi_stat, df=n-1) else pchisq(chi_stat, df=n-1)
      decision <- if(p_val < alpha) "REJECT H0" else "FAIL TO REJECT H0"
      tagList(
        h4(strong("Chi-Square Test on Variance")),
        p(strong("Test Statistic (X²): "), round(chi_stat, 4)),
        p(strong("Degrees of Freedom: "), n - 1),
        p(strong("p-value: "), round(p_val, 5)),
        h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
      )
    }
  })
  dataset_reactivo <- reactive({
    if (input$dataSource == "default") {
      data.frame(
        Alumno = c("Miguel", "Pedro", "Luis", "José", "Antonio", "María", "Lucía", "Jorge", "Ana", "Elena", "Sofía", "Carlos", "Pablo", "Laura", "Diego"), 
        Nota = c(4.5, 5.0, 5.8, 7.1, 8.0, 6.2, 5.5, 9.1, 4.8, 7.5, 6.8, 5.2, 8.4, 7.0, 6.5), 
        Asistencia = c(70, 75, 80, 90, 95, 82, 78, 98, 72, 88, 85, 74, 92, 86, 81),
        Horas_Estudio = c(5, 7, 10, 15, 20, 12, 8, 25, 6, 16, 14, 7, 22, 13, 11),
        Entregas = c(2, 3, 4, 5, 5, 4, 3, 5, 2, 5, 4, 3, 5, 4, 4),
        Carrera = c("Industrial Engineering", "Telecommunications Engineering", "Doctor", "Scientist", "Lawyer", "Teacher", "Biomedical Engineering", "Software Engineering", "Chemistry Engineering", "BBA", "Economy", "Filosophy", "Energy Engineering", "Aeronautic Engineering", "Fireman"),
        stringsAsFactors = FALSE
      )
    } else {
      req(input$fileCSV)
      read.csv(input$fileCSV$datapath)
    }
  })
  
  # 2. Actualizar selectores (solo numéricos para regresión)
  observe({
    req(dataset_reactivo())
    df <- dataset_reactivo()
    cols_num <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(session, "regS_x", choices = cols_num)
    updateSelectInput(session, "regS_y", choices = cols_num)
    updateSelectInput(session, "regM_y", choices = cols_num)
    updateCheckboxGroupInput(session, "regM_x", choices = cols_num)
  })
  
  # 3. LÓGICA DE MODELOS
  modelSimple <- eventReactive(input$runRegS, {
    req(input$regS_x, input$regS_y)
    data <- dataset_reactivo()
    lm(as.formula(paste(input$regS_y, "~", input$regS_x)), data = na.omit(data))
  })
  
  # 3. Lógica del Modelo Múltiple (Protegida)
  modelMult <- eventReactive(input$runRegM, {
    req(input$regM_y, input$regM_x)
    
    # Validar que Y no sea X
    if (input$regM_y %in% input$regM_x) return(NULL)
    
    data <- dataset_reactivo()
    # Usamos solo columnas numéricas seleccionadas
    data_clean <- na.omit(data[, c(input$regM_y, input$regM_x)])
    
    formula_reg <- as.formula(paste(input$regM_y, "~", paste(input$regM_x, collapse = "+")))
    lm(formula_reg, data = data_clean)
  })
  
  # 4. GRÁFICOS (Ahora incluyen nombres de alumnos)
  output$plotRegSimple <- renderPlot({
    req(input$runRegS)
    isolate({
      modelo <- modelSimple()
      df <- dataset_reactivo()
      
      ggplot(df, aes_string(x = input$regS_x, y = input$regS_y)) +
        geom_point(size = 3, color = "steelblue") +
        # Usamos check_overlap = TRUE para que los nombres no se solapen
        geom_text(aes(label = Alumno), vjust = -1, size = 3, check_overlap = TRUE) +
        geom_smooth(method = "lm", color = "darkred", fill = "orange", alpha = 0.2) +
        theme_minimal() +
        labs(title = paste("Relación entre", input$regS_x, "y", input$regS_y))
    })
  })
  
  output$plotRegMultCoef <- renderPlot({
    req(modelMult())
    modelo <- modelMult()
    
    # Usamos broom::tidy y filtramos el intercepto
    coefs <- broom::tidy(modelo, conf.int = TRUE) %>% 
      filter(term != "(Intercept)") # <--- ESTO ES LA CLAVE
    
    ggplot(coefs, aes(x = estimate, y = term)) + 
      geom_point(size = 4, color = "blue") + 
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "blue") + 
      geom_text(aes(label = round(estimate, 2)), vjust = -1) + # Muestra el valor exacto
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
      theme_minimal() +
      labs(title = "Effect of Grade on Attendance", 
           x = "Estimated increase in Attendance for each grade point", 
           y = "Predictor")
  })
  
  output$plotRegMultObsPred <- renderPlot({
    req(input$runRegM)
    isolate({
      modelo <- modelMult()
      df <- dataset_reactivo()
      
      # 1. Creamos un dataframe con los datos y las predicciones
      df_plot <- data.frame(
        Alumno = df$Alumno,
        Obs = modelo$model[[1]], 
        Pred = fitted(modelo)
      )
      
      # 2. Calculamos el R-cuadrado para el título
      r_sq <- summary(modelo)$r.squared
      
      # 3. Gráfico mejorado
      ggplot(df_plot, aes(x = Obs, y = Pred)) +
        # Puntos con color atractivo
        geom_point(size = 4, color = "#2c3e50", alpha = 0.8) +
        # Línea de referencia (ajuste perfecto)
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
        # Etiquetas de los alumnos
        geom_text(aes(label = Alumno), vjust = -1, size = 3.5, color = "#34495e") +
        # Sombra de error para ver la dispersión
        geom_smooth(method = "lm", color = "blue", fill = "blue", alpha = 0.1) +
        theme_minimal() +
        labs(
          title = "Model Quality Analysis: Observed vs. Predicted",
          subtitle = paste("R-squared (Adjusted):", round(r_sq, 3), 
                           "| Points on the red line indicate a perfect prediction."),
          x = "Actual observed value (Data)", 
          y = "Value predicted by the model"
        ) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.title = element_text(size = 12))
    })
  })
  
  
  # 5. TABLAS Y OTROS (Mantén aquí todo el resto de tu lógica original)
  output$tablaSimple <- output$tablaMultiple <- renderDT({
    req(dataset_reactivo())
    datatable(dataset_reactivo(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  
  output$one_formula <- renderUI({
    req(input$one_param)
    formula <- switch(input$one_param,
                      "mean" = if(input$one_known_var) "$$Z_0 = \\frac{\\bar{X} - \\mu_0}{\\sigma / \\sqrt{n}}$$ " else "$$T_0 = \\frac{\\bar{X} - \\mu_0}{S / \\sqrt{n}}, \\quad df = n - 1$$",
                      "prop" = "$$Z_0 = \\frac{\\hat{P} - p_0}{\\sqrt{\\frac{p_0(1-p_0)}{n}}}$$",
                      "var" = "$$\\chi^2_0 = \\frac{(n-1)S^2}{\\sigma_0^2}, \\quad df = n - 1$$")
    withMathJax(formula)
  })
  
  output$two_results_text <- renderUI({
    req(input$two_type, input$n1, input$n2)
    alpha <- input$two_alpha
    
    if(input$two_type == "means_ind") {
      req(input$sd1, input$sd2)
      if(input$equal_vars) {
        sp2 <- ((input$n1 - 1)*input$sd1^2 + (input$n2 - 1)*input$sd2^2) / (input$n1 + input$n2 - 2)
        sp <- sqrt(sp2)
        t_stat <- (input$mean1 - input$mean2) / (sp * sqrt(1/input$n1 + 1/input$n2))
        df_val <- input$n1 + input$n2 - 2
        p_val <- 2 * (1 - pt(abs(t_stat), df=df_val))
        decision <- if(p_val < alpha) "REJECT H0 (Means are significantly different)" else "FAIL TO REJECT H0"
        tagList(
          h4(strong("Two Independent Means Test (Equal Variances)")),
          p(strong("Pooled SD (Sp): "), round(sp, 4)),
          p(strong("Test Statistic (T0): "), round(t_stat, 4)),
          p(strong("p-value: "), round(p_val, 5)),
          h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
        )
      } else {
        se <- sqrt(input$sd1^2/input$n1 + input$sd2^2/input$n2)
        t_stat <- (input$mean1 - input$mean2) / se
        num <- (input$sd1^2/input$n1 + input$sd2^2/input$n2)^2
        den <- (input$sd1^2/input$n1)^2/(input$n1-1) + (input$sd2^2/input$n2)^2/(input$n2-1)
        df_val <- num / den
        p_val <- 2 * (1 - pt(abs(t_stat), df=df_val))
        decision <- if(p_val < alpha) "REJECT H0" else "FAIL TO REJECT H0"
        tagList(
          h4(strong("Welch t-Test (Unequal Variances)")),
          p(strong("Approximated Degrees of Freedom (v): "), round(df_val, 2)),
          p(strong("Test Statistic (T0*): "), round(t_stat, 4)),
          p(strong("p-value: "), round(p_val, 5)),
          h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
        )
      }
    } else if(input$two_type == "means_paired") {
      t_stat <- (input$mean1 - input$mean2) / (2.5 / sqrt(input$n1))
      p_val <- 2 * (1 - pt(abs(t_stat), df=input$n1-1))
      decision <- if(p_val < alpha) "REJECT H0" else "FAIL TO REJECT H0"
      tagList(
        h4(strong("Paired t-test (Dependent Samples)")),
        p(strong("Test Statistic (T0): "), round(t_stat, 4)),
        p(strong("Degrees of Freedom: "), input$n1 - 1),
        p(strong("p-value: "), round(p_val, 5)),
        h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
      )
    } else if(input$two_type == "prop_two") {
      p1_raw <- as.numeric(gsub(",", ".", as.character(input$mean1)))
      p2_raw <- as.numeric(gsub(",", ".", as.character(input$mean2)))
      
      if (is.na(p1_raw) || is.na(p2_raw)) {
        return(p(style = "color:orange; font-weight:bold;", "Please enter valid numeric metrics."))
      }
      p1 <- if(p1_raw > 1) p1_raw / 100 else p1_raw
      p2 <- if(p2_raw > 1) p2_raw / 100 else p2_raw
      
      if(p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
        return(p(style = "color:red; font-weight:bold;", "Error: rror: Proportions must be between 0 and 1."))
      }
      p_pool <- (p1 * input$n1 + p2 * input$n2) / (input$n1 + input$n2)
      if(p_pool <= 0 || p_pool >= 1) {
        return(p(style = "color:red; font-weight:bold;", "Error en P-pool."))
      }
      z_stat <- (p1 - p2) / sqrt(p_pool * (1 - p_pool) * (1/input$n1 + 1/input$n2))
      p_val <- 2 * (1 - pnorm(abs(z_stat)))
      decision <- if(p_val < alpha) "REJECT H0 (Proportions are significantly different)" else "FAIL TO REJECT H0"
      tagList(
        h4(strong("Inference on Two Proportions (Z-Test)")),
        p(strong("Proporción 1 (p1): "), round(p1, 4)),
        p(strong("Proporción 2 (p2): "), round(p2, 4)),
        p(strong("Test Statistic (Z0): "), round(z_stat, 4)),
        p(strong("p-value: "), round(p_val, 5)),
        h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
      )
    } else if(input$two_type == "var_two") {
      f_stat <- input$sd1^2 / input$sd2^2
      p_val <- 2 * (1 - pf(f_stat, df1=input$n1-1, df2=input$n2-1))
      decision <- if(p_val < alpha) "REJECT H0" else "FAIL TO REJECT H0"
      tagList(
        h4(strong("F-Distribution Test on Variance Equality")),
        p(strong("Test Statistic (F0): "), round(f_stat, 4)),
        p(strong("p-value: "), round(p_val, 5)),
        h3(strong(decision), style = paste0("color:", if(p_val < alpha) "red" else "green"))
      )
    } else {
      u_val <- input$n1 * input$n2 + (input$n1 * (input$n1 + 1)) / 2 - (input$n1 * input$n2 * 0.4)
      tagList(
        h4(strong("Wilcoxon-Mann Whitney Non-parametric Framework")),
        p(strong("Asymptotic U1 calculation approximation: "), round(u_val, 1))
      )
    }
  })
 
  
  output$two_formula <- renderUI({
    req(input$two_type)
    formula <- switch(input$two_type,
                      "means_ind" = if(input$equal_vars) "$$T_0 = \\frac{\\bar{X}_1 - \\bar{X}_2}{S_p \\sqrt{\\frac{1}{n1}+\\frac{1}{n2}}}$$" else "$$T_0^* = \\frac{\\bar{X}_1 - \\bar{X}_2}{\\sqrt{\\frac{S_1^2}{n1}+\\frac{S_2^2}{n2}}}$$",
                      "means_paired" = "$$T_0 = \\frac{\\bar{D} - \\Delta_0}{S_D / \\sqrt{n}}$$ ",
                      "prop_two" = "$$Z_0 = \\frac{\\hat{P}_1 - \\hat{P}_2}{\\sqrt{\\hat{P}(1-\\hat{P})(\\frac{1}{n1}+\\frac{1}{n2})}}$$ ",
                      "var_two" = "$$F_0 = \\frac{S_1^2}{S_2^2}$$",
                      "wilcox" = "$$U_1 = N_1N_2 + \\frac{N_1(N_1+1)}{2} - R_1$$")
    withMathJax(formula)
  })
  # Lógica ANOVA
  
  
  # --- LOGIC: DESCRIPTIVE DATA ---
  cleanData <- reactive({
    if (input$dataSource == "default") {
      val <- c(105, 221, 183, 186, 121, 181, 180, 143, 97, 154, 153, 174, 120, 168, 167, 141)
    } else if (input$dataSource == "manual") {
      val <- as.numeric(unlist(strsplit(input$manualData, "[,\\s]+")))
    } else {
      req(input$fileCSV)
      df <- read.csv(input$fileCSV$datapath)
      val <- df[[1]]
    }
    req(length(na.omit(val)) > 0)
    return(na.omit(val))
  })
  
  output$didacticGuide <- renderUI({
    req(input$plotType)
    guide_content <- switch(input$plotType,
                            "Histogram & Density" = tagList(
                              h5(strong("Histogram & Density")),
                              p("Used to visualize the underlying frequency distribution of a continuous variable.")
                            ),
                            "Advanced Boxplot" = tagList(
                              h5(strong("Box and Whisker Plot")),
                              p("Summarizes the data's shape, central tendency, and variability.")
                            ),
                            "Time Series" = tagList(
                              h5(strong("Time Series Plot")),
                              p("Displays data points indexed in time order.")
                            ),
                            "X-Y Scatter Plot" = tagList(
                              h5(strong("Scatter Plot & Regression")),
                              p("Shows relationships between two numeric variables.")
                            )
    )
    div(class = "user-guide-box", guide_content)
  })
  
  output$mainPlot <- renderPlot({
    x <- cleanData(); req(x)
    p <- ggplot(data.frame(x), aes(x)) + theme_minimal()
    if(input$plotType == "Histogram & Density") {
      p + geom_histogram(aes(y=..density..), bins=round(sqrt(length(x))), fill="#2c3e50", color="white") +
        geom_density(color="#e74c3c", size=1) + labs(title="Histogram with Density Curve", x="Value")
    } else if(input$plotType == "Advanced Boxplot") {
      ggplot(data.frame(x), aes(y=x)) + geom_boxplot(fill="#27ae60") + 
        theme_minimal() + labs(title="Box and Whisker Plot", y="Value")
    } else if(input$plotType == "Time Series") {
      ggplot(data.frame(i=1:length(x), x), aes(i, x)) + geom_line(color="#2980b9") + geom_point() +
        labs(title="Time Series Plot", x="Index", y="Value")
    } else {
      y <- x * 0.8 + rnorm(length(x), 0, 10)
      ggplot(data.frame(x, y), aes(x, y)) + geom_point() + geom_smooth(method="lm", color="red", formula = y ~ x) +
        labs(title="Scatter Plot with Regression Line", x="Variable X", y="Variable Y")
    }
  })
  
  output$statsTable <- renderDT({
    x <- cleanData(); req(x)
    df <- data.frame(Metric = c("Mean", "Median", "SD", "IQR"), Value = round(c(mean(x), median(x), sd(x), IQR(x)), 3))
    datatable(df, options = list(dom = 't'))
  })
  
  output$stemLeaf <- renderPrint({ x <- cleanData(); req(x); stem(x) })
  
  output$combResult <- renderPrint({
    m <- input$m; n <- input$n
    if(input$method == "Combination") cat("Result:", choose(m, n))
    else if(input$method == "Variation") cat("Result:", factorial(m)/factorial(m-n))
    else cat("Result:", factorial(m))
  })
  
  output$probBayes <- renderUI({
    prob <- (input$sensibilidad * input$incidencia) / ((input$sensibilidad * input$incidencia) + ((1-input$especificidad) * (1-input$incidencia)))
    tagList(span("P(Disease | Positive):", style="font-size: 15px;"), br(), strong(paste0(round(prob*100, 4), "%")))
  })
  
  output$vennPlot <- renderPlot({
    x <- list(Event_A = 1:input$venn_A, Event_B = (input$venn_A - input$venn_Inter + 1):(input$venn_A - input$venn_Inter + input$venn_B))
    ggVennDiagram(x) + scale_fill_gradient(low = "#e1f5fe", high = "#01579b") + theme(legend.position = "none")
  })
  
  output$distInputs <- renderUI({
    switch(input$distType,
           "binom" = tagList(
             numericInput("bin_n", "Trials (n):", 18, min = 1), 
             sliderInput("bin_p", "Prob (p):", min = 0, max = 1, value = 0.5, step = 0.01)
           ),
           "geom" = sliderInput("geom_p", "Prob (p):", min = 0.01, max = 1, value = 0.2, step = 0.01),
           "nbinom" = tagList(
             numericInput("nb_r", "Successes (r):", 3, min = 1), 
             sliderInput("nb_p", "Prob (p):", min = 0.01, max = 1, value = 0.2, step = 0.01)
           ),
           "pois" = numericInput("pois_lambda", "Lambda (λ):", 2.3, min = 0.1))
  })
  
  output$distMoments <- renderUI({
    req(input$distType)
    stats <- switch(input$distType,
                    "binom" = {
                      n <- input$bin_n; p <- input$bin_p
                      list(mean = n * p, var = n * p * (1 - p))
                    },
                    "geom" = {
                      p <- input$geom_p
                      list(mean = 1 / p, var = (1 - p) / (p^2))
                    },
                    "nbinom" = {
                      r <- input$nb_r; p <- input$nb_p
                      list(mean = (r * (1 - p)) / p, var = (r * (1 - p)) / (p^2))
                    },
                    "pois" = {
                      lambda <- input$pois_lambda
                      list(mean = lambda, var = lambda)
                    }
    )
    tagList(
      div(style = "margin-bottom: 10px;", strong("Mean (Expected Value): "), span(round(stats$mean, 3))),
      div(strong("Variance: "), span(round(stats$var, 3))), hr(),
      p(em("Standard Deviation: "), strong(round(sqrt(stats$var), 3)), style = "font-size: 12px;")
    )
  })
  
  output$distPlot <- renderPlot({
    req(input$distType)
    data <- switch(input$distType,
                   "binom" = data.frame(x=0:input$bin_n, p=dbinom(0:input$bin_n, input$bin_n, input$bin_p)),
                   "geom" = data.frame(x=0:input$xlim_user, p=dgeom(0:input$xlim_user, input$geom_p)),
                   "nbinom" = data.frame(x=0:input$xlim_user, p=dnbinom(0:input$xlim_user, input$nb_r, input$nb_p)),
                   "pois" = data.frame(x=0:input$xlim_user, p=dpois(0:input$xlim_user, input$pois_lambda)))
    ggplot(data, aes(x, p)) + geom_segment(aes(xend=x, yend=0), color="#3498db") + geom_point() + theme_minimal() + labs(y="P(X=x)", title="Probability Mass Function")
  })
  
  output$distCumPlot <- renderPlot({
    req(input$distType)
    data <- switch(input$distType,
                   "binom" = data.frame(x=0:input$bin_n, p=pbinom(0:input$bin_n, input$bin_n, input$bin_p)),
                   "geom" = data.frame(x=0:input$xlim_user, p=pgeom(0:input$xlim_user, input$geom_p)),
                   "nbinom" = data.frame(x=0:input$xlim_user, p=pnbinom(0:input$xlim_user, input$nb_r, input$nb_p)),
                   "pois" = data.frame(x=0:input$xlim_user, p=ppois(0:input$xlim_user, input$pois_lambda)))
    ggplot(data, aes(x, p)) + geom_step(color="#2c3e50") + geom_point() + theme_minimal() + labs(y="P(X≤x)", title="Cumulative Distribution Function")
  })
  
  output$contDistInputs <- renderUI({
    if(input$contDistType == "norm") {
      tagList(
        numericInput("norm_mean", "Mean (μ):", 0),
        numericInput("norm_sd", "SD (σ):", 1, min = 0.1),
        sliderInput("cont_eval", "Value to evaluate (x):", -3, 3, 0, step = 0.1)
      )
    } else {
      tagList(
        numericInput("exp_lambda", "Rate (λ):", 1, min = 0.01),
        sliderInput("cont_eval", "Value to evaluate (x):", 0, 10, 1, step = 0.1)
      )
    }
  })
  
  output$contProbResult <- renderUI({
    req(input$contDistType, input$cont_eval)
    prob <- if(input$contDistType == "norm") pnorm(input$cont_eval, input$norm_mean, input$norm_sd) else pexp(input$cont_eval, input$exp_lambda)
    div(class="result-box", strong(paste0("P(X ≤ ", input$cont_eval, "): ")), round(prob, 4), p(em("Note: In continuous variables, point probability P(X=x) is always 0."), style="font-size:11px; margin-top:5px;"))
  })
  
  output$contPDFPlot <- renderPlot({
    req(input$contDistType)
    if(input$contDistType == "norm") {
      mu <- input$norm_mean; s <- input$norm_sd
      ggplot(data.frame(x = c(mu-4*s, mu+4*s)), aes(x)) +
        stat_function(fun = dnorm, args = list(mean = mu, sd = s), color = "#2980b9", size = 1) +
        geom_area(stat = "function", fun = dnorm, args = list(mean = mu, sd = s), xlim = c(mu-4*s, input$cont_eval), fill = "#3498db", alpha = 0.3) +
        theme_minimal() + labs(title = "Normal Density Function (PDF)", y = "f(x)")
    } else {
      l <- input$exp_lambda
      ggplot(data.frame(x = c(0, 10/l)), aes(x)) +
        stat_function(fun = dexp, args = list(rate = l), color = "#e67e22", size = 1) +
        geom_area(stat = "function", fun = dexp, args = list(rate = l), xlim = c(0, input$cont_eval), fill = "#f39c12", alpha = 0.3) +
        theme_minimal() + labs(title = "Exponential Density Function (PDF)", y = "f(x)")
    }
  })
  
  output$contCDFPlot <- renderPlot({
    req(input$contDistType)
    if(input$contDistType == "norm") {
      ggplot(data.frame(x = c(input$norm_mean-4*input$norm_sd, input$norm_mean+4*input$norm_sd)), aes(x)) +
        stat_function(fun = pnorm, args = list(mean = input$norm_mean, sd = input$norm_sd), color = "#2c3e50") +
        theme_minimal() + labs(title = "Cumulative Distribution (CDF)", y = "F(x)")
    } else {
      ggplot(data.frame(x = c(0, 10/input$exp_lambda)), aes(x)) +
        stat_function(fun = pexp, args = list(rate = input$exp_lambda), color = "#c0392b") +
        theme_minimal() + labs(title = "Cumulative Distribution (CDF)", y = "F(x)")
    }
  })
  
  output$jointInputs <- renderUI({
    if(input$jointCase == "digital") {
      tagList(sliderInput("j_x", "Acceptable Bits (X):", min = 0, max = 4, value = 2, step = 1), sliderInput("j_y", "Dubious Bits (Y):", min = 0, max = 4, value = 1, step = 1), p(strong("INTERACTIVE & DIDACTIC EXAMPLE"), style = "color: #0073b7; margin-top: 15px; margin-bottom: 5px; font-size: 11px; letter-spacing: 0.5px;"), p(em("Note: Sum of X + Y cannot exceed 4.")))
    } else {
      tagList(sliderInput("j_tx", "Time Component X (h):", min = 0, max = 3000, value = 0), sliderInput("j_ty", "Time Spare Y (h):", min = 0, max = 3000, value = 2000), p(strong("INTERACTIVE & DIDACTIC EXAMPLE"), style = "color: #0073b7; margin-top: 15px; margin-bottom: 5px; font-size: 11px; letter-spacing: 0.5px;"), p(em("Note: Valid for x <= y (PDF restriction).")))
    }
  })
  
  output$jointPlot <- renderPlot({
    req(input$jointCase)
    if(input$jointCase == "digital") {
      req(input$j_x, input$j_y)
      df <- expand.grid(x = 0:4, y = 0:4) %>% filter(x + y <= 4)
      df$highlight <- (df$x == input$j_x & df$y == input$j_y)
      ggplot(df, aes(x, y)) + geom_tile(aes(fill = x+y, color = highlight, size = highlight)) + scale_color_manual(values = c("white", "orange"), guide = "none") + scale_size_manual(values = c(0.5, 2), guide = "none") + geom_text(aes(label = paste0("x=",x,"\ny=",y))) + scale_fill_gradient(low="#e1f5fe", high="#01579b") + theme_minimal() + labs(title="Joint PMF: Digital Information Transmission")
    } else {
      req(input$j_tx, input$j_ty) 
      grid <- expand.grid(x = seq(0, 2500, by = 50), y = seq(0, 3000, by = 50))
      grid$z <- 6e-6 * exp(-0.001 * grid$x - 0.002 * grid$y)
      ggplot(grid, aes(x, y)) + geom_contour_filled(aes(z = z), bins = 10) + geom_rect(aes(xmin = 0, xmax = input$j_tx, ymin = input$j_tx, ymax = input$j_ty), fill = "orange", alpha = 0.15, color = "orange", linetype = "dashed", size = 0.8) + geom_polygon(data = data.frame(x = c(0, 3010, 3010), y = c(0, 0, 3010)), aes(x = x, y = y), fill = "white", color = "white", size = 1) + geom_abline(intercept = 0, slope = 1, color = "#7f8c8d", size = 0.8) + annotate("point", x = input$j_tx, y = input$j_ty, color = "orange", size = 4) + theme_minimal() + coord_cartesian(xlim = c(0, 2500), ylim = c(0, 3000), expand = FALSE) + labs(title = "Joint PDF: 3D Printer Life Components", x = "Component Life X (h)", y = "Spare Life Y (h)", fill = "Density Level")
    }
  })
  
  output$jointResults <- renderUI({
    req(input$jointCase)
    if(input$jointCase == "digital") {
      req(input$j_x, input$j_y); n <- 4; x <- input$j_x; y <- input$j_y; z <- n - x - y
      if(z < 0) return(div(class="result-box", style="color:red", "Invalid: X+Y > 4"))
      prob <- (factorial(n)/(factorial(x)*factorial(y)*factorial(z))) * (0.9^x * 0.08^y * 0.02^z)
      div(class="result-box", strong(paste0("P(X=", x, ", Y=", y, ") = ", round(prob, 4))))
    } else {
      req(input$j_tx, input$j_ty); x_val <- input$j_tx; y_val <- input$j_ty
      if(x_val > y_val) return(div(class="result-box", style="color:red", "Invalid: Spare (Y) must be >= Component (X)"))
      term1 <- (1 - exp(-0.003 * x_val)) / 0.003
      term2 <- exp(-0.002 * y_val) * (1 - exp(-0.001 * x_val)) / 0.001
      prob <- 0.003 * (term1 - term2)
      div(class="result-box", strong(paste0("P(X < ", x_val, ", Y < ", y_val, ") = ", round(prob, 4))))
    }
  })
  
  output$jointTheory <- renderUI({
    req(input$jointCase)
    if(input$jointCase == "digital") {
      tagList(
        h3("Joint Probability Mass Function (PMF)"),
        p("Definition using the joint probability mass function for two-dimensional discrete random variables."),
        hr(),
        div(style = "background: #fdfefe; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0; margin-bottom: 15px;",
            h4(strong("Fundamental conditions of the joint probability function"), style = "color: #2c3e50; margin-top: 0;"),
            br(),
            p(strong("1. Non-negativity:")),
            withMathJax("$$f_{XY}(x, y) \\ge 0 \\quad \\text{para todo } x, y$$"),
            p("The probability assigned to any pair of values in the joint sample space must be non-negative."),
            br(),
            p(strong("2. Normalization condition:")),
            withMathJax("$$\\sum_{x} \\sum_{y} f_{XY}(x, y) = 1$$"),
            p("The sum of the probabilities associated with all possible combinations of values in the discrete support must be equal to unity."),
            br(),
            p(strong("3. Point probability relationship:")),
            withMathJax("$$f_{XY}(x, y) = P(X = x, Y = y)$$"),
            p("The mass function directly evaluates the probability of simultaneous occurrence of specific events.")
        )
      )
    } else {
      tagList(
        h3("Joint Probability Density Function (PDF)"), 
        p("Fundamental properties of two-dimensional continuous random variables."), 
        hr(), 
        div(style = "background: #fdfefe; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0; margin-bottom: 15px;",
            h4(strong("Fundamental conditions of the joint density function"), style = "color: #2c3e50; margin-top: 0;"),
            br(),
            p(strong("1. Non-negativity:")),
            withMathJax("$$f_{XY}(x, y) \\ge 0 \\quad \\text{for all } x, y$$"),
            br(),
            p(strong("2. Normalization condition (Total volume):")),
            withMathJax("$$\\int_{-\\infty}^{\\infty} \\int_{-\\infty}^{\\infty} f_{XY}(x, y) \\, dx \\, dy = 1$$"),
            br(),
            p(strong("3. Calculation of regional probability:")),
            withMathJax("$$P([X, Y] \\in R) = \\iint_{R} f_{XY}(x, y) \\, dx \\, dy$$")
        )
      )
    }
  })
  # --- LÓGICA PARA LOS BOUNDS (CÁLCULOS) ---
  output$est_lower <- renderText({
    req(input$estDataSource)
    # Reutilizamos la lógica de datos
    if (input$estDataSource == "example") {
      val <- c(41.92, 42.15, 41.68, 42.01, 41.88, 42.31, 41.52, 42.22, 41.74, 41.81)
    } else {
      req(input$estManualData)
      val <- as.numeric(unlist(strsplit(input$estManualData, "[,\\s]+")))
    }
    val <- na.omit(val)
    req(length(val) > 1)
    
    # Cálculo
    n <- length(val)
    media <- mean(val)
    sd_val <- sd(val)
    alpha <- 1 - input$est_conf
    # Usamos qt para intervalo t (más preciso para muestras pequeñas como n=10)
    # Nota: Tu texto UI dice Z, pero tu UI de configuración y gráfico sugieren muestra pequeña.
    # Si quieres Z estrictamente, usa qnorm(1 - alpha/2)
    error <- qt(1 - alpha/2, df = n - 1) * (sd_val / sqrt(n))
    
    round(media - error, 4)
  })
  
  output$est_upper <- renderText({
    req(input$estDataSource)
    if (input$estDataSource == "example") {
      val <- c(41.92, 42.15, 41.68, 42.01, 41.88, 42.31, 41.52, 42.22, 41.74, 41.81)
    } else {
      req(input$estManualData)
      val <- as.numeric(unlist(strsplit(input$estManualData, "[,\\s]+")))
    }
    val <- na.omit(val)
    req(length(val) > 1)
    
    n <- length(val)
    media <- mean(val)
    sd_val <- sd(val)
    alpha <- 1 - input$est_conf
    error <- qt(1 - alpha/2, df = n - 1) * (sd_val / sqrt(n))
    
    round(media + error, 4)
  })
  
  output$confPlot <- renderPlot({
    req(input$estDataSource)
    if (input$estDataSource == "example") {
      val <- c(41.92, 42.15, 41.68, 42.01, 41.88, 42.31, 41.52, 42.22, 41.74, 41.81)
    } else {
      req(input$estManualData)
      val <- as.numeric(unlist(strsplit(input$estManualData, "[,\\s]+")))
    }
    val <- na.omit(val)
    req(length(val) > 1)
    
    n_val <- length(val)
    mean_val <- mean(val)
    sd_val <- sd(val)
    alpha <- 1 - input$est_conf
    t_val <- qt(1 - alpha/2, df = n_val - 1)
    err <- t_val * (sd_val / sqrt(n_val))
    
    df_points <- data.frame(val = val, x = 1)
    df_bounds <- data.frame(x = 1.15, y = mean_val, lower = mean_val - err, upper = mean_val + err)
    
    ggplot() +
      geom_hline(yintercept = mean_val, linetype = "dashed", color = "#2c3e50", alpha = 0.6, size = 0.8) +
      geom_jitter(data = df_points, aes(x = x, y = val), width = 0.04, size = 3.5, color = "#7f8c8d", alpha = 0.7) +
      geom_point(aes(x = 1, y = mean_val), size = 5, color = "#2c3e50") +
      geom_errorbar(data = df_bounds, aes(x = x, ymin = lower, ymax = upper), width = 0.05, size = 1.2, color = "#3c8dbc") +
      geom_point(data = df_bounds, aes(x = x, y = y), size = 3, color = "#3c8dbc") +
      annotate("text", x = 0.93, y = mean_val, label = "Media (X)", color = "#2c3e50", fontface = "bold") +
      annotate("text", x = 1.23, y = mean_val, label = paste0("I.C. (", input$est_conf*100, "%)"), color = "#3c8dbc", fontface = "bold") +
      theme_minimal() +
      labs(title = "Sampling Distribution and Calculated Confidence Interval",
           subtitle = paste0("Metrics: Mean = ", round(mean_val, 3), " | s = ", round(sd_val, 3), " | n = ", n_val),
           y = "Value of the parameter / Variable", x = "") +  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(), plot.title = element_text(face = "bold", size = 14))
  })
  # --- Lógica ANOVA ---
  # --- LÓGICA ANOVA UNIFICADA (Sustituye todo lo anterior) ---
  # --- LÓGICA ANOVA UNIFICADA ---
  anovaData <- eventReactive(input$runAnova, {
    if (input$anovaType == "indep") {
      a <- as.numeric(unlist(strsplit(input$manualA, "[,\\s]+")))
      b <- as.numeric(unlist(strsplit(input$manualB, "[,\\s]+")))
      c <- as.numeric(unlist(strsplit(input$manualC, "[,\\s]+")))
      req(length(a) > 0, length(b) > 0, length(c) > 0)
      data.frame(Grupo = factor(c(rep("A", length(a)), rep("B", length(b)), rep("C", length(c)))),
                 Valor = c(a, b, c))
    } else {
      raw <- unlist(strsplit(input$pairedData, ";"))
      mat <- do.call(rbind, lapply(strsplit(raw, ","), as.numeric))
      df <- data.frame(Sujeto = factor(1:nrow(mat)), mat)
      colnames(df) <- c("Sujeto", "T1", "T2", "T3")
      
      # Corregido: 'names_to' genera "Tiempo" y 'values_to' genera "Valor"
      df %>% pivot_longer(cols = -Sujeto, names_to = "Tiempo", values_to = "Valor") %>%
        mutate(Tiempo = as.factor(Tiempo), Sujeto = as.factor(Sujeto))
    }
  })
  
  output$anovaPlot <- renderPlot({
    df <- anovaData(); req(nrow(df) > 0)
    if (input$anovaType == "indep") {
      ggplot(df, aes(x = Grupo, y = Valor, fill = Grupo)) + 
        geom_boxplot(alpha = 0.7) + theme_minimal() + labs(title = "Independent Groups Analysis")
    } else {
      ggplot(df, aes(x = Tiempo, y = Valor, group = Sujeto, color = Sujeto)) + 
        geom_line(size = 1) + geom_point(size = 3) + 
        theme_minimal() + labs(title = "Individual Trajectories (Repeated Measures)")
    }
  })
  
  output$anovaResult <- renderPrint({
    df <- anovaData(); req(nrow(df) > 0)
    if (input$anovaType == "indep") {
      print(summary(aov(Valor ~ Grupo, data = df)))
    } else {
      # ANOVA de medidas repetidas básico
      print(summary(aov(Valor ~ Tiempo + Error(Sujeto/Tiempo), data = df)))
    }
  })
  
  output$anovaExplanation <- renderUI({
    df <- anovaData(); req(nrow(df) > 0)
    # Modelo para extraer el p-valor de forma consistente
    res <- if(input$anovaType == "indep") aov(Valor ~ Grupo, data = df) else aov(Valor ~ Tiempo + Sujeto, data = df)
    
    # Extraer p-valor del factor principal
    p_val <- summary(res)[[1]][["Pr(>F)"]][1]
    
    req(!is.na(p_val))
    div(style = "padding:20px; border-radius:5px; background:#f4f4f4;",
        h4(if(p_val < 0.05) "Resultado: Estadísticamente Significativo" else "Resultado: No significativo"),
        p(paste("El p-valor obtenido es:", round(p_val, 5))),
        p(if(p_val < 0.05) "Existen diferencias significativas entre los grupos o condiciones." 
          else "No hay evidencia suficiente para rechazar la hipótesis de igualdad de medias.")
    )
  })
}

shinyApp(ui, server)