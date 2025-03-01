#' assumptionsTest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_FixedModel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:5000px",
             box(width = 12, 
                 p(HTML("On this page, we present several tests designed to assess model assumptions 
                      and variance analyses (ANOVA) for fixed-effects linear models, applied across multiple 
                      environments.For the analyses, factorial arrangements of treatments were implemented to better 
                      capture the effects and estimate interactions. Grouping experiments allowed
                      replication across different locations, enabling the assessment of the consistency of genotype performance.
                      <ul>
                  Please keep the following points in mind:
                 <ul>
                 <li>Follow each step and press the button at the end of each one;</li>
                 <li>If you select something incorrectly or wish to change, you can return to the specific section to modify it and proceed with the subsequent steps;</li>
                        </ul>")),
             ),
             box(width = 12,
                 selectInput(ns("design"), label = h4("Experimental design - Factorial Arrangements"), 
                             choices = list("Randomized complete block" = "block"), 
                             selected = "block")
             ),
             # Input file
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="primary", title = "Input file",
                 p("The input file is a tab delimited table with a column called 'local' defining the environment, 
                   other called 'gen' defining the genotypes, other called 'season' defining the season and other called 'block' defining the block number. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                 downloadButton(ns("data_example")), hr(),
                 p("Upload here your file:"),
                 fileInput(ns("data_input"), label = h6("Select file: .csv .xls .txt"), multiple = F),
                 p("If you do not have an file to be upload you can still check this app features with our example file. 
                     The example file is automatically upload, you just need to procedure to the other buttons."), hr(),
                 p("Data View:"),
                 box(width = 4,
                     radioButtons(ns("separator"), label = p("Select the separator"),
                                  choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                                  selected = ";")
                 ),
                 
                 #Visualização dos dados
                 box(width = 8, tableOutput(ns("dataview"))
                 ), hr(),
                 box(width = 12,
                     actionButton(ns("read"), "Read the file",icon("file-text"))), 
                 hr()
             ),
             
             # Data Processing
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status= "primary", title = "Data Processing",
                 box(width = 12,
                     radioButtons(ns("transform"), label = p("Select the transformation type:"),
                                  choices = list("none" = "none","log" = "log", "sqrt(x + 0.5)" = "sqrt(x + 0.5)", "boxcox" = "boxcox"),
                                  selected = "none"), hr(),
                     p("If your data still not present the assumptions safter transformation, we suggest the usage of Generalized Linear Models (still not implemented in this app).")
                 ),
                 hr(),
                 
                 box(width = 12,
                     p(HTML("In this step, you will be able to filter the levels of the factors you want to analyse, as well as the locations you want to evaluate.")),
                     radioButtons(ns("filter_choice"), label = p("Do you need to filter your data?"),
                                  choices = c("Yes", "No"),
                                  selected = "No"),
                     p("If you don't need to filter your data, just press the 'Data Filters' button and continue with the next steps.")
                 ),
                 
                 uiOutput(ns("filter_dynamic_factor")),
                 br(),
                 uiOutput(ns("filter_dynamic_button")),
                 br(),
                 uiOutput(ns("filter_dynamic_level")),
                 
                 actionButton(ns("filter_ready"), "Filter Data", icon("filter")),
                 br(),
                 h6("Click here to proceed to the next step")
             ),
             
             # Select Parameters
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status= "primary", title = "Select parameters",
                 box(width = 6,
                     checkboxGroupInput(ns("local"), label = p("Choose the 'environment' variable to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 ),
                 box(width = 6,
                     radioButtons(ns("factor1"), label = p("Choose the first factor to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update")
                 ),
                 box(width = 6,
                     radioButtons(ns("factor2"), label = p("Choose the second factor to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update")
                 ),
                 box(width = 6,
                     radioButtons(ns("trait"), label = p("Choose the 'trait' variable to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update"),
                 ),
                 
                 box(width = 12,
                     actionButton(ns("run"), "Run analysis", icon("refresh")), 
                     hr(),
                     p("Expand the windows above to access the results")
                 )
                 
             ),
             
             # Exploratory Data Analysis (EDA)
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Exploratory Data Analysis (EDA)"
             ),
             
             # treatment-vs-environment interaction (GEI)
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Genotype by Environment Interaction (GEI)"
             ),
             
             # ANOVA
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "ANOVA",
                 DT::dataTableOutput(ns("assum_anova_out"))
             ),
             
             # Assumptions Plots
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Assumptions plots",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Residuals vs fitted values",
                     p("Checking the linearity of the model: if the blue line is close to horizontal line."),
                     p("Checking the homoscedasticity of variances: retangular draw of the dots (not triangular)."),
                     plotOutput(ns("assum1_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Normal Q-Q",
                     p("Checking the residuals normal distribution: dots should overlap the dashed line."),
                     plotOutput(ns("assum2_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Scale-Location",
                     p("Checking the homoscedasticity or homogeneity of variances: line should be close to horizontal line and dots should present a retangular draw"),
                     plotOutput(ns("assum3_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Residuals vs factor levels",
                     p("Checking outilers: red line should overlay the dashed line and the dots should not be out of a determined range (e.g. [-2,2])."),
                     plotOutput(ns("assum4_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Histogram of residuals",
                     plotOutput(ns("assum5_plot_out")),
                 )
             ),
             
             # Assumptions Tests
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Assumptions tests",
                 # Shapiro-Wilk
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Shapiro-Wilk normality test",
                     p("H0: normal distribuition (p-value > 0.05)."),
                     p("H1: we can't consider that it is a normal distribuition (p-value < 0.05)."),
                     DT::dataTableOutput(ns("assum_sha_out"))
                 ),
                 
                 # Durbin-Watson
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Durbin-Watson Test for Autocorrelated Errors",
                     p("Computes residual autocorrelations and generalized Durbin-Watson statistics and their bootstrapped p-values."),
                     p("Used for normal distribuited longitudinal datasets. Usually, values of D-W between 1.5 and 2.5 indicate residual independence."),
                     DT::dataTableOutput(ns("assum_dur_out"))
                 ),
                 
                 # Breusch-Pagan
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Breusch-Pagan Test",
                     p("Performs the Breusch-Pagan test against heteroskedasticity."),
                     p("Has as assumption normal distribuition."),
                     p("H0: there is homocedascity of variances (p-value > 0.05)."),
                     p("H1: we can't consider that there is homocedascity of variances (p-value < 0.05)."),
                     DT::dataTableOutput(ns("assum_bp_out"))
                 )
             ),
             
             # Download
             
             box(width = 12,
                 p(HTML("Click here to download the complete analysis data in '.RData' format.  
                       Once you import this into R or RStudio, an object named 'Fixedmodel-GroupsOfExperiments' will be created, enabling you to work with it.")),
                 downloadButton(ns('download_rdata'), "Download .RData", class = "butt"))
             
    )
  )
}


#' assumptionsTest Server Function
#'
#' @import ggfortify
#' @import ggplot2
#' @import lmtest
#' @import car
#' @import psych
#' @import multtest
#' @import dplyr
#' @import emmeans
#' 
#' @noRd 
#' 
mod_FixedModel_server <- function(input, output, session){
  ns <- session$ns
}