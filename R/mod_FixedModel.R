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
                   p("Here, we present several tests designed to assess model assumptions 
                      and variance analyses (ANOVA) for fixed-effects models, applied across multiple 
                      traits and environments.
                     For the analyses, factorial arrangements of treatments were implemented to better 
                     capture the effects and estimate interactions. Grouping experiments allowed
                     replication across different locations, enabling the assessment of the consistency of genotype performance."),
                   ),
               box(width = 12,
                   selectInput(ns("design"), label = h4("Experimental design - Factorial Arrangements"), 
                               choices = list("Randomized complete block" = "block", "Completely randomized" = "crfa"), 
                               selected = "block")
               ),
               # Input file
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="primary", title = "Input file",
                   p("The input file is a tab delimited table with a column called 'local' defining the environment, 
                   other called 'gen' defining the genotypes, other called 'season' defining the season and other called 'block' defining the block number. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                   downloadButton(ns("data_example")), hr(),
                   p("Upload here your file:"),
                   fileInput(ns("data_input"), label = h6("File: data.txt"), multiple = F),
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
                   actionButton(ns("read"), "Read the file",icon("refresh")), hr()
               ),
               
               # Select Variables
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="primary", title = "Select variables",
                   box(width = 6,
                       checkboxGroupInput(ns("local"), label = p("Choose the locations to be evaluated:"),
                                          choices = "Press 'Read the file' button to update",
                                          selected = "Press 'Read the file' button to update")
                   ),
                   box(width = 6,
                       checkboxGroupInput(ns("factors"), label = p("Choose the factors to be evaluated:"),
                                          choices = "Press 'Read the file' button to update",
                                          selected = "Press 'Read the file' button to update")
                   ),
                   box(width = 6,
                       radioButtons(ns("trait"), label = p("Choose the trait to be evaluated:"),
                                    choices = "Press 'Read the file' button to update",
                                    selected = "Press 'Read the file' button to update"),
                   ),
                   actionButton(ns("run"), "Run analysis", icon("refresh")), 
                   hr(),
                   
                   box(width = 12,
                       p("Expand the windows above to access the results")
                   )
        
               ),
               
               # treatment-vs-environment interaction
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Graphical analysis of treatment-vs-environment interaction"
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
                   )
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