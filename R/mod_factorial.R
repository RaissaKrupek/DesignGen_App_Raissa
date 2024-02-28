#' assumptionsTest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_factorial_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:8000px",
             box(width = 12,
                 p("Here we present several tests for checking model assumptions for single trait and environment.")
             ),
             box(width = 12,
                 selectInput(ns("assum_design"), label = h4("Experiment design - Factorial Arrangement"), 
                             choices = list("Completely randomized (CRSP)" = "crsp" ,"Randomized complete block (RBSP)" = "rbsp", 
                                            selected = "rbsp")
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                     p("The input file is a tab delimited table with a column called 'local' defining the environment and 
                   other called 'gen' defining the genotypes. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                     downloadButton(ns("assum_input_exemple")), hr(),
                     p("Upload here your file:"),
                     fileInput(ns("data_assum"), label = h6("File: data.txt"), multiple = F),
                     p("If you do not have an file to be upload you can still check this app features with our example file. The example file is automatically upload, you just need to procedure to the other buttons."),
                     hr(),
                     p("Data View:"),
                     box(width = 4,
                         radioButtons(ns("assum6"), label = p("Select the separator"),
                                      choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                                      selected = ";")
                     ),
                     box(width = 8,  
                         #Visualização dos dados
                         tableOutput(ns("dataview"))
                     ),
                     hr(),
                     actionButton(ns("assum1"), "Read the file",icon("refresh")), hr()
                 ),
                 
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                     box(width = 12,
                         radioButtons(ns("assum4"), label = p("Select the transformation type:"),
                                      choices = list("none" = "none","log" = "log", "sqrt(x + 0.5)" = "sqrt(x + 0.5)", "boxcox" = "boxcox"),
                                      selected = "none"), hr(),
                         p("If your data still not present the assumptions safter transformation, we suggest the usage of Generalized Linear Models (still not implemented in this app).")
                     ),
                     hr(),
                     box(width = 6,
                         radioButtons(ns("assum7"), label = p("Choose the whole-plot factor to be evaluated:"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update"),
                     ),
                     box(width = 6,
                         radioButtons(ns("assum8"), label = p("Choose the split-plot factor to be evaluated:"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update"),
                     ),
                     hr(),
                     box(width = 6,
                         radioButtons(ns("assum2"), label = p("Choose the traits to be evaluated"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update"),
                     ),
                     box(width = 6,
                         radioButtons(ns("assum3"), label = p("Choose the location to be evaluated:"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update")
                         # ,
                         # hr(),
                         # actionButton(ns("assum5"), "Run analysis",icon("refresh")), br(),
                     ),
                     
                     actionButton(ns("assum5"), "Run analysis", icon("refresh")), hr(),
                     p("Expand the windows above to access the results")
                     
                     # box(width = 12,
                     #     p("Expand the windows above to access the results")
                     #     )
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Plots",
                     
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
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Anova",
                     DT::dataTableOutput(ns("assum_anova_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Shapiro-Wilk normality test",
                     p("H0: normal distribuition (p-value > 0.05)."),
                     p("H1: we can't consider that it is a normal distribuition (p-value < 0.05)."),
                     DT::dataTableOutput(ns("assum_sha_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Durbin-Watson Test for Autocorrelated Errors",
                     p("Computes residual autocorrelations and generalized Durbin-Watson statistics and their bootstrapped p-values."),
                     p("Used for normal distribuited longitudinal datasets. Usually, values of D-W between 1.5 and 2.5 indicate residual independence."),
                     DT::dataTableOutput(ns("assum_dur_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Breusch-Pagan Test",
                     p("Performs the Breusch-Pagan test against heteroskedasticity."),
                     p("Has as assumption normal distribuition."),
                     p("H0: there is homocedascity of variances (p-value > 0.05)."),
                     p("H1: we can't consider that there is homocedascity of variances (p-value < 0.05)."),
                     DT::dataTableOutput(ns("assum_bp_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Bonferroni-Holm tests for the adjusted p-values",
                     tableOutput(ns("assum_out_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Variance Inflation Factors",
                     p("Checking multicollinearity: VIF value higher than 10 indicates multicollinearity."),
                     tableOutput(ns("assum_vif_out"))
                 )
             )
    )
  )
}
