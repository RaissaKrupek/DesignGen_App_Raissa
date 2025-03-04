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
#' @import GAD
#' 
#' @noRd 
#' 
mod_FixedModel_server <- function(input, output, session){
  ns <- session$ns
  
  output$data_exemple <- downloadHandler(
    filename =  function() {
      if(input$design == "block"){
        paste("example_factorial_dbc.txt")
      } 
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$assum_design == "block"){
        dat <- read.csv("~/STATGEN/Iniciacao Cientifica/DesignGen_App_Raissa/Examples/example_factorial_dbc.csv")
      } 
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  # Observe the file input and display the data .txt, .xls e .csv 
  observeEvent(input$separator, {
    if (is.null(input$data_input)) {
      output$dataview <- renderTable({
        return(p("Please upload your file to update this section."))
      })
    } else {
      dat <- read.csv(input$data_input$datapath, sep = input$separator)
      output$dataview <- renderTable({
        return(head(dat))
      })
    }
  })
  
  # Data Loading
  button1 <- eventReactive(input$read, {
    #Aqui esta pegando os exemplos
    if(is.null(input$data_input$datapath)){
      if(input$assum_design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } 
    } else {
      #Aqui entra o upload
      dat <- read.csv(input$data_input$datapath,
                      sep = input$separator)
    }
    cat(colnames(dat))
    dat
  })
  
  # Dynamic UI for filtering data
  observeEvent(input$data_input, {
    output$filter_dynamic_factor <- renderUI({
      req(input$filter_choice == "Yes")
      data_names <- colnames(button1())  # Obtém os nomes das colunas
      
      box(width = 12,
          checkboxGroupInput(ns("factor"), label = p("Choose the factors to be filtered:"),
                             choices = data_names,
                             selected = data_names[1])
      )
    })
    showNotification("Data loaded")
  })
  
  observeEvent(input$data_input, {
    output$filter_dynamic_button <- renderUI({
      req(input$filter_choice == "Yes")
      dat <- button1()    # Obtém os dados
      
      actionButton(ns("filter_in_process"), "Select levels", icon("plus"))  # botão será usado para atualizar os níveis das colunas selecionadas.
    })
  })
  
  # Reactive filter data
  observeEvent(input$data_input, {
    output$filter_dynamic_level <- renderUI({
      req(input$filter_choice == "Yes")
      
      num <- length(input$factor) # Conta quantos fatores foram selecionados
      col_names <- input$factor   # Obtem os nomes dos fatores
      
      lapply(seq_len(num), function(i) {  # Itera sobre cada fator 
        box(width = 12,
            checkboxGroupInput(ns(paste0("filter", i)),  # Cria inputs dinâmicos
                               label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                               choices = "Press 'Select levels' button to update")
        )
      })
    })
  })
  
  observeEvent(input$filter_in_process, {
    req(input$filter_choice == "Yes")
    
    dat <- button1()
    if (length(input$factor) > 0) {
      n <- length(input$factor)
      for (i in 1:n) {
        dat[[input$factor[i]]] <- as.factor(dat[[input$factor[i]]])  # Converte para fator
      }
    }
    
    num <- length(input$factor)
    col_names <- input$factor
    
    lapply(seq_len(num), function(i) {
      if(is.factor(dat[[input$factor[i]]])) {
        box(width = 12,
            updateCheckboxGroupInput(session, paste0("filter", i),
                                     label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                                     choices = unique(dat[[input$factor[i]]]))
        )
      }
    })
  })
  
  # Data Filtering
  button2 <- eventReactive(input$filter_ready, {
    if(input$filter_choice == "Yes") {
      dat <- button1()
      if (length(input$factor) > 0) {
        n <- length(input$factor)
        for (i in 1:n) {
          dat[[input$factor[i]]] <- as.factor(dat[[input$factor[i]]])  # Converte para fator
        }
      }
      
      num <- length(input$factor)
      col_names <- input$factor
      
      for (i in 1:num) {
        dat <- dat %>%
          filter(dat[[input$factor[i]]] %in% c(input[[paste0("filter", i)]])) %>%  # Filtra os valores selecionados
          droplevels()  # Remove níveis não utilizados
      }
      dat
      
    } else {
      dat <- button1()
      dat # Retorna os dados sem filtro
    }
  })
  
  # Update choices for analysis
  observeEvent(input$filter_ready, {
    data_names <- colnames(button1())
    
    updateRadioButtons(session, "local",
                       label="Choose the 'environment' variable to be evaluated:",
                       choices = data_names,
                       selected = unlist(data_names)[1])
    
    updateRadioButtons(session, "factor1",
                       label="Choose the first factor to be evaluated:",
                       choices = data_names)
    
    updateRadioButtons(session, "factor2",
                       label="Choose the first factor to be evaluated:",
                       choices = data_names)
    
    updateRadioButtons(session, "trait",
                       label="Choose the 'trait' variable to be evaluated:",
                       choices = data_names,
                       selected = unlist(data_names)[1])
  
  })
  
  #ler explicacao do chat
  
  # -----------------------------

  
}