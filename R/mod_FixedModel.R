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
                   selectInput(ns("design"), label = h4("Experiment design - Factorial Arrangements"), 
                               choices = list("Randomized complete block" = "block", "Completely randomized" = "crfa"), 
                               selected = "block")
               ),
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="primary", title = "Input file"
               ),
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="primary", title = "Select variables"
               ),
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Graphical analysis of treatment-vs-environment interaction"
               ),
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "ANOVA"
               ),
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Assumptions plots"
               ),
               box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status="info", title = "Assumptions tests"
               ),
                   ))
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