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
  tagList()
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