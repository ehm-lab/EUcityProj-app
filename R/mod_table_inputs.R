#' table_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("lev_per"), "View by:", choices = c("Warming level","Five-year periods")),
    selectizeInput(ns("area"), label=NULL, choices = c("City","Country","Regions"))
  )
}

#' table_inputs Server Functions
#'
#' @noRd
mod_table_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 return(
   list(
     lev_per = reactive(input$lev_per),
     area = reactive(input$area)
   )
 )
    })
}

## To be copied in the UI
# mod_table_inputs_ui("table_inputs_1")

## To be copied in the server
# mod_table_inputs_server("table_inputs_1")
