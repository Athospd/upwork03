#' viz01 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz01_ui <- function(id, height = 500){
  ns <- NS(id)
  tagList(
    p("Average poverty rate against average health expenditures as % of GDP,
           against average % of population with basic sanitation, and against 
           average % of population drinking clean water"),
    p("Average of top 40 countries"),
    plotOutput(ns("viz"), height = paste0(height, "px"))
  )
}
    
#' viz01 Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer renderPlot
mod_viz01_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      averages <- reactive({
        upwork03::averages
      })
      
      output$viz <- renderPlot({
        averages() %>% upwork03::viz_01_poverty_scatterplot() 
      })
    }
  )
}
    
## To be copied in the UI
# mod_viz01_ui("viz01_ui_1")
    
## To be copied in the server
# callModule(mod_viz01_server, "viz01_ui_1")
 
