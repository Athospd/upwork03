#' viz03 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz03_ui <- function(id, width = 6, height = 500){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = width,
      title = "Poverty Ratio", 
      p("(Above average in red, below average in green)"),
      plotOutput(ns("viz"), height = paste0(height, "px"))
    )
  )
}
    
#' viz03 Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer renderPlot
mod_viz03_server <- function(id, year, country) {
  moduleServer(
    id,
    function(input, output, session) {
      output$viz <- renderPlot({
        shiny::validate(
          shiny::need(year(), "year"),
          shiny::need(country(), "country")
        )
        viz_03_poverty_ratio(year(), country())
      })
    }
  )
}
    
## To be copied in the UI
# mod_viz03_ui("viz03_ui_1")
    
## To be copied in the server
# callModule(mod_viz03_server, "viz03_ui_1")
 
