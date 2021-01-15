#' viz04 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz04_ui <- function(id, width = 6, height = 500){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = width,
      title = "Highest Incidence", 
      uiOutput(ns("title")),
      plotOutput(ns("viz"), height = paste0(height, "px"))
    )
  )
}
    
#' viz04 Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer renderPlot
mod_viz04_server <- function(id, year, TMH, country) {
  moduleServer(
    id,
    function(input, output, session) {
      
      title <- reactive({
        glue::glue("Incidence of Tuberculosis in 2017")
      })
      
      output$title <- renderUI({
        p(title())
      })
      
      output$viz <- renderPlot({
        viz_04_highest_incidence(year = year(), TMH = TMH(), country = country())
      })
    }
  )
}
    
## To be copied in the UI
# mod_viz04_ui("viz04_ui_1")
    
## To be copied in the server
# callModule(mod_viz04_server, "viz04_ui_1")
 
