#' viz05 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz05_ui <- function(id, width = 6, height = 500){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = width,
      title = "Incidence Growth", 
      uiOutput(ns("title")),
      plotOutput(ns("viz"), height = paste0(height, "px"))
    )
  )
}
    
#' viz05 Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer renderPlot
mod_viz05_server <- function(id, country) {
  moduleServer(
    id,
    function(input, output, session) {
      
      title <- reactive({
        glue::glue("Growth rates trend of average incidence of HIV, Malaria and Tuberculosis")
      })
      
      output$title <- renderUI({
        p(title())
      })
      
      output$viz <- renderPlot({
        viz_05_incidence_growth(country())
      })
    }
  )
}
    
## To be copied in the UI
# mod_viz05_ui("viz05_ui_1")
    
## To be copied in the server
# callModule(mod_viz05_server, "viz05_ui_1")
 
