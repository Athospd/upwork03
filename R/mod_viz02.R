#' viz02 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz02_ui <- function(id, width = 6, height = 500 ){
  ns <- NS(id)
  tagList(
    shinydashboard::tabBox(
      width = width,
      title = "Correlation Heatmap",
      shiny::tabPanel(
        "Poverty Scatterplot",
        mod_viz01_ui("viz01", height)
      ),
      shiny::tabPanel(
        ("Indicators"),
        p("Poverty ratio against health expenditure, 
          % of population drinking clean water, 
          and % of population with basic sanitation"),
        plotOutput(ns("A"), height = paste0(height, "px"))
      ),
      shiny::tabPanel(
        ("TMH"),
        p("Poverty ratio against the incidence of 
          Tuberculosis, Malaria and HIV"),
        plotOutput(ns("B"), height = paste0(height, "px"))
      )
    )
  )
}
    
#' viz02 Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer renderPlot
mod_viz02_server <- function(id, year, country) {
  stopifnot(is.reactive(year))
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$A <- renderPlot({
        shiny::validate(
          shiny::need(year(), "year"),
          shiny::need(country(), "country")
        )
        upwork03::viz_02_correlation_heatmap(
          year(),
          country()
        )
      })
      
      output$B <- renderPlot({
        shiny::validate(
          shiny::need(year(), "year"),
          shiny::need(country(), "country")
        )
        upwork03::viz_02_correlation_heatmap(
          year(), 
          country(),
          vars = c("Poverty", "Tuberculosis", "Malaria", "HIV")
        )
      })
    }
  )
}
    
## To be copied in the UI
# mod_viz02_ui("viz02_ui_1")
    
## To be copied in the server
# callModule(mod_viz02_server, "viz02_ui_1")
 
