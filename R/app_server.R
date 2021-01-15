#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  output$country <- renderUI({
    shinyWidgets::pickerInput(
      "country",
      label = "Countries: ",
      multiple = TRUE,
      choices = unique(upwork03::poverty_average$`Country Name`),
      selected = unique(upwork03::poverty_average$`Country Name`),
      width = '100%',
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE
      )
    )
  })
  
  # List the first level callModules here
  year = shiny::reactive({input$year})
  TMH = shiny::reactive({input$TMH})
  country = shiny::reactive({input$country})
  mod_viz01_server("viz01")
  mod_viz02_server("viz02", year = year, country = country)
  mod_viz03_server("viz03", year = year, country = country)
  mod_viz04_server("viz04", year = year, TMH = TMH, country = country)
  mod_viz05_server("viz05", country = country)
}
