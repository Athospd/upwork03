#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinymaterial
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shiny::navbarPage(selected = "Home",
      shinyWidgets::useShinydashboard(),
      title = "Poverty Viz",
      shiny::tabPanel(
        title = "Home",
        
        column(
          width = 12,
          shinydashboard::box(
            width = 12,
            col_3(
              shiny::selectizeInput("year", label = "Year", choices = unique(upwork03::averages$year), selected = 2017)
            ),
            col_3(
              shiny::selectizeInput("TMH", label = "TMH", choices = unique(highest_incidence$TMH), selected = "Tuberculosis")
            ),
            col_6(
              uiOutput(("country"))
            )
          )
        ),
        fluidRow(
            mod_viz02_ui("viz02", 7, 400),
            mod_viz05_ui("viz05", 5, 400)
        ),
        fluidRow(
          mod_viz03_ui("viz03", 6, 700),
          mod_viz04_ui("viz04", 6, 700)
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    golem::activate_js(),
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Poverty Viz'
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

